#include "jsexecengine.h"
#include <QDebug>
#include <QUrl>
#include <QUrlQuery>
#include <QNetworkReply>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QStandardPaths>
#include <QFileInfo>
#include <QFile>
#include <QTextStream>

JSExecEngine::JSExecEngine(QString _baseURL, QString _projExt, QObject *parent, QPlainTextEdit *editor) : QObject(parent)
{
    logger = Logger(editor, "JSExecEngine")[1];
    baseURL = QString("http://" + _baseURL + (standardEnd(&_baseURL) ? "" : "/") + "cgi-bin/main.cgi");
    projURL = QString("http://" + _baseURL + (standardStart(&_projExt) && !standardEnd(&_baseURL) ? "/" : "") +
                      "cgi-bin/" + _projExt);
    logger << "Base URL: " += baseURL;
    logger << "Project URL: " += projURL;

    netHub = new QNetworkAccessManager(this);
}

JSExecEngine::~JSExecEngine()
{
    netHub->deleteLater();
}

void JSExecEngine::exists_user(QString userID)
{
    nethub_poll *instr = new nethub_poll();
    instr->queryType = getUser;
    instr->userID = new QString(userID);
    instr->returnSignal = existsUser;
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger << QString("Started existance check ") + instr->request->url().toString();
}

void JSExecEngine::register_user(QString firstName, QString lastName)
{
    nethub_poll *instr = new nethub_poll();
    instr->queryType = regUser;
    instr->returnSignal = userReg;
    nethub_poll_data_names *names = new nethub_poll_data_names;
    names->firstName = QString(firstName);
    names->lastName  = QString(lastName);
    instr->data.names = names;
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger << QString("Started user registration ") + instr->request->url().toString();

}

void JSExecEngine::get_projects()
{
    nethub_poll *instr = new nethub_poll();
    instr->queryType = getProjs;
    instr->returnSignal = retProjs;
    instr->userID = new QString(getUserID());
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger << QString("Started get projects request ") + instr->request->url().toString();
}

void JSExecEngine::run_project()
{
    if (!projURL.endsWith(".cgi")) {
        logger << "Ignored project request" += "No project URL provided";
        return;
    }
    nethub_poll *instr = new nethub_poll();
    instr->queryType = getJS;
    instr->returnSignal = jsReady;
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger << QString("Requested js ") + instr->request->url().toString();
}

bool JSExecEngine::standardEnd(QString *check)
{
    int len = check->length();
    QChar lastChar = check->data()[len-1];
    return lastChar == '/';
}

bool JSExecEngine::standardStart(QString *check)
{
    QChar firstChar = check->data()[0];
    return firstChar != "/";
}

void JSExecEngine::buildRequest(JSExecEngine::nethub_poll *inst)
{
    switch (inst->queryType) {
        case getUser : {
            QUrl url(baseURL);
            QUrlQuery query;
            query.addQueryItem("action","GetUser");
            query.addQueryItem("id", *inst->userID);
            url.setQuery(query);
            inst->request = new QNetworkRequest(url);
            break;
        }
        case regUser : {
            QUrl url(baseURL);
            QUrlQuery query;
            query.addQueryItem("action","RegUser");
            query.addQueryItem("firstName", inst->data.names->firstName);
            query.addQueryItem("secondName", inst->data.names->lastName);
            url.setQuery(query);
            inst->request = new QNetworkRequest(url);
            break;
        }
        case getProjs : {
            QUrl url(baseURL);
            QUrlQuery query;
            query.addQueryItem("action","GetTasks");
            url.setQuery(query);
            inst->request = new QNetworkRequest(url);
            break;
        }
        case getJS : {
            QUrl url(projURL);
            QUrlQuery query;
            query.addQueryItem("event","RequestJS");
            url.setQuery(query);
            inst->request = new QNetworkRequest(url);
            break;
        }
        case getJSInput : {
            QUrl url(projURL);
            QUrlQuery query;
            query.addQueryItem("event","RequestInput");
            url.setQuery(query);
            inst->request = new QNetworkRequest(url);
            break;
        }
        case returnJS : {
            QUrl url(projURL);
            QUrlQuery query;
            query.addQueryItem("event",QString("ReturnAns ") + inst->data.result);
            url.setQuery(query);
            inst->request = new QNetworkRequest(url);
            break;
        }
        case noQuery : return;

    }
}

void JSExecEngine::parseReturn(QNetworkReply *reply, nethub_poll *instr)
{ //TODO: James -> Error signal?
    if (reply->error() == QNetworkReply::NoError) {
        QByteArray data = reply->readAll();
        logger << "returned: " += QString::fromUtf8(data);
        if (instr == nullptr) return;
        switch(instr->returnSignal) {
        case existsUser : {
            if (data.startsWith("{}")) emit exists_user_result(false);
            else emit exists_user_result(true);
            deleteNethubPoll(instr);
            break;
        }
        case userReg : {
            if (data.startsWith("{}")) {
                emit register_user_result("");
                return;
            }
            else {
                QJsonDocument doc = QJsonDocument::fromJson(QString::fromUtf8(data).toUtf8());
                QJsonObject obj;
                if (doc.isNull() || !doc.isObject()) {emit register_user_result(""); logger << "doc is null or not an object"; return;}
                obj = doc.object();
                emit register_user_result(QString::number(obj["userID"].toDouble()));
            }
            deleteNethubPoll(instr);
            break;
        }
        case retProjs : {
            QJsonDocument doc = QJsonDocument::fromJson(QString::fromUtf8(data).toUtf8());
            QJsonObject projectObj;
            QJsonArray projects;
            if (doc.isNull() || !doc.isObject()) {emit get_projects_result(QLinkedList<Project>()); logger << "doc is null or not an object"; return;}
            projectObj = doc.object();
            projects = projectObj["projects"].toArray();
            QLinkedList<Project> retList;
            Q_FOREACH(const QJsonValue &val, projects) {
                QJsonObject project = val.toObject();
                Project p;
                p.name        = project["name"].toString();
                p.description = project["desc"].toString();
                p.URL         = project["url" ].toString();
                retList << p;
            }
            emit get_projects_result(retList);
            deleteNethubPoll(instr);
            break;
        }
        case jsReady : {
            QString unfilteredJS = QString::fromUtf8(data);
            QString filteredJS = unfilteredJS.mid(6, unfilteredJS.length()-7);
            logger << "Got JS" += filteredJS;
            get_project_input(filteredJS, instr);
            break;
        }
        case jsInputReady : {
            QString unfilteredIn = QString::fromUtf8(data);
            QString filteredIn = unfilteredIn.mid(6, unfilteredIn.length()-7);
            logger << "Got JS" += filteredIn;
            QJSEngine engine;
            QJSValue fun = engine.evaluate(*instr->data.js);
            QJSValueList args;
            args << *instr->userID << filteredIn;
            QJSValue ret = fun.call(args);
            return_answer(ret.toString(), instr);
            break;
        }
        case noSignal   : return;
        }
    }

    reply->deleteLater();
}

void JSExecEngine::deleteNethubPoll(JSExecEngine::nethub_poll *poll)
{//TODO: James -> Remove union members
    if (poll == nullptr) return;
    if (poll->request != nullptr) delete poll->request;
    if (poll->userID != nullptr) delete poll->userID;
    delete poll;
}

QString JSExecEngine::getUserID()
{
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    auto fileName= path + "/userID.txt"; //TODO: James -> Add to magic strings
    QFileInfo checkFile(fileName);
    if (checkFile.exists() && checkFile.isFile()) {
        QFile file(fileName);
        file.open(QIODevice::ReadOnly);
        QTextStream in(&file);
        QString id = in.readLine();
        file.close();
        return id;
    }
    return "";
}

void JSExecEngine::get_project_input(QString js, nethub_poll *instr)
{
    instr->queryType = getJSInput;
    instr->returnSignal = jsInputReady;
    instr->data.js = new QString(js);
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger << QString("Requested js Input") + instr->request->url().toString();
}

void JSExecEngine::return_answer(QString result, JSExecEngine::nethub_poll *instr)
{
    instr->queryType = returnJS;
    instr->returnSignal = noSignal;
    if (instr->data.js != nullptr) delete instr->data.js;
    instr->data.result = new QString (result);
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger << QString("Returning Answer") + instr->request->url().toString();
}
