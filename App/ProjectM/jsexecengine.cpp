#include "jsexecengine.h"
#include <QDebug>
#include <QUrl>
#include <QUrlQuery>
#include <QNetworkReply>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

JSExecEngine::JSExecEngine(QString _baseURL, QString _projExt, QObject *parent, QPlainTextEdit *editor) : QObject(parent)
{
    baseURL = QString("http://" + _baseURL + (standardEnd(&_baseURL) ? "" : "/") + "cgi-bin/main.cgi");
    projURL = QString("http://" + _baseURL + (standardStart(&_projExt) && !standardEnd(&_baseURL) ? "/" : "") +
                      "cgi-bin/" + _projExt);
    logger[1] << "Base URL: " += baseURL;
    logger[1] << "Project URL: " += projURL;

    netHub = new QNetworkAccessManager(this);
    logger = Logger(editor);
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
    logger[1] << QString("Started existance check ") + instr->request->url().toString();
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
    logger[1] << QString("Started user registration ") + instr->request->url().toString();

}

void JSExecEngine::get_projects()
{
    nethub_poll *instr = new nethub_poll();
    instr->queryType = getProjs;
    instr->returnSignal = retProjs;
    buildRequest(instr);
    QNetworkReply *reply = netHub->get(*instr->request);
    connect(reply, &QNetworkReply::finished, this, [reply,instr,this](){this->parseReturn(reply, instr);});
    logger[1] << QString("Started get projects request ") + instr->request->url().toString();
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
            query.addQueryItem("id", *inst->userID);
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
        logger[1] << "returned: " += QString::fromUtf8(data);
        if (instr == nullptr) return;
        switch(instr->returnSignal) {
        case existsUser : {
            if (data.startsWith("{}")) emit exists_user_result(false);
            else emit exists_user_result(true);
            break;
        };
        case userReg : {
            if (data.startsWith("{}")) {
                emit register_user_result("");
                return;
            }
            else {
                QJsonDocument doc = QJsonDocument::fromJson(QString::fromUtf8(data).toUtf8());
                QJsonObject obj;
                if (doc.isNull() || !doc.isObject()) {emit register_user_result(""); logger[1] << "doc is null or not an object"; return;}
                obj = doc.object();
                emit register_user_result(QString::number(obj["userID"].toDouble()));
            }
            break;
        };
        case retProjs : {
            QJsonDocument doc = QJsonDocument::fromJson(QString::fromUtf8(data).toUtf8());
            QJsonObject projectObj;
            QJsonArray projects;
            if (doc.isNull() || !doc.isObject()) {emit get_projects_result(QLinkedList<Project>()); logger[1] << "doc is null or not an object"; return;}
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
            break;
        };
        case noSignal   : return;
        }
    }

    reply->deleteLater();
    deleteNethubPoll(instr);
}

void JSExecEngine::deleteNethubPoll(JSExecEngine::nethub_poll *poll)
{
    if (poll == nullptr) return;
    if (poll->request != nullptr) delete poll->request;
    if (poll->userID != nullptr) delete poll->userID;
    delete poll;
}
