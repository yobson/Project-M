#include "jsexecengine.h"
#include <QDebug>
#include <QUrl>
#include <QUrlQuery>
#include <QNetworkReply>
#include <QJsonDocument>
#include <QJsonObject>

JSExecEngine::JSExecEngine(QString _baseURL, QString _projExt, QObject *parent) : QObject(parent)
{
    baseURL = QString("http://" + _baseURL + (standardEnd(&_baseURL) ? "" : "/") + "cgi-bin/main.cgi");
    projURL = QString("http://" + _baseURL + (standardStart(&_projExt) && !standardEnd(&_baseURL) ? "/" : "") +
                      "cgi-bin/" + _projExt);
    qDebug() << "Base URL: " << baseURL;
    qDebug() << "Project URL: " << projURL;

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
    qDebug() << "Started existance check " << instr->request->url();
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
    qDebug() << "Started user registration: " << instr->request->url();

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
        case noQuery : return;

    }
}

void JSExecEngine::parseReturn(QNetworkReply *reply, nethub_poll *instr)
{ //Needs a test
    if (reply->error() == QNetworkReply::NoError) {
        QByteArray data = reply->readAll();
        qDebug() << "returned: " << QString::fromUtf8(data);
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
                if (doc.isNull() || !doc.isObject()) {emit register_user_result(""); qDebug() << "doc is null or not an object"; return;}
                obj = doc.object();
                qDebug() << "userID: " << obj["userID"];
                emit register_user_result(QString::number(obj["userID"].toDouble()));
            }
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
