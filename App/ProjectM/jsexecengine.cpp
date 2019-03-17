#include "jsexecengine.h"
#include <QDebug>
#include <QUrl>
#include <QUrlQuery>
#include <QNetworkReply>

JSExecEngine::JSExecEngine(QString *_baseURL, QString *_projExt, QObject *parent) : QObject(parent)
{
    baseURL = QString(*_baseURL + (standardEnd(_baseURL) ? "" : "/") + "cgi+bin/main.cgi");
    projURL = QString(*_baseURL + (standardStart(_projExt) && !standardEnd(_baseURL) ? "/" : "") +
                      "cgi-bin/" + *_projExt);
    qDebug() << "Base URL: " << baseURL;
    qDebug() << "Project URL: " << projURL;

    netHub = new QNetworkAccessManager(this);
}

JSExecEngine::~JSExecEngine()
{
    netHub->deleteLater();
}

void JSExecEngine::exists_user(QString *userID)
{
    nethub_poll *instruct = new nethub_poll();
    instruct->queryType = getUser;
    instruct->userID = userID;
    instruct->returnSignal = existsUser;
    buildRequest(instruct);
    QNetworkReply *reply = netHub->get(*instruct->request);
    connect(reply, &QNetworkReply::finished, this, [reply, instruct,this](){this->parseReturn(reply, instruct);});
    netHub->get(*instruct->request);
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
        case noQuery : return;

    }
}

void JSExecEngine::parseReturn(QNetworkReply *reply, nethub_poll *instruct)
{ //Needs a test
    if (reply->error() == QNetworkReply::NoError) {
        QByteArray data = reply->readAll();
        if (instruct == nullptr) return;
        switch(instruct->returnSignal) {
        case existsUser : {
            if (data.startsWith("{}")) emit exists_user_result(false);
            else emit exists_user_result(true);
            break;
        };
        case noSignal   : return;
        }
    }

    reply->deleteLater();
    deleteNethubPoll(instruct);
}

void JSExecEngine::deleteNethubPoll(JSExecEngine::nethub_poll *poll)
{
    if (poll == nullptr) return;
    if (poll->request != nullptr) delete poll->request;
    delete poll;
}
