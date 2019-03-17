#include "jsexecengine.h"
#include <QDebug>
#include <QUrl>
#include <QUrlQuery>

JSExecEngine::JSExecEngine(QString *_baseURL, QString *_projExt, QObject *parent) : QObject(parent)
{
    baseURL = QString(*_baseURL + (standardEnd(_baseURL) ? "" : "/") + "cgi+bin/main.cgi");
    projURL = QString(*_baseURL + (standardStart(_projExt) && !standardEnd(_baseURL) ? "/" : "") +
                      "cgi-bin/" + *_projExt);
    qDebug() << "Base URL: " << baseURL;
    qDebug() << "Project URL: " << projURL;

    netHub = new QNetworkAccessManager(this);
    connect(netHub, &QNetworkAccessManager::finished, this, &JSExecEngine::parseReturn);
}

JSExecEngine::~JSExecEngine()
{
    netHub->deleteLater();
    if (instruct != nullptr) {
        if (instruct->request != nullptr) delete instruct->request;
        delete instruct;
    }
}

void JSExecEngine::exists_user(QString *userID)
{
    instruct = new nethub_poll();
    instruct->queryType = getUser;
    instruct->userID = userID;
    instruct->returnSignal = existsUser;
    buildRequest(instruct);
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

void JSExecEngine::parseReturn(QNetworkReply *reply)
{ //Needs a test
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

    reply->deleteLater();
}
