#ifndef JSEXECENGINE_H
#define JSEXECENGINE_H

#include <QObject>
#include <QJSEngine>
#include <QString>
#include <QNetworkAccessManager>
#include <QNetworkReply>

//This class is in charge of all comunication between the server and the app
//Server Queries will be in snake case starting with the type of query:
//	exists_[] :: a -> bool

class JSExecEngine : public QObject
{
    Q_OBJECT

public:
    JSExecEngine(QString _baseURL, QString _projExt, QObject *parent = nullptr);
    ~JSExecEngine();
    void exists_user(QString userID);
    void register_user(QString firstName, QString lastName);
    void get_projects();

signals:
    void exists_user_result(bool);
    void register_user_result(QString);

private:
    enum query_type {getUser, regUser, noQuery, getProjs};
    enum return_signal {existsUser, userReg, noSignal, retProjs};
    typedef struct {
        QString firstName;
        QString lastName;
    } nethub_poll_data_names;
    typedef union {
        nethub_poll_data_names *names;
    } nethub_poll_data;
    typedef struct{
        query_type queryType = noQuery;
        return_signal returnSignal = noSignal;
        QNetworkRequest *request = nullptr;
        QString *userID = nullptr;
        nethub_poll_data data;
    } nethub_poll;
    QString baseURL; //URL of user managment server
    QString projURL; //URL of project CGI.
    QNetworkAccessManager *netHub;

    bool standardEnd(QString *check);
    bool standardStart(QString *check);
    void buildRequest(nethub_poll *inst);
    void parseReturn(QNetworkReply *reply, nethub_poll *instr);
    void deleteNethubPoll(nethub_poll *poll);
};

#endif // JSEXECENGINE_H
