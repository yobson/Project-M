#ifndef JSEXECENGINE_H
#define JSEXECENGINE_H

#include <QObject>
#include <QJSEngine>
#include <QString>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QLinkedList>
#include "logger.h"
#include <QPlainTextEdit>
#include <QGeoPositionInfoSource>
#include <QGeoCoordinate>
#include <QGeoPositionInfo>

//This class is in charge of all comunication between the server and the app
//Server Queries will be in snake case starting with the type of query:
//	exists_[] :: a -> bool

class JSExecEngine : public QObject
{
    Q_OBJECT

public:
    JSExecEngine(QString _baseURL, QString _projExt = "", QObject *parent = nullptr, QPlainTextEdit *editor = nullptr);
    ~JSExecEngine();
    void exists_user(QString userID);
    void register_user(QString firstName, QString lastName);
    void get_projects();
    void run_project();
    void get_score();
    void get_permissions();
    typedef struct {
        QString name;
        QString description;
        QString URL;
    } Project;

signals:
    void exists_user_result(bool);
    void get_score_result(int);
    void register_user_result(QString);
    void get_projects_result(QLinkedList<Project>);
    void web_error(QNetworkReply::NetworkError);
    void get_permissions_result(QStringList permissionList);

private:
    enum query_type {getUser, regUser, noQuery, getProjs, getJS, getJSInput, returnJS, getPermissions};
    enum return_signal {existsUser, userReg, noSignal, retProjs, jsReady, jsInputReady, retScore, retPermissions, prepPermissions};
    enum permissions_enum {Location};
    typedef struct {
        QString firstName;
        QString lastName;
    } nethub_poll_data_names;
    typedef union {
        nethub_poll_data_names *names;
        QString *js;
        QString *result;
    } nethub_poll_data;
    typedef struct{
        query_type queryType = noQuery;
        return_signal returnSignal = noSignal;
        QNetworkRequest *request = nullptr;
        QString *userID = nullptr;
        nethub_poll_data data;
        int32_t permFlags = 0;
        QGeoCoordinate *locCoord = nullptr;
    } nethub_poll;
    QString baseURL; //URL of user managment server
    QString projURL; //URL of project CGI.
    QNetworkAccessManager *netHub;
    QGeoPositionInfoSource *locationSource = nullptr;

    bool standardEnd(QString *check);
    bool standardStart(QString *check);
    void buildRequest(nethub_poll *inst);
    void parseReturn(QNetworkReply *reply, nethub_poll *instr);
    void deleteNethubPoll(nethub_poll *poll);
    QString getUserID();
    void getRequestedPhoneData(nethub_poll *instr);
    void get_project_input(QString js, nethub_poll *instr);
    void return_answer(QString result, nethub_poll *instr);
    void locationUpdate(const QGeoPositionInfo &info, nethub_poll *inst);
    void prep_permissions(nethub_poll *instr);
    Logger logger;
};

#endif // JSEXECENGINE_H
