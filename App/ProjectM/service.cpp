#include "service.h"
#include "magic.h"
#include "projectsettings.h"
#include "jsexecengine.h"
#include <QTimer>
#include <QDebug>
#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>
#include <QFile>
#include <QTime>
#include <QStandardPaths>
#include <QFileInfo>
#include <QDir>
#include <QAndroidJniObject>
#include <QtAndroid>

void writeSampleProjectsToFile();

Service::Service(QObject *parent) : QObject (parent)
{
    writeSampleProjectsToFile();
}

/** Check if the file exists. Create the directory if it doesn't exist. */
bool checkFileExists(QString fileName) {
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    fileName = path + fileName;
    QDir dir(path);
    if (!dir.exists()) dir.mkpath(".");
    QFileInfo checkFile(fileName);
    return checkFile.exists();
}

/** Load a Json file and return it, or return an empty JsonDocument if the file doesn't exist. */
QJsonDocument loadJson(QString fileName) {
    if (checkFileExists(fileName)) {
        auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
        fileName = path + fileName;
        QFile jsonFile(fileName);
        jsonFile.open(QFile::ReadOnly);
        QString data = jsonFile.readAll();
        jsonFile.close();
        QJsonDocument jsonDocument = QJsonDocument::fromJson(data.toUtf8());
        if (jsonDocument.isNull()) return QJsonDocument();
        else return jsonDocument;
    } else {
        return QJsonDocument();
    }
}

void saveJson(QJsonDocument jsonDocument, QString fileName) {
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    fileName = path + fileName;
    QFile jsonFile(fileName);
    jsonFile.open(QFile::WriteOnly);
    QTextStream stream(&jsonFile);
    stream << jsonDocument.toJson();
    jsonFile.flush();
    jsonFile.close();
}

QJsonArray loadProjects()
{
    QJsonDocument jsonDoument = loadJson(ACTIVE_PROJECTS_FILE);
    QJsonObject jsonObject = jsonDoument.object();
    if (jsonObject.contains(ProjectSettings::PROJECTS)) {
        return jsonObject[ProjectSettings::PROJECTS].toArray();
    } else {
        return QJsonArray();
    }
}

QJsonObject loadTimes() {
    QJsonDocument jsonDocument = loadJson(LAST_RUN_FILE);
    return jsonDocument.object();
}

void saveNewTimes(QJsonObject &jsonObject) {
    saveJson(QJsonDocument(jsonObject), LAST_RUN_FILE);
}

bool shouldRun(QJsonObject &project, QJsonObject &lastRun) {
    QString projectName = project[ProjectSettings::NAME].toString();

    if (lastRun.contains(projectName)) {
        QDateTime lastTime = QDateTime::fromString(lastRun[projectName].toString());
        if (QDateTime::currentDateTime() < lastTime.addSecs(project[ProjectSettings::FREQUENCY].toInt()))
            return false;
    }

    if (project.contains(ProjectSettings::TIME_FRAME)) {
        QJsonObject timeFrame = project[ProjectSettings::TIME_FRAME].toObject();
        QTime startTime = QTime::fromString(timeFrame[ProjectSettings::TIME_FRAME_START].toString());
        QTime endTime = QTime::fromString(timeFrame[ProjectSettings::TIME_FRAME_END].toString());
        QTime currTime = QTime::currentTime();

        if (startTime <= endTime) {
            if (currTime < startTime || endTime < currTime)
                return false;
        } else {
            if (endTime < currTime && currTime < startTime)
                return false;
        }
    }

    if (project.contains(ProjectSettings::WIFI_ONLY) && project[ProjectSettings::WIFI_ONLY].toBool()) {
        bool ret = QAndroidJniObject::callStaticMethod<jboolean>("space/hobson/ProjectM/Helpers", "isWiFiConnected",
                                                                 "(Landroid/content/Context;)Z", QtAndroid::androidContext().object());
        qDebug() << "WiFi: " << ret;
        if (!ret)
            return false;
    }

    if (project.contains(ProjectSettings::CHARGING_ONLY) && project[ProjectSettings::CHARGING_ONLY].toBool()) {
        bool ret = QAndroidJniObject::callStaticMethod<jboolean>("space/hobson/ProjectM/Helpers", "isCharging",
                                                                 "(Landroid/content/Context;)Z", QtAndroid::androidContext().object());
        qDebug() << "Charging: " << ret;
        if (!ret)
            return false;
    }

    if (project.contains(ProjectSettings::MIN_BATTERY_LEVEL)) {
        int ret = QAndroidJniObject::callStaticMethod<jint>("space/hobson/ProjectM/Helpers", "batteryLevel",
                                                            "(Landroid/content/Context;)I", QtAndroid::androidContext().object());
        qDebug() << "Battery Level: " << ret;
        if (ret < project[ProjectSettings::MIN_BATTERY_LEVEL].toInt())
            return false;
    }

    return true;
}

void Service::triggered()
{
    qDebug() << "SERVICE :: TRIGGERED";
    QJsonArray projects = loadProjects();
    QJsonObject times = loadTimes();
    for (auto projectValue : projects) {
        QJsonObject project = projectValue.toObject();
        if (shouldRun(project, times)) {
            // TODO: Create another thread and run the project
            QString projectName = project[ProjectSettings::NAME].toString();
            qDebug() << "Project " << projectName << " run";
            times.insert(projectName, QDateTime::currentDateTime().toString());
        }
    }
    saveNewTimes(times);
    QTimer::singleShot(ProjectSettings::CHECKING_FREQ, this, &Service::triggered);
}

void writeSampleProjectsToFile() {
    QJsonObject project;
    QJsonArray projects;

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "A");
    project.insert(ProjectSettings::FREQUENCY, 20);
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "B");
    project.insert(ProjectSettings::FREQUENCY, 10);
    project.insert(ProjectSettings::WIFI_ONLY, true);
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "C");
    project.insert(ProjectSettings::FREQUENCY, 15);
    project.insert(ProjectSettings::CHARGING_ONLY, true);
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "D");
    project.insert(ProjectSettings::FREQUENCY, 25);
    project.insert(ProjectSettings::MIN_BATTERY_LEVEL, 50);
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "E");
    project.insert(ProjectSettings::FREQUENCY, 15);
    project.insert(ProjectSettings::TIME_FRAME, QJsonObject({{ProjectSettings::TIME_FRAME_START, QTime(18, 0).toString()},
                                                             {ProjectSettings::TIME_FRAME_END, QTime(20, 0).toString()}}));
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "F");
    project.insert(ProjectSettings::FREQUENCY, 15);
    project.insert(ProjectSettings::TIME_FRAME, QJsonObject({{ProjectSettings::TIME_FRAME_START, QTime(18, 0).toString()},
                                                             {ProjectSettings::TIME_FRAME_END, QTime(11, 0).toString()}}));
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "G");
    project.insert(ProjectSettings::FREQUENCY, 15);
    project.insert(ProjectSettings::TIME_FRAME, QJsonObject({{ProjectSettings::TIME_FRAME_START, QTime(12, 0).toString()},
                                                             {ProjectSettings::TIME_FRAME_END, QTime(17, 0).toString()}}));
    projects.push_back(project);

    project = QJsonObject();
    project.insert(ProjectSettings::NAME, "H");
    project.insert(ProjectSettings::FREQUENCY, 15);
    project.insert(ProjectSettings::TIME_FRAME, QJsonObject({{ProjectSettings::TIME_FRAME_START, QTime(22, 0).toString()},
                                                             {ProjectSettings::TIME_FRAME_END, QTime(11, 0).toString()}}));
    projects.push_back(project);

    QJsonObject json;
    json.insert(ProjectSettings::PROJECTS, projects);
    qDebug() << json;
    saveJson(QJsonDocument(json), ACTIVE_PROJECTS_FILE);
}
