#include "service.h"
#include "magic.h"
#include "projectsettings.h"
#include "jsexecengine.h"
#include <QTimer>
#include <QDebug>
#include <QTime>
#include <QAndroidJniObject>
#include <QtAndroid>
#include <QSettings>
#include <QHash>

void writeSampleProjectsToFile();

Service::Service(QObject *parent) : QObject (parent)
{
    writeSampleProjectsToFile();
    projectEngines.clear();
}

/** Check if the project should be run now. */
bool shouldRun(QSettings &project, QSettings &lastRun, QString &projectName) {
    assert(project.contains(ProjectSettings::ENABLED));
    if (!project.value(ProjectSettings::ENABLED).toBool()) {
        return false;
    }

    if (lastRun.contains(projectName)) {
        QDateTime lastTime = QDateTime::fromString(lastRun.value(projectName).toString());
        assert(project.contains(ProjectSettings::FREQUENCY));
        if (QDateTime::currentDateTime() < lastTime.addSecs(project.value(ProjectSettings::FREQUENCY).toInt()))
            return false;
    }

    if (project.contains(ProjectSettings::TIME_FRAME_START)) {
        QTime startTime = QTime::fromString(project.value(ProjectSettings::TIME_FRAME_START).toString());
        assert(project.contains(ProjectSettings::TIME_FRAME_END));
        QTime endTime = QTime::fromString(project.value(ProjectSettings::TIME_FRAME_END).toString());
        QTime currTime = QTime::currentTime();

        if (startTime <= endTime) {
            if (currTime < startTime || endTime < currTime)
                return false;
        } else {
            if (endTime < currTime && currTime < startTime)
                return false;
        }
    }

    if (project.value(ProjectSettings::WIFI_ONLY, false).toBool()) {
        bool isWiFiConnected = QAndroidJniObject::callStaticMethod<jboolean>("space/hobson/ProjectM/Helpers", "isWiFiConnected",
                                                                 "(Landroid/content/Context;)Z", QtAndroid::androidContext().object());
        qDebug() << "WiFi: " << isWiFiConnected;
        if (!isWiFiConnected)
            return false;
    }

    if (project.value(ProjectSettings::CHARGING_ONLY, false).toBool()) {
        bool isCharging = QAndroidJniObject::callStaticMethod<jboolean>("space/hobson/ProjectM/Helpers", "isCharging",
                                                                 "(Landroid/content/Context;)Z", QtAndroid::androidContext().object());
        qDebug() << "Charging: " << isCharging;
        if (!isCharging)
            return false;
    }

    if (project.contains(ProjectSettings::MIN_BATTERY_LEVEL)) {
        int batteryLevel = QAndroidJniObject::callStaticMethod<jint>("space/hobson/ProjectM/Helpers", "batteryLevel",
                                                            "(Landroid/content/Context;)I", QtAndroid::androidContext().object());
        qDebug() << "Battery Level: " << batteryLevel;
        if (batteryLevel < project.value(ProjectSettings::MIN_BATTERY_LEVEL).toInt())
            return false;
    }

    return true;
}

void Service::runProject(QString &projectName, QSettings &settings) {
    if (!projectEngines.count(projectName)) {
        assert(settings.contains(ProjectSettings::PROJECT_EXTENSION));
        projectEngines[projectName] = new JSExecEngine(PROJECT_BASE_IP, settings.value(ProjectSettings::PROJECT_EXTENSION).toString());
    }
    projectEngines[projectName]->run_project();
}

void Service::triggered() {
    qDebug() << "SERVICE :: TRIGGERED";
    QSettings projects(COMPANY_NAME, APP_NAME);
    projects.beginGroup(ALL_PROJECTS_DIR);
    QStringList projectNames = projects.childGroups();
    QSettings times(COMPANY_NAME, APP_NAME);
    times.beginGroup(LAST_RUN_DIR);
    for (auto projectName : projectNames) {
        projects.beginGroup(projectName);
        if (shouldRun(projects, times, projectName)) {
            runProject(projectName, projects);
            qDebug() << "Project " << projectName << " run";
            times.setValue(projectName, QDateTime::currentDateTime().toString());
        }
        projects.endGroup();
    }
    times.sync();
    QTimer::singleShot(ProjectSettings::CHECKING_FREQ, this, &Service::triggered);
}

void writeSampleProjectsToFile() {
    QSettings projects(COMPANY_NAME, APP_NAME);
    projects.beginGroup(ALL_PROJECTS_DIR);

    projects.beginGroup("A");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 20);
    projects.endGroup();

    projects.beginGroup("B");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 10);
    projects.setValue(ProjectSettings::WIFI_ONLY, true);
    projects.endGroup();

    projects.beginGroup("C");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 15);
    projects.setValue(ProjectSettings::CHARGING_ONLY, true);
    projects.endGroup();

    projects.beginGroup("D");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 25);
    projects.setValue(ProjectSettings::MIN_BATTERY_LEVEL, 50);
    projects.endGroup();

    projects.beginGroup("E");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 15);
    projects.setValue(ProjectSettings::TIME_FRAME_START, QTime(19, 0));
    projects.setValue(ProjectSettings::TIME_FRAME_END, QTime(22, 0));
    projects.endGroup();

    projects.beginGroup("F");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 15);
    projects.setValue(ProjectSettings::TIME_FRAME_START, QTime(19, 0));
    projects.setValue(ProjectSettings::TIME_FRAME_END, QTime(11, 0));
    projects.endGroup();

    projects.beginGroup("G");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 15);
    projects.setValue(ProjectSettings::TIME_FRAME_START, QTime(12, 0));
    projects.setValue(ProjectSettings::TIME_FRAME_END, QTime(14, 0));
    projects.endGroup();

    projects.beginGroup("H");
    projects.setValue(ProjectSettings::ENABLED, true);
    projects.setValue(ProjectSettings::PROJECT_EXTENSION, "");
    projects.setValue(ProjectSettings::FREQUENCY, 15);
    projects.setValue(ProjectSettings::TIME_FRAME_START, QTime(22, 0));
    projects.setValue(ProjectSettings::TIME_FRAME_END, QTime(11, 0));
    projects.endGroup();
    projects.sync();
}
