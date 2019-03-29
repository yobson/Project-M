#ifndef PROJECTSETTINGS_H
#define PROJECTSETTINGS_H

#include <string>
#include <QString>

class ProjectSettings
{
public:
    // The value is: QJsonArray
    static const QString PROJECTS;
    // The value is: unique string to identify the project
    static const QString NAME;
    // The value is: int that corresponds to the frequency in seconds
    static const QString FREQUENCY;

    // Th value is: bool
    static const QString WIFI_ONLY;
    // The value is: bool
    static const QString CHARGING_ONLY;
    // The value is: int (between 0 and 100)
    static const QString MIN_BATTERY_LEVEL;
    // The value is: QJsonObject(start, end)
    static const QString TIME_FRAME;
    // The value is: hh:mm:ss
    static const QString TIME_FRAME_START;
    // The value is: hh:mm:ss
    static const QString TIME_FRAME_END;

    // Check every CHECKING_FREQ milliseconds for projects to run.
    static const int CHECKING_FREQ;
};

const QString ProjectSettings::PROJECTS = "Projects";
const QString ProjectSettings::NAME = "Name";
const QString ProjectSettings::FREQUENCY = "Frequency";

const QString ProjectSettings::WIFI_ONLY = "WiFi Only";
const QString ProjectSettings::CHARGING_ONLY = "Charging Only";
const QString ProjectSettings::MIN_BATTERY_LEVEL = "Minimum Battery Level";
const QString ProjectSettings::TIME_FRAME = "Time Frame";
const QString ProjectSettings::TIME_FRAME_START = "Start";
const QString ProjectSettings::TIME_FRAME_END = "End";

const int ProjectSettings::CHECKING_FREQ = 60000;

#endif // PROJECTSETTINGS_H

/*
 * Example of a Json with all types of settings:
 *  QJsonObject({"Projects":[
 *      {"Frequency":20,"Name":"A"},
 *      {"Frequency":10,"Name":"B","WiFi Only":true},
 *      {"Charging Only":true,"Frequency":15,"Name":"C"},
 *      {"Frequency":25,"Minimum Battery Level":50,"Name":"D"},
 *      {"Frequency":15,"Name":"E","Time Frame":{"End":"20:00:00","Start":"18:00:00"}}
 *  ]})
*/
