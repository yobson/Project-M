#ifndef PROJECTSETTINGS_H
#define PROJECTSETTINGS_H

#include <string>
#include <QString>

class ProjectSettings
{
public:
    // The corresponding value is: bool
    static const QString ENABLED;
    // The project extension that is used to actually run the project
    static const QString PROJECT_EXTENSION;
    // The corresponding value is: int that corresponds to the frequency in seconds
    static const QString FREQUENCY;

    // The corresponding value is: bool
    static const QString WIFI_ONLY;
    // The corresponding value is: bool
    static const QString CHARGING_ONLY;
    // The corresponding value is: int (between 0 and 100)
    static const QString MIN_BATTERY_LEVEL;
    // The corresponding value is: hh:mm:ss
    static const QString TIME_FRAME_START;
    // The corresponding value is: hh:mm:ss
    static const QString TIME_FRAME_END;
    // Use QTime(hh, mm, ss) for those two

    // Check every CHECKING_FREQ milliseconds for projects to run.
    static const int CHECKING_FREQ;
};

#endif // PROJECTSETTINGS_H

/*
 * The settings should be stored as:
 * QSettings projects(COMPANY_NAME, APP_NAME);
 * projects.beginGroup(ALL_PROJECTS_DIR);
 *
 * settings.beginGroup(projectName);
 * settings.addValue(ProjectSettings::xxx, xxx);
 * ...
 * settings.endGroup();
 *
 * ...
*/
