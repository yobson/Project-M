#include "project.h"
#include "magic.h"
#include "projectsettings.h"
#include <QSettings>

Project::Project(QString name, QString short_desc, QString full_desc, QString URL, int frequency, bool enabled, bool wifi_only, bool plugged_in_only)
{
    this->name_ = name;
    this->short_desc_ = short_desc;
    if (full_desc == "")
        this->full_desc_ = short_desc;
    else
        this->full_desc_ = full_desc;
    this->url_ = URL;
    this->frequency_ = frequency;
    this->enabled_ = enabled;
    this->wifi_only_ = wifi_only;
    this->plugged_in_only_ = plugged_in_only;

    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(name);
    settings.setValue(ProjectSettings::PROJECT_EXTENSION, URL);
    settings.setValue(ProjectSettings::ENABLED, enabled);
    settings.setValue(ProjectSettings::FREQUENCY, frequency);
    settings.setValue(ProjectSettings::WIFI_ONLY, wifi_only);
    settings.setValue(ProjectSettings::CHARGING_ONLY, plugged_in_only);
    settings.sync();
}

auto Project::name() -> const QString& { return this->name_; }
auto Project::short_desc() -> const QString& { return this->short_desc_; }
auto Project::full_desc() -> const QString& { return this->full_desc_; }
auto Project::url() -> const QString& { return this->url_; }
auto Project::enabled() -> bool& { return this->enabled_; }
auto Project::wifi_only() -> bool& { return this->wifi_only_; }
auto Project::plugged_in_only() -> bool& { return this->plugged_in_only_; }
