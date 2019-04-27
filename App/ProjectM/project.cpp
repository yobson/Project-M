#include "project.h"
#include "magic.h"
#include "projectsettings.h"
#include <QSettings>
#include <vector>

Project::Project(QString name, QString short_desc, QString full_desc, QString URL, int frequency, bool enabled, bool wifi_only, bool plugged_in_only,
	QString (*freq_labels)(int), std::vector<int> freq_values)
{
    this->name_ = name;

    this->short_desc_ = short_desc;
    if (full_desc == "")
        this->full_desc_ = short_desc;
    else
        this->full_desc_ = full_desc;
	
    this->*freq_labels_ = &freq_labels;  //are these correct?
	
	this->freq_values_ = freq_values;

    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(name);

    this->url_ = URL;
    settings.setValue(ProjectSettings::PROJECT_EXTENSION, URL);

    if (!settings.contains(ProjectSettings::FREQUENCY)) {
        this->frequency_ = frequency;
        settings.setValue(ProjectSettings::FREQUENCY, frequency);
    } else {
        this->frequency_ = settings.value(ProjectSettings::FREQUENCY).toInt();
    }

    if (!settings.contains(ProjectSettings::ENABLED)) {
        this->enabled_ = enabled;
        settings.setValue(ProjectSettings::ENABLED, enabled);
    } else {
        this->enabled_ = settings.value(ProjectSettings::ENABLED).toBool();
    }

    if (!settings.contains(ProjectSettings::WIFI_ONLY)) {
        this->wifi_only_ = wifi_only;
        settings.setValue(ProjectSettings::WIFI_ONLY, wifi_only);
    } else {
        this->wifi_only_ = settings.value(ProjectSettings::WIFI_ONLY).toBool();
    }

    if (!settings.contains(ProjectSettings::CHARGING_ONLY)) {
        this->plugged_in_only_ = plugged_in_only;
        settings.setValue(ProjectSettings::CHARGING_ONLY, plugged_in_only);
    } else {
        this->plugged_in_only_ = settings.value(ProjectSettings::CHARGING_ONLY).toBool();
    }

    settings.sync();
}

auto Project::name() -> const QString& { return this->name_; }
auto Project::short_desc() -> const QString& { return this->short_desc_; }
auto Project::full_desc() -> const QString& { return this->full_desc_; }
auto Project::url() -> const QString& { return this->url_; }

auto Project::enabled() -> bool& { return this->enabled_; }
auto Project::wifi_only() -> bool& { return this->wifi_only_; }
auto Project::plugged_in_only() -> bool& { return this->plugged_in_only_; }
auto Project::frequency() -> int& { return this->frequency_; }
auto Project::freq_labels(int slid) -> const QString& { static QString label = freq_labels(slid); return label; } //are these two correct?
auto Project::freq_values() -> std::vector<int>& { return this->freq_values_; }
