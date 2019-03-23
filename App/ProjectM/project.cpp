#include "project.h"

Project::Project(std::string name, std::string short_desc, std::string full_desc, bool enabled, bool wifi_only, bool plugged_in_only)
{
    this->name_ = name;
    this->short_desc_ = short_desc;
    if (full_desc == "")
        this->full_desc_ = short_desc;
    else
        this->full_desc_ = full_desc;
    this->enabled_ = enabled;
    this->wifi_only_ = wifi_only;
    this->plugged_in_only_ = plugged_in_only;
}

auto Project::name() -> const std::string& { return this->name_; }
auto Project::short_desc() -> const std::string& { return this->short_desc_; }
auto Project::full_desc() -> const std::string& { return this->full_desc_; }

auto Project::enabled() -> bool& { return this->enabled_; }
auto Project::wifi_only() -> bool& { return this->wifi_only_; }
auto Project::plugged_in_only() -> bool& { return this->plugged_in_only_; }
