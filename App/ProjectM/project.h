#ifndef PROJECT_H
#define PROJECT_H

#include <string>
#include <cstring>

class Project
{
public:
    Project(std::string name, std::string short_desc, std::string full_desc = "", bool enabled = false, bool wifi_only = true, bool plugged_in_only = true);

    auto name() -> const std::string&;
    auto short_desc() -> const std::string&;
    auto full_desc() -> const std::string&;

    auto enabled() -> bool&;
    auto wifi_only() -> bool&;
    auto plugged_in_only() -> bool&;

private:
    std::string name_;
    std::string short_desc_;
    std::string full_desc_;
    bool enabled_;
    bool wifi_only_;
    bool plugged_in_only_;
};

#endif // PROJECT_H
