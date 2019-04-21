#ifndef PROJECT_H
#define PROJECT_H

#include <QString>

class Project
{
public:
    Project(QString name, QString short_desc, QString full_desc = "", QString URL = "", int frquency = 300, bool enabled = false, bool wifi_only = true, bool plugged_in_only = true);
    auto name() -> const QString&;
    auto short_desc() -> const QString&;
    auto full_desc() -> const QString&;
    auto url() -> const QString&;

    auto enabled() -> bool&;
    auto wifi_only() -> bool&;
    auto plugged_in_only() -> bool&;
    auto frequency() -> int&;

private:
    QString name_;
    QString short_desc_;
    QString full_desc_;
    QString url_;
    int frequency_;
	string freq_labels_[]
    bool enabled_;
    bool wifi_only_;
    bool plugged_in_only_;
};

#endif // PROJECT_H
