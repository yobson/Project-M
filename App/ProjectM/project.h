#ifndef PROJECT_H
#define PROJECT_H

#include <QString>
#include <vector>

class Project
{
public:
    Project(QString name, QString short_desc, QString full_desc = "", QString URL = "", int frquency = 300,
		bool enabled = false, bool wifi_only = true, bool plugged_in_only = true, 
		QString (*freq_label)(int) = [](int slid){  if (slid == 1) return QString("Every 30 seconds");
													if (slid == 2) return QString("Every minute");
													return "Every " + QString::number(slid-1) + " minutes"; }, 
		std::vector<int> freq_values = {30, 60, 120, 300, 600} );
    auto name() -> const QString&;
    auto short_desc() -> const QString&;
    auto full_desc() -> const QString&;
    auto url() -> const QString&;

    auto enabled() -> bool&;
    auto wifi_only() -> bool&;
    auto plugged_in_only() -> bool&;
    auto frequency() -> int&;
    auto freq_labels(int slid) -> const QString&; //how to say int->QString function?
	auto freq_values() -> std::vector<int>&;

private:
    QString name_;
    QString short_desc_;
    QString full_desc_;
    QString url_;
    int frequency_;
	QString freq_labels_(int slid);      //function from slider position {0,1,2,3,4} to a label
	std::vector<int> freq_values_;              //array of ints corresponding to labels eg "1 min" <-> 60
    bool enabled_;
    bool wifi_only_;
    bool plugged_in_only_;
};

#endif // PROJECT_H
