#include "projectwindow.h"
#include "ui_projectwindow.h"
#include "project.h"
#include "magic.h"
#include "projectsettings.h"
#include <QSettings>
#include <QString>
#include <QDebug>
#include <vector>
#include <algorithm>


ProjectWindow::ProjectWindow(Project *project, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ProjectWindow)
{
    this->project = project;
    ui->setupUi(this);
    setWindowState((windowState() & ~(Qt::WindowMinimized | Qt::WindowFullScreen))
                       | Qt::WindowMaximized);

    ui->project_name->setText(project->name());
    ui->project_desc->setText(project->full_desc());

    //Set checkboxes to the saved setting, or unchecked if this project has never been viewed before
    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(project->name());

    assert(settings.contains(ProjectSettings::ENABLED));
    bool enabled_setting = settings.value(ProjectSettings::ENABLED).toBool();
    ui->enabled_check_box->setCheckState(enabled_setting ? Qt::Checked : Qt::Unchecked);

    assert(settings.contains(ProjectSettings::WIFI_ONLY));
    bool wifi_setting = settings.value(ProjectSettings::WIFI_ONLY).toBool();
    ui->wifi_check_box->setCheckState(wifi_setting ? Qt::Checked : Qt::Unchecked);

    assert(settings.contains(ProjectSettings::CHARGING_ONLY));
    bool pluggedin_setting = settings.value(ProjectSettings::CHARGING_ONLY).toBool();
    ui->plugged_in_check_box->setCheckState(pluggedin_setting ? Qt::Checked : Qt::Unchecked);

    assert(settings.contains(ProjectSettings::FREQUENCY));
    int freq = settings.value(ProjectSettings::FREQUENCY).toInt();
    ui->freq_slider->setValue(sliderFromSeconds(freq));

}

ProjectWindow::~ProjectWindow()
{
    /// TODO: update project settings;

    delete ui;
}

void ProjectWindow::on_enabled_check_box_stateChanged(int arg1)
{
	
    this->project->enabled() = arg1;
    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(this->project->name());

    settings.setValue(ProjectSettings::ENABLED, this->project->enabled());
    settings.sync();

}


void ProjectWindow::on_wifi_check_box_stateChanged(int arg1)
{
    this->project->wifi_only() = arg1;

    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(this->project->name());

    settings.setValue(ProjectSettings::WIFI_ONLY, project->wifi_only());
    settings.sync();

}

void ProjectWindow::on_plugged_in_check_box_stateChanged(int arg1)
{
    this->project->plugged_in_only() = arg1;


    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(this->project->name());


    settings.setValue(ProjectSettings::CHARGING_ONLY, this->project->plugged_in_only());
    settings.sync();
}

void ProjectWindow::on_freq_slider_valueChanged(int value)
{
    int sec = sliderToSeconds(value);
    this->project->frequency() = sec;
    ui->freq_text->setText(freq_text(value));

    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(this->project->name());

    settings.setValue(ProjectSettings::FREQUENCY, this->project->frequency());
    settings.sync();

}

int ProjectWindow::sliderToSeconds(int slid)
{
	/*
   if (slid == 1) return 30;
   return (slid-1) * 60;
   */
   
   return this->project->freq_values()[slid];
}

int ProjectWindow::sliderFromSeconds(int secs)
{
	/*
    if (secs == 30) return 1;
    return secs / 60;
	*/
	
	//will this work?
	//int count = 0;
	//for(int i = 0; this->project->freq_values()[i] != secs; i++){count++;}
	//return count;
	
	//this finds the index in the vector
	std::vector<int> vals = this->project->freq_values();
	std::vector<int>::iterator it = std::find(vals.begin(), vals.end(), secs);
	int x = -1;

	if (it != vals.end()){ x = it - vals.begin(); }
	 
    return x;
	
	
}

QString ProjectWindow::freq_text(int slid)
{
	/*
    if (slid == 1) return QString("Every 30 seconds");
    if (slid == 2) return QString("Every minute");
    return "Every " + QString::number(slid-1) + " minutes";
	*/
	
    return this->project->freq_label(slid);
}
