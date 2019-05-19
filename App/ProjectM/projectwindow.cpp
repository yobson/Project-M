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
#include <QMessageBox>
#include "magic.h"


ProjectWindow::ProjectWindow(Project *project, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ProjectWindow)
{
    this->project = project;
    ui->setupUi(this);
    setWindowState((windowState() & ~(Qt::WindowMinimized | Qt::WindowFullScreen))
                       | Qt::WindowMaximized);
    engine = new JSExecEngine(PROJECT_BASE_IP, project->url());
    qDebug() << project->url();
    connect(engine, &JSExecEngine::get_permissions_result, this, &ProjectWindow::get_permissions);

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
    ui->freq_text->setText(freq_text(sliderFromSeconds(freq)));

    uiLoaded = true;
}

ProjectWindow::~ProjectWindow()
{
    /// TODO: update project settings;
    qDebug() << "exiting projectwindow";
    if (engine != nullptr) delete engine;
    delete ui;
}

void ProjectWindow::on_enabled_check_box_stateChanged(int arg1)
{
    if (!uiLoaded) {
        actually_enable(arg1);
        return;
    }
    if (arg1 == Qt::Unchecked) enable(arg1);
    else {
        engine->get_permissions();
        arg = arg1;
        connect(this, &ProjectWindow::actually_enable, this, &ProjectWindow::enable);
    }
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

void ProjectWindow::get_permissions(QStringList perms)
{
    qDebug() << "Checking perms";
    if (perms.empty()) emit actually_enable(arg);
    else {
        QMessageBox warning;
        warning.setText("This project requires permissions!");
        QString permsFlat;
        Q_FOREACH(QString s, perms) {
            permsFlat += s + " ";
        }
        warning.setInformativeText("PERMISSIONS REQUIRED: " + permsFlat);
        warning.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
        warning.setDefaultButton(QMessageBox::No);
        int ret = warning.exec();
        if (ret == QMessageBox::Yes)
            emit actually_enable(Qt::Checked);
        else emit actually_enable(Qt::Unchecked);
    }
}

void ProjectWindow::enable(int arg1)
{
    this->project->enabled() = arg1;
    uiLoaded = false;
    ui->enabled_check_box->setCheckState(arg1 ? Qt::Checked : Qt::Unchecked);
    uiLoaded = true;
    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    settings.beginGroup(this->project->name());

    settings.setValue(ProjectSettings::ENABLED, this->project->enabled());
    settings.sync();
    disconnect(this,&ProjectWindow::actually_enable, this, &ProjectWindow::enable);
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
