#include "projectwindow.h"
#include "ui_projectwindow.h"
#include "project.h"
#include "magic.h"
#include "projectsettings.h"
#include <QSettings>
#include <QString>
#include <QDebug>

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
    bool pluggedin_setting = settings.value(ProjectSettings::CHARGING_ONLY).toBool();    ui->plugged_in_check_box->setCheckState(pluggedin_setting ? Qt::Checked : Qt::Unchecked);

    // Ensures that if the project is enabled, URL already set
    QString q_url_setting_key = project->name() + "_url_setting";
    settings.setValue(q_url_setting_key,project->url());


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

    settings.setValue(ProjectSettings::WIFI_ONLY, this->project->wifi_only());
    settings.sync();
    qDebug() << "Wifi changed for project: " + this->project->name();
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
