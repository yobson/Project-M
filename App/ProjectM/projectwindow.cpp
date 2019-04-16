#include "projectwindow.h"
#include "ui_projectwindow.h"
#include "project.h"
#include <QSettings>
#include <QString>


ProjectWindow::ProjectWindow(Project *project, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ProjectWindow)
{
    this->project = project;
    ui->setupUi(this);
    setWindowState((windowState() & ~(Qt::WindowMinimized | Qt::WindowFullScreen))
                       | Qt::WindowMaximized);
    ui->project_name->setText(QString::fromStdString(project->name()));
    ui->project_desc->setText(QString::fromStdString(project->full_desc()));


    //ui->enabled_check_box->setTristate(project->enabled());
    //ui->wifi_check_box->setTristate(project->wifi_only());
    //ui->plugged_in_check_box->setTristate(project->plugged_in_only());


    //Set checkboxes to the saved setting, or unchecked if this project has never been viewed before
    QSettings settings;

    //const char * enabled_setting_key = (project->name() + "_enabled_setting").c_str();

    QString q_enabled_setting_key = QString::fromUtf8((project->name() + "_enabled_setting").c_str());
    bool enabled_setting = settings.value(q_enabled_setting_key,false).toBool();
    ui->enabled_check_box->setCheckState(enabled_setting ? Qt::Checked : Qt::Unchecked);

    QString q_wifi_setting_key = QString::fromUtf8((project->name() + "_wifi_setting").c_str());
    bool wifi_setting = settings.value(q_wifi_setting_key,false).toBool();
    ui->wifi_check_box->setCheckState(wifi_setting ? Qt::Checked : Qt::Unchecked);

    QString q_pluggedin_setting_key = QString::fromUtf8((project->name() + "_pluggedin_setting").c_str());
    bool pluggedin_setting = settings.value(q_pluggedin_setting_key,false).toBool();
    ui->plugged_in_check_box->setCheckState(pluggedin_setting ? Qt::Checked : Qt::Unchecked);


/*
    std::string wifi_setting_key = (project->name() + "_wifi_setting").c_str();

    ui->wifi_check_box->setTristate(settings.value(QString::fromUtf8(wifi_setting.c_str()),false));

    std::string pluggedin_setting = project->name() + "_pluggedin_setting";
    ui->plugged_in_check_box->setTristate(settings.value(QString::fromUtf8(pluggedin_setting),false));
*/



}

ProjectWindow::~ProjectWindow()
{
    /// TODO: update project settings;
    /// What, if anything, causes this to be run?!

/*
    QSettings settings;

    //kinda redundant calculating the key value each time, where can i define it just once and reuse?
    QString q_enabled_setting_key = QString::fromUtf8((project->name() + "_enabled_setting").c_str());
    settings.setValue(q_enabled_setting_key,project->enabled());

    QString q_wifi_setting_key = QString::fromUtf8((project->name() + "_wifi_setting").c_str());
    settings.setValue(q_wifi_setting_key,project->wifi_only());

    QString q_pluggedin_setting_key = QString::fromUtf8((project->name() + "_pluggedin_setting").c_str());
    settings.setValue(q_pluggedin_setting_key,project->plugged_in_only());
*/
    delete ui;
}

void ProjectWindow::on_enabled_check_box_stateChanged(int arg1)
{
    //too inefficient to update QSettings here?
    //at least that would guaruntee the setting is saved
    //even necessary to have project->enabled() etc in the object?
    this->project->enabled() = arg1;

    QSettings settings;
    QString q_enabled_setting_key = QString::fromUtf8((project->name() + "_enabled_setting").c_str());
    settings.setValue(q_enabled_setting_key,project->enabled());

}


void ProjectWindow::on_wifi_check_box_stateChanged(int arg1)
{
    this->project->wifi_only() = arg1;

    QSettings settings;
    QString q_wifi_setting_key = QString::fromUtf8((project->name() + "_wifi_setting").c_str());
    settings.setValue(q_wifi_setting_key,project->wifi_only());
}

void ProjectWindow::on_plugged_in_check_box_stateChanged(int arg1)
{
    this->project->plugged_in_only() = arg1;

    QSettings settings;
    QString q_pluggedin_setting_key = QString::fromUtf8((project->name() + "_pluggedin_setting").c_str());
    settings.setValue(q_pluggedin_setting_key,project->plugged_in_only());
}
