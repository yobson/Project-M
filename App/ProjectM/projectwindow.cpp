#include "projectwindow.h"
#include "ui_projectwindow.h"
#include "project.h"

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
    ui->enabled_check_box->setTristate(project->enabled());
    ui->wifi_check_box->setTristate(project->wifi_only());
    ui->plugged_in_check_box->setTristate(project->plugged_in_only());
}

ProjectWindow::~ProjectWindow()
{
    /// TODO: update project settings;
    delete ui;
}

void ProjectWindow::on_enabled_check_box_stateChanged(int arg1)
{
    this->project->enabled() = arg1;
}


void ProjectWindow::on_wifi_check_box_stateChanged(int arg1)
{
    this->project->wifi_only() = arg1;
}

void ProjectWindow::on_plugged_in_check_box_stateChanged(int arg1)
{
    this->project->plugged_in_only() = arg1;
}
