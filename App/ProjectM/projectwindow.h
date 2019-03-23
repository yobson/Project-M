#ifndef PROJECTWINDOW_H
#define PROJECTWINDOW_H

#include "project.h"

#include <QDialog>

namespace Ui {
class ProjectWindow;
}

class ProjectWindow : public QDialog
{
    Q_OBJECT

public:
    explicit ProjectWindow(Project *project = nullptr, QWidget *parent = nullptr);
    ~ProjectWindow();

private slots:
    void on_enabled_check_box_stateChanged(int arg1);

    void on_wifi_check_box_stateChanged(int arg1);

    void on_plugged_in_check_box_stateChanged(int arg1);

private:
    Ui::ProjectWindow *ui;
    Project *project;
};

#endif // PROJECTWINDOW_H
