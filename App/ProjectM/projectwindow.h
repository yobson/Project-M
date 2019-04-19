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

    void on_freq_slider_valueChanged(int value);

private:
    Ui::ProjectWindow *ui;
    Project *project;
    int sliderToSeconds(int slid);
    int sliderFromSeconds(int secs);
    QString freq_text(int slid);
};

#endif // PROJECTWINDOW_H
