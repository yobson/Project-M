#ifndef REGISTRATION_H
#define REGISTRATION_H

#include <QMainWindow>
#include "mainwindow.h"

namespace Ui {
class Registration;
}

class Registration : public QMainWindow
{
    Q_OBJECT

public:
    explicit Registration(QWidget *parent = nullptr);
    ~Registration();

private slots:
    void on_register_btn_clicked();

    void on_skip_btn_clicked();

private:
    Ui::Registration *ui;
    MainWindow *mainWindow;
};

#endif // REGISTRATION_H
