#ifndef LOGINWINDOW_H
#define LOGINWINDOW_H

#include <QMainWindow>
#include "mainwindow.h"
#include "registration.h"
#include "jsexecengine.h"

namespace Ui {
class LoginWindow;
}

class LoginWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit LoginWindow(QWidget *parent = nullptr);
    ~LoginWindow();

signals:
    void didFinishLoading();

private:
    Ui::LoginWindow *ui;
    MainWindow   *wM = nullptr;
    Registration *wR = nullptr;
    JSExecEngine *engine = nullptr;

private slots:
    void existsUser(bool exists);
    void checkLogin();

};

#endif // LOGINWINDOW_H
