#include <QApplication>
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>
#include "loginwindow.h"

//TODO: James -> Move pretty much all of this code into registration.cpp or logging in window!

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    LoginWindow w;
    w.show();
    w.checkLogin();

    return a.exec();
}
