#include "mainwindow.h"
#include "registration.h"
#include <QApplication>
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>

//TODO: James -> Move pretty much all of this code into registration.cpp or logging in window!

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    //See if user has registered
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    auto fileName= path + "/userID";
    QFileInfo checkFile(fileName);
    MainWindow   *wM = nullptr;
    Registration *wR = nullptr;
    if (checkFile.exists() && checkFile.isFile()) { //TODO: James -> Check user exists on server
        qDebug() << "No need to reg";
        wM = new MainWindow();
        wM->show();
    } else {
        qDebug() << "Needs to reg";
        wR = new Registration();
        wR->show();
    }

    int ret = a.exec();
    if (wM != nullptr) delete wM;
    if (wR != nullptr) delete wR;
    return ret;
}
