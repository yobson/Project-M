#include "loginwindow.h"
#include "ui_loginwindow.h"
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>
#include "magic.h"
#include <QDir>

LoginWindow::LoginWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::LoginWindow)
{
    ui->setupUi(this);
}

LoginWindow::~LoginWindow()
{
    delete ui;
    if (wM != nullptr) delete wM;
    if (wR != nullptr) delete wR;
    if (engine != nullptr) delete engine;
}

void LoginWindow::existsUser(bool exists)
{
    if (exists) {
        qDebug() << "No need to reg";
        wM = new MainWindow();
        wM->show();
    } else {
        qDebug() << "Needs to reg";
        wR = new Registration();
        wR->show();
    }
    this->hide(); // prevent back button

}

void LoginWindow::checkLogin()
{
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation); //TODO: James -> Trigger this code after loop exec
    auto fileName= path + "/userID.txt";
    QDir dir(path);
    if(!dir.exists()) dir.mkpath(".");
    QFileInfo checkFile(fileName);
    if (checkFile.exists() && checkFile.isFile()) {
        engine = new JSExecEngine(PROJECT_BASE_IP, "");
        engine->exists_user("1002"); //TODO: James -> remove hardcode
        existsUser(true);
    } else {
        existsUser(false);
    }
}
