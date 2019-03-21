#include "loginwindow.h"
#include "ui_loginwindow.h"
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>
#include "magic.h"
#include <QDir>
#include <QFile>
#include <QTextStream>
#include <QTimer>

LoginWindow::LoginWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::LoginWindow)
{
    ui->setupUi(this);
    connect(this, &LoginWindow::didFinishLoading, this, &LoginWindow::checkLogin);
    QTimer::singleShot(0, this, &LoginWindow::checkLogin); //Mega hack to wait for main exec loop #Qt hacks
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
    qDebug() << "Check login";
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    auto fileName= path + USER_ID_FILE;
    QDir dir(path);
    if(!dir.exists()) dir.mkpath(".");
    QFileInfo checkFile(fileName);
    if (checkFile.exists() && checkFile.isFile()) {
        qDebug() << "File exists";
        engine = new JSExecEngine(PROJECT_BASE_IP, "");
        connect(engine, &JSExecEngine::exists_user_result, this, &LoginWindow::existsUser);
        QFile file(fileName);
        file.open(QIODevice::ReadOnly);
        QTextStream in(&file);
        engine->exists_user(in.readLine());
        file.close();
    } else {
        existsUser(false);
    }
}
