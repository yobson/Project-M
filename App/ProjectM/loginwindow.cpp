#include "loginwindow.h"
#include "ui_loginwindow.h"
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>

LoginWindow::LoginWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::LoginWindow)
{
    ui->setupUi(this);

    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation); //TODO: James -> Trigger this code after loop exec
    auto fileName= path + "/userID";
    QFileInfo checkFile(fileName);
    if (checkFile.exists() && checkFile.isFile()) {
        engine = new JSExecEngine("localhost:3000", "");
        engine->exists_user("1000"); //TODO: James -> remove hardcode
    } else {
        existsUser(false);
    }
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
