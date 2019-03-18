#include "registration.h"
#include "ui_registration.h"
#include <QStandardPaths>
#include <QFileInfo>
#include <QFile>
#include <QTextStream>
#include <QDebug>
#include "magic.h"

Registration::Registration(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::Registration)
{
    mainWindow = nullptr;
    ui->setupUi(this);
    engine = new JSExecEngine(PROJECT_BASE_IP, "", this);
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    fileName= path + "/userID.txt"; //TODO: James -> Add to magic strigs
}

Registration::~Registration()
{
    delete ui;
    if (mainWindow != nullptr) delete mainWindow;
    if (engine != nullptr)     delete engine;
}

void Registration::on_register_btn_clicked()
{
    connect(engine, &JSExecEngine::register_user_result, this, &Registration::userRegistered);
    engine->register_user(ui->fstName->text(), ui->sndName->text());
}

void Registration::on_skip_btn_clicked()
{
    mainWindow = new MainWindow();
    mainWindow->show();
    this->hide(); //To prevent back button returning to registration page!
}

void Registration::userRegistered(QString id)
{
    qDebug() << "user registered: " << id;
    if (id == "") {qDebug() << "User didn't register properly"; return;} //TODO: James -> GUI error message?
    QFileInfo checkFile(fileName);
    if (checkFile.exists() && checkFile.isFile()) {
        QFile file(fileName);
        file.remove();
    }
    QFile file(fileName);
    file.open(QIODevice::WriteOnly);
    QTextStream stream(&file);
    stream << id;
    file.flush();
    file.close();
    on_skip_btn_clicked();
}
