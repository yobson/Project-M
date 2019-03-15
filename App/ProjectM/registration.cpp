#include "registration.h"
#include "ui_registration.h"
#include <QStandardPaths>
#include <QFileInfo>

Registration::Registration(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::Registration)
{
    mainWindow = nullptr;
    ui->setupUi(this);
}

Registration::~Registration()
{
    delete ui;
    if (mainWindow != nullptr) delete mainWindow;
}

void Registration::on_register_btn_clicked()
{
    auto path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    auto fileName= path + "/userID";
}

void Registration::on_skip_btn_clicked()
{
    mainWindow = new MainWindow();
    mainWindow->show();
    this->hide(); //To prevent back button returning to registration page!
}
