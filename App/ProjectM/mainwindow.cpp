#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QtAndroid>
#include <QAndroidJniObject>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    QAndroidJniObject::callStaticMethod<void>("space/hobson/ProjectM/MService", "startMService", "(Landroid/content/Context;)V",
                                              QtAndroid::androidActivity().object());
}

MainWindow::~MainWindow()
{
    if (tp != nullptr) delete tp;
    delete ui;
}

void MainWindow::on_james_test_btn_clicked()
{
    tp = new TestPage(this);
    tp->show();
}
