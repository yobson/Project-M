#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
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
