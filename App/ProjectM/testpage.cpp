#include "testpage.h"
#include "ui_testpage.h"
#include <QDebug>

TestPage::TestPage(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::TestPage)
{
    ui->setupUi(this);
    logger = Logger(ui->logPage);
}

TestPage::~TestPage()
{
    delete ui;
    if (engine != nullptr) delete engine;
}

void TestPage::on_get_projects_btn_clicked()
{
    logger << "Getting Projects";
}

void TestPage::on_run_prime_btn_clicked()
{
    logger << "Running prime experiment";
}
