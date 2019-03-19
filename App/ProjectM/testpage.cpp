#include "testpage.h"
#include "ui_testpage.h"

TestPage::TestPage(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::TestPage)
{
    ui->setupUi(this);
}

TestPage::~TestPage()
{
    delete ui;
}
