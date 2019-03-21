#include "testpage.h"
#include "ui_testpage.h"
#include <QDebug>
#include "magic.h"

TestPage::TestPage(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::TestPage)
{
    ui->setupUi(this);
    logger = Logger(ui->logPage, "Test page");
    engine = new JSExecEngine(PROJECT_BASE_IP, "Primes.cgi", this, ui->logPage);
    connect(engine, &JSExecEngine::get_projects_result, this, &TestPage::gotProjects);
}

TestPage::~TestPage()
{
    delete ui;
    if (engine != nullptr) delete engine;
}

void TestPage::on_get_projects_btn_clicked()
{
    logger << "Getting Projects";
    engine->get_projects();
}

void TestPage::on_run_prime_btn_clicked()
{
    logger << "Starting primes";
    engine->run_project();
}

void TestPage::gotProjects(QLinkedList<JSExecEngine::Project> p)
{
    logger << "Got projects:";
    int i = 1;
    Q_FOREACH(JSExecEngine::Project project, p) {
        logger[1] << QString("Project " + QString::number(i++)) + ":";
        QString name = "Name: " + project.name;
        QString desc = "Description: " + project.description;
        QString proj = "Project Extention: " + project.URL;
        logger[2] << name << desc << proj;
    }
}
