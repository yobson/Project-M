#include "testpage.h"
#include "ui_testpage.h"
#include <QDebug>
#include "magic.h"
#include <QMessageBox>

TestPage::TestPage(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::TestPage)
{
    ui->setupUi(this);
    logger = Logger(ui->logPage, "Test page");
    engine = new JSExecEngine(PROJECT_BASE_IP, "Primes.cgi", this, ui->logPage);
    testEngine = new JSExecEngine(TEST_IP, "Primes.cgi", this, ui->logPage);
    connect(engine, &JSExecEngine::get_projects_result, this, &TestPage::gotProjects);
    connect(testEngine, &JSExecEngine::finished_project_exec, this, &TestPage::ranTest);
}

TestPage::~TestPage()
{
    delete ui;
    if (engine != nullptr) delete engine;
    if (testEngine != nullptr) delete testEngine;
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

void TestPage::ranTest(QString in, QString ret)
{
   int i = in.toInt();
   QString r = ret.split(' ').last().trimmed();
   bool prime = isPrime(i);
   if (prime && r != "0") {logger.passedTest(in + " is prime");goto passed;}
   if (!prime && r == "0") {logger.passedTest(in + " isn't prime");goto passed;}
   logger.failedTest(in + " said to be " +(r == "0" ? "not " : " ")+"prime "
                     "But was calculated to be" + (prime ? " " : " not ") + "prime");
   done = 0;
   return;
passed:
   done++;
   if (done >= 100) {
       done = 0;
       logger.htmlText("\nPassed!");
       return;
   }
   logger.test("Checking another pime");
   testEngine->run_project();
   return;
}

void TestPage::on_test_btn_clicked()
{
    logger.enterTestMode();
    testEngine->logger.enterTestMode();
    QMessageBox testMsg;
    testMsg.setText("Prepare for tests");
    testMsg.setDetailedText("Please reseet th local redis DB, launch the server."
                            "We will run 1000 tests");
    testMsg.setStandardButtons(QMessageBox::Ok);
    testMsg.exec();
    logger.test("Checking for prime");
    testEngine->run_project();
}

bool TestPage::isPrime(int i)
{
    for(int j = 3; j < i; j+=2) {
        if(i % j == 0) return false;
    }
    return true;
}
