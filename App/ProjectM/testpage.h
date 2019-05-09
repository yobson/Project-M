#ifndef TESTPAGE_H
#define TESTPAGE_H

#include <QMainWindow>
#include <QPlainTextEdit>
#include "jsexecengine.h"
#include "logger.h"
#include <QLinkedList>

namespace Ui {
class TestPage;
}

// Logger class for controlling the logger
// 'logger << "Hello" << "World"' will produce:
// Hello
// World
// If you want an indent, logger[n] will indent n times for everything on that line!

class TestPage : public QMainWindow
{
    Q_OBJECT

public:
    explicit TestPage(QWidget *parent = nullptr);
    ~TestPage();

private slots:
    void on_get_projects_btn_clicked();
    void on_run_prime_btn_clicked();
    void gotProjects(QLinkedList<JSExecEngine::Project> p);
    void ranTest(QString in, QString ret);


    void on_test_btn_clicked();

private:
    Ui::TestPage *ui;
    Logger logger;
    JSExecEngine *engine = nullptr;
    JSExecEngine *testEngine = nullptr;

    bool isPrime(int i);

    int done = 0;
};

#endif // TESTPAGE_H
