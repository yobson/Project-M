#ifndef TESTPAGE_H
#define TESTPAGE_H

#include <QMainWindow>
#include <QPlainTextEdit>

namespace Ui {
class TestPage;
}

// Logger class for controlling the logger
// 'logger << "Hello" << "World"' will produce:
// Hello
// World
// If you want an indent, logger[n] will indent n times for everything on that line!

class Logger {
public:
    Logger(QPlainTextEdit *edit = nullptr);

    Logger operator<<(QString string);
    Logger operator<<(const char *string);
    Logger operator[](int ind);

private:
    Logger(const QPlainTextEdit *edit, int ind);
    const QPlainTextEdit *target;
    void printIndent(int i);
    int indent = 0;
};

class TestPage : public QMainWindow
{
    Q_OBJECT

public:
    explicit TestPage(QWidget *parent = nullptr);
    ~TestPage();

private slots:
    void on_get_projects_btn_clicked();

private:
    Ui::TestPage *ui;
    Logger logger;
};

#endif // TESTPAGE_H
