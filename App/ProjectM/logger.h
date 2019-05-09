#ifndef LOGGER_H
#define LOGGER_H

#include <QPlainTextEdit>

class Logger
{
public:
    Logger(QPlainTextEdit *edit = nullptr, QString name = "");

    Logger operator<<(QString string);
    Logger operator<<(const char *string);
    Logger operator+=(QString string);
    Logger operator+=(const char *string);
    Logger operator[](int ind);

    void clear();
    void test(QString s);
    void passedTest(QString s = "Passed");
    void failedTest(QString s = "Failed");
    void enterTestMode();
    void leaveTestMode();

    void htmlText(QString s);

private:
    Logger(const QPlainTextEdit *edit, int ind);
    const QPlainTextEdit *target;
    void printIndent(int i);
    int indent = 0;
    bool testMod = false;

};

#endif // LOGGER_H
