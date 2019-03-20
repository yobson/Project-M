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

private:
    Logger(const QPlainTextEdit *edit, int ind);
    const QPlainTextEdit *target;
    void printIndent(int i);
    int indent = 0;

};

#endif // LOGGER_H
