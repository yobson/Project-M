#include "logger.h"
#include <QDebug>

Logger::Logger(QPlainTextEdit *edit) : target(edit)
{
    if (target == nullptr) return;
    *this << "Logger initialised";
}

Logger::Logger(const QPlainTextEdit *edit, int ind) : target(edit)
{
    indent = ind;
}

Logger Logger::operator<<(QString string)
{
    printIndent(indent);
    QString tmp = target != nullptr ? target->toPlainText() : "";
    tmp = tmp + string;
    qDebug() << tmp;
    tmp += '\n';
    if (target != nullptr) target->document()->setPlainText(tmp);
    Logger ret(target, indent);
    return *this;
}

Logger Logger::operator<<(const char *string)
{
    QString str = QString::fromLatin1(string);
    return operator<<(str);
}

Logger Logger::operator+=(QString string)
{
    printIndent(1);
    return operator<<(string);
}

Logger Logger::operator+=(const char *string)
{
    QString str = QString::fromLatin1(string);
    return operator+=(str);
}

Logger Logger::operator[](int ind)
{
    return Logger(target, ind);
}

void Logger::printIndent(int i)
{
    if (target == nullptr) return;
    QString tmp = target->toPlainText();
    for(int j = 0; j < i; j++) tmp += "    ";
    target->document()->setPlainText(tmp);
}
