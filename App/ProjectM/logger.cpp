#include "logger.h"
#include <QDebug>
#include <QScrollBar>

Logger::Logger(QPlainTextEdit *edit, QString name) : target(edit)
{
    if (target == nullptr) return;
    if (name != "") *this << QString(name + " attached to logger");
}

Logger::Logger(const QPlainTextEdit *edit, int ind) : target(edit)
{
    indent = ind;
}

Logger Logger::operator<<(QString string)
{
    printIndent(indent);
    qDebug() << string;
    QString tmp = target != nullptr ? target->toPlainText() : "";
    tmp = tmp + string + '\n';
    if (target != nullptr && !testMod)
    {
        target->document()->setPlainText(tmp);
        target->verticalScrollBar()->setValue(target->verticalScrollBar()->maximum());
    }
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

void Logger::clear()
{
    if (target != nullptr) {
        target->document()->setPlainText("");
    }
}

void Logger::test(QString s)
{
    if(target != nullptr) {
        QTextCursor cur = target->textCursor();
        cur.insertText(s + " :: ");
        target->verticalScrollBar()->setValue(target->verticalScrollBar()->maximum());
    }
}

void Logger::passedTest(QString s)
{
    if(target != nullptr) {
        QTextCursor cur = target->textCursor();
        QTextCharFormat format;
        format.setForeground(QBrush(QColor("green")));
        cur.setCharFormat(format);
        cur.insertText(s);
        format.setForeground(QBrush(QColor("black")));
        cur.setCharFormat(format);
        cur.insertText("\n");
    }
}

void Logger::failedTest(QString s)
{
    if(target != nullptr) {
        QTextCursor cur = target->textCursor();
        QTextCharFormat format;
        format.setForeground(QBrush(QColor("red")));
        cur.setCharFormat(format);
        cur.insertText(s);
        format.setForeground(QBrush(QColor("black")));
        cur.setCharFormat(format);
        cur.insertText("\n");
    }
}

void Logger::enterTestMode()
{
   testMod = true;
}

void Logger::leaveTestMode()
{
    testMod = false;
}

void Logger::htmlText(QString s)
{
    if(target != nullptr) {
        QTextCursor cur = target->textCursor();
        cur.insertText(s + "");
        target->verticalScrollBar()->setValue(target->verticalScrollBar()->maximum());
    }
}

void Logger::printIndent(int i)
{
    if (target == nullptr || testMod) return;
    QString tmp = target->toPlainText();
    for(int j = 0; j < i; j++) tmp += "    ";
    target->document()->setPlainText(tmp);
}
