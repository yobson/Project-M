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
}

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
    QString tmp = target->toPlainText();
    tmp = tmp + string + '\n';
    target->document()->setPlainText(tmp);
    return *this;
}

Logger Logger::operator<<(const char *string)
{
    QString str = QString::fromLatin1(string);
    return operator<<(str);
}

Logger Logger::operator[](int ind)
{
    return Logger(target, ind);
}

void Logger::printIndent(int i)
{
    QString tmp = target->toPlainText();
    for(int j = 0; j < i; j++) tmp += "    ";
    target->document()->setPlainText(tmp);
}

void TestPage::on_get_projects_btn_clicked()
{
    logger << "Getting Projects";
}
