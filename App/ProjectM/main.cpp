#include <QApplication>
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>
#include "loginwindow.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    LoginWindow w;
    w.show();

    qDebug() << "Starting event loop";
    return a.exec();
}
