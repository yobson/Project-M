#include <QApplication>
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>
#include "loginwindow.h"
#include "service.h"
#include "testpage.h"
#ifdef Q_OS_ANDROID
#include <QAndroidService>
#include <QAndroidJniObject>
#include <QtAndroid>
#endif

int main(int argc, char *argv[])
{
    for(int i = 0; i < argc; i++) {
        qDebug() << "Argument: " << argv[i];
#ifdef Q_OS_ANDROID
        if (!strcmp(argv[i], "-service")){
            QAndroidService a(argc, argv);
            Service service(nullptr);
            service.triggered();
            return a.exec();
        }
#endif
        if (!strcmp(argv[i], "-tests")){
            QApplication a(argc, argv);
            TestPage w;
            w.show();
            return a.exec();
        }
    }
    QApplication a(argc, argv);
    LoginWindow w;
#ifdef Q_OS_ANDROID
    qDebug () << "Try to start service";
    QAndroidJniObject::callStaticMethod<void>("space/hobson/ProjectM/MService", "startMService", "(Landroid/content/Context;)V",
                                              QtAndroid::androidActivity().object());
#endif
    w.show();
    qDebug() << "Starting event loop";
    return a.exec();
}
