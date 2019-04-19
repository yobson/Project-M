#include <QApplication>
#include <QStandardPaths>
#include <QFileInfo>
#include <QDebug>
#include "loginwindow.h"
#include "service.h"
#include <QAndroidService>
#include <QAndroidJniObject>
#include <QtAndroid>

int main(int argc, char *argv[])
{
    for(int i = 0; i < argc; i++) {
        qDebug() << "Argument: " << argv[i];
        if (!strcmp(argv[i], "-service")){
            QAndroidService a(argc, argv);
            Service service(nullptr);
            service.triggered();
            return a.exec();
        }
    }
    QApplication a(argc, argv);
    LoginWindow w;
    qDebug () << "Try to start service";
    QAndroidJniObject::callStaticMethod<void>("space/hobson/ProjectM/MService", "startMService", "(Landroid/content/Context;)V",
                                              QtAndroid::androidActivity().object());
    w.show();
    qDebug() << "Starting event loop";
    return a.exec();
}
