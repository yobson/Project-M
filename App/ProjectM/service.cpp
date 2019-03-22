#include "service.h"
#include <QTimer>
#include <QDebug>

Service::Service(QObject *parent) : QObject (parent)
{

}

void Service::triggered()
{
    qDebug() << "SERVICE :: TRIGGERED";
    QTimer::singleShot(5000, this, &Service::triggered);
}
