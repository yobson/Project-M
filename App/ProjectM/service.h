#ifndef SERVICE_H
#define SERVICE_H

#include <QObject>
#include <QString>
#include <QSettings>
#include "jsexecengine.h"

class Service : public QObject
{
    Q_OBJECT
public:
    explicit Service(QObject *parent = nullptr);

private:
    QHash<QString, JSExecEngine*> projectEngines;
    void runProject(QString&, QSettings&);

public slots:
    void triggered();
};

#endif // SERVICE_H
