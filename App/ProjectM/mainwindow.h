#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QStringList>
#include <QStringListModel>
#include <QAbstractItemView>

#include "testpage.h"
#include "project.h"
#include "projectwindow.h"
#include "jsexecengine.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void on_james_test_btn_clicked();
    void on_get_projects(QLinkedList<JSExecEngine::Project> p);
    void on_project_list_view_clicked(const QModelIndex &index);

    void on_refresh_btn_clicked();

private:
    Ui::MainWindow *ui;
    TestPage *tp = nullptr;

    ProjectWindow *projectWindow = nullptr;
    QStringListModel *project_list_model = nullptr;
    QList<Project> *project_list = nullptr;
    QStringList *project_name_list = nullptr;
    JSExecEngine *engine;

};

#endif // MAINWINDOW_H
