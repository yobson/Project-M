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
    void on_get_score(int score);
    void on_refresh_btn_clicked();

    void on_project_list_view_en_clicked(const QModelIndex &index);
    void on_project_list_view_dis_clicked(const QModelIndex &index);



    void on_proj_list_view_dis_clicked(const QModelIndex &index);

    void on_proj_list_view_en_clicked(const QModelIndex &index);

    void on_proj_list_view_dis_pressed(const QModelIndex &index);

private:
    Ui::MainWindow *ui;
    TestPage *tp = nullptr;

    ProjectWindow *projectWindow = nullptr;

    //en for enabled list and dis for disabled list
    QStringListModel *project_list_model_en = nullptr;
    QList<Project> *project_list_en = nullptr;
    QStringList *project_name_list_en = nullptr;

    QStringListModel *project_list_model_dis = nullptr;
    QList<Project> *project_list_dis = nullptr;
    QStringList *project_name_list_dis = nullptr;

    JSExecEngine *engine;

};

#endif // MAINWINDOW_H
