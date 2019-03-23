#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QStringList>
#include <QStringListModel>
#include <QAbstractItemView>

#include "testpage.h"
#include "project.h"
#include "projectwindow.h"

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

    void on_project_list_view_clicked(const QModelIndex &index);

private:
    Ui::MainWindow *ui;
    TestPage *tp = nullptr;

    ProjectWindow *projectWindow;
    QStringListModel *project_list_model;
    QList<Project> *project_list;
    QStringList *project_name_list;
};

#endif // MAINWINDOW_H
