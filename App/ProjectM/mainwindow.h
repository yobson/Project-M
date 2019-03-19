#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "testpage.h"

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

private:
    Ui::MainWindow *ui;
    TestPage *tp = nullptr;
};

#endif // MAINWINDOW_H
