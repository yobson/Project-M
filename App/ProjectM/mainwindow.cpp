#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QtAndroid>
#include <QAndroidJniObject>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    QAndroidJniObject::callStaticMethod<void>("space/hobson/ProjectM/MService", "startMService", "(Landroid/content/Context;)V",
                                              QtAndroid::androidActivity().object());

    /** List view with projects set-up */
    // Create model
    project_list_model = new QStringListModel(this);

    /// TODO: Get real data
    // Make mock data
    project_list = new QList<Project>();
    project_list->append(Project("PrimesAAAA", "Calculate Primes"));
    project_list->append(Project("Pollution", "Crowdsourced pollution measurement"));
    project_list->append(Project("Project X", "???"));

    QStringList project_name_list;
    for(int i=0;i<(*project_list).size();i++) {
        project_name_list << QString::fromStdString((*project_list)[i].name());
    }

    // Populate our model
    project_list_model->setStringList(project_name_list);

    // Glue model and view together
    ui->project_list_view->setModel(project_list_model);
}

MainWindow::~MainWindow()
{
    if (tp != nullptr) delete tp;
    delete ui;
}

void MainWindow::on_james_test_btn_clicked()
{
    tp = new TestPage(this);
    tp->show();
}

void MainWindow::on_project_list_view_clicked(const QModelIndex &index)
{
    projectWindow = new ProjectWindow(&(*project_list)[index.row()], this);
    projectWindow->show();
}
