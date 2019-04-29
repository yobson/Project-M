#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "magic.h"
#include <QSettings>

QLinkedList<JSExecEngine::Project> sampleProjects() {
    QLinkedList<JSExecEngine::Project> ps;
    ps.append({"ExampleA", "The first example", ""});
    ps.append({"ExampleB", "The second example", ""});
    return ps;
}

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


    engine = new JSExecEngine(PROJECT_BASE_IP);
    connect(engine, &JSExecEngine::get_projects_result, this, &MainWindow::on_get_projects);
    engine->get_projects();

    // on_get_projects(sampleProjects());
    engine->get_score();
}

MainWindow::~MainWindow()
{
    if (tp != nullptr) delete tp;
    if (projectWindow != nullptr) delete projectWindow;
    if (project_list_model != nullptr) delete project_list_model;
    if (project_list != nullptr) delete project_list;
    if (project_name_list != nullptr) delete project_name_list;
    delete ui;
}

void MainWindow::on_james_test_btn_clicked()
{
    tp = new TestPage(this);
    tp->show();
}

void MainWindow::on_get_projects(QLinkedList<JSExecEngine::Project> ps)
{
    if (project_list_model == nullptr) project_list_model = new QStringListModel(this);
    if (project_list == nullptr) project_list = new QList<Project>();
    if (project_name_list == nullptr) project_name_list = new QStringList();
    Q_FOREACH(JSExecEngine::Project p, ps) {
        project_list->append(Project(p.name, p.description, "", p.URL));
        *project_name_list << p.name;
    }

    // Delete from QSettings the projects that are not returned by the server anymore
    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    QStringList projectNames = settings.childGroups();
    for (auto projectName : projectNames) {
        if (!project_name_list->contains(projectName)) {
            settings.remove(projectName);
        }
    }
    settings.sync();

    // Populate our model
    project_list_model->setStringList(*project_name_list);

    // Glue model and view together
    ui->project_list_view->setModel(project_list_model);
}

void MainWindow::on_project_list_view_clicked(const QModelIndex &index)
{
    projectWindow = new ProjectWindow(&(*project_list)[index.row()], this);
    projectWindow->show();
    //got rid of line below so pressing back on a project page takes
    //user back to list of projects
    //But pressing back does not run the projectwindow destructor so
    //settings arent saved!! ugh
    //this->hide();
}

void MainWindow::on_refresh_btn_clicked()
{
   project_list->clear();
   project_name_list->clear();
   engine->get_projects();
   engine->get_score();
}
