#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "projectsettings.h"
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

    if (project_list_model_en != nullptr) delete project_list_model_en;
    if (project_list_en != nullptr) delete project_list_en;
    if (project_name_list_en != nullptr) delete project_name_list_en;

    if (project_list_model_dis != nullptr) delete project_list_model_dis;
    if (project_list_dis != nullptr) delete project_list_dis;
    if (project_name_list_dis != nullptr) delete project_name_list_dis;

    delete ui;
}

void MainWindow::on_james_test_btn_clicked()
{
    tp = new TestPage(this);
    tp->show();
}

void MainWindow::on_get_projects(QLinkedList<JSExecEngine::Project> ps)
{

    ps += sampleProjects();

    if (project_list_model_en == nullptr) project_list_model_en = new QStringListModel(this);
    if (project_list_en == nullptr) project_list_en = new QList<Project>();
    if (project_name_list_en == nullptr) project_name_list_en = new QStringList();

    if (project_list_model_dis == nullptr) project_list_model_dis = new QStringListModel(this);
    if (project_list_dis == nullptr) project_list_dis = new QList<Project>();
    if (project_name_list_dis == nullptr) project_name_list_dis = new QStringList();

    Q_FOREACH(JSExecEngine::Project p, ps) {
        //add default settings if project is new?

        Project* currentProj = new Project(p.name, p.description, "", p.URL);

        //Get the enabled setting for that project
        QSettings settings(COMPANY_NAME, APP_NAME);
        settings.beginGroup(ALL_PROJECTS_DIR);
        settings.beginGroup(currentProj->name());
        bool enabled = settings.value(ProjectSettings::ENABLED).toBool();

        if(enabled){
            //project_list_en->append(Project(p.name, p.description, "", p.URL));
            project_list_en->append(*currentProj);
            *project_name_list_en << currentProj->name();
        }
        else{
            //project_list_dis->append(Project(p.name, p.description, "", p.URL));
            project_list_dis->append(*currentProj);
            *project_name_list_dis << currentProj->name();
        }

        settings.sync();
    }


    // Delete from QSettings the projects that are not returned by the server anymore
    QSettings settings(COMPANY_NAME, APP_NAME);
    settings.beginGroup(ALL_PROJECTS_DIR);
    QStringList projectNames = settings.childGroups();
    for (auto projectName : projectNames) {
        if (!(project_name_list_en->contains(projectName) || project_name_list_dis->contains(projectName))){
            settings.remove(projectName);
        }
    }

    settings.sync();


    // Populate our models
    project_list_model_en->setStringList(*project_name_list_en);
    project_list_model_dis->setStringList(*project_name_list_dis);

    // Glue models and view together
    ui->proj_list_view_en->setModel(project_list_model_en);
    ui->proj_list_view_dis->setModel(project_list_model_dis);
}


void MainWindow::on_project_list_view_en_clicked(const QModelIndex &index)
{
    projectWindow = new ProjectWindow(&(*project_list_en)[index.row()], this);
    qDebug() << "Clicked on enabled project";
    projectWindow->show();

}

void MainWindow::on_project_list_view_dis_clicked(const QModelIndex &index)
{
    projectWindow = new ProjectWindow(&(*project_list_dis)[index.row()], this);
    qDebug() << "Clicked on disbaled project";
    projectWindow->show();

}

void MainWindow::on_refresh_btn_clicked()
{
   project_list_en->clear();
   project_list_dis->clear();
   project_name_list_en->clear();
   project_name_list_dis->clear();
   engine->get_projects();
   engine->get_score();

}

void MainWindow::on_proj_list_view_dis_clicked(const QModelIndex &index)
{
    projectWindow = new ProjectWindow(&(*project_list_dis)[index.row()], this);
    qDebug() << "Clicked on disabled project";
    projectWindow->show();
}

void MainWindow::on_proj_list_view_en_clicked(const QModelIndex &index)
{
    projectWindow = new ProjectWindow(&(*project_list_en)[index.row()], this);
    qDebug() << "Clicked on enabled project";
    projectWindow->show();
}


//qt expects this to be here and cant work out how to make it not expect it :(
void MainWindow::on_proj_list_view_dis_pressed(const QModelIndex &index)
{

}
