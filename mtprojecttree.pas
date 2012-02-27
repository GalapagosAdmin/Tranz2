unit mtprojecttree;
//@001 2011.11.16 Noah Silva : Added option to add/delete files to/from projects
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls;

type

  { TfraProject }

  TfraProject = class(TFrame)
    ToolBar1: TToolBar;
    tbNewProject: TToolButton;
    tbDelProject: TToolButton;
    tbNewFile: TToolButton;
    tbDelFile: TToolButton;
    ToolButton3: TToolButton;
    tvProject: TTreeView;
    procedure tbDelProjectClick(Sender: TObject);
    procedure tbNewProjectClick(Sender: TObject);
    procedure tbNewFileClick(Sender: TObject);
    procedure tbDelFileClick(Sender: TObject);
    procedure tvProjectChange(Sender: TObject; Node: TTreeNode);
    procedure tvProjectClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses
   dialogs, mtdatamodule;
{$R *.lfm}

{ TfraProject }

procedure TfraProject.tbNewProjectClick(Sender: TObject);
begin
  mtdatamodule.DataModule1.acNewProject.Execute;
end;

procedure TfraProject.tbNewFileClick(Sender: TObject);                        //@001+
begin
  mtdatamodule.DataModule1.acAddFileToProject.Execute;
end;

procedure TfraProject.tbDelFileClick(Sender: TObject);                        //@001+
begin
  mtdatamodule.DataModule1.acDeleteFileFromProject.Execute;
end;

procedure TfraProject.tvProjectChange(Sender: TObject; Node: TTreeNode);
begin
end;

procedure TfraProject.tvProjectClick(Sender: TObject);
begin
  mtdatamodule.DataModule1.acProjectTreeClick.Execute;
end;

procedure TfraProject.tbDelProjectClick(Sender: TObject);
begin
  mtdatamodule.DataModule1.acDeleteProject.Execute;

end;

end.

