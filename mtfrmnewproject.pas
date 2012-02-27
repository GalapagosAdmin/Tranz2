unit mtfrmnewproject;
//@000 2011.11.16 Noah Silva : Initial Version
//@001 2011.11.18 Noah Silva = Changed from Direct DB Access to OOP
//@002 2011.11.21 Noah Silva : Added additional project fields
//                             Population of language combo-boxes

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, EditBtn;

type

  { TfrmNewProject }

  TfrmNewProject = class(TForm)
    bbCreate: TBitBtn;
    cbDestLang: TComboBox;
    cbSrcLang: TComboBox;
    deBasePath: TDirectoryEdit;
    ebProjectName: TEdit;
    lblProjectName: TLabel;
    lblProjectDescription: TLabel;
    lblBasePath: TLabel;
    lblDestLang: TLabel;
    lblSrcLang: TLabel;
    mmProjectDescription: TMemo;
    pnlHeader: TPanel;
    procedure bbCreateClick(Sender: TObject);
    procedure ebProjectNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmNewProject: TfrmNewProject;

implementation

Uses
     TransDB,
     TransConst, mtDataModule,
     toProjectList, toLangList;                                                 //@001+

{$R *.lfm}

{ TfrmNewProject }

procedure TfrmNewProject.bbCreateClick(Sender: TObject);
begin
  With ProjectList do                                                           //@001+
    Case AddProject(ebProjectName.Text, mmProjectDescription.Text,              //@001=
                     deBasePath.Text, cbSrcLang.Text, cbDestLang.Text) of       //@002+
      True:begin                                                                //@001+
            DataModule1.DisplaySuccess(rsProjectAdded);                         //@001+
            close;
           end
     Else
       DataModule1.DisplayError(GetLastErrorMessage);
    end; // of CASE
   DataModule1.acProjectListUpdate.Execute;
end;

procedure TfrmNewProject.ebProjectNameChange(Sender: TObject);
  begin
    bbCreate.Enabled := Length(ebProjectName.Text) > 0;
    { TODO 1 -oshiruba -cerror-proofing : Implement additional validation logic }
  end;

procedure TfrmNewProject.FormShow(Sender: TObject);
 begin
  bbCreate.Enabled := False;
  cbSrcLang.Items := LangList.Strings;                                          //@002+
  cbDestLang.Items := LangList.Strings;                                         //@002+
 end;

end.

