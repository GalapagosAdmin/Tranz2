unit mtfrmprojectfileadd;
//@001 2011.11.18 Noah Silva = Migrated from direct DB procedures to OOP Support
//@002 2011.11.21 Noah Silva = Update to use notifications unit
//                           + Populate language combo-boxes
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, Buttons;

type

  { TfrmProjectFileAdd }

  TfrmProjectFileAdd = class(TForm)
    bbImport: TBitBtn;
    cbProjectName: TComboBox;
    cbSourceLang: TComboBox;
    cbDestLang: TComboBox;
    FileNameEdit1: TFileNameEdit;
    lblDestLang: TLabel;
    lblSrcLang: TLabel;
    lblDescription: TLabel;
    lblFile: TLabel;
    lblProject: TLabel;
    mmDescription: TMemo;
    pnlHeader: TPanel;
    procedure bbImportClick(Sender: TObject);
    procedure cbDestLangChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmProjectFileAdd: TfrmProjectFileAdd;

implementation

{$R *.lfm}

{ TfrmProjectFileAdd }

Uses
  Transdb, mtDataModule,
  toProjectList, TransConst,                                                    //@001+
  mtNotification, toLangList;                                                   //@002+

procedure TfrmProjectFileAdd.bbImportClick(Sender: TObject);
  begin
    Try
//     If ProjectFileAdd(cbProjectName.Text, FileNameEdit1.Text,
      With ProjectList.GetProjectByName(cbProjectName.Text).DocumentList do     //@002=
        If AddDocument(cbProjectName.Text, FileNameEdit1.Text,                  //@002=
                                 mmDescription.Text,
                                 cbSourceLang.Text,
                                 cbDestLang.Text) then
            begin
              DisplaySuccess(rsFileAdded);                                      //@002+
              Close;
            end
           Else
            DisplayError(GetLastErrorMessage);                                  //@002=
    except on E: exception do
      DisplayError(e.message);                                                  //@002=
            // Update the display from the database to reflect the new file.
    end;
    DataModule1.acProjectListUpdate.Execute;
  end;

procedure TfrmProjectFileAdd.cbDestLangChange(Sender: TObject);
begin

end;

procedure TfrmProjectFileAdd.FormShow(Sender: TObject);
//Var                                                                           //@002-
//  TmpSL:TStringList;                                                          //@002-
begin
// Update Projects drop-down
//  try                                                                         //@002-
//    TmpSL := TStringList.Create;                                              //@002-
//    ProjectListGet(TmpSL);                                                    //@002-
//    cbProjectName.Items := TStrings(TmpSL);                                   //@002-
    cbProjectName.Items  := ProjectList.Strings;                                //@002+
//  finally                                                                     //@002-
//    TmpSl.Free;                                                               //@002-
//  end;                                                                        //@002-
  cbSourceLang.Items := LangList.Strings;                                       //@002+
  cbDestLang.Items := LangList.Strings;                                         //@002+

  { TODO 1 -oshiruba -cfunctionality : update srcLang, destLang, and basePath from project. }
end;

end.

