unit mtfraprojprops;
//@001 2011.11.21 Noah Silva + Code Refactoring to mode update code to this unit
//@002 2011.11.23 Noah Silva + Debugging
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, EditBtn,
  toProject;                                                                    //@001+

type

  { TfraProjProps }

  TfraProjProps = class(TFrame)
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
  private
    { private declarations }
    _Project:TProject;                                                          //@001+
    Procedure SetProject(Proj:TProject);                                        //@001+
  public
    { public declarations }
    Property Project:TProject Write SetProject;                                 //@001+
  end;

implementation

uses
  toLangList;                                                                   //@001+

Procedure TFraProjProps.SetProject(Proj:TProject);                              //@001+
  Begin
    _Project := Proj;
    // update display fields from new document
    If not Assigned(_Project) then                                              //@002+
      exit;                                                                     //@002+
    With _Project do
      begin
        ebProjectName.Text := Name;
        mmProjectDescription.Text := Description;
        deBasePath.Text := BasePath;
        cbSrcLang.Text := SrcLang;
        cbDestLang.Text := DestLang;
      end;

    cbSrcLang.Items := LangList.Strings;
    cbDestLang.Items := LangList.Strings;

   end;

{$R *.lfm}

end.
