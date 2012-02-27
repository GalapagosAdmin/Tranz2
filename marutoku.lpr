program marutoku;
// Main program for Translation workbench GUI using Tranz

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, zcomponent, lazcontrols, mtfrmmain, mtprojecttree, mtfraintro,
  turbopoweripro, mtdatamodule, mtfrmprojectfileadd, mtfrmnewproject,
  mtFraDocProps, mtfraprojprops, mtfrablockimport, mtfrasegedit;

{$R *.res}

begin
  Application.Title:='Maruyaku';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TfrmProjectFileAdd, frmProjectFileAdd);
  Application.CreateForm(TfrmNewProject, frmNewProject);
  Application.Run;
end.

