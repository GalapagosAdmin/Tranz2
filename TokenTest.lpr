program TokenTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrmTTmain, LResources, TransTokenRemix, ZConnection, zcomponent,
  ZDbcDbLib, ZClasses, ZSelectSchema, ZPlainDbLibDriver
  { you can add units after this };

{$IFDEF WINDOWS}{$R TokenTest.rc}{$ENDIF}

begin
  {$I TokenTest.lrs}
  //Writeln('Main Program');
  Application.Initialize;
  Application.CreateForm(TfrmTokenTst, frmTokenTst);
  Application.Run;
end.

