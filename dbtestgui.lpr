program dbtestgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrmMain, LResources, zcomponent, transdb
  { you can add units after this };

{$IFDEF WINDOWS}{$R dbtestgui.rc}{$ENDIF}

begin
  {$I dbtestgui.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

