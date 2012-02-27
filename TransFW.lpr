program TransFW;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrmTransFWmain, LResources, TransToken, TransConst, TBAConst,
  zcomponent, toLangListCombo
  { you can add units after this };

{$IFDEF WINDOWS}{$R TransFW.rc}{$ENDIF}

begin
  {$I TransFW.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmTBAMain, frmTBAMain);
  Application.Run;
end.

