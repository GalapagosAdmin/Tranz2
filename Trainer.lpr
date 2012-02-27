program Trainer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmTrainerMain, LResources, TransTrain, zcomponent
  { you can add units after this };

{$IFDEF WINDOWS}{$R Trainer.rc}{$ENDIF}

begin
  {$I Trainer.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

