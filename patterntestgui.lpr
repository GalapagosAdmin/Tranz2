program patterntestgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, PatternTestGUIMnFrm, LResources, zcomponent
  { you can add units after this };

{$IFDEF WINDOWS}{$R patterntestgui.rc}{$ENDIF}

begin
  {$I patterntestgui.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

