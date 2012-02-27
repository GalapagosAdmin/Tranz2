program dbtest;
// A simple SQL Query interface

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TDBTest }

  TDBTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDBTest }

procedure TDBTest.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  writeln('Main program execution');
  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TDBTest.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDBTest.Destroy;
begin
  inherited Destroy;
end;

procedure TDBTest.WriteHelp;
begin
  { add your help code here }
  writeln('Database test application');
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TDBTest;

{$IFDEF WINDOWS}{$R dbtest.rc}{$ENDIF}

begin
  Application:=TDBTest.Create(nil);
  Application.Title:='Database Test';
  Application.Run;
  Application.Free;
end.

