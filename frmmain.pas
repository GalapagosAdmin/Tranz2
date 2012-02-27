unit FrmMain;
// Dependencies: ZEOS library for SQLite
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, DBGrids, ExtCtrls, StdCtrls, ComCtrls, Menus, DbCtrls, Buttons,
  ZConnection, ZDataset, ZSqlMetadata;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNoTrans: TButton;
    btnSrcOrphan: TButton;
    btnDestCROrphan: TButton;
    dsTable: TDatasource;
    dsQuery: TDatasource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBNavigator1: TDBNavigator;
    leDBName: TLabeledEdit;
    MainMenu1: TMainMenu;
    mQuery: TMemo;
    miDBOpen: TMenuItem;
    mhFile: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    pnlQueryPane: TPanel;
    pnlRightSide: TPanel;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    splQueryPane: TSplitter;
    StatusBar1: TStatusBar;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    ZSQLMetadata1: TZSQLMetadata;
    ZTable1: TZTable;
    procedure btnDBSelectClick(Sender: TObject);
    procedure BtnNoTransClick(Sender: TObject);
    procedure btnSrcOrphanClick(Sender: TObject);
    procedure btnDestCROrphanClick(Sender: TObject);
    procedure dsTableDataChange(Sender: TObject; Field: TField);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
//    procedure btnDBSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miDBOpenClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.miDBOpenClick(Sender: TObject);
begin
  IF OpenDialog1.Execute then
    begin
      ZSQLMetadata1.Active:= false;
      ZConnection1.Connected := false;
      leDBName.Text := OpenDialog1.FileName;
      ZConnection1.database := OpenDialog1.FileName;
      ZConnection1.Connected := true;
 //     ZTable1.Active := true;
      Statusbar1.SimpleText := 'Connected to Database.';
      ZSQLMetadata1.Active:= true;
    end;
end;

procedure TForm1.btnDBSelectClick(Sender: TObject);
begin

end;

procedure TForm1.BtnNoTransClick(Sender: TObject);
begin
  mQuery.Text:='select * from sentence where hash not in (select hash1 from sentencecr where sentencecr.lang1 = sentence.lang)';
end;

procedure TForm1.btnSrcOrphanClick(Sender: TObject);
begin
  mQuery.Text:='select * from sentencecr where hash1 not in (select hash from sentence where sentencecr.lang1 = sentence.lang)';;
end;

procedure TForm1.btnDestCROrphanClick(Sender: TObject);
begin
  mQuery.Text := 'select * from sentencecr where hash2 not in (select hash from sentence where sentencecr.lang2 = sentence.lang)';
end;

procedure TForm1.dsTableDataChange(Sender: TObject; Field: TField);
begin

end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  Statusbar1.SimpleText := 'Welcome to SQLite DB Browser.';
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

  ZConnection1.connected := false;
end;


procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin

  if ZQuery1.Active then
    ZQuery1.Active := false;
  ZQuery1.SQL := mQuery.Lines;
  ZQuery1.Active := true;
  StatusBar1.SimpleText := inttostr(ZQuery1.RowsAffected) + ' Rows Affected.';
end;

initialization
  {$I frmmain.lrs}

end.
