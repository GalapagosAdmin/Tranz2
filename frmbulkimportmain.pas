unit frmBulkImportMain;
//@000 2012.02.08 Noah SILVA - Started Project
// Imports terms (wordforms) to the glossary (Mainly for Tokenizer usage).

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons;

type

  { TfrmBIMain }

  TfrmBIMain = class(TForm)
    bbImport: TBitBtn;
    cbSrcLang: TComboBox;
    lblLang: TLabel;
    lblTerms: TLabel;
    mmTerms: TMemo;
    ProgressBar1: TProgressBar;
    procedure bbImportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmBIMain: TfrmBIMain;

implementation

uses
  TransConst, TransDB, toLangList;

{$R *.lfm}

{ TfrmBIMain }

procedure TfrmBIMain.bbImportClick(Sender: TObject);
const
  min = 0;
var
  LastError:string;
  RC:integer;
  row, rows:Integer;
  ThisTerm:UTF8String;
  Lang:TLang;
begin
  bbImport.enabled := False;
  rows := mmTerms.Lines.Count;
  if rows = 0 then exit;
  Lang := cbSrcLang.Text;
  ProgressBar1.Max := Rows;
  ProgressBar1.Min := min;
  ProgressBar1.Visible:=True;
  for row := min to Rows do
    begin
      Application.ProcessMessages;
      ProgressBar1.Position:= row;
      ThisTerm := Trim(mmTerms.Lines[row]);
      if Length(ThisTerm) = 0 then continue;
  // Add the word to the glossary for tokenization
  // Should really check for spaces, unwanted punctuation, etc.
      If not TextCheck(Lang, ThisTerm, mdWord) then
        begin
          RC := TextAdd(Lang, ThisTerm,  mdWord);
  //ShowMessage(mmSrcText.SelText);
  // Check the error code, display error is mecessary, etc.
          If RC <> 0 then
          ShowMessage(GetLastErrorMessage);

        end;
    end;

  ProgressBar1.Visible:=False;
  bbImport.enabled := True

end;

procedure TfrmBIMain.FormShow(Sender: TObject);
 begin
  cbSrcLang.Items := LangList.Strings;
 end;

end.

