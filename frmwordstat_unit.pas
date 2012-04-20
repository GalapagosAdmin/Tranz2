unit frmWordStat_Unit;

//@000 2012.01.27 Noah SILVA Initial Version
//@001 2012.01.30 Noah SILVA Bug Fixes, Added Database updates
//@002 2012.01.31 Noah SILVA Testing support for N-Tuple
//@003 2012.02.05 Noah SILVA Added DB Self-Check at start-up
//@004 2012.04.20 Noah SILVA Feature for adding new words to glossary
// WordStat: Calculates Frequency of words based on text input
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls;

type

  { TfrmWordStat }



  TfrmWordStat = class(TForm)
    bbUpdate: TBitBtn;
    btnProcess: TBitBtn;
    btnAddWord: TButton;
    lblTokensPerEntry: TLabel;
    cbSrcLang: TComboBox;
    ListView1: TListView;
    mmSrcText: TMemo;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    procedure bbUpdateClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure btnAddWordClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmWordStat: TfrmWordStat;

implementation

Uses TransToken,
     TransDB,                                                                   //@001+
     TransTokenRemix,                                                           //@002+
     toLangList,                                                                //@003+
     TransConst;                                                                //@004+

{ TfrmWordStat }

procedure TfrmWordStat.btnProcessClick(Sender: TObject);
 var
   raw:TStringList;  // StringList of Single Token Entries
   raw2:TStringList; // stringList of 2-token entries                           //@002+
   CurrentItem:Longint;
   TokenStats:TTokenStatArray;
   n:Integer;                                                                   //@002=


begin
  try
    mmSrcText.Text:= NormalizeQuotes(mmSrcText.Text);
    n := Trackbar1.Position;                                                    //@002+
    Raw := TStringList.Create;
    Raw2 := TStringList.Create;                                                 //@002+
    Raw := Tokenize(cbSrcLang.text, mmSrcText.text);

    Tuple_1_to_n(cbSrcLang.text,Raw, Raw2, n);                                  //@002+

    Raw2.Sort;  // Doesn't Remove Duplicates                                    //@002=

    SL2TS(Raw2, TokenStats);                                                    //@002=

    ListView1.Clear;
    // Add to listview
      For CurrentItem := 0 to Length(TokenStats)-1 do
      With ListView1.Items.Add do
        begin
         Caption := TokenStats[CurrentItem].Token;
         SubItems.Add(IntToStr(TokenStats[CurrentItem].Count));
        end;


  finally
    raw.free;
    raw2.free;                                                                  //@002+
  end;
end;

procedure TfrmWordStat.btnAddWordClick(Sender: TObject);                           //@004+
begin
//  ShowMessage(mmSrcText.SelText);
  if mmSrcText.SelText = '' then exit;
  TextAdd(cbSrcLang.Text, mmSrcText.SelText, mdWord)
end;

procedure TfrmWordStat.FormShow(Sender: TObject);
begin
  frmWordStat.Enabled := (DBSelfTest = 0);                                      //@003+
  if not (DBSelfTest = 0) then ShowMessage(rsDBCheckFailed);                    //@003+
  cbSrcLang.Items := LangList.Strings;                                          //@003+
  cbSrcLang.ItemIndex:=0;                                                       //@003+

end;

procedure TfrmWordStat.bbUpdateClick(Sender: TObject);
 var
    raw:TStringList;
    raw2:TStringList; // stringList of 2-token entries                          //@002+
    CurrentItem:Longint;
    TokenStats:TTokenStatArray;
    n:Integer;                                                                   //@002=
 begin
   try
    mmSrcText.Text:= NormalizeQuotes(mmSrcText.Text);
    n := Trackbar1.Position;                                                    //@002+

    Raw := TStringList.Create;
    Raw := Tokenize(cbSrcLang.text, mmSrcText.text);
    Raw2 := TStringList.Create;                                                 //@002+

    Tuple_1_to_n(cbSrcLang.text,Raw, Raw2, n);                                  //@002+

//    Raw.Sort;  // Doesn't Remove Duplicates
    Raw2.Sort;  // Doesn't Remove Duplicates                                    //@002=

//    SL2TS(Raw, TokenStats);                                                   //@002-
    SL2TS(Raw2, TokenStats);                                                    //@002+




    // Add to listview


    // Add to database
    ProgressBar1.Visible := True;
    ProgressBar1.Max := Length(TokenStats)-1;
    For CurrentItem := 0 to Length(TokenStats)-1 do
      begin
        ProgressBar1.Position:= CurrentItem;
        Application.ProcessMessages;
        StatSegAdd(cbSrcLang.text, TokenStats[CurrentItem].Token,
                   n, TokenStats[CurrentItem].Count);
      end;
    ProgressBar1.Visible := False;


  finally
    raw.free;
    raw2.free;                                                                  //@002+
  end;
end;



{$R *.lfm}

end.

