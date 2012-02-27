unit FrmTTmain;

{$mode objfpc}{$H+}

//@001 2011.09.06 Noah SILVA Added Debug
//@002 2011.09.19 Noah SILVA Changed terminology Block->Fragment
//                           Added Fragment count
//@003 2011.09.26 Noah SILVA String -> UTF8String
//@004 2011.10.07 Noah SILVA Added Icons, modified company name
//@005 2012.02.07 Noah SILVA Convert language selection from LabelEdit to ComboBox
//                           Convert hard-coded test strings to ResourceString
//                           "Add Word" function for glossary (fine-tune tokenizer)

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Grids, CheckLst, ComCtrls, {PairSplitter, }Menus, ActnList,
  Buttons; //, ValEdit;

type

  { TfrmTokenTst }

  TfrmTokenTst = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    bbCoverageMap: TBitBtn;
    bbCandidateShow: TBitBtn;
    bbAddWord: TBitBtn;
    btnRemix: TButton;
    cbSrcLang: TComboBox;
    cbDestLang: TComboBox;
    ilFlag: TImageList;
    imgSrcLang: TImage;
    imgDestLang: TImage;
    lblSrcLAng: TLabel;
    lblFragmentCnt: TLabel;
    lblDestLang: TLabel;
  //  cbDestLang: TLabeledEdit;
    lbRemix: TCheckListBox;
    lblCMEntries: TLabel;
    lblHoles: TLabel;
    lblTranslation: TLabel;
    lblRecomposed: TLabel;
    lblRLL: TLabel;
    lblTranslations: TLabel;
    lblRemix: TLabel;
    lblTokens: TLabel;
    lbTokens: TListBox;
    lbTranslated: TListBox;
    MainMenu1: TMainMenu;
    mmSrcText: TMemo;
    miAbout: TMenuItem;
    mhHelp: TMenuItem;
    sgCandidates: TStringGrid;
    tmrRemix: TTimer;
    procedure bbAddWordClick(Sender: TObject);
    procedure btnCoverageMapGenClick(Sender: TObject);
    procedure btnRemixClick(Sender: TObject);
    procedure btnTokenizeClick(Sender: TObject);
    procedure btnCandidateShowClick(Sender: TObject);
    procedure cbSrcLangChange(Sender: TObject);
    procedure edSrcTextChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgDestLangClick(Sender: TObject);
    procedure lbRemixClick(Sender: TObject);
    procedure lbRemixClickCheck(Sender: TObject);
    procedure lbRemixEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbRemixItemClick(Sender: TObject; Index: integer);
    procedure leDestLangChange(Sender: TObject);
//    procedure lbRemixSelectionChange(Sender: TObject; User: boolean);
//    procedure cbSrcLangChange(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure tmrRemixTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmTokenTst: TfrmTokenTst;

implementation

uses TransToken, TransTokenRemix, TransDB, TransConst,
  dbugintf,                                                                     //@001+
  toLangList  ;                                                                 //@005+

{ TfrmTokenTst }

ResourceString                                                                  //@005+
  rsCoverageMapEmpty = 'Empty coverage map.';                                   //@005+
  rsGridHeaderError  = 'Grid Header Populate Error: ';                          //@005+
  rsNoTranslation    = '(no translation)';                                      //@005+
  rsPRoductName      = 'Tokenizer Test and Coverage Map Analyzer';              //@005+
  rsCopyrightNotice  = 'Copyright 2009-2012 by Takanawa Software, Galapagos Software'; //@005+

procedure TfrmTokenTst.btnTokenizeClick(Sender: TObject);
begin
  lbTokens.Items := Tokenize(cbSrcLang.text, mmSrcText.text);
end;



procedure TfrmTokenTst.btnCandidateShowClick(Sender: TObject);
Const
 FixedRows = 1;
 FixedCols = 3;
var
 col, row:integer;
 RemixList:TRemixList;
 RC:Integer;
begin

  SetLength(RemixList, 0);

// fill with numbers for a test
//  for Row := 0 to high(RemixList) do
//    begin
//      SetLength(RemixList, high(RemixList)+2);
//      SetLength(RemixList[Row].Tokens, sgCandidates.ColCount-1);
//      showmessage(inttostr(high(remixlist[row].Tokens)));
//      For Col := 0 to high(RemixList[row].Tokens) do
//        RemixList[row].Tokens[col] := inttostr(col);
//     end;

// Call TokenRemix Function
  Try
    RC := TokenRemix(cbSrcLang.text, TTokenList(lbTokens.Items), RemixList);
  Except  on E: Exception do
     showmessage(inttostr(rc) + e.message);
  End; // of TRY

// Display results
// Will result in an access violation if there aren't enough rows/cols on the grid
  Try
  // Size the grid appropriately
  sgCandidates.RowCount := high(Remixlist) + 1 + FixedRows;
  sgCandidates.ColCount := lbTokens.Count + FixedCols;
  // fill in the header text;
  sgCandidates.FixedRows:= FixedRows;
  sgCandidates.FixedCols:= FixedCols;
  For Col := 0 to sgCandidates.ColCount - 1 do
   case col of
    0: sgCandidates.Cells[col, 0] := 'BlockNo';
    1: sgCandidates.Cells[col, 0] := 'Offset';
    2: sgCandidates.Cells[col, 0] := 'Length';
   else
    sgCandidates.Cells[col, 0] := inttostr(col-FixedCols);
   end;
  For Row := 1 to sgCandidates.RowCount - 1 do
   sgCandidates.Cells[0, Row] := inttostr(row);
  // Fill in the actual content with the blocks from the remix list
  For Row := 0 to high(Remixlist) do
    For Col := 0 to high(remixlist[row].Tokens) do
      sgCandidates.Cells[col+FixedCols, row+FixedRows] := RemixList[row].Tokens[col];
    except  on E: Exception do   showmessage('output ' + e.message);
  end;
 // Fill in the Rn start and length information
   For Row := 0 to high(Remixlist) do
     begin
      sgCandidates.Cells[1, row+1] := inttostr(RemixList[row].RunStart);
      sgCandidates.Cells[2, row+1] := inttostr(RemixList[row].RunLength);
     end;

//  sgCandidates.Clear;
//  For Col := 1 to lbTokens.Count do
//  sgCandidates.Cells[Col, 1] := lbTokens.Items[Col-1];
end;


procedure TfrmTokenTst.btnRemixClick(Sender: TObject);
var
 tmp:TStringList;
 BlockNo:Integer;
 Mode:Integer;                                                                  //@005+
begin
  Mode := mdFragment;                                                           //@005+
  tmrRemix.Enabled := false;
  tmp := TStringList(lbTokens.Items);
  // Get the Token Remixes and populate the Permutations box with them
  lbRemix.Items := TokenRemix(cbSrcLang.text, tmp);
  lblFragmentCnt.Caption:='(' + IntToStr(lbRemix.Count) + ')';
  // Translate and populate the translation listbox
  lbTranslated.Clear;
  For BlockNo := 0 to (lbRemix.Count - 1) do
    Begin
      CASE translateCoverage(cbSrcLang.Text, lbRemix.Items[BlockNo],
                                                    cbDestLang.Text, Mode) of   //@005=
        True:Begin
               lbRemix.Checked[BlockNo] := True;
               lbTranslated.Items.Append(
                   TranslateText(cbSrcLang.Text, lbRemix.Items[BlockNo],
                                    cbDestLang.text, Mode)                      //@005=
                                         );
              end;
        False:Begin
                lbRemix.Checked[BlockNo] := False;
                lbTranslated.Items.Append(rsNoTranslation);                     //@005=
              end;
      end; // of CASE

    end; // of FOR blockNo
end;

procedure TfrmTokenTst.btnCoverageMapGenClick(Sender: TObject);
Const
 FixedRows = 1;
 FixedCols = 1;
var
 rc:integer;
 row, col:integer;
 CoverageMap:TCoverageMap;
 rll:TRemixList;
 hole_rll:TRemixList;
 Msg:UTF8String;                                                                //@003=
 Token:Integer;
 holeptr:integer;
 RunStop:Integer;
begin

// First, actually build the coverage map in memory
  sgCandidates.Clear;

  try
     RC :=
       CoverageMatrixBuild(cbSrcLang.text, TTokenList(lbTokens.Items),
                         cbDestLang.text, CoverageMap);
   except  on E: Exception do
     showmessage(e.message);
  end;
  if High(CoverageMap) < 0 then
   begin
     ShowMessage(rsCoverageMapEmpty);                                           //@005=
     abort;
   end;
// Reset and resize the grid

  sgCandidates.RowCount:= High(CoverageMap)+1+FixedRows;
  sgCandidates.ColCount:= lbTokens.Count+FixedCols;
//  sgCandidates.RowCount:= High(CoverageMap)+1;
//  sgCandidates.ColCount:= High(CoverageMap[0].bitmap)+1;
// Reset the fixed rows
  sgCandidates.FixedRows:= FixedRows;
  sgCandidates.FixedCols:= FixedCols;
// Fill in the header row
 try
  For Col := 1 to sgCandidates.ColCount - 1 do
   sgCandidates.Cells[col, 0] := inttostr(col-1);
  For Row := 1 to sgCandidates.RowCount - 1 do
   sgCandidates.Cells[0, Row] := inttostr(row);
  except  on E: Exception do
     showmessage(rsGridHeaderError + e.message);                                //@005+
  end; // of TRY

  try
// Populate grid with Coverage Map
   For Row := 0 to sgCandidates.RowCount-1-FixedRows do
    For Col := 0 to sgCandidates.ColCount-1-FixedCols do
      sgCandidates.Cells[col+FixedCols, row+FixedRows] :=
//  showmessage('RowCount: ' + inttostr(sgCandidates.RowCount));
//  showmessage('ColCount: ' + inttostr(sgCandidates.ColCount));
//   For Row := 0 to sgCandidates.RowCount-1 do
//    For Col := 0 to sgCandidates.ColCount-1 do
//      sgCandidates.Cells[col, row] :=
        inttostr(CoverageMap[row].bitmap[col]);
  except  on E: Exception do
     showmessage('CM Populate Error: ' + e.message + ' ' + inttostr(row)+
     ',' + inttostr(col));
  end; // of TRY
// update the caption with the number of entries
   lblCMEntries.Caption:= inttostr(sgCandidates.RowCount-FixedRows) + ' CM Entries';

  CMR2RLL(CoverageMap[0], rll, hole_rll);
// Show RLL entries in a message box
  msg := 'Fragments: ';
  For row := 0 to high(rll) do
    msg := msg + '' + inttostr(rll[row].RunStart)
     + ', ' + inttostr(rll[row].RunLength) + '  ';
  lblRll.Caption := msg;
//  ShowMessage(msg);
// Show holes
  msg := 'Holes: ';
  For row := 0 to high(hole_rll) do
    msg := msg + '' + inttostr(hole_rll[row].RunStart)
     + ', ' + inttostr(hole_rll[row].RunLength) + '  ';
  lblHoles.Caption := msg;
// show recomposed source language in a message box
  msg := '';
  Token := 0;
  For row := 0 to high(rll) do
    begin
     If token <> rll[row].RunStart then // there is a hole (or miscalculation)
      begin
       holeptr := 0;
       while (holeptr < high(hole_rll))
             and (hole_rll[holeptr].RunStart <> token) do
         inc(holeptr);
       ASSERT(hole_rll[holeptr].RunStart = token, 'Main Program: ASSERT failed.');
       RunStop := hole_rll[holeptr].RunStart + hole_rll[holeptr].RunLength - 1;
       for holeptr := hole_rll[holeptr].RunStart to RunStop do
         msg := TokenConcat(cbSrcLang.Text, msg, '<*>');
      end  ; // of IF (hole detection)

     Token := (rll[row].RunStart + rll[row].RunLength);
     msg := TokenConcat(cbSrcLang.Text, msg,
             RemixRow2String(cbSrcLang.text, rll[row], TTokenList(lbTokens.Items))
                       );
    end; // of FOR row
  lblRecomposed.Caption:= msg;
//  ShowMessage(msg);
// show translated version in a message box, including untranslated words from
// the source language.
  msg := '';
  Token := 0;
  For row := 0 to high(rll) do
   begin
     If token <> rll[row].RunStart then // there is a hole (or miscalculation)
      begin
        holeptr := 0;

        while (holeptr < high(hole_rll))
              and (hole_rll[holeptr].RunStart <> token) do
          inc(holeptr);

        ASSERT(hole_rll[holeptr].RunStart = token, 'Main Program: ASSERT failed.');
        RunStop := hole_rll[holeptr].RunStart + hole_rll[holeptr].RunLength - 1;
        for holeptr := hole_rll[holeptr].RunStart to RunStop do
           msg := TokenConcat(cbSrcLang.Text, msg, lbTokens.Items[holeptr]);
      end;  // of IF
     Token := (rll[row].RunStart + rll[row].RunLength);
     msg := TokenConcat(cbDestLang.text, msg,
                        TranslateText(cbSrcLang.text,
                                      RemixRow2String(cbSrcLang.text,
                                                      rll[row],
                                                      TTokenList(lbTokens.Items)
                                      ),
                                      cbDestLang.Text,
                                      mdAuto));
   end;  // of FOR loop

  lblTranslation.caption := msg;
//  ShowMessage(msg);
end;

procedure TfrmTokenTst.bbAddWordClick(Sender: TObject);                         //@005+
var LastError:string;
    RC:integer;
begin
  // Exit if nothing selected;
  if Length(mmSrcText.SelText) = 0 then exit;
  // Add the word to the glossary for tokenization
  // Should really check for spaces, unwanted punctuation, etc.
  RC := TextAdd(cbSrcLang.Text, mmSrcText.SelText,  mdWord);
  //ShowMessage(mmSrcText.SelText);
  // Check the error code, display error is mecessary, etc.
  If RC <> 0 then
    begin
     // LastError := ;
      ShowMessage(GetLastErrorMessage);
    end;
//  Else
//    StatusBar1.Panels[0].Text := msgSrcEntryUpdateOk;
  // Refresh the token list
  edSrcTextChange(self);
end;

procedure TfrmTokenTst.edSrcTextChange(Sender: TObject);
  begin
    try
      lbTokens.Items := Tokenize(cbSrcLang.text, mmSrcText.text);
    except on E: Exception do
     showmessage('Tokenizer Error: ' + e.message);
//    finally
    end;  // of TRY .. EXCEPT
    // Since the remix takes some time, we don't want to do it after every
    // single keypress, we'll wait until they have stopped typing for about
    // half a second.
    tmrRemix.Interval:= 500;
    tmrRemix.Enabled:= true;
  end;

procedure TfrmTokenTst.FormShow(Sender: TObject);
begin
     SendMethodEnter('FrmTTMain.TfrmTokenTst.FormShow');                        //@001+
     cbSrcLang.Items := LangList.Strings;                                       //@005+
     cbDestLang.Items := LangList.Strings;                                      //@005+
end;

procedure TfrmTokenTst.imgDestLangClick(Sender: TObject);
begin

end;

procedure TfrmTokenTst.lbRemixClick(Sender: TObject);
var
  ItemCount :integer;
begin
  For ItemCount := 0 to (lbRemix.Count - 1) do
    lbTranslated.Selected[ItemCount] := lbRemix.Selected[ItemCount];
end;

procedure TfrmTokenTst.lbRemixClickCheck(Sender: TObject);
begin

end;

procedure TfrmTokenTst.lbRemixEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
end;

procedure TfrmTokenTst.lbRemixItemClick(Sender: TObject; Index: integer);
//var
 //itemCount:Integer;
begin
//    For ItemCount := 0 to (lbRemix.Count - 1) do
//    lbTranslated.Selected[ItemCount] := lbRemix.Selected[ItemCount];
end;

procedure TfrmTokenTst.leDestLangChange(Sender: TObject);                       //@004+

 Procedure Update_Flag;
   begin
       // Set Flag Icon
    If Length (cbDestLang.Text) > 0 then
    CASE cbDestLang.Text[1] of
      'E': imgDestLang.Picture.LoadFromFile('.\flag\png\us.png');
      'J': imgDestLang.Picture.LoadFromFile('.\flag\png\jp.png');
      else
        imgDestLang.Picture.Clear;
    end
    else  // Zero Length
      imgDestLang.Picture.Clear;
   end;

begin
  Update_Flag;
end;


procedure TfrmTokenTst.cbSrcLangChange(Sender: TObject);
  Procedure Update_Flag;
    Begin
      // Set Flag Icon
  { TODO 2 -oshiruba -cnice-to-have : Convert images to use TImageList instead of files }
      If Length (cbSrcLang.Text) > 0 then                                       //@004+
       CASE cbSrcLang.Text[1] of                                                //@004+
        'E': imgSrcLang.Picture.LoadFromFile('.\flag\png\us.png');              //@004+
        'J': imgSrcLang.Picture.LoadFromFile('.\flag\png\jp.png');              //@004+
        else                                                                    //@004+
          imgSrcLang.Picture.Clear;                                             //@004+
       end //                                                                   //@004+
      else  // Zero Length                                                      //@004+
        imgSrcLang.Picture.Clear;                                               //@004+
    end; // of [Sub]Procedure

begin
 Update_Flag;                                                                   //@004+

  IF length(mmSrcText.text) = 2 then
     lbTokens.Items := Tokenize(cbSrcLang.text, mmSrcText.text)
 else
     lbTokens.Clear;
end;

procedure TfrmTokenTst.miAboutClick(Sender: TObject);
  begin
    ShowMessage(rsPRoductName + ' ' + rsCopyrightNotice);                       //@004=@005=
  end;

procedure TfrmTokenTst.tmrRemixTimer(Sender: TObject);
begin

end;

initialization
  {$I frmttmain.lrs}

end.
