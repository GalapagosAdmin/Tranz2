unit frmTrainerMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ActnList;

type

  { TForm1 }

  TForm1 = class(TForm)
    acUpdateCoverage: TAction;
    ActionList1: TActionList;
    bbGo: TBitBtn;
    cbSB1: TCheckBox;
    cbSB2: TCheckBox;
    cbSB3: TCheckBox;
    edDestPart1: TEdit;
    edDestPart2: TEdit;
    edSrcPart3: TEdit;
    EdDestPart3: TEdit;
    edSrcPart1: TEdit;
    edSrcPart2: TEdit;
    edSrc1: TEdit;
    edSrc2: TEdit;
    edDest1: TEdit;
    edDest2: TEdit;
    gbPair1: TGroupBox;
    gbPair2: TGroupBox;
    edSrcLang: TLabeledEdit;
    edDestLang: TLabeledEdit;
    gbResults: TGroupBox;
    gbLangSelect: TGroupBox;
    sbAddPair1: TSpeedButton;
    dbAddPair2: TSpeedButton;
    sbAddPair3: TSpeedButton;
    sbDelPair1: TSpeedButton;
    sbDelPair2: TSpeedButton;
    sbDelPair3: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure acUpdateCoverageExecute(Sender: TObject);
    procedure bbGoClick(Sender: TObject);
    procedure dbAddPair2Click(Sender: TObject);
    procedure edSrcPart2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gbPair2Click(Sender: TObject);
    procedure edSrcLangChange(Sender: TObject);
    procedure gbResultsClick(Sender: TObject);
    procedure sbAddPair1Click(Sender: TObject);
    procedure sbAddPair3Click(Sender: TObject);
    procedure sbDelPair1Click(Sender: TObject);
    procedure sbDelPair2Click(Sender: TObject);
    procedure sbDelPair3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }
Uses TransTrain, transDB;

procedure TForm1.gbPair2Click(Sender: TObject);
begin

end;

procedure TForm1.bbGoClick(Sender: TObject);
var
 SrcBlock1, SrcBlock2, SrcBlock3:string;
 DestBlock1, DestBlock2, DestBlock3:string;
begin
  if PairSplit(edSrc1.text, edDest1.text,
               edSrc2.Text, edDest2.Text,
               SrcBlock1, DestBlock1,
               SrcBlock2, DestBlock2,
               SrcBlock3, DestBlock3) = 0 then
    begin
      edSrcPart1.Text := SrcBlock1;
      edSrcPart2.Text := SrcBlock2;
      edSrcPart3.Text := SrcBlock3;
      acUpdateCoverage.Execute;
      edDestPart1.Text := DestBlock1;
      edDestPart2.Text := DestBlock2;
      edDestPart3.Text := DestBlock3;
    end
  else
  //if BlockSplit(edSrc1.text, edSrc2.Text, block1, block2, block3) = 0 then
  //  begin
  //    edSrcPart1.Text := block1;
  //    edSrcPart2.Text := block2;
  //    edSrcPart3.Text := block3;
  //  end
  //else
   begin
     edSrcPart1.Text := '';
     edSrcPart2.Text := '';
     edSrcPart3.Text := '';
     cbSB1.Checked:= False;
     cbSB2.Checked:= False;
     cbSB3.Checked:= False;
  // end;
  //if BlockSplit(edDest1.text, edDest2.Text, block1, block2, block3) = 0 then
  //  begin
  //   edDestPart1.Text := block1;
  //   edDestPart2.Text := block2;
  //   edDestPart3.Text := block3;
  //  end
  //else
  // begin
     edDestPart1.Text := '';
     edDestPart2.Text := '';
     edDestPart3.Text := '';
   end;
end; // of PROCEDUREベ

procedure TForm1.dbAddPair2Click(Sender: TObject);
begin
 TextPairAdd(edSrcLang.Text, edSrcPart2.Text, edDestLang.Text, edDestPart2.Text);
 acUpdateCoverage.Execute;
end;

procedure TForm1.acUpdateCoverageExecute(Sender: TObject);
// Update database coverage after changes
begin
  cbSB1.Checked := TranslateCoverage(edSrcLang.text, edSrcPart1.Text, edDestLang.text);
  cbSB2.Checked := TranslateCoverage(edSrcLang.text, edSrcPart2.Text, edDestLang.text);
  cbSB3.Checked := TranslateCoverage(edSrcLang.text, edSrcPart3.Text, edDestLang.text);

end;

procedure TForm1.edSrcPart2Change(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.edSrcLangChange(Sender: TObject);
begin

end;

procedure TForm1.gbResultsClick(Sender: TObject);
begin

end;

procedure TForm1.sbAddPair1Click(Sender: TObject);
begin
 TextPairAdd(edSrcLang.Text, edSrcPart1.Text, edDestLang.Text, edDestPart1.Text);
 acUpdateCoverage.Execute;
end;

procedure TForm1.sbAddPair3Click(Sender: TObject);
begin
 TextPairAdd(edSrcLang.Text, edSrcPart3.Text, edDestLang.Text, edDestPart3.Text);
 acUpdateCoverage.Execute;
end;

procedure TForm1.sbDelPair1Click(Sender: TObject);
begin
 TextPairDelete(edSrcLang.Text, edSrcPart1.Text, edDestLang.Text, edDestPart1.Text);
 acUpdateCoverage.Execute;
end;

procedure TForm1.sbDelPair2Click(Sender: TObject);
begin
 TextPairDelete(edSrcLang.Text, edSrcPart2.Text, edDestLang.Text, edDestPart2.Text);
 acUpdateCoverage.Execute;
end;

procedure TForm1.sbDelPair3Click(Sender: TObject);
begin
 TextPairDelete(edSrcLang.Text, edSrcPart3.Text, edDestLang.Text, edDestPart3.Text);
 acUpdateCoverage.Execute;
end;

initialization
  {$I frmtrainermain.lrs}

end.

