unit FrmTransFWmain;

// Change Log
//@001 2011.01.12 Noah Silva : Implemented Database Self-Test feature
//                             Added pattern detection
//@002 2011.09.07 Noah Silva : Added TransConst unit to fix GUI
//@003 2011.09.19 Noah Silva : Changes for supporting glossary (mdWord)
//@004 2011.09.23 Noah Silva : Status bar changes
//@005 2011.09.24 Noah Silva : Fixes for pattern mode
//@006 2012.01.12 Noah Silva : Misc. Clean-up
//                             Move resource strings into TBAConst
//@007 2012.01.15 Noah Silva : Use Language List Object
//                             Added function to delete result
//@008 2012.02.05 Noah SILVA : Added DB Self-Check at Start-up
//@009 2012.02.21 Noah SILVA : Brought TransDB function calls up to date
//@010 2012.02.24 Noah SILVA : Switch to helper class for populating language
//                             combo boxes.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, Menus, ActnList;

type

  { TfrmTBAMain }

  TfrmTBAMain = class(TForm)
    acCheckPattern: TAction;
    acInitLangCombo: TAction;
    ActionList1: TActionList;
    bbResultBlockDel: TBitBtn;
    bbSrcBlockAdd: TBitBtn;
    bbDestBlockAdd: TBitBtn;
    bbFwdCRAdd: TBitBtn;
    bbRevCRAdd: TBitBtn;
    bbTextPairAdd: TBitBtn;
    bbSrcBlockDel: TBitBtn;
    bbDestBlockDel: TBitBtn;
    bbFwdCRDel: TBitBtn;
    bbRevCRDel: TBitBtn;
    bbTextPairDel: TBitBtn;
    cbSrcInDB: TCheckBox;
    cbFwdCR: TCheckBox;
    cbDestInDB: TCheckBox;
    cbRevCR: TCheckBox;
    cbSrcLang: TComboBox;
    cbDestLang: TComboBox;
    cbDestSegType: TComboBox;
    ComboBox1: TComboBox;
    cbSrcSegType: TComboBox;
    GroupBox2: TGroupBox;
    gbTextPairAdd: TGroupBox;
    edDestText: TEdit;
    edSrcText: TEdit;
    gbBlockText: TGroupBox;
    gbHash2Test: TGroupBox;
    gbHashCRAdd: TGroupBox;
    lblDestLang: TLabel;
    lblSrcSegmentType: TLabel;
    lblRelationType: TLabel;
    lblSrcLang: TLabel;
    lblHash2Text: TLabel;
    lblLength: TLabel;
    lblDestSegmentType1: TLabel;
    MainMenu1: TMainMenu;
    miAbout: TMenuItem;
    mhHelp: TMenuItem;
    miDBShowLastError: TMenuItem;
    miDBSelfTest: TMenuItem;
    miDestHashShow: TMenuItem;
    miSourceHashShow: TMenuItem;
    mhDestination: TMenuItem;
    mhSource: TMenuItem;
    mhUtilities: TMenuItem;
    mhFile: TMenuItem;
    rgMode: TRadioGroup;
    StatusBar1: TStatusBar;
//    procedure bbHashCRAddClick(Sender: TObject);
    procedure acCheckPatternExecute(Sender: TObject);
    procedure acInitLangComboExecute(Sender: TObject);
    procedure bbDestBlockDelClick(Sender: TObject);
    procedure bbFwdCRDelClick(Sender: TObject);
    procedure bbResultBlockDelClick(Sender: TObject);
    procedure bbRevCRDelClick(Sender: TObject);
    procedure bbSrcBlockDelClick(Sender: TObject);
    procedure brnHashCRAddClick(Sender: TObject);
    procedure btnErrMsgGetClick(Sender: TObject);
//    procedure btnHash1TextGetClick(Sender: TObject);
//    procedure btnHash2GetClick(Sender: TObject);
    procedure btnHash2Text2AddClick(Sender: TObject);
    procedure btnHash2TextGetClick(Sender: TObject);
    procedure btnHashTextAddClick(Sender: TObject);
    procedure btnTextPairAddClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnDBTestClick(Sender: TObject);
    procedure cbRevCRChange(Sender: TObject);
    procedure cbSrcLangChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure edDestTextChange(Sender: TObject);
    procedure edSrcTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
//    procedure FormShow(Sender: TObject);
//    procedure gbBlockTextClick(Sender: TObject);
//    procedure edTTSrcChange(Sender: TObject);
//    procedure FormCreate(Sender: TObject);
//    procedure gbHashCRGetClick(Sender: TObject);
//    procedure lblDBTestResultClick(Sender: TObject);
//    procedure lblHashClick(Sender: TObject);
    procedure lblReverseCRAddClick(Sender: TObject);
    procedure mhHelpClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miDBSelfTestClick(Sender: TObject);
//    procedure mhDestinationClick(Sender: TObject);
//    procedure mhUtilitiesClick(Sender: TObject);
//    procedure miDBSelfTestClick(Sender: TObject);
    procedure miDBShowLastErrorClick(Sender: TObject);
    procedure miDestHashShowClick(Sender: TObject);
//    procedure miOpenClick(Sender: TObject);
//    procedure mhFileClick(Sender: TObject);
    procedure miSourceHashShowClick(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmTBAMain: TfrmTBAMain;

implementation

{ TfrmTBAMain }
uses
 TBAConst, TransDB,
 pattern,                                                                       //@001+
 transconst,                                                                    //@002+
 dbugintf,                                                                      //@003+
 tolanglist,                                                                    //@007+
 toLangListCombo;                                                               //@010+

procedure TfrmTBAMain.Button1Click(Sender: TObject);
Var
 TmpStr:String;
begin
//  TmpStr := string2hash(edSrcText.Text);
//  lblHash.caption := TmpStr;
//  LblLength.caption := IntToStr(length(TmpStr));
end;

procedure TfrmTBAMain.btnErrMsgGetClick(Sender: TObject);
begin
//  lblErrMsg.caption := GetLastErrorMessage;
end;

procedure TfrmTBAMain.brnHashCRAddClick(Sender: TObject);
Var
 rc:integer;
begin
//  rc :=  HashCRAdd(leSrcLang.text, string2hash(edSrcText.Text),               //@003-
//                      leDestLang.text, string2hash(edDestText.Text));         //@003-
  rc :=  BlockCRAdd(cbSrcLang.text, edSrcText.Text,                             //@003+@007=
                    cbDestLang.text, edDestText.Text, rgMode.ItemIndex);        //@003+@007=
  //cbPatternMode.Checked);
  If RC <> 0 then
    showmessage(msgError)                                                       //@006=
  else
   StatusBar1.Panels[0].Text := msgFwdCrUpdateOk;                               //@004=@006=

// Update Checkbox
  edSrcTextChange(Self);
end;

procedure TfrmTBAMain.bbSrcBlockDelClick(Sender: TObject);
var
  LastError:string;
  RC:integer;
begin
//  RC := HashTextDelete(leSrcLang.text, string2hash(edSrcText.Text));
   RC := TextDelete(cbSrcLang.text,                                             //@007=
               edSrcText.Text, rgMode.ItemIndex);                               //@003+
               //cbPatternMode.checked);                                        //@003-
  If RC <> 0 then
    begin
      LastError := GetLastErrorMessage;
      ShowMessage(LastError) ;
    end
   Else
     StatusBar1.Panels[0].Text := msgSrcEntryDeleteOk;                          //@004=@006=
  edSrcTextChange(Self);
end;

procedure TfrmTBAMain.bbDestBlockDelClick(Sender: TObject);
var
  LastError:string;
  RC:integer;
begin
//  RC := HashTextDelete(leDestLang.text, string2hash(edDestText.Text));
  RC := TextDelete(cbDestLang.text,                                             //@007=
     edDestText.Text, rgMode.ItemIndex);                                        //@003=
     //cbPatternMode.Checked);                                                  //@003-
  If RC <> 0 then
    begin
      LastError := GetLastErrorMessage;
      ShowMessage(LastError);
    end
  Else
    StatusBar1.Panels[0].Text := msgDestEntryDeleteOK ;                         //@004=@006=

  edDestTextChange(Self);
end;

procedure TfrmTBAMain.acCheckPatternExecute(Sender: TObject);
begin
// cbPatternMode.Checked :=
// ( Contains_Pattern(edSrcText.Text)   and
// Contains_Pattern(edDestText.Text) );
end;

procedure TfrmTBAMain.acInitLangComboExecute(Sender: TObject);                  //@007+
begin
// cbSrcLang.Items := LangList.Strings;                                         //@010-
  cbSrcLang.fill;                                                               //@010+
  cbSrcLang.ItemIndex:=0;
//  cbDestLang.Items := LangList.Strings;                                       //@010-
  cbDestLang.Fill;                                                              //@010+
  // Perhaps we should check the number of entries first...
  cbDestLang.ItemIndex:=1;

  frmTBAMain.Enabled := (DBSelfTest = 0);                                       //@008+
  if not (DBSelfTest = 0) then ShowMessage(rsDBCheckFailed);                    //@008+

end;

procedure TfrmTBAMain.bbFwdCRDelClick(Sender: TObject);
Var
 rc:integer;
 mode:Integer;                                                                  //@006+
begin
  mode := rgMode.ItemIndex;                                                     //@006+
  rc :=  BlockCRDelete(cbSrcLang.text, edSrcText.Text,                          //@007=
                    cbDestLang.text, mode);                                     //@003=@007=
                    //cbPatternMode.Checked);                                   //@003-
  If RC <> 0 then
    showmessage(msgError + ': '+GetLastErrorMessage)                            //@006=
  else
   StatusBar1.Panels[0].Text := msgFwdCRDeleted;                                //@004=@006=
// Update Checkbox
  edSrcTextChange(Self);
end;

procedure TfrmTBAMain.bbResultBlockDelClick(Sender: TObject);                      //@007=
var
  LastError:string;
  RC:integer;
begin
  RC := TextDelete(cbDestLang.text,
     lblHash2Text.Caption, rgMode.ItemIndex);
     //cbPatternMode.Checked);
  If RC <> 0 then
    begin
      LastError := GetLastErrorMessage;
      ShowMessage(LastError);
    end
  Else
    StatusBar1.Panels[0].Text := msgDestEntryDeleteOK ;

  edDestTextChange(Self);
end;

procedure TfrmTBAMain.bbRevCRDelClick(Sender: TObject);
Var
 rc:integer;
begin
  rc :=  BlockCRDelete(cbDestLang.text, edDestText.Text,                        //@007=
                    cbSrcLang.text,  rgMode.ItemIndex);                         //@003=//@007=
  //cbPatternMode.Checked);                                                     //@003-//@007=
  If RC <> 0 then
   showmessage('Error')
  else
   StatusBar1.Panels[0].Text := msgRevCRDeleted;                                //@004=@006=
// Update Checkboxes
  edDestTextChange(Self)
end;







procedure TfrmTBAMain.btnHash2Text2AddClick(Sender: TObject);
var LastError:string;
    RC:integer;
begin
//  RC := HashTextAdd(string2hash(edDestText.Text), leDestLang.text, edDestText.text);
//  Case cbPatternMode.Checked of
//    True:RC := TextAdd(leDestLang.text, edDestText.text, mdPattern);
//    False:RC := TextAdd(leDestLang.text, edDestText.text, mdFragment);
//  end;
  RC := TextAdd(cbDestLang.Text, edDestText.text,  rgMode.ItemIndex);           //@007=
  If RC <> 0 then
    begin
      LastError := GetLastErrorMessage;
      ShowMessage(LastError);
   end
  else
    StatusBar1.Panels[0].Text := msgDestEntryUpdateOK;                          //@004=
// Update Checkbox
  edDestTextChange(Self);
end;

procedure TfrmTBAMain.btnHash2TextGetClick(Sender: TObject);
 var                                                                            //@009+
     mode:Integer;                                                              //@009+
begin
  Mode := rgMode.ItemIndex;                                                     //@009+
// Retrieves translated result from the database
    lblHash2text.Caption:= //HashTextGet(lblhash2.caption);
      TranslateText(cbSrcLang.Text, edSrcText.Text,                             //@007=
                           cbDestLang.text, Mode);                              //@007=@009=
end;

procedure TfrmTBAMain.btnHashTextAddClick(Sender: TObject);
var LastError:string;
    RC:integer;
begin
//  RC := HashTextAdd(string2hash(edSrcText.Text), leSrcLang.text, edSrcText.text);
//  Case cbPatternMode.Checked of
//    True:  RC := TextAdd(leSrcLang.text, edSrcText.Text, mdPattern);
//    False:   RC := TextAdd(leSrcLang.text, edSrcText.Text, mdFragment);
//  end;
  RC := TextAdd(cbSrcLang.Text, edSrcText.Text,  rgMode.ItemIndex);             //@007=
  If RC <> 0 then
    begin
      LastError := GetLastErrorMessage;
      ShowMessage(LastError) ;
    end
  Else
    StatusBar1.Panels[0].Text := msgSrcEntryUpdateOk;                           //@004=@006=

  edSrcTextChange(Self);
end;

procedure TfrmTBAMain.btnTextPairAddClick(Sender: TObject);
Var
 rc:integer;
 Mode:Integer;
begin
  //SendDebug('Selected Mode:' + IntToStr(rgMode.ItemIndex));
  rc := TextPairAdd(cbSrcLang.text, edSrcText.text,                             //@007=
                    cbDestLang.Text, edDestText.text,                           //@007=
                    rgMode.ItemIndex);
                   //                cbPatternMode.Checked);
 if RC <> 0 then
  showmessage(inttostr(RC))
 else
  begin
    StatusBar1.Panels[0].Text := msgEntryPairCrUpdateOk;                        //@004=@006=
    StatusBar1.Color:= clGreen;                                                 //@005+
  end;
// Update Checkboxes
  edSrcTextChange(Self);
  edDestTextChange(Self);
end;

procedure TfrmTBAMain.btnDBTestClick(Sender: TObject);
begin
//  lblDBTestResult.Caption := inttostr(DBSelfTest);
end;

procedure TfrmTBAMain.cbRevCRChange(Sender: TObject);
begin

end;

procedure TfrmTBAMain.cbSrcLangChange(Sender: TObject);
begin

end;

procedure TfrmTBAMain.ComboBox1Change(Sender: TObject);
begin

end;

procedure TfrmTBAMain.edDestTextChange(Sender: TObject);
Var
 DestHash:String;
 RC:Integer;
begin
  acCheckPattern.execute;                                                       //@001+
  Case Contains_Pattern(edDestText.Text) of                                     //@001+
    true:edDestText.Color := clLime                                             //@001+
  else                                                                          //@001+
    edDestText.Color := clWindow ;                                              //@001+
  end;                                                                          //@001+
  DestHash := string2hash(edDestText.Text);
  (*                                                                            //@007-
  Case rgMode.ItemIndex of                                                      //@003+
    mdFragment: RC := HashTextCheck(leDestLang.text, DestHash, mdFragment);     //@005=
    //  TmpStr := string2hash(edDestText.Text);
//    mdPattern:  RC := HashPatternCheck(leDestLang.text, DestHash);            //@001+@003=@005-
    mdPattern:  RC := HashTextCheck(leDestLang.text, DestHash, mdPattern);      //@005+
    mdWord:     RC := HashTextCheck(leDestLang.text, DestHash, mdWord);         //@003+
  End;                                                                          //@003+
  *)
  RC := HashTextCheck(cbDestLang.text, DestHash, rgMode.ItemIndex);             //@007+
    cbDestInDb.Checked := (RC = 0);
//  lblHash2entered.caption := TmpStr;
  cbRevCR.Checked := TranslateCoverage(cbDestLang.text,                         //@007=
                                       edDestText.Text,
                                       cbSrcLang.text, rgMode.ItemIndex);       //@003=@007=
end;

procedure TfrmTBAMain.edSrcTextChange(Sender: TObject);
Var
 SrcHash:String;
 RC:Integer;
 STR:String;
 coverage:boolean;                                                              //@001+
begin
 try
  acCheckPattern.execute;                                                       //@001+
  Case Contains_Pattern(edSrcText.Text) of                                      //@001+
    true:edSrcText.Color := clLime ;                                            //@001+
  else                                                                          //@001+
    edSrcText.Color := clWindow ;                                               //@001+
  end;                                                                          //@001+
// @005 not sure why we were Stripping the pattern placeholders, the DB entry
//      has them, so it won't find a match if we strip them.
  SrcHash := string2hash(edSrcText.Text);                                       //@001-@005+
//  SrcHash := string2hash(Strip_Pattern(edSrcText.Text));                      //@001+@005-
//  Case cbPatternMode.Checked of                                               //@001+003-
//  SendDebug('WayPoint1');
  Case rgMode.ItemIndex of                                                      //@003+
    mdFragment, mdPattern, mdWord:                                              //@005=
    //    RC := HashTextCheck(leSrcLang.text, SrcHash, mdFragment);             //@003=@005-
       RC := HashTextCheck(cbSrcLang.text, SrcHash, rgMode.ItemIndex);          //@005+//@007=
//    mdPattern:  RC := HashPatternCheck(leSrcLang.text, SrcHash);              //@001+003=@005-
//    mdWord:     RC := HashTextCheck(leSrcLang.text, SrcHash, mdWord);         //@003+@005-
    else
      ShowMessage(msgUnsupportedMode + IntToStr(rgMode.ItemIndex));             //@006=
  end; // of CASE                                                               //@001+
//  SendDebug('WayPoint2');
//  str := HashTextGet(leSrcLang.text, SrcHash);
//  StatusBar1.SimpleText:= IntToStr(rc);
//  StatusBar1.SimpleText:= IntToStr(str);
 cbSrcInDb.Checked := (RC = 0);
//   case RC of
//    IF str <> '' then
//    0:cbSrcInDb.Checked := true;
//   else
//    cbSrcInDb.Checked := false;
//  end; // of CASE
  // show the current resulting destination hash from Cross-reference (if any)
//  lblHash2.caption := HashCRGet(leSrcLang.text, string2hash(edSrcText.Text),
//                      leDestLang.Text);
// Case cbPatternMode.Checked of                                                //@003-
//  SendDebug('Waypoint 3');
//  Case rgMode.ItemIndex of                                                    //@003+@005-
  // mdPattern:Coverage :=  PatternCoverage(leSrcLang.text,                     //@003+@005-
   //@003 Not sure why the strip pattern is here, leaving it for now
//   Strip_Pattern(edSrcText.Text), leDestLang.text);                           //@005-
  //@005 Remove strip_pattern after all
  //                                         edSrcText.Text, leDestLang.text);  //@005+
 //  else                                                                       //@003+@005-
      Coverage := TranslateCoverage(cbSrcLang.text, edSrcText.Text,             //@007=
                                  cbDestLang.text, rgMode.ItemIndex );          //@003+@007=
// end; // of CASE                                                              //@005-
//  SendDebug('Waypoint 4');

//  Case TranslateCoverage(leSrcLang.text, edSrcText.Text, leDestLang.text) of  //@001-
  Case Coverage of                                                              //@001+
   TRUE:Begin
     //HashTextGet(lblhash2.caption);
//          Case rgMode.ItemIndex of                                            //@003+@005-
// Case cbPatternMode.Checked of                                                //@001+003-
//     mdPattern: lblHash2text.Caption :=  TranslatePattern(leSrcLang.Text,     //@001+@003=@005-
//     Strip_Pattern(edSrcText.Text), leDestLang.text);                         //@001+@005-
//     edSrcText.Text, leDestLang.text);                                        //@005+-
//     else                                                                     //@003+@005-
       lblHash2text.Caption := TranslateText(cbSrcLang.Text,                    //@007=
                            edSrcText.Text, cbDestLang.text, rgMode.ItemIndex); //@003+@007=
// End;                                                                         //@001+@005-
     cbFwdCR.Checked := True
        end;
   FALSE:Begin
          cbFwdCR.Checked := False;
          lblHash2text.Caption := msgNoTransAvail;                              //@006=
         end;
  end;   // of CASE

  except
    SendDebug('TransFW: OnSrcTextChange Error.');
  end;
end;   // of PROCEDURE

procedure TfrmTBAMain.FormCreate(Sender: TObject);
begin

end;




procedure TfrmTBAMain.lblReverseCRAddClick(Sender: TObject);
Var
 rc:integer;
begin
//  rc :=  HashCRAdd( leDestLang.text, string2hash(edDestText.Text),
//                    leSrcLang.text, string2hash(edSrcText.Text)
  rc :=  HashCRAdd( cbDestLang.text, edDestText.Text,                           //@007=
                    cbSrcLang.text, edSrcText.Text, rgMode.ItemIndex);          //@003=@007=
                    //cbPatternMode.Checked);                                   //@003-
  If RC <> 0 then
   showmessage(msgError)                                                        //@007=
  else
   StatusBar1.Panels[0].Text := msgRevCRUpdateOK;                               //@004=@006=
// Update Checkboxes
  edDestTextChange(Self)
end;

procedure TfrmTBAMain.mhHelpClick(Sender: TObject);
begin

end;

procedure TfrmTBAMain.miAboutClick(Sender: TObject);
begin
  ShowMessage(ApplicationName + ' ' + CopyrightMessage);
end;

// 2011.01.12 NS Added Procedure
procedure TfrmTBAMain.miDBSelfTestClick(Sender: TObject);
begin
  Case DBSelfTest of
   0:ShowMessage(msgDbTestOk);                                                  //@006=
   else
     ShowMessage(msgDbTestErr);                                                 //@006=
  end;
end;


procedure TfrmTBAMain.miDBShowLastErrorClick(Sender: TObject);
 var
  msg:string;
begin
  msg := GetLastErrorMessage;
  if MSG = '' then
    ShowMessage(msgNoErrMsg)                                                    //@006=
  else
    ShowMessage(msg);
end;

procedure TfrmTBAMain.miDestHashShowClick(Sender: TObject);
begin
  ShowMessage(string2hash(edDestText.Text));
end;

procedure TfrmTBAMain.miSourceHashShowClick(Sender: TObject);
begin
  ShowMessage(string2hash(edSrcText.Text) );
end;

procedure TfrmTBAMain.rgModeClick(Sender: TObject);
begin
    edSrcTextChange(Self);
end;


initialization
  {$I frmtransfwmain.lrs}

end.

