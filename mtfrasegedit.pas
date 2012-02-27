unit mtfrasegedit;
//@000 2011.11.25 Noah Silva : Initial Version
//@001 2011.11.26 Noah Silva + Added Update action
//@002 2011.11.27 Noah Silva + Exit box functionality
//@003 2011.11.28 Noah Silva + Confirm changes when leaving editing
//@004 2011.12.04 Noah Silva + Add ShowFrame event, populate combo boxes
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Grids, StdCtrls,
  PairSplitter, Buttons, ExtCtrls, ActnList, DividerBevel;

type

  { TfraSegmentEdit }

  TfraSegmentEdit = class(TFrame)
    acFrameShow: TAction;
    acUpdate: TAction;
    alSegEdit: TActionList;
    BitBtn1: TBitBtn;
    cbDestLang: TComboBox;
    cbDocument: TComboBox;
    cbProject: TComboBox;
    cbSrcLang: TComboBox;
    bbDelete: TBitBtn;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    mmDestText: TMemo;
    mmSrcText: TMemo;
    pnlTool: TPanel;
    pnlHeader: TPanel;
    psText: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    sgSegment: TStringGrid;
    Splitter1: TSplitter;
    procedure acFrameShowExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure bbDeleteClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure mmDestTextChange(Sender: TObject);
    procedure mmDestTextExit(Sender: TObject);
    procedure sgSegmentSelection(Sender: TObject; aCol, aRow: Integer);
    procedure TabControl1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

{ TfraSegmentEdit }
uses                                                                            //@001+
  toDocument, TransDB, TransConst,                                              //@001+
  mtNotification,                                                               //@002+
  dialogs, LCLType,                                                             //@003+
   toProjectList, toLangList;                                                   //@004+

procedure TfraSegmentEdit.TabControl1Change(Sender: TObject);
begin

end;

procedure TfraSegmentEdit.acUpdateExecute(Sender: TObject);                     //@001+
var
 SLSourceHash:TStringList;
 SLSourceText:TStringList;
 SLDestText:TStringList;
 EntryNo:Integer;
 DestHash:THash;
 DestText:UTF8String;
begin
  { TODO 1 -oshiruba -cencapsulation : Convert from using TransDB to OOP  }
 if not Assigned(SharedDocument) then exit;
 with SharedDocument do
   begin
     cbDocument.Text := FileNameOnly;
     cbSrcLang.Text := SrcLang;
     cbDestLang.Text := DestLang;
     cbProject.Text :=  ProjectName;
     try
       SLSourceHash := TStringList.Create;
       SLSourceText := TStringList.Create;
       SLDestText := TStringList.Create;
       FileBlockCRGetList(Name, SLSourceHash);
       // we should check the return code too....
       // convert source hashes to source text
       for EntryNo := 0 to SLSourceHash.Count - 1 do
        SLSourceText.Append(HashTextGet(SharedDocument.SrcLang,
                                        SLSourceHash.Strings[EntryNo],
                                                mdFragment));
       for EntryNo := 0 to SLSourceHash.Count - 1 do
        begin
          DestText := TranslateText(SharedDocument.SrcLang,
                                  SLSourceText.Strings[EntryNo],
                                  SharedDocument.DestLang,
                                  mdFragment) ;
         SLDestText.Append(DestText);
        end;
        // look up dest hash
        //DestHash :=
        // look up the text for the dest hash
        // assign the text to a SLDestText
        ;
       // set the size of the string-grid big enough to hold all our entries
      sgSegment.RowCount := SLSourceHash.Count + 1;

       sgSegment.Cols[1] := SLSourceText;
       sgSegment.Cols[2] := SLDestText;

     finally
       SLSourceHash.Free;
       SLSourceText.Free;
       SLDestText.Free;
     end;
   end;
end;

procedure TfraSegmentEdit.acFrameShowExecute(Sender: TObject);                  //@004+
begin
  // Populate list contents from globals
  cbProject.Items := ProjectList.Strings;
  cbSrcLang.Items := LangList.Strings;
  cbDestLang.Items := LangList.Strings;
  // Populate current text if we have a selected document
  If Assigned(SharedDocument) then
    begin
      cbProject.Text:= SharedDocument.ProjectName;
      cbDocument.Text:= SharedDocument.FileNameOnly;
      cbSrcLang.Text := SharedDocument.SrcLang;
      cbDestLang.Text:= SharedDocument.DestLang;
    end
  else   // if no selected document, blank everything out
    begin
      cbProject.Text:= '';
      cbDocument.Text:= '';
      cbSrcLang.Text := '';
      cbDestLang.Text:= '';
    end;
end;

procedure TfraSegmentEdit.BitBtn1Click(Sender: TObject);
var
  rc:Integer;
begin
    { TODO 1 -oshiruba -cencapsulation : re-implement this as using transobjects }
   // note using these friendly methods, we end up calculating the hashes more
   // than once, a new function should be added to transdb for effiency,
   // something like
   // TranslationAdd(SrcText, SrcLang, DestText, DextLang, mdFragment)
   // this would be the same as TextPairAdd, except that it wouldn't try to add
   // the source block to the database.
   { TODO 1 -oshiruba -cmisc. : When adding a translation where a cross-reference already exists, ask user if we should delete the original translation or not.  (It may become orphaned if we don't delete it, butthen again, it may be used elsewhere). }
   rc := TextAdd(SharedDocument.DestLang, mmDestText.Text, mdFragment);
   if not RC = 0 then
     begin
       DisplayError('Error while adding translated text.');
       exit;
     end;
   with SharedDocument do
     rc := BlockCRAdd(SrcLang, mmSrcText.Text, DestLang, mmDestText.Text, mdFragment);
   if not RC = 0 then
     begin
       DisplayError('Error while adding translated text Cross-reference');
       exit;
     end;
   // We probably don't need to update the entire grid, but for now...
   acUpdate.Execute;
   DisplaySuccess('Translation added/updated.');

end;

procedure TfraSegmentEdit.bbDeleteClick(Sender: TObject);
begin
  DisplayInfo('Delete function currently not implemented.');
end;

procedure TfraSegmentEdit.FrameClick(Sender: TObject);
begin

end;

procedure TfraSegmentEdit.mmDestTextChange(Sender: TObject);
begin
  bitBtn1.Enabled:=True;
end;

procedure TfraSegmentEdit.mmDestTextExit(Sender: TObject);                      //@003+
begin
  if bitBtn1.Enabled then
    case MessageDlg ('There are unsaved changes to the translation.  Would you like to save these changes now?',
                     mtInformation, [mbYes, mbNo], 0) of
                       mrYes: bitbtn1.Click;
                       mrNo: DisplayInfo('Unsaved changes reverted to original');
                     end;
end;

procedure TfraSegmentEdit.sgSegmentSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
//  DisplayInfo('Row = ' + IntToStr(aRow));
  mmSrcText.Text:=sgSegment.Cols[1].Strings[aRow];
  mmDestText.Text:=sgSegment.Cols[2].Strings[aRow];
  BitBtn1.Enabled:= False;
  // enable delete only if there is destination text
  bbDelete.Enabled := sgSegment.Cols[2].Strings[aRow] <> '';                    //@003+
end;

end.

