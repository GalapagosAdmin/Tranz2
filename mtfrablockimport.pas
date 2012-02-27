unit mtfrablockimport;
//@000 2011.11.25 Noah Silva : Initial Version
//@001 2011.12.04 Noah Silva : Convert to use shared Document
//@002 2011.12.05 Noah SILVA : Additional error checking/avoidance
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, CheckLst, ExtCtrls, Buttons,
  StdCtrls, ActnList, toDocument;

type

  { TFraBlockImport }

  TFraBlockImport = class(TFrame)
    acFrameShow: TAction;
    alBlockImport: TActionList;
    bbExtract: TBitBtn;
    bbImportBlocks: TBitBtn;
    clbBlocks: TCheckListBox;
    cbProject: TComboBox;
    cbDocument: TComboBox;
    cbSrcLang: TComboBox;
    lblSrcLang: TLabel;
    lblProject: TLabel;
    lblDoc: TLabel;
    pnlHeader: TPanel;
    procedure acFrameShowExecute(Sender: TObject);
    procedure bbExtractClick(Sender: TObject);
    procedure bbImportBlocksClick(Sender: TObject);
  private
    { private declarations }
    _Document:TDocument;  // we don't own this, it just points to SharedDocument
  public
    Procedure SetDocument; //(Const Doc:TDocument);                             //@001=
    { public declarations }
  end; 

implementation

uses
   dbugintf, BlockExtract, mtNotification,
   transdb, TransConst;
   { TODO 1 -oshiruba -cencapsulation : Remove TransDB dependency }
{$R *.lfm}

procedure TFraBlockImport.bbExtractClick(Sender: TObject);
  var
    BlockExtractor:TBlockExtractor;
    BlockNo:Integer;
  begin
    try
      if not assigned(_Document) then exit;
      SendDebug(self.classname + ' extract ' + _Document.Name);
      try
        BlockExtractor := TBlockExtractor.Create(_Document.Name);
        clbBlocks.Items := BlockExtractor.Blocks;
        for BlockNo := 0 to clbBlocks.Items.Count -1 do
            clbBlocks.Checked[Blockno] := True;
        DisplaySuccess(IntToStr(BlockExtractor.Blocks.Count)+' blocks extracted');
      finally
        BlockExtractor.Free;
      end;
      bbImportBlocks.Enabled:=True;
     except on E: exception do
      DisplayError(e.message);
    end;
  end;

procedure TFraBlockImport.acFrameShowExecute(Sender: TObject);                  //@004+
begin
  SetDocument;
end;

procedure TFraBlockImport.bbImportBlocksClick(Sender: TObject);
  var
    BlockNo:Integer;
    RC:Integer;
  begin
  // To import the blocks into the DB as segments,
  // we need to loop and call the following:
    if clbBlocks.Items.Count = 0 then exit;
    for BlockNo := 0 to clbBlocks.Items.Count -1 do
      If clbBlocks.Checked[BlockNo] then
        with clbBlocks do
          begin
            RC := TextAdd(_Document.SrcLang, Items.Strings[BlockNo],
                                                        mdFragment);
            IF RC = 0 then
              RC := FileBlockCRAdd(_Document.Name,
                                 Items.Strings[BlockNo]);
            If RC <> 0 then
              begin
                DisplayError(GetLastErrorMessage);
                exit;
              end;
          end;
    DisplaySuccess('Blocks successfully imported to database');
  // The above will put the segments in, but not associate them to the project
  // (that functionality is yet to be implemented in TransDB)
  end;

Procedure TFraBlockImport.SetDocument; //(Const Doc:TDocument);                 //@001=
  begin
    If not Assigned(SharedDocument) then exit;                                  //@002+
    _Document := Nil;                                                           //@002+
    _Document := SharedDocument;
    clbBlocks.Clear;
    bbImportBlocks.Enabled:=False;

    if not Assigned(_Document) then exit;
    with _Document do
      begin
       cbProject.Items.clear;
       cbProject.AddItem(ProjectName, nil);
       cbProject.Text  := ProjectName;
       cbDocument.Items.Clear;
       cbDocument.AddItem(FileNameOnly, _Document);
       cbDocument.Text := FileNameOnly;
       cbSrcLang.Items.Clear;
       cbSrcLang.AddItem(SrcLang, nil);
       cbSrcLang.text  := SrcLang;

      end;
  end;

end.

