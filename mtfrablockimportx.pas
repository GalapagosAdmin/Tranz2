unit mtFraBlockImport;
//@000 2011.11.24 Noah Silva : Initial version

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Buttons, CheckLst, StdCtrls, toDocument;

type

  { TfraBlockImport }
          {$M+}
  TfraBlockImport = class(TFrame)
    bbExtractBlocks: TBitBtn;
    Import: TBitBtn;
    clbBlocks: TCheckListBox;
    cbProject: TComboBox;
    cbDocument: TComboBox;
    cbLang: TComboBox;
    Document:TDocument;
   // Procedure SetDocument(Const Doc:TDocument);
  //  procedure bbExtractBlocksClick(Sender: TObject);

    { public declarations }
  end; 

implementation

{$R *.lfm}

{ TfraBlockImport }

Procedure TFraBlockImport.SetDocument(Const Doc:TDocument);
  begin
    Document := Doc;
    cbDocument.Text:= Document.FileNameOnly;
    clbBlocks.Clear;
  end;

procedure TfraBlockImport.bbExtractBlocksClick(Sender: TObject);
begin
    try
    BlockExtractor := TBlockExtractor.Create(Document.Name);
//    BlockExtractor := TBlockExtractor.Create;
//    BlockExtractor.SetFile(OpenDialog1.FileName);
    clbBlocks.Items := BlockExtractor.Blocks;
  finally
    BlockExtractor.Free;
  end;

end;

end.

