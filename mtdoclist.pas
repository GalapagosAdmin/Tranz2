unit mtdoclist;
//@000 2011.11.22 Noah Silva : Initial Version
//@001 2011.11.23 Noah Silva : Debugging & Error checking
//@002 2011.11.24 Noah Silva : Addition of ability to call block import
//@003 2011.11.25 Noah Silva : Debugging
//@004 2011.11.26 Noah SILVA : Add Segment Edit button
//@005 2011.12.04 Noah SILVA = Convert to SharedDocument architecture
//@006 2011.12.05 Noah SILVA = Bug fix
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, ActnList, toDocument;

type

  { TfraDocList }

  TfraDocList = class(TFrame)
    acImportDocument: TAction;
    acUpdateDoctListing: TAction;
    alDocList: TActionList;
    bbImport: TBitBtn;
    bbOpen: TBitBtn;
    bbExport: TBitBtn;
    bbSegEdit: TBitBtn;
    gbProjects: TGroupBox;
    gbDocuments: TGroupBox;
    lbProjects: TListBox;
    ListView1: TListView;
    pnlDocActions: TPanel;
    pnlHeader: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure acImportDocumentExecute(Sender: TObject);
    procedure acUpdateDoctListingExecute(Sender: TObject);
  //  procedure bbImportClick(Sender: TObject);
    procedure bbImportContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure bbOpenClick(Sender: TObject);
    procedure bbSegEditClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
  private
    { private declarations }
  public
    SelDoc:TDocument;                                                           //@003+
    procedure update;

    { public declarations }
  end; 

implementation

  {$R *.lfm}
  uses
     toProjectList, toProject,
     mtNotification,                                                            //@001+
     mtDataModule,                                                              //@002+
     dbugintf;                                                                  //@003+

  procedure TfraDocList.acUpdateDoctListingExecute(Sender: TObject);
    var
      tmpProject:TProject;
      tmpItem:TListItem;
      DocumentNo:Integer;
    begin
      SharedDocument := nil;                                                    //@006+
      tmpProject := nil;                                                        //@001+
      if lbProjects.SelCount = 1 then
        tmpProject := ProjectList.GetProjectByName(lbProjects.GetSelectedText);
      if assigned(tmpProject) then
        with ListView1 do
          try
            Items.BeginUpdate;
            Items.Clear;
              with tmpProject do
                if DocumentList.Count > 0 then
                  for DocumentNo := 0 to DocumentList.Count-1 do
                    begin
                      tmpItem := items.Add;
                      tmpItem.Data:=DocumentList.Document[DocumentNo];
                      tmpItem.ImageIndex:=11; // generic blank document icon
                      tmpitem.SubItems.Add('50%');
                      with DocumentList.Document[DocumentNo] do
                        begin
                          tmpItem.Caption := FileNameOnly;
                          tmpitem.SubItems.Add(SrcLang);
                          tmpitem.SubItems.Add(DestLang);
                        end;
                    end;
          finally
            Items.EndUpdate;
//            bbImport.Enabled := False;                                          //@001+@003-
          end; // of TRY..FINALLY
    end;  // of PROCEDURE


  procedure TfraDocList.acImportDocumentExecute(Sender: TObject);               //@001+
  begin
    SendDebug(self.ClassName+'.acImportDocument ListView1.Selcount=' + inttostr(ListView1.SelCount));
    If ListView1.SelCount <> 1 then exit;                                       //@001+@003-
    SelDoc := TDocument(ListView1.Selected.Data);
    DataModule1.acShowBlockImport.Execute;
   // DisplayInfo(tmpDoc.Name); // Full Path
  end;

  procedure TfraDocList.bbImportContextPopup(Sender: TObject; MousePos: TPoint;
    var Handled: Boolean);
  begin

  end;

  procedure TfraDocList.bbOpenClick(Sender: TObject);
  begin
     SendDebug(self.ClassName+'.bbOpenClick ListView1.Selcount=' + inttostr(ListView1.SelCount));
    If ListView1.SelCount <> 1 then exit;                                       //@001+@003-
    SelDoc := TDocument(ListView1.Selected.Data);

  end;

  procedure TfraDocList.bbSegEditClick(Sender: TObject);                        //@004+
  begin
    SendDebug(self.ClassName+'.acImportDocument ListView1.Selcount=' + inttostr(ListView1.SelCount));
    If ListView1.SelCount <> 1 then exit;
    SelDoc := TDocument(ListView1.Selected.Data);
    SharedDocument := SelDoc;
    DataModule1.acShowSegEdit.Execute;

  end;

  procedure TfraDocList.ListView1Click(Sender: TObject);                        //@001+
  begin
    SendDebug(self.ClassName+' ListView1.Selcount=' + inttostr(ListView1.SelCount));
     If ListView1.SelCount <> 1 then exit;                                      //@001+
     bbImport.Enabled := Assigned(ListView1.Selected.Data);
     bbOpen.Enabled := Assigned(ListView1.Selected.Data);                       //@005+
     bbSegEdit.Enabled := Assigned(ListView1.Selected.Data);                    //@005+
//     bbExport.Enabled := Assigned(ListView1.Selected.Data);                   //@005+
     if not Assigned(ListView1.Selected.Data) then
       SendDebug(self.ClassName+' ListView1.Selected.Data not assigned')
     else
       begin
//       SendDebug(self.ClassName+' ListView1.Selected.Data IS assigned')
         with ListView1 do SharedDocument := TObject(Selected.Data) as TDocument; //@005+
       end;
  end;

  procedure TfraDocList.update;
    var
      CurrentProjectNo:Integer;
    begin
      lbProjects.Items := ProjectList.Strings;
    end;



end.

