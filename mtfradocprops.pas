unit mtFraDocProps;
//@000 2011.11.18 Noah Silva : Initial Version
//@001 2011.11.20 Noah Silva + Property Updates
//@002 2011.11.21 Noah SILVA = Refactor some code into this frame
//@003 2011.11.22 Noah Silva = BugFix: Languages update properly now
//@004 2011.11.23 Noah Silva + Debugging / Crash Proofing
//@005 2011.12.04 Noah Silva = Convert to SharedDocument Architecture

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, EditBtn,
  Buttons, ActnList,
  toDocument;                                                                   //@002+

type

  { TfraDocProps }

  TfraDocProps = class(TFrame)
    acDocProp: TActionList;
    acFrameShow: TAction;
    bbCommitChanges: TBitBtn;
    cbDestLang: TComboBox;
    cbProjectName: TComboBox;
    cbSrcLang: TComboBox;
    FileNameEdit1: TFileNameEdit;
    lblDescription: TLabel;
    lblDestLang: TLabel;
    lblFile: TLabel;
    lblProject: TLabel;
    lblSrcLang: TLabel;
    mmDescription: TMemo;
    pnlHeader: TPanel;
    procedure acFrameShowExecute(Sender: TObject);
    procedure bbCommitChangesClick(Sender: TObject);
    procedure cbDestLangChange(Sender: TObject);
    procedure cbSrcLangChange(Sender: TObject);
    procedure mmDescriptionChange(Sender: TObject);
  private
    { private declarations }
    _Document:TDocument;                                                        //@002+
    Procedure SetDocument(Doc:TDocument);                                       //@002+
  public
    { public declarations }
  //  Property Document:TDocument Write SetDocument;                              //@002+@005-
  end; 

implementation

{$R *.lfm}

{ TfraDocProps }
uses
 // mtdatamodule,                                                               //@001+
  mtNotification, toLangList, toProjectList;                                    //@002+

procedure TfraDocProps.bbCommitChangesClick(Sender: TObject);
begin
 //   DataModule1.acDocPropCommit.Execute;                                      //@002-
  Assert(Assigned(_Document),                                                   //@002+
   'TFraDocProps.acDocPropCommit: Internal Error: document not assigned');      //@002+
  _Document.Commit;                                                             //@002+
  { TODO 1 -oshiruba -ci18n : Conver text into resource string }                //@002+
  DisplaySuccess('Document properties updated.');                               //@002+

end;

procedure TfraDocProps.acFrameShowExecute(Sender: TObject);
begin
  SetDocument(SharedDocument);                                                  //@005+
end;

procedure TfraDocProps.cbDestLangChange(Sender: TObject);
begin
    With _Document do                                                           //@003+
         DestLang := cbDestLang.text;                                           //@003+
end;

procedure TfraDocProps.cbSrcLangChange(Sender: TObject);
begin
    With _Document do                                                           //@003+
         SrcLang := cbSrcLang.Text;                                             //@003+
end;

procedure TfraDocProps.mmDescriptionChange(Sender: TObject);
  begin
   // DataModule1.acDocPropUpdate.Execute;                                      //@001+@002-
   Assert(Assigned(_Document),                                                  //@002+
   'TFraDocProps.acDocPropUpdate: Internal Error: Selected.Data not assigned'); //@002+
   With _Document do                                                            //@002+
//     begin                                                                    //@002+@003-
         Description := mmDescription.Text;                                     //@002+
// changing these two here caused strange bugs, as this got called before they
// were initialized
//         SrcLang := cbSrcLang.Text;                                           //@002+@003-
//         DestLang := cbDestLang.text;                                         //@002+@003-
//     end;                                                                     //@002+@003-
  end;

Procedure TFraDocProps.SetDocument(Doc:TDocument);                              //@002+
  Begin
   // Reset the drop-down boxes.
   cbProjectName.Items  := ProjectList.Strings;
   cbSrcLang.Items := LangList.Strings;
   cbDestLang.Items := LangList.Strings;

   _Document := Doc; // Assign local reference
    // update display fields from new document
   If not Assigned(_Document) then                                              //@004+
     Exit;                                                                      //@004+
   with _Document do
      begin
                cbProjectName.Text := ProjectName;
                FileNameEdit1.Text := FileNameOnly;
                mmDescription.Text := Description;
                cbSrcLang.Text     := SrcLang;
                cbDestLang.text    := DestLang;
  { TODO 3 -oshiruba -cBUGFIX : often-times by the time we get to here, the language codes for the document are wrong somehow. (They are correct when read from the DB) }
      end;



  end;


end.   // of UNIT
