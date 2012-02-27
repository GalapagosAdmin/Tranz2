unit toDocumentList;
// TransObjects Project List Object
//@000 2011.11.18 Noah Silva : Initial version
//@001 2011.11.21 Noah Silva + Add Strings property for combo-box use
//@002 2011.11.24 Noah Silva + RemoveDocument functionality
//                           + Add link to self to documents managed by this list

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransDB, toDocument, TransConst;

Type
  TDocumentList = Class(TObject)   // TComponent?
    private
      _ProjectName:UTF8String;
      _DocumentSL:TStringList;
      { TODO 1 -oshiruba -cstandardization : Convert _Documents to TObjectList }
      // better: use FPGObjectList from FGL
      _Count:Integer; // no. of projects.
      _Documents:Array of TDocument;
      _Strings:TStringList;                                                     //@001+
      Function GetDocument (Index : Integer):TDocument;
      Procedure FreeObjects;
      Function GetStrings:TStrings;                                             //@001+
    public
      Constructor Create(Const ProjectName:UTF8String);
      Destructor Destroy;
      Procedure Refresh;
      Property Document[Index : Integer]:TDocument Read GetDocument;
      Property Count:Integer Read _Count;
      Function AddDocument(Const ProjectName:String;
                           Const DocumentName:UTF8String; Const Desc:UTF8String;
                           Const SrcLang:TLang; Const DestLang:TLang):Boolean;
      // Removes a document from the database (and from the document list
      // of the current project
      Function RemoveDocument(Const FullPath:UTF8String):Boolean;               //@002+
      Property Strings:TStrings Read GetStrings;                                //@001+
      Function GetDocumentByName(Const FullPath:UTF8String):TDocument;          //@002+
  end;

implementation


Constructor TDocumentList.Create(Const ProjectName:UTF8String);
  Begin
    Inherited create;
    CheckProject(ProjectName);
    _ProjectName := ProjectName;
    Refresh;
  end;

Procedure TDocumentList.Refresh;
// Reload list from database
  var
    CurrentDocumentNo:Integer;
  Begin
    FreeObjects;
    try
      If not Assigned(_Strings) then // Stringlist lazy create                  //@002+
        _Strings := TStringList.Create;                                         //@002=
      If _Strings.Count > 0 then _Strings.Clear;
      ProjectFileListGet(_ProjectName, _Strings);                               //@001+@002=
      SetLength(_Documents, _Strings.Count);                                    //@002=
      If _Strings.Count > 0 then                                                //@002+
     // Loop through the projects
        for CurrentDocumentNo := 0 to (_Strings.Count - 1) do                   //@002=
          begin
            _Documents[CurrentDocumentNo] :=
              TDocument.Create(_Strings.Strings[CurrentDocumentNo]);            //@002=
            _Documents[CurrentDocumentNo].DocumentList := Self;                 //@003+
          end; // of FOR
      _Count := _Strings.Count;                                                 //@002=
    finally
//      _DocumentSL.free;                                                       //@002-
    end;

  end;

Function TDocumentList.GetDocument (Index : Integer): TDocument;
  begin
    Result := _Documents[Index];
  end;

Function TDocumentList.AddDocument(Const ProjectName:String;
                                   Const DocumentName:UTF8String;
                                   Const Desc:UTF8String;
                                   Const SrcLang:TLang; Const DestLang:TLang):Boolean;
  var
    NewCount:Integer;
  begin
    // This creates the document in the database, and associates it to the project
    Result := (ProjectFileAdd(ProjectName, DocumentName,
                               Desc, SrcLang, DestLang) = rcNoProblem);
    // If that worked, then update our object list in memory too
    If result then
      begin
        NewCount := Length(_Documents)+1;
        SetLength(_Documents, NewCount);
        _Documents[High(_Documents)] :=
          TDocument.Create(DocumentName);  // Create w/load an existing Document
        _Documents[High(_Documents)].DocumentList := Self;                      //@002+
      end;
  end;


Procedure TDocumentList.FreeObjects;
  var
    CurrentDocumentNo:Integer;
  begin
    If Length(_Documents) > 0 then
      Begin
        for CurrentDocumentNo := 0 to (_Count - 1) do
         _Documents[CurrentDocumentNo].Destroy;
        SetLength(_Documents, 0);
      end;
  end;

Destructor TDocumentList.Destroy;
  Begin
    FreeObjects; // Free the Document Entries
    If Assigned(_Strings) then _Strings.Free;                                  //@002+
    inherited;
  end;


Function TDocumentList.GetStrings:TStrings;                                     //@001+
  var
    CurrentDocumentNo:Integer;
  Begin
    Result := _Strings;
  end;

Function TDocumentList.GetDocumentByName(Const FullPath:UTF8String):TDocument;//@002
  var
    CurrentDocNo:Integer;
    Found:Boolean=False;
  begin
    for CurrentDocNo := Low(_Documents) to High(_Documents) do
      If _Documents[CurrentDocNo].Name = FullPath then
        begin
          Found := true;
          Break;
        end;
    Case Found of
      True:Result := _Documents[CurrentDocNo];
      False: begin
               Result := nil;
               Raise ENoSuchDocument.Create
                                ('GetDocumentByName: ' + rsNoSuchDocument);

             end;
    end;

  end;


Function TDocumentList.RemoveDocument(Const FullPath:UTF8String):Boolean;       //@002+
  Var
    DocToRemove:TDocument;
  begin
    Result := False;
    DocToRemove := nil;
    DocToRemove := GetDocumentByName(FullPath);
    if not assigned(DocToRemove) then exit; // there should have been an excaption
    // delete from DB
    With DocToRemove do
     Case ProjectFileDelete(_ProjectName,
                           Name,
                           SrcLang,
                           DestLang) of
         rcNoProblem:begin
                       Result := True;
                       { TODO 1 -oshiruba -cefficiency : Delete doc from list memory instead of doing an overkill complete refresh }
                       Refresh; // refresh TDocumentList from database
                     end
         else
           { TODO 1 -oshiruba -cerror handling : convert this to use raise an exception on error }
           Result := False;
     end; // of CASE
  end;

end.

