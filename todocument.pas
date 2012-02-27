unit toDocument;
// TransObjects Document Object
//@000 2011.11.17 Noah Silva : Initial version
//@001 2011.11.28 Noah Silva : Improvements
//@002 2011.11.20 Noah Silva : Ability to make changes, debugging, bug-fix
//@003 2011.11.24 Noah Silva : Add a link to the parent DocumentList
//@004 2011.11.26 Noah Silva : Add SharedDocument
//@005 2011.12.05 Noah SILVA : Fix Unicode issue.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransConst;
 // toDocumentList;                                                             //@003+-

type
    TDocument = class(TOBject)   // TComponent?
    private
      //      _DocumentList:TDocumentList;                                              //@003+
      _DocumentList:TObject;                                              //@003+
      _DocumentInfo:TDocumentInfo;
      _Dirty:Boolean; // Pending Updates?
      { private declarations }
//      Function DocumentHashGet : THash;
      Function GetFileNameOnly:UTF8String;
      Function GetFilePathOnly:UTF8String;
      Function GetProjectName:UTF8String;
      Procedure Refresh;                                                        //@001+
      Procedure FileDescSet(Const Description:UTF8String);                      //@002+
      Procedure SrcLangSet(Const Lang:Tlang);                                   //@002+
      Procedure DestLangSet(Const Lang:Tlang);                                  //@002+
    public
      // Set for ProjectHash would require de-associating from old project first
      Property ProjectHash : THash Read _DocumentInfo.ProjectHash;
      // Set for ProjectName would require de-associating from old project firsy
      Property ProjectName : UTF8String Read GetProjectName;
      // File name of document with full path
      Property Name : UTF8String Read _DocumentInfo.FileName;
      Property FileNameOnly : UTF8String Read GetFileNameOnly;
      Property FilePathOnly : UTF8String Read GetFilePathOnly;
      Property Description : UTF8String Read _DocumentInfo.FileDesc
                                        Write FileDescSet;                      //@002+
//      Property BasePath : UTF8String Read _DocumentInfo.BasePath;
      Property SrcLang : TLang Read _DocumentInfo.SrcLang
                               Write SrcLangSet;                                //@002+
      Property DestLang : TLang Read _DocumentInfo.DestLang
                                Write DestLangSet;                              //@002+
//      Property DocumentList Read _DocumentList Write _DocumentList;             //@003+
      Property DocumentList:TObject Read _DocumentList Write _DocumentList;             //@003+
   // Published
      Constructor Create(Const DocumentName:UTF8String);  // Create w/load an existing Document
//      Constructor CreateNew(DocumentName:UTF8String);  // Create a new Document
      Destructor Destroy;                                                       //@001+
      Procedure Commit; // Writes out all changes since the last commit
      Procedure Rollback; // Aborts all changes and reloads data
      { public declarations }
    end;

var
  SharedDocument : TDocument;

implementation

uses
   TransDB,
   dbugintf,                                                                    //@002+
   FileUtil;                                                                    //@005+


Procedure TDocument.Refresh;                                                    //@001+
  begin
    _DocumentInfo := FileInfoRead(Name);
    _Dirty := False;
  end;

Constructor TDocument.Create(Const DocumentName: UTF8String);
 Begin
  Inherited Create;
//   CheckDocument(DocumentName); // Will throw exception if it doesn't exist
   // Next we call the function that loads the details...
   // (as soon as we implement that in TransDB)
   _DocumentInfo.FileName:=DocumentName;                                        //@002+
   _DocumentList := Nil;                                                        //@003+
   refresh;                                                                     //@001+
 end;

Procedure TDocument.Commit;
  begin
    If not _Dirty then exit; // If nothing to update, then skip
    { TODO 1 -oshiruba -cfunctionality : Implement commit function to write out in-memory changes to db. }
    SendDebug('TDocument.Commit: About to write to DB');
    Case ProjectFileAdd(ProjectName, Name,
                               Description, SrcLang, DestLang) of
      rcNoProblem: _Dirty := False;
      else
        Raise EDatabaseError.Create(self.ClassName + '.Commit:' + rsDocumentUpdateErr);//@003=
    end;
  end;

Procedure TDocument.Rollback;
 begin
  // Reload data
  _Dirty := False;
  refresh;                                                                      //@001+
 end;

Function TDocument.GetFileNameOnly:UTF8String;
  begin
    try                                                                         //@005+
//      Result := SystoUTF8(ExtractFileName(UTF8toSys(_DocumentInfo.FileName)));  //@005=
      Result := ExtractFileName(_DocumentInfo.FileName);

    Except                                                                      //@005+
      Result := '';                                                             //@005+
      SendDebug(self.ClassName + '.GetFileNameOnly : Error ');                  //@005+
    end;                                                                        //@005+
  end;

Function TDocument.GetFilePathOnly : UTF8String;
  Begin
//    Result := SystoUTF8(ExtractFileDir(UTF8toSys(_DocumentInfo.FileName)));     //@005=
    Result := ExtractFileDir(_DocumentInfo.FileName);
  End;

Function TDocument.GetProjectName:UTF8String;
  var
    ProjectInfo:TProjectInfo;
  begin
    ProjectInfo := ProjectInfoReadHash(_DocumentInfo.ProjectHash);
    Result := ProjectInfo.ProjectName;
  end;

Destructor TDocument.Destroy;                                                   //@001+
  begin
    Inherited Destroy;
  end;

Procedure TDocument.FileDescSet(Const Description:UTF8String);
  Begin
    _DocumentInfo.FileDesc := Description;
    _Dirty := True;  // we don't write it out just yet, but we mark it as dirty
  end;

Procedure TDocument.SrcLangSet(Const Lang:Tlang);                               //@002+
  Begin
    _DocumentInfo.SrcLang := Lang;
    _Dirty := True;  // we don't write it out just yet, but we mark it as dirty
  end;

Procedure TDocument.DestLangSet(Const Lang:Tlang);                              //@002+
  Begin
    _DocumentInfo.DestLang := Lang;
    _Dirty := True;  // we don't write it out just yet, but we mark it as dirty
  end;



end. // of UNIT

