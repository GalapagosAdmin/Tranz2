unit toProject;
// TransObjects Project Object
//@000 2011.11.18 Noah Silva : Initial version
//@001 2011.11.21 Noah Silva : Typo Fix

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransConst, toDocumentList;

type
    TProject = class(TOBject)   // TComponent?
    private
      _ProjectInfo:TProjectInfo;
      _Dirty:Boolean; // Pending Updates?
      { private declarations }
      Function ProjectHashGet : THash;
    public
      DocumentList:TDocumentList;
      Property Hash : THash Read ProjectHashGet;
      Property Name : UTF8String Read _ProjectInfo.ProjectName;
      Property Description : UTF8String Read _ProjectInfo.ProjectDesc;
      Property BasePath : UTF8String Read _ProjectInfo.BasePath;
      Property SrcLang : TLang Read _ProjectInfo.SrcLang;
      Property DestLang : TLang Read _ProjectInfo.DestLang;                     //@001=
   // Published
      Constructor Create(Const ProjectName:UTF8String);  // Create w/load an existing project
//      Constructor CreateNew(ProjectName:UTF8String);  // Create a new project
      Destructor Destroy;
      Procedure Commit; // Writes out all changes since the last commit
      Procedure Rollback; // Aborts all changes and reloads data
    end;


implementation

uses
   TransDB;

Constructor TProject.Create(Const ProjectName: UTF8String);
 Begin
  Inherited Create;
   CheckProject(ProjectName); // Will throw exception if it doesn't exist
   // Next we call the function that loads the details...
   // (as soon as we implement that in TransDB)
   _ProjectInfo := ProjectInfoRead(ProjectName);
   DocumentList := TDocumentList.Create(ProjectName);
   _Dirty := False;
 end;

Function  TProject.ProjectHashGet : THash;
  Begin
    Result := _ProjectInfo.ProjectHash //:= String2Hash(_ProjectName);
  end;

Procedure TProject.Commit;
  begin
    If not _Dirty then exit; // If nothing to update, then skip
    _Dirty := False;
  end;

Procedure TProject.Rollback;
 begin
  // Reload data
  _Dirty := False;
 end;


Destructor TProject.Destroy;
  begin
    DocumentList.Destroy; // Free the Document List
    Inherited Destroy;
  end;

end. // of UNIT

