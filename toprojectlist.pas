unit toProjectList;
// TransObjects Project List Object
//@000 2011.11.18 Noah Silva : Initial version
//@001 2011.11.21 Noah Silva : Added additional project fields
//                             Exposed Stringlist for GUI Combo-Boxes


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransDB, toProject,  TransConst;

Type
  TProjectList = Class(TObject)   // TComponent?
    private
      //type
     // TProjectList = specialize TList<TProject>;

      _ProjectSL:TStringList;
      { TODO 1 -oshiruba -cstandardization : Convert _Projects to TObjectList }
      // better: use FPGObjectList from FGL
      _Count:Integer; // no. of projects.
      _Projects:Array of TProject;
      //_Projects : TProjectList;
      Function GetProject(Index : Integer):TProject;
      Procedure FreeObjects;
      Function GetStrings:TStrings;                                             //@001+

    public
      Constructor Create;
      Destructor Destroy;
      Procedure Refresh;
      Property Project[Index : Integer]:TProject Read GetProject;
      Function GetProjectByName(Const ProjectName:UTF8String):TProject;
      Property Count:Integer Read _Count;
      Function AddProject(Const ProjectName:UTF8String; Const Desc:UTF8String;
                          Const BasePath:UTF8String;                            //@001+
                          Const SrcLang:TLang; Const DestLang:TLang):Boolean;   //@001+
      Function DeleteProject(Const ProjectName:UTF8String):Boolean;
      Property Strings:TStrings Read GetStrings;                                //@001+
    end;

// This is a global/shared projectlist.  This should be used by any other unit
// that needs to access the list of projects above the DB level.
 var
   ProjectList : TProjectList;


implementation


Constructor TProjectList.Create;
  Begin
    Inherited;
    Refresh;
  end;


Procedure TProjectList.Refresh;
// Reload list from database
  var
    CurrentProjectNo:Integer;
  Begin
    FreeObjects;
    try
      If not Assigned(_ProjectSL) then                                          //@001+
        _ProjectSL := TStringList.Create;
      If _ProjectSl.Count > 0 then                                              //@001+
        _ProjectSL.Clear;                                                       //@001+
      ProjectListGet(_ProjectSL);   // TransDB function
      SetLength(_Projects, _ProjectSL.Count);
      If _ProjectSL.Count > 0 then
     // Loop through the projects
        for CurrentProjectNo := 0 to (_ProjectSL.Count - 1) do
          begin
            _Projects[CurrentProjectNo] :=
              TProject.Create(_ProjectSL.Strings[CurrentProjectNo]);  // Create w/load an existing project
          end; // of FOR
      _Count := _ProjectSL.Count;
    finally
//      _ProjectSL.free;                                                        //@001-
    end;

  end;

Function TProjectList.GetProject (Index : Integer): TProject;
  begin
    Result := _Projects[Index];
  end;

Procedure TProjectList.FreeObjects;
  var
    CurrentProjectNo:Integer;
  begin
    If Length(_Projects) > 0 then
      Begin
        for CurrentProjectNo := 0 to (_Count - 1) do
         If Assigned(_Projects[CurrentProjectNo]) then
           _Projects[CurrentProjectNo].Destroy;
        SetLength(_Projects, 0);
      end;
  end;

Function TProjectList.AddProject(Const ProjectName:UTF8String;
                          Const Desc:UTF8String;
                          Const BasePath:UTF8String;                            //@001+
                          Const SrcLang:TLang; Const DestLang:TLang):Boolean;   //@001+

  var
    NewCount:Integer;
  begin
    // This creates the project in the database
    Result := (ProjectListAdd(ProjectName, Desc, BasePath, SrcLang, DestLang)   //@001=
                = rcNoProblem);
    // If that worked, then update our object list in memory too
    If result then
      begin
        NewCount := Length(_Projects)+1;
        SetLength(_Projects, NewCount);
        _Projects[High(_Projects)] :=
          TProject.Create(ProjectName);  // Create w/load an existing project
      end;
  end;

Function TProjectList.GetProjectByName(Const ProjectName:UTF8String):TProject;
  var
    CurrentProjectNo:Integer;
    Found:Boolean=False;
  begin
    for CurrentProjectNo := Low(_Projects) to High(_Projects) do
      If _Projects[CurrentProjectno].Name = ProjectName then
        begin
          Found := true;
          Break;
        end;
    Case Found of
      True:Result := _Projects[CurrentProjectNo];
      False: begin
               Result := nil;
               Raise ENoSuchProject.Create                                              //@012+
                                ('GetProjectByName: ' + rsNoSuchProject);//@012+@013+

             end;
    end;

  end;


Function TProjectList.DeleteProject(Const ProjectName:UTF8String):Boolean;
  var
    CurrentProjectNo:Integer;
    Found:Boolean=False;
  begin
    { TODO 1 -oshiruba -ccode duplication : Convert to use GetProjectByName }
    // Search for a project with matching name
    for CurrentProjectNo := Low(_Projects) to High(_Projects) do
      If _Projects[CurrentProjectno].Name = ProjectName then
        begin
          Found := true;
          Break;
        end;
    // If we found a match, go ahead and delete the project
    Case Found of
      True:begin

        Case ProjectListDelete(_Projects[CurrentProjectNo].Name) of
          rcNoProblem :
            Begin // Database Deletion was ok
{ TODO 1 -oshiruba -cefficiency : Replace Refresh call with code to remove the old entry from the array }
              _Projects[CurrentProjectNo].Destroy;  // We can delete it from memory
              _Projects[CurrentProjectNo] := nil;
              Refresh;
              Result := True;
            end; // Database Deletion was ok
          else
           Result := False;
        end; // of CASE RC
      end; // Case TRUE
      False:Result := False
    end; // of CASE Found
  end; // of Method

Function TProjectList.GetStrings:TStrings;                                      //@001+
  begin
    Result := _ProjectSL;
  end;

Destructor TProjectList.Destroy;
  Begin
    FreeObjects;  // Free the project entries
    _ProjectSL.free;                                                            //@001+
    inherited;
  end;

initialization
  ProjectList := TProjectList.Create;

Finalization
  ProjectList.Destroy;

end.

