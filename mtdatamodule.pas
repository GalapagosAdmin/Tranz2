unit mtdatamodule;
//@000 2011.09.xx Noah Silva : Initial Version
//@001 2011.11.16 Noah Silva : Adding project list update/add/delete feature
//                             Added DefaultTranslator
//@002 2011.11.18 Noah Silva + Added code for project tree sidebar.
//                             Modified to use OOP DB Wrapper
//@003 2011.11.20 Noah Silva + Allow Document Property Changes
//@004 2011.11.21 Noah Silva - Refactored some code to FraDocProps
//@005 2011.11.22 Noah Silva : Notification changes (Added DisplayInfo)
//@006 2011.11.23 Noah Silva : Debugging project -> remove document
//@007 2011.11.24 Noah Silva : Debugging, add error checking
//                             Added action to perform block import
//@008 2011.11.26 Noah Silva : Added acShowSegEdit
//@009 2011.12.04 Noah Silva = Converted to SharedDocument architecture
//@010 2011.12.05 Noah SILVA = Bug Fix for SharedDocument (Clear when updating)
{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, FileUtil, ActnList, Menus, Forms,
  //LazHelpHTML,                                                                //@005-
  Controls,
  Dialogs, ExtCtrls, UTF8Process, Ipfilebroker;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    acNewProject: TAction;
    acDeleteProject: TAction;
    acProjectTreeRefresh: TAction;
    acInitIntro: TAction;
    acProjectListUpdate: TAction;
    acAddFileToProject: TAction;
    acDeleteFileFromProject: TAction;
    acProjectTreeClick: TAction;
    acDocPropUpdate: TAction;
    acDocPropCommit: TAction;
    acShowBlockImport: TAction;
    acShowSegEdit: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    ilProjectTree: TImageList;
    ilMessageTypes: TImageList;
    IpFileDataProvider1: TIpFileDataProvider;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    ProcessUTF8_1: TProcessUTF8;
    tmrMessage: TTimer;
    procedure acAddFileToProjectExecute(Sender: TObject);
    procedure acDeleteFileFromProjectExecute(Sender: TObject);
    procedure acDeleteProjectExecute(Sender: TObject);
    procedure acDocPropCommitExecute(Sender: TObject);
//    procedure acDocPropUpdate(Sender: TObject);
    procedure acDocPropUpdateExecute(Sender: TObject);
    procedure acInitIntroExecute(Sender: TObject);
    procedure acNewProjectExecute(Sender: TObject);
    procedure acProjectListUpdateExecute(Sender: TObject);
    procedure acProjectTreeClickExecute(Sender: TObject);
    procedure acShowBlockImportExecute(Sender: TObject);
    procedure acShowSegEditExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    Procedure DisplayError(Const Message:UTF8String);                           //@001+
    Procedure DisplaySuccess(Const Message:UTF8String);                         //@001+
    Procedure DisplayInfo(Const Message:UTF8String);                            //@005+
    procedure tmrMessageTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataModule1: TDataModule1; 

ResourceString
  rsCreateProjectWindowTitle = 'Create New Project';
  rsCreateProjectPromptText  = 'Enter the name of the new project to create:';
  rsDeleteProjectWindowTitle = 'Delete Project';
  rsDeleteProjectPromptText  = 'Are you certain you wish to delete the selected project?';
  rsDeleteFileWindowTitle    = 'Remove Document From Project';
  rsDeleteFilePromptText     = 'Are you certain you wish to remove the selected document from the project?';
  rsHTMLCantOpen             = 'Unable to open HTML file';
  rsMustSelectFile           = 'You must first select a file.';
  rsMustSelectProject        = 'You must first select a project.';
  rsProjectAdded             = 'New project created successfully.';
  rsProjectDeleted           = 'Project deleted successfully.';
  rsFileAdded                = 'Document added to project successfully.';
  rsFileDeleted              = 'Document removed from project.';

implementation

uses
   mtfrmMain, iputils, iphtml,
   transdb, // Want to remove references to Transdb, but we have to implement projectdelete, etc. first
   ComCtrls, DefaultTranslator, Graphics, TransConst,                           //@001+
   mtFrmProjectFileAdd, mtFrmNewProject,                                        //@001+
   toProject, toDocument, toProjectList,                                        //@002+
   toDocumentList;                                                              //@007-
{$R *.lfm}

type
  TSimpleIpHtml = class(TIpHtml)
    public
//  property OnGetImageX;
  end;

Const
  lvlTree     = 0;                                                              //@002+
  lvlProject  = 1;
  lvlDocument = 2;

var
  MainNode:TTreeNode;                                                           //@001+


{ TDataModule1 }

{ TODO 1 -oshiruba -crefactoring : Move notification routines into mtNotification entirely }
Procedure TDataModule1.DisplayError(Const Message:UTF8String);                  //@001+
  begin
    with frmMain.StatusBar1 do
      begin
        Tag := tgError;
        Panels.Items[sbpMain].Text := Message;
        InvalidatePanel(sbpMain, [ppText]); // only needed for same text
        InvalidatePanel(sbpIcon, [ppBorder])
      end;
    tmrMessage.Enabled:=True;
  //  MessageDlg (Message, mtError, [mbCancel], 0);
  end;

Procedure TDataModule1.DisplaySuccess(Const Message:UTF8String);                //@001+
  begin
    with frmMain.StatusBar1 do
      begin
        Tag := tgSuccess;
        Panels.Items[sbpMain].Text := Message;
        InvalidatePanel(sbpMain, [ppText]); // only needed for same text
        InvalidatePanel(sbpIcon, [ppBorder])
      end;
    tmrMessage.Enabled:=True;
  //  MessageDlg (Message, mtInfo, [mbCancel], 0);
  end;

Procedure TDataModule1.DisplayInfo(Const Message:UTF8String);                   //@005+
  begin
    with frmMain.StatusBar1 do
      begin
        Tag := tgInfo;
        Panels.Items[sbpMain].Text := Message;
        InvalidatePanel(sbpMain, [ppText]); // only needed for same text
        InvalidatePanel(sbpIcon, [ppBorder])
      end;
    tmrMessage.Enabled:=True;
  end;

procedure TDataModule1.tmrMessageTimer(Sender: TObject);                        //@001+
begin
  tmrMessage.Enabled:= False;
  with frmMain.StatusBar1 do
    begin
      If Tag < tgStale then Tag := Tag + tgStale;   // Mark it as stale (background goes back to normal)
      InvalidatePanel(sbpMain, [ppText]);
//      Panels.Items[1].Text := Panels.Items[1].Text + ' '; // This just makes it refresh
    end;
end;

procedure TDataModule1.acNewProjectExecute(Sender: TObject);
Const
  DefaultText = '';
var
  NewNode:TTreeNode;
  NewProjectName:UTF8String;
begin
  frmNewProject.Show;
  (*
  NewProjectName :=  InputBox (rsCreateProjectWindowTitle,
                         rsCreateProjectPromptText, DefaultText);
    //ShowMessage('NewProjectClick');
//  with mtFrmMain.frmMain.fraProject1.tvProject do
//    NewNode := Items.AddChild(MainNode, 'New Project');
//  NewNode. := true;
  Case (NewProjectName = DefaultText) of
    True:;
    False: Begin
             If ProjectListAdd(NewProjectName) = 0 then
               DisplaySuccess(rsProjectAdded);                                  //@001+
             acProjectListUpdate.Execute;

           end;
  end; // of CASE  *)
end;

// This is for the project treeview tab
procedure TDataModule1.acProjectListUpdateExecute(Sender: TObject);             //@001+
const
  iiProjectTree = 9;
  iiFolder = 5;
  iiGenericFile = 11;
var
//  ProjectList : TStringList;                                                  //@002-
//  FileList : TStringList;                                                     //@002-
  CurrentProjectNo:Word;
  CurrentFileNo:Word;
  ThisProjectNode:TTreeNode;
  ThisFileNode:TTreeNode;
begin
  Try
    with mtFrmMain.frmMain.fraProject1.tvProject do
      begin
        BeginUpdate;
        Items.Clear;
        MainNode := Items.Add(nil, 'Projects');
        MainNode.ImageIndex := iiProjectTree;
      end;
//    ProjectList := TStringList.Create;                                        //@002-
//    FileList := TStringList.Create;                                           //@002-
//    ProjectListGet(ProjectList);                                              //@002-
    SharedDocument := nil;                                                      //@010+
    ProjectList.Refresh;                                                        //@002+
    If ProjectList.Count > 0 then
     // Loop through the projects
     for CurrentProjectNo := 0 to (ProjectList.Count - 1) do
     // Add each project as a child to the "Projects" main node.
      with mtFrmMain.frmMain.fraProject1.tvProject do
        begin
          // Add the project

          ThisProjectNode := Items.AddChildObject(MainNode,                     //@002=
                        ProjectList.Project[CurrentProjectNo].Name,
                        ProjectList.Project[CurrentProjectNo]);                 //@002+
          // Set the Project's Icon
          ThisProjectNode.ImageIndex := iiFolder;

          // Scan for files in the project
//          ProjectFileListGet(ProjectList.Project[CurrentProjectNo].Name,      //@002-
//                             FileList);                                       //@002-
//          If FileList.Count > 0 then
          with ProjectList.Project[CurrentProjectNo] do
            if DocumentList.Count > 0 then
             // Add each file for the current project
//              For CurrentFileNo := 0 to (FileList.Count - 1) do               //@002-
              For CurrentFileNo := 0 to (DocumentList.Count - 1) do             //@002+
                Begin
//                  ThisFileNode := Items.AddChild(ThisProjectNode,             //@002-
//                                           FileList.Strings[CurrentFileNo]);  //@002-
                  With DocumentList do                                          //@002+
                    ThisFileNode := Items.AddChildObject(ThisProjectNode,       //@002+
                                          Document[CurrentFileNo].FileNameOnly, //@002+
                                          Document[CurrentFileNo]);             //@002+
                  ThisFileNode.ImageIndex := iiGenericFile;
                end;
        end;  // of WITH  ... tvProject
  finally
//    FileList.free;                                                            //@002-
//    ProjectList.free;                                                         //@002-
    with mtFrmMain.frmMain.fraProject1.tvProject do EndUpdate;
  end;
end;

procedure TDataModule1.acProjectTreeClickExecute(Sender: TObject);
// var                                                                          //@004-
//   tmpProject:TProject;                                                       //@004-
//   tmpDocument:TDocument;                                                     //@004-
begin
   try
    with mtFrmMain.frmMain.fraProject1.tvProject do
     begin
       if (not assigned(selected)) then exit;
       Case Selected.Level of
         lvlProject:begin
//                      try
//                         tmpProject := TProject.Create(Selected.Text);        //@002-
                       frmMain.fraProjProps1.Project := TProject(Selected.Data);//@004+
//                        tmpProject := TProject(Selected.Data);                //@002+@004-
//                        with frmMain.fraProjProps1 do                         //@004-
//                           Begin                                              //@004-
//                                                                              //@004-
//                             ebProjectName.Text:=tmpProject.Name;             //@004-
//                             mmProjectDescription.Text:=tmpProject.Description;//@004-
//                           end;                                               //@004-
                        with FrmMain do
                          nbProjSidePanel.ActivePage := frmMain.tsProjProp;
//                      finally
//                        tmpProject.Destroy;                                   //@002-
//                      end;
                    end; // of lvlProject
         lvlDocument:Begin
//           try
//           tmpDocument := TDocument.Create(Selected.Text);                    //@002-
           // the next line shouldn't even be necessary...
           SharedDocument := TObject(Selected.Data) as TDocument;               //@009+
           // Update the document data on display
           with frmMain.fraDocProps1 do acFrameShow.Execute;                    //@009+
           //frmMain.fraDocProps1.Document := TDocument(Selected.Data);         //@004+@009-
(*                                                                              //@004-
             tmpDocument := TDocument(Selected.Data);                           //@002+
             with frmMain.fraDocProps1.Document do
                Begin

                  cbProjectName.Text := tmpDocument.ProjectName;                //@002+
                  FileNameEdit1.Text := tmpDocument.FileNameOnly;
                  mmDescription.Text := tmpDocument.Description;
                  cbSrcLang.Text     := tmpDocument.SrcLang;                    //@002+
                  cbDestLang.text    := tmpDocument.DestLang;                   //@002+
                end;
*)
             with FrmMain do
               nbProjSidePanel.ActivePage := frmMain.tsFileProp;
//           finally
//             tmpDocument.Destroy;                                             //@002-
//           end;  // of TRY..FINALLY
                     end; // of lvlDocument
       end; // of CASE
     end; // of WITH
  except on E: Exception do
    DisplayError(e.Message);
  end; // of TRY..EXCEPT
end; // of PROCEDURE

procedure TDataModule1.acShowBlockImportExecute(Sender: TObject);               //@007+
begin
  with frmMain do
   begin
     SharedDocument := fraDocList1.SelDoc;                                      //@009+
     FraBlockImport1.SetDocument;//( fraDocList1.SelDoc );                      //@009=
    sbBlockImport.Click;                                                        //@008+
  //    ExtendedNotebook1.ActivePage := tsBlockImport;                          //@008-
    sbBLockImport.Down:=True;
  end;
 end;

procedure TDataModule1.acShowSegEditExecute(Sender: TObject);                   //@008+
begin
  with frmMain do
    begin
      sbSegEdit.Click();
      sbSegEdit.Down := True;
    end;
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin

end;

procedure TDataModule1.acDeleteProjectExecute(Sender: TObject);
begin
  with mtFrmMain.frmMain.fraProject1.tvProject do
    begin
      // Short-Circuit Logic compilation should be enabled or the following IF
      // could crash with an invalid reference
      if (not assigned(selected))
        or (Selected = MainNode)
        or (Selected.Level <> lvlProject) then
         begin
           DisplayError(rsMustSelectProject);
           exit;
         end;
      if MessageDlg (rsDeleteProjectWindowTitle, rsDeleteProjectPromptText,
                     mtConfirmation,
                     [mbYes, mbNo],0) = mrYes then
        Begin // User chose mrYes
          //          If ProjectListDelete(Selected.Text) = 0 then              //@002-
          Case ProjectList.DeleteProject(Selected.Text) of                      //@002=
            True : Begin
                     DisplaySuccess(rsProjectDeleted);                          //@002=
                     acProjectListUpdate.Execute;
                   end;
            else                                                                //@002+
              DisplayError(GetLastErrorMessage);                                //@002+
          end; // of CASE                                                       //@002+
        end; //of IF
    end; // of WITH
end; // of PROCEDURE

procedure TDataModule1.acDocPropCommitExecute(Sender: TObject);                 //@003+
// var
//  tmpDocument:TDocument;
begin
 (* with mtFrmMain.frmMain.fraProject1.tvProject do                             //@004-
    // The following should always be true, but just in case...
    if (assigned(selected)) AND (Selected.Level = lvlDocument) then
      with frmMain.fraDocProps1 do
      begin
       Assert(Assigned(Selected.Data),
        'TDataModule1.acDocPropCommit: Internal Error: Selected.Data not assigned');
       TDocument(Selected.Data).Commit;
       DisplaySuccess('Document properties updated.');
      end;
   *)
end;


procedure TDataModule1.acDocPropUpdateExecute(Sender: TObject);                 //@003+
//   var
//    tmpDocument:TDocument;
  begin
  (*  with mtFrmMain.frmMain.fraProject1.tvProject do                           //@004-
      // The following should always be true, but just in case...
      if (assigned(selected)) AND (Selected.Level = lvlDocument) then
        with frmMain.fraDocProps1 do
        begin
         Assert(Assigned(Selected.Data),
          'TDataModule1.acDocPropUpdate: Internal Error: Selected.Data not assigned');
         tmpDocument := TDocument(Selected.Data);
         tmpDocument.Description := mmDescription.Text;
         tmpDocument.SrcLang := cbSrcLang.Text;
         tmpDocument.DestLang := cbDestLang.text;
        end;   *)
  end;


procedure TDataModule1.acAddFileToProjectExecute(Sender: TObject);
begin
  with mtFrmMain.frmMain.fraProject1.tvProject do
    begin
      if (not assigned(selected)) or (Selected = MainNode)
        or (Selected.Level <> lvlProject) then
         begin
           DisplayError(rsMustSelectProject);
           exit;
         end;
      // There should be a wizard or something here to help the user pick the
      // file, language, etc.
      With frmProjectFileAdd do
        begin
          cbProjectName.Text := Selected.Text;
          Show;
        end;
      (*
      If OpenDialog1.Execute then
         begin
           If ProjectFileAdd(Selected.text, OpenDialog1.FileName,
                               '',
                               'EN',
                               'JA') = 0 then
             DisplaySuccess(rsFileAdded);
           // Update the display from the database to reflect the new file.
           acProjectListUpdate.Execute;
         end;  // of IF *)
    end; // of WITH

end;

procedure TDataModule1.acDeleteFileFromProjectExecute(Sender: TObject);
  var                                                                           //@007+
    Doc:TDocument;                                                              //@007+
    DocList:TDocumentList;                                                      //@007+
  begin
    with mtFrmMain.frmMain.fraProject1.tvProject do
      begin
        if (not assigned(selected))
        or (Selected = MainNode)
        or (Selected.Level <> lvlDocument) then
          begin
            DisplayError(rsMustSelectFile);
            exit;
          end;
        Case MessageDlg (rsDeleteFileWindowTitle, rsDeleteFilePromptText,       //@006=
                       mtConfirmation,
                       [mbYes, mbNo],0)  of                                     //@006=
         mrYes: begin                                                           //@006=
                  Doc := TDocument(Selected.Data);                              //@007+
                  If not Assigned(Doc) then exit;                               //@007+
                  DocList := TDocumentList(Doc.DocumentList);                   //@007+
                  If not Assigned(DocList) then exit;                           //@007+
           IF DocList.RemoveDocument(Doc.Name) then                             //@007+
//           With TDocument(Selected.Data) do                                   //@007=
 { TODO 1 -oshiruba -cencapsulation : Convert from ProjectFileDelete DocumentList.RemoveDoc, etc. }
//             if ProjectFileDelete(Selected.Parent.Text,                                 //@007-
//                           Name,                                                //@007=@007-
//                           SrcLang,                                             //@006=@007-
//                           DestLang) = rcNoProblem then                         //@006=@007=@007-
              DisplaySuccess(rsFileDeleted)
             else                                                               //@007+
               DisplayError('Error' + GetLastErrorMessage);                     //@007+
        // Update the display from the database to reflect the new file.
            acProjectListUpdate.Execute;

          end; // of CASE mrYes
         mrNo: DisplayInfo('Operation canceled.');                              //@006+
         else                                                                   //@006+
           DisplayError('Unknown dialog result');                               //@006+
        end; // of CASE                                                         //@006+
      end; // of WITH
  end; // of PROCEDURE

procedure TDataModule1.acInitIntroExecute(Sender: TObject);


var
  FN:UTF8String;
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;

begin
    FN:= 'index.html';

  // begin of sample code
    try
    fs := TFileStream.Create (FN, fmOpenRead);
    try
      NewHTML := TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
 //     NewHTML.OnGetImageX := @HTMLGetImageX;
      NewHTML.LoadFromStream (fs);
      frmmain.fraIntro.IpHtmlPanel1.SetHtml (NewHTML);
  //    if  Anchor <> ''
  //    then  fraIntro.IpHtmlPanel1.MakeAnchorVisible (Anchor);
  //    UpdateSB;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg (rsHTMLCantOpen+sLineBreak+
        'HTML File: '+Fn+sLineBreak+
        'Error: '+E.Message, mtError, [mbCancel], 0);
    end;
  end;
  // end of sample code
end;

end.

