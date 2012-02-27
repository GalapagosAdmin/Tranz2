unit mtfrmmain;
//@001 2011.11.16 Noah Silva : Added DefaultTranslator
//                             Added OwnerDraw StatusBar
//@002 2011.11.22 Noah Silva : Added Document Actions Tab Functionality.
//                             Added Translation Unit (Segment Editing) Tab.
//@003 2011.11.?? Noah Silva + Added BlockImport and SegmentEdit SpeedButtons
//@004 2011.12.04 Noah Silva + Added call to ShowFrame for Segment Edit

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, ExtCtrls, mtprojecttree, mtfraintro, mtFraDocProps, mtfraprojprops,
  mtdoclist, mtfrablockimport, mtfrasegedit, ExtendedNotebook;//, mtfrablockimport;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ExtendedNotebook1: TExtendedNotebook;
    FraBlockImport1: TFraBlockImport;
    fraDocList1: TfraDocList;
    fraDocProps1: TfraDocProps;
    fraProjProps1: TfraProjProps;
    fraSegmentEdit1: TfraSegmentEdit;
  //  lblProjectDescription: TLabel;
  //  lblProjectName: TLabel;
    nbProjSidePanel: TExtendedNotebook;
    fraIntro: TFraIntro;
    fraProject1: TfraProject;
    sbSegEdit: TSpeedButton;
    ScrollBox1: TScrollBox;
    sbProjectTree: TSpeedButton;
    sbDocumentList: TSpeedButton;
    sbBlockImport: TSpeedButton;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tsSegEdit: TTabSheet;
    tsBlockImport: TTabSheet;
    tsProjProp: TTabSheet;
    tsFileProp: TTabSheet;
    tsDocument: TTabSheet;
    tsIntro: TTabSheet;
    tsProject: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure sbBlockImportClick(Sender: TObject);
    procedure sbSegEditClick(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure sbProjectTreeClick(Sender: TObject);
    procedure sbDocumentListClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure tsIntroContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses mtDataModule,
     DefaultTranslator, TransConst,                                             //@001+
     mtNotification;                                                            //@002+
{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.sbProjectTreeClick(Sender: TObject);
begin
  ExtendedNotebook1.ActivePage := tsProject;
  with DataModule1 do acProjectListUpdate.execute;
end;

procedure TfrmMain.sbDocumentListClick(Sender: TObject);
  begin
    ExtendedNotebook1.ActivePage := tsDocument;                                 //@001+
    fraDocList1.update;                                                         //@002+
  end;

procedure TfrmMain.StatusBar1DrawPanel(StatusBar: TStatusBar;                   //@001+
  Panel: TStatusPanel; const Rect: TRect);

begin
  with Statusbar.Canvas do
    begin
     case Panel.Index of

      sbpIcon: //first panel
       begin
         Brush.Color := clDefault;
         Font.Color := clBlack;
         Font.Style := [fsBold];
         FillRect(Rect) ;
         With DataModule1 do
           // The timer adds 100 to the tag to reflect that the message is "stale" ,
           // but we should still keep the same icon.
           If StatusBar.Tag >= tgStale then
             ilMessageTypes.Draw(StatusBar1.Canvas, Rect.Left, Rect.Top, StatusBar.Tag-100)
           else
             ilMessageTypes.Draw(StatusBar1.Canvas, Rect.Left, Rect.Top, StatusBar.Tag) ;
       end;
      sbpMain: //second panel
       begin
         Case StatusBar.Tag of
           tgNormal:  Brush.Color := clDefault;
           tgError:   Brush.Color := clRed;
           tgWarning: Brush.Color := clYellow;
           tgSuccess: Brush.Color := clGreen;
           tgInfo:    Brush.Color := clGray;
           else
             Brush.Color := clDefault;
         end;
         Font.Color := clBlack;
         FillRect(Rect) ;
         TextRect(Rect,2 + Rect.Left, 2 + Rect.Top,Panel.Text) ;
    //     Font.Style := [fsItalic];
       end; // of Second Panel
     end; // of CASE
 //      TextRect(Rect,2 + Rect.Left, 2 + Rect.Top,Panel.Text) ;
    end; // of WITH

end;

procedure TfrmMain.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  DataModule1.acInitIntro.Execute;
  DisplayInfo('Welcome to Maruyaku');                                           //@002+
  { TODO 1 -oshiruba -ci18n : Convert into string resource }
end;

procedure TfrmMain.sbBlockImportClick(Sender: TObject);
begin
  ExtendedNotebook1.ActivePage := tsBlockImport;                                //@003+
 // fraBlockImport1.update;                                                     //@003+
 FraBlockImport1.acFrameShow.Execute;                                           //@004+

end;

procedure TfrmMain.sbSegEditClick(Sender: TObject);
begin
    ExtendedNotebook1.ActivePage := tsSegEdit;                                  //@003+
    FraSegmentEdit1.acFrameShow.Execute;                                        //@004+
    FraSegmentEdit1.acUpdateExecute(self);
end;


procedure TfrmMain.tsIntroContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

end.

