unit FrmSegment_Test;

//@003 2011.11.30 Noah SILVA : Adapted for Segmenter unit name change.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, PairSplitter;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnDoSegment: TButton;
    dbBlock: TDividerBevel;
    dbSegments: TDividerBevel;
    lbSegments: TListBox;
    leLang: TLabeledEdit;
    mmBlock: TMemo;
    PairSplitter1: TPairSplitter;
    psBlock: TPairSplitterSide;
    psSegments: TPairSplitterSide;
    procedure btnDoSegmentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mmBlockChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation


uses toSegment,                                                                 //@003=
     TransConst;                                                                //@002=
{$R *.lfm}

{ TfrmMain }


{ TfrmMain }

procedure TfrmMain.btnDoSegmentClick(Sender: TObject);
var
  Segmenter:TSegmenter;
begin
  Segmenter := TSegmenter.init(leLang.text, mmBlock.Lines.Text);
  lbSegments.Items := Segmenter.segments;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.mmBlockChange(Sender: TObject);
begin

end;

end.

