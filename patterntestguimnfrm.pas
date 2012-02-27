unit PatternTestGUIMnFrm;

{$mode objfpc}{$H+}
//001 2011.09.09 Noah Silva Added unit required for constants.

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnJikou: TButton;
    Button1: TButton;
    ebInput: TEdit;
    ebResult: TEdit;
    procedure btnJikouClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ebInputChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
 pattern, transdb,
 transconst;  //001+

{ TForm1 }

procedure TForm1.btnJikouClick(Sender: TObject);
begin
    ebResult.text := TranslateText('EN', ebInput.text, 'JA', mdAuto);
//    ebResult.text := Pattern_AutoSearch('EN', ebInput.text, 'JA');
end;

// no mix/match, just look up the pattern as-is in the DB
procedure TForm1.Button1Click(Sender: TObject);
begin
   ebResult.Text := TranslateText('EN', ebinput.text, 'JA', mdPattern)
end;

procedure TForm1.ebInputChange(Sender: TObject);
begin

end;

initialization
  {$I patterntestguimnfrm.lrs}

end.
