unit frmShiritoriMain;
//@000 2012.02.06 Noah SILVA Shiritori Game - Because I can


//http://en.wikipedia.org/wiki/Shiritori
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ActnList;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acComputerLoses: TAction;
    acPlayerLoses: TAction;
    ActionList1: TActionList;
    bbGo: TBitBtn;
    bbStartOver: TBitBtn;
    ebInput: TEdit;
    lblMake: TLabel;
    lblWord: TLabel;
    procedure acComputerLosesExecute(Sender: TObject);
    procedure acPlayerLosesExecute(Sender: TObject);
    procedure bbGoClick(Sender: TObject);
    procedure bbStartOverClick(Sender: TObject);
    procedure ebInputChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

uses lazutf8, transdb, transconst;

procedure TfrmMain.bbGoClick(Sender: TObject);
CONST
  Lang = 'JA';
var
 Input16:UnicodeString;
 UserLastChar:WideChar;
 UserFirstChar:WideChar;
 ComputerLastChar:WideChar;
 SearchString:UTF8String;
 Answer:UTF8String;
 Answer16:UnicodeString;
begin
 Input16 := UTF8toUTF16(ebInput.text);
 Answer16 := UTF8toUTF16(lblWord.Caption);

 // First, check the User's answer
 UserFirstChar := Input16[1];
 // Get last char
 UserLastChar := Input16[Length(Input16)];
 // Get last char
 ComputerLastChar := Answer16[Length(Answer16)];

 // Player loses if their first char doesn't match the computer's last
 // Really, we should accept kanakana or hiragana, and look up pronunciation for
 // kanji.
 If (not (UserFirstChar = ComputerLastChar))

// Another way to lose is if the player used a word ending in N.
// katakana N or kanji words ending in N are also not allowed.
 or (UTF16toUTF8(UserLastChar) = 'ん') then
    begin
      acPlayerLoses.Execute;
      exit;
    end;

 // Possible Additional checks After that:
 // 1. Check to see if their word is in the dictionary
 // 2. Could check to make sure it's a noun.


 // ShowMessage(UTF16toUTF8(LastChar));
 SearchString := UTF16toUTF8(UserLastChar) + '%';



 Answer := TextSearch(Lang, SearchString, mdWord);
 If Answer <> '' then
   lblWord.Caption := Answer
 else
   begin
     lblWord.Caption := '???';
     acComputerLoses.Execute;
     exit;
   end;

// Check the Computer's answer
Answer16 := UTF8toUTF16(Answer);
// Get last char
ComputerLastChar := Answer16[Length(Answer16)];
If UTF16toUTF8(ComputerLastChar) = 'ん' then
  begin
    acComputerLoses.Execute;
    exit;
  end;

// Ready for next word input
 ebInput.Clear;





end;

procedure TfrmMain.acComputerLosesExecute(Sender: TObject);
begin
  lblMake.Caption:='   私の負け :(';
  lblMake.Visible:= True;
  ebInput.Visible:=False;
  bbGo.Visible:=False;
  bbStartOver.Visible := True;
  lblWord.Color:= clRed;

  exit;
end;

procedure TfrmMain.acPlayerLosesExecute(Sender: TObject);
begin
  lblMake.Caption:='   あなたの負け:)';
  lblMake.Visible:= True;
  ebInput.Visible:=False;
  bbGo.Visible:=False;
  bbStartOver.Visible := True;
  exit;
end;

procedure TfrmMain.bbStartOverClick(Sender: TObject);
begin
  lblMake.Visible:= False;
  bbStartOver.Visible := False;
  ebInput.clear;
  ebInput.Visible:=True;
  bbGo.Visible:=True;
  lblWord.Caption:= 'しりとり';
  lblWord.Color:= clNone;
end;

procedure TfrmMain.ebInputChange(Sender: TObject);
begin
  bbGo.Enabled := ebInput.Text <> '';
end;

end.

