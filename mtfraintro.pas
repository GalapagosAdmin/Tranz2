unit mtfraintro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, IpHtml;

type

  { TfraIntro }

  TfraIntro = class(TFrame)
    IpHtmlPanel1: TIpHtmlPanel;
    procedure IpHtmlPanel1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

{ TfraIntro }

procedure TfraIntro.IpHtmlPanel1Click(Sender: TObject);
begin

end;

end.

