unit frmJDImport_u;
// Modified LGPL License (Static Linking Allowed)
//@000 2012.02.19 Noah SILVA : Initial Version


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  Buttons;

type

  { TfrmJDImport }

  TfrmJDImport = class(TForm)
    bbImport: TBitBtn;
    FileNameEdit1: TFileNameEdit;
    procedure bbImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmJDImport: TfrmJDImport;

implementation

  uses toJMDic;

  { TfrmJDImport }

  procedure TfrmJDImport.bbImportClick(Sender: TObject);
   var
     JDMImporter:TJMDicExtractor;
  begin
    try
      JDMImporter := TJMDicExtractor.Create;
      JDMImporter.SetFile(FileNameEdit1.Text);
    finally
      JDMImporter.Free;
    end;
  end;

  procedure TfrmJDImport.FormCreate(Sender: TObject);
  begin

  end;

{$R *.lfm}

end.

