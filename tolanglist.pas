unit toLangList;
//@000 2011.11.21 Noah Silva : Initial Version
//@001 2012.01.15 Noah Silva : Convert to use database
//@002 2012.01.31 Noah SILVA : Changed procedure name to match with TransDB
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransConst;

Type
  TLangList = Class(TObject)
  private
    // later we will have a dynamic list of TLangEntry which will have icons, etc.
  public
    Strings:TStringList;                                                        //@001=
    Constructor Create;
    Destructor Destroy;
  end;

Var
  LangList:TLangList;

implementation

Uses transdb;                                                                   //@001+

Constructor TLangList.Create;
  begin
    inherited;
    Strings := TStringList.Create;
    LangCodeListGet(Strings);                                                   //@001+@002=
                                                                                //@001-
    // Hard-coded test entries
{    Strings.Add('EN');
    Strings.Add('JA');
    Strings.Add('KO');
    Strings.Add('CN');
    Strings.Add('DE');  }

  end;

Destructor TLangList.Destroy;
  begin
    Strings.Free;
    inherited;
  end;

Initialization
  LangList :=TLangList.Create;

Finalization
  LangList.Free;

end.

