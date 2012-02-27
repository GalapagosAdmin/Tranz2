unit mtNotification;
//@000 2011.11.21 Noah Silva + Initial Version
//@001 2011.11.22 Noah Silva + Added Info

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Procedure DisplayError(Const Message:UTF8String);
Procedure DisplaySuccess(Const Message:UTF8String);
Procedure DisplayInfo(Const Message:UTF8String);                                //@001+


implementation

uses mtdatamodule;

Procedure DisplayError(Const Message:UTF8String);                  //@001+
  begin
    DataModule1.DisplayError(Message);
  end;

Procedure DisplaySuccess(Const Message:UTF8String);                //@001+
  begin
    DataModule1.DisplaySuccess(Message);
  end;

Procedure DisplayInfo(Const Message:UTF8String);                                //@001+
  begin
    DataModule1.DisplaySuccess(Message);
  end;


end.

