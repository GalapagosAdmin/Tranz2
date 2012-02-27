unit toLangListCombo;
// Helper Class to fill in contents of TComboBox with language options
// Implemented outside of toLangList so as not to introduce GUI dependencies
// there.
{$mode objfpc}{$H+}
//@000 2012.02.24 Noah Silva : Initial Version

interface

uses
  Classes, SysUtils, StdCtrls;

type
  TComboBoxHelper = class helper for TComboBox
//  private
//    function GetCheckedCount: Integer;
  public
    Procedure Fill;
  end;

implementation

 Uses toLangList;

 Procedure TComboBoxHelper.Fill;
   begin
     Items := LangList.Strings;
   end;

end.

