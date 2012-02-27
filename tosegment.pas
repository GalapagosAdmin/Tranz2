unit toSegment; // TransObjects 2.0 Segmenter
// COPYRIGHT (C) 1996, 2008-2011 Galapagos Software Japan ALL RIGHTS RESERVED
// Breaks "Blocks" (i.e. inputs that could contain entire paragraphs) into
// segments (essentially sentences).
// 000 2011.09.11 Noah SILVA Started Segmenter 2.0 unit
// 001 2011.09.17 Noah SILVA Changes Char Detection Logic
// 002 2011.09.18 Noah SILVA Added erasure to prevent false abbreviation
//                           detection when there are duplicated of the same
//                           abbr in the block being processed.
// 003 2011.09.26 Noah SILVA Changes to compile on FPC 2.4.2/Win32
// 004 2011.11.30 Noah SILVA Renamed Unit Segment -> toSegment

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lclproc,
  transconst;                                                                   //@002+

type
  TSegmenter=class
    private
      block:UnicodeString; // input block UTF16
      segment_markers:UnicodeString; // String containing end of sentence markers//@001+
      abbr_list: Array of UnicodeString;                                        //@001+
      Lang:TLang;                                                               //@002+
      function do_segment:integer;
      Function Check_Single_Abbr(Const Seg_Delim_Pos:Integer;                   //@001+
//                               Const Test_Abbr:WideString):Boolean;           //@003-
                                 Const Test_Abbr:UnicodeString):Boolean;        //@003+
      Function Check_Any_Abbr(Const Seg_Delim_Pos:Integer):Boolean;             //@001+
      Procedure init_abbr_list_lazy;                                            //@001+
      Procedure init_segment_markers_lazy;                                      //@001+
    public
      segments:TStringList; // output
      constructor init(Const Lang_Code:TLang; Const inp_string:UTF8String);
      destructor done;
    end;

implementation

uses dbugintf, transdb;                                                         //@002+

Constructor TSegmenter.init(Const Lang_Code:TLang; Const inp_string:UTF8String);//@002=
  begin
    Lang := Lang_Code;                                                          //@002+
    Segments := TStringList.Create;
    block := UTF8ToUTF16(inp_String);
    do_segment;
    inherited;
  end;

Destructor TSegmenter.done;
  Begin
    Segments.Free;
    inherited;
  End;

// Check whether we should disqualify this block for abbreviations
// Returns true if the test abbreviation is found at the same position in the
// block where the current segment delimiter is located.
// i.e. returns true if this is a delimiter we should skip because it is really
// an abbreviation period instead of a sentence-ending period.
Function TSegmenter.Check_Single_Abbr(Const Seg_Delim_Pos:Integer;              //@001+
                                      Const Test_Abbr:UnicodeString//WideString //@003=
                                      {Const block:WideString}):Boolean;
  var
    Seg_Abbr_Pos:Integer; // Position of the first char of the abbr in the block
    erase_pos :Integer;   // position for erasure code
  begin
   Seg_Abbr_Pos := POS(Test_Abbr, block);
   If Seg_Abbr_Pos <= 0 then
       Result := False
   else
       begin
         // Also handles abbreviations which contain internal periods.
         Result := (Seg_Delim_Pos >= Seg_Abbr_Pos) and
                   (Seg_Delim_Pos <= (Seg_Abbr_Pos + Length(Test_Abbr) -1));
         // If this was an abbr we need to skip, then we also need to erase it
         // so we don't find it again when we are looking for the same abbr
         // later.
         // Note that this is ok to erase since the string is already copied
         // into the temp_ws by this point.
         If Result then                                                         //@002+
         // only erase if the current pos is at the end of the abbreviation
         // (Otherwise abbr with internal periods will break)
          if Seg_Delim_Pos = (Seg_Abbr_Pos + Length(Test_Abbr) -1) then         //@002+
           for erase_pos := Seg_Abbr_Pos to Seg_Delim_Pos do                    //@002+
             Block[erase_pos] := '*';                                           //@002+
       end;
  end;

// Populated the abbreviation list from the database, if required
Procedure TSegmenter.init_abbr_list_lazy;                                       //@001+
 begin
   If NOT (Length(Abbr_List) = 0) then exit;
  // test data for now,
   // real ones will be loaded from the database, table SEGMENT_ABBR

   // Table SEGMENT_ABBR contains segment exclusion abbreviations
 //  SetLength(Abbr_List, 10);
 //   Abbr_List[0] := 'Mr.';
 //   Abbr_List[1] := 'Mt.';
 //   Abbr_List[2] := 'Ms.';
 //   Abbr_List[3] := 'ie.';
 //   Abbr_List[4] := 'etc.';  // could be sentence end
 //   Abbr_List[5] := '...';
 //   Abbr_List[6] := 'i.e.';
 //   Abbr_List[7] := 'D.C.';
 //   Abbr_List[8] := 'U.S.';
 //   Abbr_List[9] := 'Mrs.';

    SegmentAbbrGet(Lang, Abbr_List);                                            //@006+

//     Test_Abbr := 'Mr.';
// Issues:
// Full names like Bob H. Smith aren't abbreviations, and so will be segmented
// at the initial.
// URLS like http://www.google.com/whatever have periods and will be segmented


 end;

Function TSegmenter.Check_Any_Abbr(Const Seg_Delim_Pos:Integer):Boolean;        //@001+
  var
    ever:Integer;
  Begin
    init_abbr_list_lazy;
    Result := false;
    for ever := low(abbr_list) to high(abbr_list) do
      begin
       Result :=  Check_Single_Abbr(Seg_Delim_Pos, Abbr_List[ever]);
       If Result then break;  // one match is all we need
     end;
  end;

// Populates the sentence end markers, if required.
Procedure TSegmenter.init_segment_markers_lazy;                                 //@001+
// var
 //   jp_chars:WideString;                                                      //@001-
//    jp_period:WideChar;                                                       //@001+
 //   jp_chars_utf8:UTF8String;

 begin
    If not Length(Segment_Markers) = 0 then exit;
    // This should use SEGMENT_IN Segment marker table to find segment markers
    // test data for now
    // this mostly takes care of English

 //   Segment_Markers := '.?'+#13;
    // Japanese
 //    jp_chars_utf8 := '？。';
 //   jp_chars := UTF8ToUTF16(jp_chars_utf8);
 //   Segment_Markers := Segment_Markers + jp_chars;                            //@003-
    Segment_Markers := SegmentDelimGet(Lang);                                   //@003+
 //   SendDebug('init_Segment_Markers_lazy: length(segment_Markers)='
 //   + IntToStr(Length(Segment_Markers)));
 end;

function TSegmenter.do_segment:integer;
  const
    min = 1; // index base for WideString
  var
    p:longint;
    { TODO 3 -oshiruba -cif-im-bored : Why is the following not UnicodeString? Investigate }
    temp_ws:WideString;
    seg_delim_pos:Integer;                                                      //@001+


  begin
    init_segment_markers_lazy;

    temp_ws := '';
    Segments.Clear;
    for P := min to length(block) do
      begin
        temp_ws := temp_ws + block[P];
        // check if we got to a period;
   //     CharInSet
   //     if block[P] in ['.', '?', #13{, jp_period[1]}] then                   //@001-
         if POS(block[P], Segment_Markers) > 0 then                             //@001+
          begin
//            SendDebug('do_segment: Segment marker found at ' + IntToStr(P));
            If Check_Any_Abbr(P) then continue;
            Segments.Append(trim(UTF16ToUTF8(temp_ws)));
            temp_ws := '';
          end;
      end;
    // loop is done, but there may be some left-over, if so, we'll put that into
    // the last segment
    If Length(temp_WS) > 0 then
      begin
            Segments.Append(trim(UTF16ToUTF8(temp_ws)));
            // we erase up to the point where we have copied
            // this is to avoid picking up already processed abbreviations later
            temp_ws := '';
      end;
  end;

end.
