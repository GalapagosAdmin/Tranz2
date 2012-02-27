unit TransTokenRemix;
// Translation Tokenizer Remix Routines   / Part of the Tranz2 Suite
// Converts a list of tokens into a list of possible combinations, so they can
// each be tested for translation.

// Coverage map routines - generate a coverage map matrix with bitmap of
// translation coverage.

// Copyright 2009 - 2012 Noah SILVA / Galapagos Software
//@000 2009.??.?? Noah SILVA : Initial version
//@001 2011.09.24 Noah SILVA : Start of Japanese Support
//                             String -> UTF8String
//@002 2011.09.26 Noah SILVA : String -> UTF8String
//@003 2012.01.31 Noah SILVA : Improvements on ContatToken

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, TransConst;

 Type
   TRemixCell = UTF8String;                                                     //@001=
   TRemixRow = record
                RunStart:integer;  // Zero-based starting token from original list
                RunLength:integer; // Length in tokens
// The actual tokens are a bit redundant since we can always get them using the
// above two parameters from the original TTokenList.
                Tokens: Array of TRemixCell;
               end; // of RECORD
   TRemixList = Array of TRemixRow;
// Remix list data structure
// Each Row represents a list of Tokens, like:
// 0     1    2     3    4
// My    Name Is    Noah .

   TCoverageMapCell = integer;
   TCoverageMapRow = record
                      bitmap:Array of TCoverageMapCell;
                      deletionFlag:boolean;
                     end;
   TCoverageMap = Array of TCoverageMapRow;
// TCoverageMap data structure
// The intention is to use this as a bitmap of translation coverage
// 0 = no coverage achieved
// 1 = first coverage group
// 2 = second coverage group (etc)
// For example:
// TRemixList[0]
// 0     1    2     3    4
// My    Name Is    Noah .
// TCoverageMap[0]
// 0     0    0     0    0    = No coverage
// 1     1    1     1    1    = Perfect coverage in one pass
// 1     1    1     2    3    = Coverage in three passes
// While zero is the worst, having 1 as the highest number is the best,
// 2 is second best, and so-on.


Function TokenRemix(const Lang:Tlang;
                    const TokenList:TTokenList):TStringList; overload;
Function TokenRemix(const Lang:Tlang; const TokenList:TTokenList;
                    var RemixList:TRemixList):Integer; overload;
// Produces a list containing a tree of possible combinations of tokens
// in a list.
// Example List: My, name, is, Noah
// Result
// Length = 1 Token
// 1. My
// 2. name
// 3. is
// 4. Noah
// Length = 2 Tokens
// 5. My name
// 6. name is
// 7. is Noah
// Length = 3 Tokens
// 8. My name is
// 9. name is Noah
// Length = 4 Tokens
// 10. My name is Noah
// We provide two versions,
// 1. One with a simple interface using TStringList.
// 2. One which provides more information, using a custom array type.

Function CoverageMatrixBuild(const SrcLang:Tlang; const TokenList:TTokenList;
                             const DestLang:Tlang;
                             var CoverageMap:TCoverageMap):Integer;

// Uses Token strings from TTokenList
Function RemixRow2String(Const Lang:TLang; Const RemixEntry:TRemixRow;
                         Const TokenList:TTokenList):UTF8String; Overload;      //@001=
// Converts a translation coverage bitmap into run-length encoded lists,
// one for fragments used, and one for remaining holes.
Procedure CMR2RLL(const CMR:TCoverageMapRow; var RLL:TRemixList;
                                             var Holes:TRemixList);

// Concatenates tokens into a string in a language specific way.
Function TokenConcat(Const Lang:Tlang;
                     Const Token1:UTF8String;                                   //@001=
                     Const Token2:UTF8String):UTF8String;                        //@001=

implementation

Uses
 TransDB;

Function TokenConcat(Const Lang:Tlang;
                     Const Token1:UTF8String;                                   //@001=
                     Const token2:UTF8String):UTF8String;                       //@001=
 begin
  Case lang[1] of
   'E':begin  // English
         if token2 = PeriodEN then
          result := token1 + token2
         else if token2 = CommaEN then
          result := token1 + token2
         else if token2 = QuestionEN then
          result := token1 + token2
         else if Length(Token1) = 0 then                                        //@003+
           Result := Token2                                                     //@003+
         else if Length(Token2) = 0 then                                        //@003+
           Result := Token1                                                     //@003+
         else
          result := token1 + DelimEN + token2;
       end;
   'J':begin  // Japanese
        result := token1 + token2;
       end;
  else
    ASSERT(LANG='EN', 'TransTokenRemix.TokenConcat: Unsupported Language: ' + Lang);  //@001=
  end;
 end; // OF FUNCTION

Function TokenRemixChunkEN(const TokenList:TTokenList;
                         Const Size:integer; const TokenCount:integer;
                         var wip:TRemixList):integer; overload;
var
  Token, StartPos : Integer;
//  Tmp : String;                                                               //@001-
//  RemixEntry:TStringList;                                                     //@001-
  Row:Integer;
Begin
// Same as TokenRemix routine, but allows selection of "chunk" size - i.e. how
// many Tokens at a time we are taking for this round.
// For the inner loop, we loop through all the tokens, taking into account that
// we need to reduce the bound by the number of items we are taking each loop.
 // RemixEntry := TStringList.Create;


     Result := 0;

       For StartPos := 0 to (TokenCount - Size) do
         Begin

 // TRemixList Array Housekeeping
 // Append an entry to the array
           SetLength(wip, high(wip)+2);
 // Select the new (empty) row for udating
           Row := high(wip);
           wip[row].RunStart  := StartPos;
           wip[row].RunLength := Size;
           SetLength(wip[row].Tokens, size);

           For Token := StartPos to (StartPos + (Size-1)) do
             Begin

//                  wip[High(wip)].Add( TokenList.Strings[Token-1]);
                wip[Row].Tokens[Token-StartPos] := (TokenList.Strings[Token]);

             End;
// We built this string, so we will add it to the result list
// Remove excess space.
// Add one more entry to the Remix List (Dynamic Array)

   //         := RemixEntry;

         End;

End; // of FUNCTION

// Copied from English version, identical for now.
Function TokenRemixChunkJA(Const TokenList:TTokenList;                          //@001+
                           Const Size:integer; Const TokenCount:Integer;        //@001+
                           Var wip:TRemixList):Integer; Overload;
  Var
    Token, StartPos : Integer;
    Row:Integer;
  Begin
// Same as TokenRemix routine, but allows selection of "chunk" size - i.e. how
// many Tokens at a time we are taking for this round.
// For the inner loop, we loop through all the tokens, taking into account that
// we need to reduce the bound by the number of items we are taking each loop.
     Result := 0;
       For StartPos := 0 to (TokenCount - Size) do
         Begin
 // TRemixList Array Housekeeping
 // Append an entry to the array
           SetLength(wip, high(wip)+2);
 // Select the new (empty) row for udating
           Row := high(wip);
           wip[row].RunStart  := StartPos;
           wip[row].RunLength := Size;
           SetLength(wip[row].Tokens, size);

           For Token := StartPos to (StartPos + (Size-1)) do
                wip[Row].Tokens[Token-StartPos] := (TokenList.Strings[Token]);
         End;  // of FOR
  End; // of FUNCTION

Function TokenRemixChunkEN(const TokenList:TTokenList;
                           Const Size:Integer; Const TokenCount:Integer;
                           wip:TStringList):Integer; Overload;

Var
  StartPos:Integer;
  Tmp:UTF8String;                                                               //@001=
  Token:Integer;
Begin
// Same as TokenRemix routine, but allows selection of "chunk" size - i.e. how many
// Tokens at a time we are taking for this round.
// For the inner loop, we loop through all the tokens, taking into account that
// we need to reduce the bound by the number of items we are taking each loop.
       For StartPos := 1 to (TokenCount - Size) + 1 do
         Begin
           Tmp := '';
           For Token := StartPos to (StartPos + Size) - 1 do
             Begin
               Case Token Of
               1: // First entry gets no delimiter
                 Tmp := Tmp + TokenList.Strings[Token-1];
               Else // Otherwise, we append the new word to the previous ones
                tmp := TokenConcat('EN', tmp, TokenList.Strings[Token-1]);
//                 If TokenList.Strings[Token-1] = periodEN Then  // in case of period
//                   Tmp := Tmp + TokenList.Strings[Token-1]   // No delimiter
//                 else
//                   Tmp := Tmp + delimEN + TokenList.Strings[Token-1];  // use delimiter
               end; // of CASE
             End;
// We built this string, so we will add it to the result list
// Remove excess space.
           wip.Append(Trim(Tmp));
         End;
  Result := 0;
End; // of FUNCTION

// Exact Copy of English version for now... these might not need to be separate
Function TokenRemixChunkJA(const TokenList:TTokenList;                          //@001+
                           Const Size:Integer; Const TokenCount:Integer;
                           wip:TStringList):Integer; Overload;
Var
  StartPos:Integer;
  tmp:UTF8String;
  Token:Integer;
Begin
// Same as TokenRemix routine, but allows selection of "chunk" size - i.e. how many
// Tokens at a time we are taking for this round.
// For the inner loop, we loop through all the tokens, taking into account that
// we need to reduce the bound by the number of items we are taking each loop.
       For StartPos := 1 to (TokenCount - Size) + 1 do
         Begin
           Tmp := '';
           For Token := StartPos to (StartPos + Size) - 1 do
             Begin
               Case Token Of
               1: // First entry gets no delimiter
                 Tmp := Tmp + TokenList.Strings[Token-1];
               Else // Otherwise, we append the new word to the previous ones
                tmp := TokenConcat('JA', tmp, TokenList.Strings[Token-1]);
               end; // of CASE
             End;
// We built this string, so we will add it to the result list
// Remove excess space.
           wip.Append(Trim(Tmp));
         End;
  Result := 0;
End; // of FUNCTION


// Language independant - produces combinations of chains of a certain number
// of tokens, given an input token list, and size directive.
Function TokenRemixChunk(const Lang:String; const TokenList:TTokenList;
                         const Size:integer; const TokenCount:integer;
                         var wip:TRemixList):integer; overload;
 begin
//  IF lang = 'EN' THEN                                                         //@001-
    Case Lang[1] of                                                             //@001+
      'E':Result := TokenRemixChunkEN(TokenList, Size, TokenCount, wip);        //@001=
      'J':Result := TokenRemixChunkJA(TokenList, Size, TokenCount, wip);        //@001=
    else                                                                        //@001-
     RESULT := -1; // not implemented yet
    end;                                                                        //@001+
  end;

 // StringList version
Function TokenRemixChunk(Const Lang:String; Const TokenList:TTokenList;         //@001=
                         Const Size:integer; Const TokenCount:Integer;
                         wip:TStringList):Integer; overload;
 begin
//  IF lang = 'EN' THEN                                                         //@001-
  Case Lang[1] of                                                               //@001+
    'E':Result := TokenRemixChunkEN(TokenList, Size, TokenCount, wip);          //@001=
    'J':Result := TokenRemixChunkJA(TokenList, Size, TokenCount, wip);          //@001+
  else
   RESULT := -1; // not implemented yet
  end; // of CASE                                                               //@001+
 end;  // of Procedure

// RemixList version
Function TokenRemix(const Lang:Tlang;
                    const TokenList:TTokenList; var RemixList:TRemixList):Integer; overload;
 Var
  TokenCount : Integer;
  Size:Integer;
 Begin
   TokenCount := TokenList.Count;
//   Size = the number of tokens we are doing for this string
// we could do as few as one, and as many as how ever many there
// are in the string.  We also want to do every possibility in-between.
   For Size := 1 to TokenCount do
      TokenRemixChunk(Lang, TokenList, Size, TokenCount, RemixList);
   result := 0;
 End; // of FUNCTION

// StringList version
Function TokenRemix(Const Lang:Tlang;
                    Const TokenList:TTokenList):TStringList; Overload;
 Var
  TokenCount : Integer;
  Size:Integer;
 Begin
   Result := TStringlist.Create;
   TokenCount := TokenList.Count;
// Size = the number of tokens we are doing for this string
// we could do as few as one, and as many as how ever many there
// are in the string.  We also want to do every possibility in-between.
   For Size := 1 to TokenCount do
      TokenRemixChunk(Lang, TokenList, Size, TokenCount, Result);
 End; // of FUNCTION

// Uses the token string data inside the RemixRow structure
Function RemixRow2String(const Lang:TLang;
                         const RemixEntry:TRemixRow):UTF8String;                //@001=
                         Overload;
  var
    Col:integer;
    RunStop:Integer;
  begin
// ASSERT(Lang = 'EN', 'RemixRow2String: Non-English support not implemented.');//@001-
   RunStop := ( RemixEntry.RunLength) - 1;
   For col := 0 to RunStop do
    If col = 0 then
       result := RemixEntry.Tokens[col]
    else
     result := TokenConcat(lang, result, RemixEntry.Tokens[col]);
//    else if RemixEntry.Tokens[col] = '.' then
//       result := result + RemixEntry.Tokens[col]
//    else
//      result := Result + delimEN + RemixEntry.Tokens[col];
  end;

// Uses Token String data from a supplied TTokenList
Function RemixRow2String(Const Lang:TLang; Const RemixEntry:TRemixRow;
                         Const TokenList:TTokenList):UTF8String; Overload;      //@011=
  var
   Col:integer;
   RunStop:Integer;
  begin
// ASSERT(Lang = 'EN', 'RemixRow2String: Non-English support not implemented.');//@001-
   RunStop := ( RemixEntry.RunLength) - 1;
   For col := 0 to RunStop do
    If col = 0 then
       result := TokenList.Strings[col+RemixEntry.RunStart]
//    else if TokenList.Strings[col+RemixEntry.RunStart] = '.' then
    else
      result := TokenConcat(Lang,
                            Result,
                            TokenList.Strings[Col+RemixEntry.RunStart]);
//       result := result + TokenList.Strings[col+RemixEntry.RunStart]
//    else
//      result := Result + delimEN + TokenList.Strings[col+RemixEntry.RunStart];
  end;

 Procedure CoverageMapRowAppend(Var CoverageMap:TCoverageMap);
   var
     Cols:integer;
   begin
     Cols := High(CoverageMap[high(coveragemap)].bitmap);
     SetLength(CoverageMap, high(coveragemap)+2);
     SetLength(CoverageMap[high(coveragemap)].bitmap, Cols);
     CoverageMap[high(coveragemap)].DeletionFlag := false;
   end;

 // removes deleted rows from the table
 Procedure CoverageMapVacuum(var CoverageMap:TCoverageMap);
  var
    Found:boolean;
    Modified:boolean;
    DelRow:integer;
    Index:integer;
  begin
   Repeat
   found := false;
   Modified := false;
   DelRow := -1;
   while (found = false) and (delrow < high(coveragemap)) do
    begin
      inc(delrow);
      Found := CoverageMap[delrow].DeletionFlag;
    end;
   If Found then
    begin
// Make sure we aren't deleting the last row, that's easier
     if delrow <> high(CoverageMap) then
       for Index := delrow to high(CoverageMap)-1 do
        CoverageMap[Index] := CoverageMap[Index+1];
// Resize the table
     SetLength(CoverageMap, high(CoverageMap));  // Remember Zero-based
     Modified := true;
    end;
// keep looping until there are no more deleted rows found
   until Modified = false;
  end; // of Procedure

 // Zeros out bitmap for new entries
 Procedure CoverageMapRowInit(var CoverageMap:TCoverageMap);
   var
    index:integer;
   begin
    For index := 0 to high(CoverageMap[high(coveragemap)].bitmap) do
      CoverageMap[high(coveragemap)].bitmap[index] := 0;
   end;


 Function CMRFragmentCount(const CMR:TCoverageMapRow):Integer;
  var
   token:integer;
  begin
   Result := 0;
     for token := low(CMR.bitmap) to high(CMR.bitmap) do
         if CMR.bitmap[token] > result then
          Result := CMR.bitmap[token];
  end;
 // Merges the bitmap of two rows, mapping fragment numbers
 Function CoverageMapRowMerge(Const Entry1:TCoverageMapRow;
                           Const Entry2:TCoverageMapRow;
                           var MergedRow:TCoverageMapRow):Boolean;
  var
   token:integer;
   Highest:integer;
  begin
   result := true;
// Scan each corrosponding token and make sure one is non-zero
// i.e. search for collissions
   for token := low(entry1.bitmap) to high(entry1.bitmap) do
    if (entry1.bitmap[token] <> 0) AND (entry2.bitmap[token] <> 0) then
      result := false;
// move on
    If result = true then
      begin
// Find the highest number fragment in entry1
        highest := CMRFragmentCount(entry1);
//        for token := low(entry1.bitmap) to high(entry1.bitmap) do
//         if entry1.bitmap[token] > highest then
//          highest := entry1.bitmap[token];
// Make sure the result row is the right length
        SetLength(MergedRow.bitmap, high(Entry1.bitmap)+1);
// Loop through the tokens again and assign from the
// appropriate row
        for token := low(entry1.bitmap) to high(entry1.bitmap) do
          if entry1.bitmap[token] <> 0 then
            MergedRow.bitmap[token] := entry1.bitmap[token]
          else if entry2.bitmap[token] <> 0 then
            MergedRow.bitmap[token] := entry2.bitmap[token] + highest
          else
            MergedRow.bitmap[token] := 0;
      end;
  end; // OF FUNCTION

// Returns the number of tokens covered
 Function CoverageMapRowCoverageGet(const CMR:TCoverageMapRow):Integer;
 var index:integer;
 begin
   result := 0;
   for Index := 0 to high(CMR.bitmap) do
    if CMR.bitmap[Index] <> 0 then Inc(Result);
 end; // of FUNCTION

 // Converts a CoverageMap row into a series of Token RLL entries
 // basically a non-overlapping remix-list
 Procedure CMR2RLL(const CMR:TCoverageMapRow; var RLL:TRemixList;
                                              var Holes:TRemixList);
 // TCoverageMapRow is a row of the CoverageMap, which contains a bitmap of
 // supported fragments, by token.
 // RLL is a run-length encoded list of [covered] fragments we want to return
 // hole_rll is a run-length encoded sections without coverage (holes).
 var
  Index:integer;
  Fragments:integer;
  FragmentID:integer;
  FragmentStart:Integer;
  FragmentLength:Integer;
  HoleStart:Integer;
  HoleLength:Integer;
 begin
   Index := 0;
   Fragments := 0;
   Fragmentid := 0;

   SetLength(rll, 0);
   SetLength(Holes, 0);
   While Index <= High(CMR.bitmap) do
    begin
     FragmentStart := 0;
     FragmentLength := 0;
// skip any zero entries and add to hole (gap) list.
     if CMR.bitmap[index] = 0 then
      begin
        HoleStart := Index;
        HoleLength := 0;
        while  (index <= High(CMR.bitmap))
          and  (CMR.bitmap[index] = 0) do
             begin
               Inc(index);
               Inc(HoleLength);
             end;
        SetLength(Holes, high(Holes)+2);
        Holes[High(Holes)].RunStart := HoleStart;
        Holes[High(Holes)].RunLength := HoleLength;
      end;  // of IF ... 0
// see if we hit a coverage fragment
     if CMR.bitmap[index] <> 0 then
       begin  // if so, save the ID
        FragmentStart := index;
        FragmentLength := 0;
        fragmentid := CMR.bitmap[index];
       end;
// keep following this fragment
      while (index <= High(CMR.bitmap))
// This AND condition could cause a range error if not compiled with
// short-circuit evaluation since the index would be past the array size
// during the second loop.
       and (CMR.bitmap[index] = fragmentID) do
             begin
              Inc(index);
              Inc(FragmentLength);
             end;
      SetLength(RLL, high(rll)+2);
      RLL[High(RLL)].RunStart := FragmentStart;
      RLL[High(RLL)].RunLength := FragmentLength;
  end;
end; // of PROCEDURE

 Procedure CMOptimizeFragments(var CoverageMap:TCoverageMap);
  var
   index:integer;
   FragmentTest, lowest:integer;
  begin
// Find the lowest fragment count in the table
   Lowest := 9999;
   for index := 0 to high(CoverageMap) do
    begin
     FragmentTest := CMRFragmentCount(CoverageMap[index]);
     If FragmentTest < lowest then
      lowest := FragmentTest;
    end;
// Remove all with counts higher than that
   for index := 0 to high(CoverageMap) do
    begin
     FragmentTest := CMRFragmentCount(CoverageMap[index]);
     If FragmentTest > lowest then
      CoverageMap[index].deletionFlag := true;
    end;
// Vacuum the table
   CoverageMapVacuum(CoverageMap);
  end; // of PROCEDURE

 Function CoverageMatrixBuild(const SrcLang:Tlang; const TokenList:TTokenList;
                         const DestLang:Tlang; var CoverageMap:TCoverageMap):Integer;
 // loop through every possible token combination ("remix")
 // Then, for each one, do the same for the non-included elements.
 // The idea is, if we have a partial translation, we want to check the
 // rest of the string for combinations to achieve maximum coverage.
 // This is more difficult to code than it sounds.
 // We need to maintain a map of the translation coverage
   Var
    RC:integer;
    MergeOK:boolean;
    MasterRemixList:TRemixList;
    RemixRow:Integer;
//    CoverageMap : TCoverageMap;
    Index, Index2:integer;
    RemixHigh:integer;
    Block:UTF8String;                                                           //@002=
    Coverage:Boolean;
    MergedRow:TCoverageMapRow;
    Modified:Boolean;
    finalMixList:TRemixList;

    // Optimize coverage map by culling less than optimal entries
     procedure OptCoverage;
     var
       CoverageTest: integer;
       CoverageMax: integer;
       Index:integer;
     begin
           CoverageMax := 0;
           // Find the best coverage first.
           For Index := 0 to High(CoverageMap) do
             begin
               CoverageTest := CoverageMapRowCoverageGet(CoverageMap[Index]);
               If CoverageTest > CoverageMax Then
                 CoverageMax := CoverageTest;
             end;
       // Delete the entries with worst coverage than our best
           For Index := 0 to High(CoverageMap) do
             begin
               CoverageTest := CoverageMapRowCoverageGet(CoverageMap[Index]);
       // If the coverage is less than our best
               If CoverageTest < CoverageMax Then
       // Mark it for deletion
                 CoverageMap[Index].DeletionFlag := true;
             end;
       // Clean up deleted entries
           CoverageMapVacuum(CoverageMap);
     end;

procedure CMSingleWordFill;
     var
      row:integer;
      token:integer;
     begin
       // Bug Fix: Since the all 0 rows were removed in prior passes, it could
       // happen that we get here with NO data at all.  In that case, we have
       // to insert a blank row.
       IF High(CoverageMap) < 0 then
         begin
                 SetLength(CoverageMap, high(coveragemap)+2);
                 SetLength(CoverageMap[High(CoverageMap)].bitmap, TokenList.count);
                 CoverageMapRowInit(CoverageMap);
         end;

       // Check each row of the coverage map
        For Row := 0 to High(CoverageMap) do
       // Check each token on the row
         For token := 0 to high(CoverageMap[Row].bitmap) do
       // If it has a 0, it's because no two-token or greater lookup filled it, so
          if CoverageMap[Row].bitmap[Token] = 0 then
       // we check is this token by itsef is covered.
           Case TranslateCoverage(SrcLang,
                                              TokenList.Strings[Token],
                                              DestLang, mdAuto) of
       // if it is, we add a fragment to it at that position (inplace edit)
            TRUE: CoverageMap[Row].bitmap[Token] :=
       // Need to make the fragment number greater than any already existing so it gets
       // decoded as a separate fragment later.
                            CMRFragmentCount(CoverageMap[Row]) + 1;
           end;  // of CASE
     end;

   Begin
     // get RemixList for the
     RC := TokenRemix(SrcLang, TokenList, MasterRemixList);
     ASSERT(RC=0, 'TransTokenRemix.CoverageMatrix: TokenRemix failed.');
     // Loop through this list, convert each remix into a string
     // and then build a first pass coverage map.
     RemixHigh := high(MasterRemixList);

     For RemixRow := 0 to RemixHigh do
// Optimization: Skip all entries of only a single token, as there is only one
// way to fit them in later, so we can add them when we're done without
// needlessly making the coverage map table larger than it needs to be.
      IF MasterRemixList[RemixRow].RunLength > 1 then
       begin
       // Check the actual coverage
         Block := RemixRow2String(SrcLang,
                                  MasterRemixList[RemixRow]);

         Coverage := TranslateCoverage(SrcLang,
                                       block,
                                       DestLang, mdAuto);
         Case Coverage of

       // If there is coverage, mark it up
           true: begin
                  // Add a new line to the coverage map
                  SetLength(CoverageMap, high(coveragemap)+2);
                  SetLength(CoverageMap[High(CoverageMap)].bitmap, TokenList.count);
// Don't use append routine because we need to set the length of the first entry
//                  CoverageMapRowAppend(CoverageMap);


                  // Set all the entries in the bitmap to 0
//                  For index := 0 to high(CoverageMap[high(coveragemap)].bitmap) do
//                     CoverageMap[high(coveragemap)].bitmap[index] := 0;
                  CoverageMapRowInit(CoverageMap);

                  // Convert RLL encoding into bitmap
                  With MasterRemixList[RemixRow] do
                    For index := RunStart to (RunStart+RunLength) - 1 do
                      CoverageMap[high(coveragemap)].bitmap[Index] := 1;
                 end;
           false:; // nothing to do, we don't want a useless all 0 entry
       end; // of CASE
       end; // of FOR
// We now have our base level coverage map, it is a bitmap of the tokens
// with 1 where each can has been translated already, and 0 where not.

// The next thing we need to do is for each contiguous block of 0s (untranslated
// text), we need to run it through a Remix function again and repeat the
// process.

// In truth, all combinations have already been tried, so all the information we
// need is already in the bitmap, in other rows.
repeat
  Modified := false;
      For Index := 0 to High(CoverageMap) do
       For Index2 := 0 to High(CoverageMap) do
// Avoid wasting time trying to merge a row with itself
        if index <> index2 then
         begin
         // we want:
         // rows with no collissions (non-zero entries in the same spot)
         // but also with non-zeros in the spots we have zeros

           MergeOK := CoverageMapRowMerge(CoverageMap[Index],
                                          CoverageMap[Index2],
                                          MergedRow);
           If MergeOK then
             begin
               Modified := true;
               CoverageMap[Index].DeletionFlag := true;
               CoverageMapRowAppend(CoverageMap);
//               SetLength(CoverageMap, high(coveragemap)+2);
//               SetLEngth(CoverageMap[high(coveragemap)].bitmap, TokenList.count);
               CoverageMap[High(CoverageMap)].bitmap := MergedRow.bitmap;
               // We should set some kind of flag for deletion for the original
               // record so that we can remove it after the loop
             end;
         end; // of FOR
// Delete entries marked for deletion
     CoverageMapVacuum(CoverageMap);
 // keep going until we can't find any better matches
    until modified = false;  // Are we done?
 // Done combination search

 // We now have non-normalized duplicate entries in the array, though
 // example, the following two are the same:
 // 11120
 // 22210

 // we built the coverage map ignoring single word translations, so we need to
 // add them in now.  Since there is only one way each one can fit in, it can
 // be done in a single pass, without the need to examine possible combinations.

   CMSingleWordFill;
   // now we need a way to score the results based on:
     // 1. The most complete translation (least number of 0s)
     // 2. The lowest number of fragments (the lower the highest non-zero
     //    number, the better).

 // test for best coverage and remove all rows with worse than that
    OptCoverage;
    CMOptimizeFragments(CoverageMap);
// At this point we can still have duplicates

     // Once we have a set of scores, then we need to convert the bitmaps into
     // RLL encoding so we can figure out which tokens we need, request the
     // strings, and then retrieve the translations.
//    CMR2RLL(CoverageMap[0], FinalMixList);

   end; // of FUNCTION


end.  // of UNIT

