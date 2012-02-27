unit TransTrain;
// Translation Trainer / Part of the Tranz2 Suite
// Copyright 2009-2012 Noah SILVA / Galapagos Software
{$mode objfpc}{$H+}
//000 2009.??.?? Noah SILVA  Initial Version

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


  Function PairSplit(Const Src1:UTF8String; const Dest1:UTF8String;
                     Const Src2:UTF8String; const Dest2:UTF8String;
                     var SrcBlock1:UTF8String; var DestBlock1:UTF8String;
                     var SrcBlock2:UTF8String; var DestBlock2:UTF8String;
                     var SrcBlock3:UTF8String; var DestBlock3:UTF8String):Integer;
 // Takes two pairs of translated text and returns translated block pairs and RC.
 // Input:
 // Src1: Source Text 1
 // Dest1: Translated version of Src1
 // Src2: Source Text 2
 // Dest2: Translated version of Src2
 // Output:
 // SrcBlock1, DestBlock1: Version of first block in both source and dest lang.
 // SrcBLock2, DestBlock2: Source and Dest. language versions of second block.
 // SrcBlock3, DestBlock3: Soutce and Dest. language versions of third block.
 // Return code: 0 if no error, -1 if unable to split.

  Function BlockSplit(Const Src1:UTF8String; const Src2:UTF8String;
                     Var Block1:UTF8String; var Block2:UTF8String;
                     var Block3:UTF8String):integer;
  // takes a pair of translated text, and returns two strings,
  // the first is the common portion between the two
  // the second is the second part of the first input string.
  // Example:
  // Src1: This is a pen.
  // Src2: This is a donkey.
  // Block1: This is a
  // Block2: pen.
  // Block3: donkey.

implementation

Function BlockSplitFwd(Const Src1:UTF8String; const Src2:UTF8String;
                     Var Block1:UTF8String; var Block2:UTF8String;
                     var Block3:UTF8String):Integer;
// Searches for a pattern like:
// Input:
// Src1: AAAAAAAABBBBB
// Src2: AAAAAAAACCCCC
// Output:
// Block1: AAAAAAAA
// Block2: BBBBB
// BLock3: CCCCC
var
 SrcBlock1End:integer;
 SrcBlock2Start:Integer;
 SrcBlock2Length:Integer;
 SrcBlock3Length:Integer;
 SrcMaxLen:Integer;
begin
// find the length of the shorter of the two strings, as that is the maximum
// safe length to do.
  If Length(Src1) < Length(Src2) then
   SrcMaxLen := Length(Src1)
  else
   SrcMaxLen := Length(Src2);

  SrcBlock1End := 0;
// find out how many chars from the beginning are the same.
  while (SrcBlock1End < SrcMaxLen)
    and (Src1[SrcBlock1End] = Src2[SrcBlock1End]) do
      inc(SrcBlock1End);
  Dec(SrcBlock1End);
  If SrcBlock1End > 0 then
    Begin
// First half of both sentences
      Block1 := Trim(copy(Src1, 1, SrcBlock1End));
// Second half of first sentence
      SrcBlock2Start := SrcBlock1End + 1;
      SrcBlock2Length := Length(Src1) - SrcBlock1End;
      Block2 := Trim(Copy(Src1, SrcBlock2Start, SrcBlock2Length));
// Second half of second sentence
      SrcBlock3Length := Length(Src2) - SrcBlock1End;
      Block3 := Trim(Copy(Src2, SrcBlock2Start, SrcBlock3Length));
      Result := 0;
    end
  else
    result := -1;
 end;  // of PROCEDURE


Function BlockSplitRev(Const Src1:UTF8String; const Src2:UTF8String;
                     Var Block1:UTF8String; var Block2:UTF8String;
                     var Block3:UTF8String):Integer;
// Searches for a pattern like:
// Input:
// Src1: BBBBBBBBAAAAA
// Src2: CCCCCCCCAAAAA
// Output:
// Block1: AAAAA
// Block2: BBBBBBBB
// BLock3: CCCCCCCC
var
 SrcBlock1Beg:Integer;
 SrcBlock1End:Integer;
 SrcBlock2Beg:Integer;
 SrcBlock2End:Integer;
 SrcBlock3Beg:Integer;

 SrcBlock1Length:Integer;
 SrcBlock2Length:Integer;
 SrcBlock3Length:Integer;
 SrcMaxLen:Integer;
begin
// find the length of the shorter of the two strings, as that is the maximum
// safe length to do.
  If Length(Src1) < Length(Src2) then
   SrcMaxLen := Length(Src1)
  else
   SrcMaxLen := Length(Src2);

  SrcBlock1End := Length(Src1);
  SrcBlock1Beg := SrcBlock1End;
  SrcBlock2End := Length(Src2);
  SrcBlock2Beg := SrcBlock2End;
// find out how many chars from the beginning are the same.

  while ((SrcBlock1Beg > 0) and (SrcBlock2Beg > 0))
    and (Src1[SrcBlock1Beg] = Src2[SrcBlock2Beg]) do
      begin
       Dec(SrcBlock1Beg);
       Dec(SrcBlock2Beg);
      end;
  Inc(SrcBlock1Beg);
  Inc(SrcBlock2Beg);
// If we got anything
  If (SrcBlock1Beg < SrcBlock1End) then
    Begin
// Note I am kind-of doing a bad thing here, as I am using the variables for
// both the source and destination blocks, to the order of the statements below
// matters.
// Second half of both sentences
      SrcBlock1Length := SrcBlock1End - SrcBlock1Beg + 1;
      Block1 := Trim(copy(Src1, SrcBlock1Beg, SrcBlock1Length));
// First half of second sentence
      SrcBlock3Beg := 1;
      SrcBlock3Length := SrcBlock2Beg - 1;
      Block2 := Trim(Copy(Src2, SrcBlock3Beg, SrcBlock3Length));
// First half of first sentence
      SrcBlock2Length := SrcBlock1Beg - 1;
      SrcBlock2Beg := 1;
      Block3 := Trim(Copy(Src1, SrcBlock2Beg, SrcBlock2Length));
      Result := 0;
    end
  else
    result := -1;
 end;  // of PROCEDURE

Function BlockSplit(Const Src1:UTF8String; const Src2:UTF8String;
                     Var Block1:UTF8String; var Block2:UTF8String;
                     Var Block3:UTF8String):Integer;
  begin
    BlockSplit := BlockSplitFwd(Src1, Src2, Block1, Block2, Block3);
  end;

  Function PairSplit(Const Src1:UTF8String; const Dest1:UTF8String;
                     Const Src2:UTF8String; const Dest2:UTF8String;
                     var SrcBlock1:UTF8String; var DestBlock1:UTF8String;
                     var SrcBlock2:UTF8String; var DestBlock2:UTF8String;
                     var SrcBlock3:UTF8String; var DestBlock3:UTF8String):Integer;
  Begin
    // Try a forward split on the source language...
    Result := BlockSplitFwd(Src1, Src2, SrcBlock1, SrcBlock2, SrcBlock3);
    // if result is non-zero, there is an error, so exit.
    If result = 0 then // Forward Split worked, try on dest. lang. as well...
      Result := BlockSplitFwd(Dest1, Dest2, DestBlock1, DestBlock2, DestBlock3);

    // If it's still no good, try to run it in reverse.
    If not (result = 0) then
     begin  // Reverse Block Run on the source...
      Result := BlockSplitRev(Src1, Src2, SrcBlock1, SrcBlock2, SrcBlock3);
      If result = 0 then // Reverse split worked, try on dest. lang. as well.
        Result := BlockSplitRev(Dest1, Dest2, DestBlock1, DestBlock2, DestBlock3);
     end; // end of IF (Reverse split attempt)

  end;  // of FUNCTION

end.  // of UNIT

