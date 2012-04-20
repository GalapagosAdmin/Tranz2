Unit TransToken;
// Translation Tokenizer Routines  / Part of the Tranz2 Suite
// Separates strings (containing sentences, etc.) into tokens (Words, etc.)

// Copyright 2009-2012 Noah SILVA / Galapagos Software

{$mode objfpc}{$H+}
//@000 2009.??.?? Noah SILVA  Initial Version
//@001 2011.09.19 Noah SILVA  Modifications to use UnicodeString
//@002 2011.09.24 Noah SILVA  Beginnings of proper Japanese support
//@003 2011.09.26 Noah SILVA  Support for FPC 2.4.2/Win32
//@004 2011.10.07 Noah SILVA  Exception Support
//@005 2012.01.27 Noah SILVA  Starting to consider DB Free option
//                           TokenStatArray
//@006 2012.01.30 Noah SILVA  Bug fix with Short-Circuit boolean evaluation
//                           Start of Delimiter support for parsing English Tokens
//                           StringList2TokenStatArray
//@007 2012.01.31 Noah SILVA  Fine-Tuning of token delimiters, filtering.
//@008 2012.02.06 Noah SILVA  RomajiCheck Function
//                           NumberCheck Function
//                           KatakanaCheck Function
//                           HiraganaCheck Function
//@009 2012.02.19 Noah SILVA  Compile on Laz 0.9.31/Darwin
//@010 2012.04.18 Noah SILVA = Moved HiraganaCheck to Interface
//                           + Added KanaCheck
//                           + Added KanaMatch

//To-Do:
// Functions for:
// Zenkaku to Hankaku (Romaji, Numbers)
// Hankaku to Zenkaku (Romaji, Numbers)
// Normalizing Kana to pre-composed, zenkaku form
// Hiragana to Katakana
// Katakana to Hiragana
// Romaji to Hiragana, Katakana
// Hiragana, Katakana to Romaji

interface

uses
  Classes, SysUtils, TransConst;

Type
  // TokenStatArray: Used to hold a list of Tokens and their associated frequencies
  TSingleTokenStatRec=record                                                    //@005+
    Token:UTF8String;                                                           //@005+
    Count:Longint;                                                              //@005+
  end;                                                                          //@005+
  TTokenStatArray=Array of TSingleTokenStatRec;                                 //@005+

Function Tokenize(Const Lang:Tlang; Const Sentence:UTF8string):TTokenList;
Function TokenizeUTF16(Const Lang:Tlang;
                                      Const Sentence:UnicodeString):TTokenList; //@001+@003=

// StringList to TokenStatArray
Procedure SL2TS(Const SL:TStringList; Var TS:TTokenStatArray);                  //@006+

// Convert oddball Unicode "fancy" quotes into standard quotes
Function NormalizeQuotes(Text:UTF8String):UTF8String;                           //@006+


// converts a stringlist of tokens into a list where each entry has N
// consecutive tokens.  Each entry is overlapped by one, such that the number
// of entries in the result is n less than the number of input entries.
Procedure Tuple_1_to_n(Const Lang:TLang;
                       Const InputArray:TStringList;                            //@007+
                       Var OutputArray:TStringList;
                       Const n:Integer);
// Separates a sentence string into individual words (and other tokens).
Function IsTokenDelim(Const C:Char; Const Lang:TLang):Boolean;                  //@007+

// Returns true if the entire string consists only of Hiragana
Function HiraganaCheck(Const Str:UnicodeString):Boolean;                        //@008+
// Returns true is the entire string consists of Romaji (ASCII)
Function RomajiCheck(Const Str:UTF8String):Boolean;                             //@008+

// Returns true if all kana match in pronunciation
Function KanaMatch(Const Str1, Str2:UnicodeString):Boolean;                     //@010+
Function KanaMatch(Const Str1, Str2:UTF8String):Boolean;                        //@010+

implementation

Uses
{$IFDEF fpc}
  LCLProc,
  dbugintf                                                                      //@001+
{$ENDIF}
{$DEFINE USEDB}                                                                 //@005+
{$IFDEF USEDB}                                                                  //@005+
  , transdb                                                                     //@002+
{$ENDIF}                                                                        //@005+
  , TransTokenRemix                                                             //@007+
 //  , LazUTF8    // in lclproc?                                                //@008+@009-
  ;

Procedure AddTokenJA(Const Token:UnicodeString; Var WIP:TTokenList);            //@001+
//Procedure AddTokenJA(const token:UTF8string; var wip:TTokenList);             //@001-
 begin
// Normally we don't need to insert spaces or anything, we can just add it
   // We convert it to a Unicode for easier processing, but
   // then we have the problem that there seems to be no TWideStringList or
   // TUnicodeStringList in FPC (Hence we convert if to UTF8 here for easier
   // storage.)
   wip.Add(UTF16toUTF8(Token));                                                 //@001=
 end;

Procedure AddTokenEN(const token:UTF8String; var wip:TTokenList);
// Adds a token to the current Work-in-progress TokenList
// Attempts to perform de-tokenization prior to addition

// some duplication of TransTokenRemix.TokenConcat
 Var
  Len:Integer;
  begin
    Len := length(token);
// CR and LF should just be ignored here, they were parsed in TokenizeEN
    If (token[len] = Chr($0d)) or (token[len] = Chr($0a)) then                  //@007+
      begin                                                                     //@007+
   //   wip.Add(space(1));      // ignore
      end                                                                       //@007+
    // Logic below for period should be made generic to handle all punctuation
    else If token[len] = periodEN then          // If the last char we were going
     begin                                     // to copy was a period, then
      wip.Add(copy(Token,1,len-1));              // add token without period, and
      wip.Add(periodEN);                         // add period as a separate token
     end // of IF period
    else if token[len] = commaEN then                // If the last char we were going
     begin                                     // to copy was a period, then
      wip.Add(copy(Token,1,len-1));              // add token without period, and
      wip.Add(commaEN);                         // add period as a separate token
     end // of IF period
    else if token[len] = QuestionEN then                // If the last char we were going
     begin                                     // to copy was a period, then
      wip.Add(copy(Token,1,len-1));              // add token without period, and
      wip.Add(QuestionEN);                       // add period as a separate token
     end // of IF period
// Could handle single quotes here
    else if token[1] = SingleQuoteEN then      // Handle Single quotes before a word //@006+
     begin
      wip.Add(SingleQuoteEN);
       if token[len] = SingleQuoteEN then      // See if there was a quote on the right, too
        begin                                  // There was left AND right quote
         wip.Add(copy(Token,2,len-2));
         wip.Add(SingleQuoteEN);
        end
     else                                      // Only left Quote
      wip.Add(copy(Token,2,len-1));              // add token without period, and
     end // of IF period
    else                                       // No period, add as-is
     wip.Add(Token);
   end;  // of PROCEDURE



{$CODEPAGE UTF8}
Function NormalizeQuotesUTF16(Text:UnicodeString):UnicodeString;                //@006+
  var
   i:Integer;
  begin
    Result := Text;
    for i := 1 to Length(Text) do
      if (Result[i] = LeftFancyQuote) then Result[i] := '"'
      else
        if (Result[i] = RightFancyQuote) then Result[i] := '"'
    else
      if (Result[i] = LeftFancySingleQuote) then Result[i] := ''''              //@007+
      else
        if (Result[i] = RightFancySingleQuote) then Result[i] := ''''           //@007+

  end;

Function NormalizeQuotes(Text:UTF8String):UTF8String;                           //@006+
  begin
    Result := UTF16toUTF8(NormalizeQuotesUTF16(UTF8toUTF16(Text)));
  end;

Function IsTokenDelim(Const C:Char; Const Lang:TLang):Boolean;                  //@007+
  var
   Delims:SET of CHAR;                                                          //@006+
  begin
    // Delims should be loaded from DB, only supports ASCII as of now
    Delims := [' ','"', '(', ')', ':', ';', '.', CommaEN, '[', ']', '/',        //@006+
               '@', '~', '?', '!', Chr($0d), Chr($0a)];//'“','”'                //@007+
    Result :=  (C IN Delims);
  end;

// Checks in input string to see if it is entirely composed of Romaji
// Should use LazUTF8 routines or convert the string to UTF16 prior to
// processing, but in this special case, it would seem not to be necessary.
// Reason: UTF8 bytes 2-4 guaranteed not to be ASCII
Function RomajiCheck(Const Str:UTF8String):Boolean;                             //@008+
  var
   p:integer;
   Romaji:Set of Char  = ['A'..'Z', 'a'..'z'];
  begin
   p := 1;
   Result := True;
   while P <= length(Str) do
     begin
       If NOT (Str[p] in Romaji) then
         begin
           result := False;
           exit;
         end;
       inc(p);
     end;

  end;

Function KatakanaCheck(Const Str:UnicodeString):Boolean;                        //@008+
  var
   p:integer;
   is_kana:Boolean;
  begin
   p := 1;
   Result := True;
   while P <= length(Str) do
     begin
//       is_kana := False;
       // First Chance, Main Zenkaku (Full Width) kana range
       If ((str[p] >= KatakanaLow) and (str[p] <= KatakanaHigh))
       // Katakana Extensions Block
         or ((str[p] >= KatakanaExtLow) and (str[p] <= KatakanaExtHigh))
       // Hankaku (Half-Width)
         or ((str[p] >= HKKatakanaLow) and (str[p] <= HKKatakanaHigh))
       then
         is_kana := True
       else
         is_kana := False;
       // Then check extended range, hankaku, etc.
       If NOT is_kana then
         begin
           result := False;
           exit;
         end;
       inc(p);
     end;

  end;


Function HiraganaCheck(Const Str:UnicodeString):Boolean;                        //@008+
  var
   p:integer;
   is_kana:Boolean;
  begin
   p := 1;
   Result := True;
   while P <= length(Str) do
     begin
       // Hiragana range
       is_kana := ((str[p] >= HiraganaLow) and (str[p] <= HiraganaHigh));
       If NOT is_kana then
         begin
           result := False;
           exit;
         end;
       inc(p);
     end;

  end;

Function KanaCheck(Const Str:UnicodeString):Boolean;                            //@009+
  begin
    Result := HiraganaCheck(Str) OR KatakanaCheck(Str);
  end;

// only handles hankaku (ASCII) numbers for now.
Function NumberCheck(Const Str:UTF8String):Boolean;                             //@008+
  var
   p:integer;
   Romaji:Set of Char  = ['0', '1'..'9'];
  begin
   p := 1;
   Result := True;
   while P <= length(Str) do
     begin
       If NOT (Str[p] in Romaji) then
         begin
           result := False;
           exit;
         end;
       inc(p);
     end;

  end;

// Separates on space/Punctuation boundaries
// (punctuation is further handled in AddTokenEN)
{ TODO 1 -oshiruba -ckaizen : Need to utilize abbreviations in SEGMENT_ABBR }
{ TODO 1 -oshiruba -ckaizen : Guess at unknown abbreviations using embedded period }
{ TODO 1 -oshiruba -ckaizen : Need to handle numbers, times, dates }

Function TokenizeEN(const sentence:UTF8String):TTokenList;
 Const
   Lang='EN'; // Since this is TokenizeEN                                       //@007+
 var
 // temp:string;                                                                //@001-
  pos:integer;    // temporary position pointer
  len:integer;    // total length of the string
  start:integer;  // start of subsection to copy
  run : integer;  // length of subsection to copy
 begin
  Result := TStringlist.create;
  Result.Clear;
  pos := 1;
  len := length(sentence);
  While  pos < len do
    begin
//    skip spaces
     {$B-}                                                                      //@006+
//     while (pos < len) and (sentence[pos] = space(1)) do                      //@006=-
      while (pos < len) and IsTokenDelim(sentence[pos], Lang) do                //@006+@007=
        begin                                                                   //@006+
          // If it wasn't a space, add it to the token list
          If (sentence[pos] <> Space(1)) Then                                   //@006+
            AddTokenEN(copy(sentence, pos, 1), Result);                         //@006+
          // Either way, move on
          inc(pos);
        end;                                                                    //@006+
//     This is the first non-space/non-delim
      start := pos;
//    Search for the first space
      run := 0;
//      while (pos <= len) and (sentence[pos] <> space(1)) do                   //@006=-
       while (pos <= len) and (NOT IsTokenDelim(sentence[pos], Lang)) do        //@006+@007=
        begin
          inc(pos);
          inc(run);
        end;
// Make sure we didn't run out of string
      if run <> 0 then
        AddTokenEN(copy(sentence, start, run), Result);

    end; // of WHILE
 // result.Add('Testing');
 end; // of FUNCTION

Function TokenizeJA(Const Sentence:UnicodeString):TTokenList;                   //@001=
// Japanese doesn't normally use spaces to separate words, so this one is
// a bit more of a challenge than English.
// This would probably be a useful starting point for Korean and Chinese as well.
 var
  SentenceLen  : Integer;
  CurrStartPos : Integer;
  CurrTokenLen : Integer;                                                       //@002+
  CurrTknLenTry : Integer;                                                      //@002+
  CurrTknTry : UTF8String;                                                      //@002+
  CurrTknTry16 : UnicodeString;                                                 //@008+
 // ws, wsr:UnicodeString; //WideString;                                        //@001=
 Begin
   Result := TStringlist.create;
   Result.Clear;

   SentenceLen := Length(Sentence);

  // WS := sentence;                                                            //@001-
   CurrStartPos := 1;
   while (CurrStartPos <= SentenceLen) do                                       //@001=
    begin
    // For now, just put each character as a separate token
      CurrTokenLen := 1;
    // plus we keep expanding our search and seeing if we can find any more tokens
    // via dictionary look-up
      For CurrTknLenTry := (CurrTokenLen+1) to (SentenceLen-CurrStartPos+1) do
       begin
         CurrTknTry16 :=                                                        //@002+//@008=
                             Copy(Sentence, CurrStartPos, CurrTknLenTry);       //@002+
         CurrTknTry := UTF16toUTF8(CurrTknTry16);                               //@008+
         // Since Japanese often contains English Acronyms, etc., we might also
         // do well to check for English words as well, but we will handle that
         // separately with unknown Katakana.
         IF TextCheck('JA', CurrTknTry, mdWord) then                            //@002+
                             CurrTokenLen := CurrTknLenTry                      //@002+
         else                                                                   //@008+
         IF RomajiCheck(currTknTry) then                                        //@008+
                             CurrTokenLen := CurrTknLenTry                      //@008+
         else
         IF NumberCheck(currTknTry) then                                        //@008+
                             CurrTokenLen := CurrTknLenTry                      //@008+
         else                                                                   //@008+
         IF KatakanaCheck(currTknTry16) then                                    //@008+
                             CurrTokenLen := CurrTknLenTry;                     //@008+
//         else
         // This is a dangerous game, because hiragana words can be combined,
         // and particles also exist, only inserted here for testing.
//         IF HiraganaCheck(currTknTry16) then                                    //@008+
//                             CurrTokenLen := CurrTknLenTry;                     //@008+

       end;                                                                     //@002+

     // Add whatever we have so far, which could be a single character, or
     // more (if we got a hit in the dictionary lookup).
     AddTokenJA(Copy(sentence, CurrStartPos, CurrTokenLen), Result);            //@001=002=
     // Advance to the next position
     inc(CurrStartPos, CurrTokenLen);                                           //@001=002=
    end;



// places to tokenize
// 1. When we see a special particle like wo
// 2. When we see a katakana to kanji or katakana to hiragana bountry (usually)
// 3. When we see a katakana or hiragana to kanji boundry. (reverse not true)
// 4. Special cases
// 5. Dictionary search
 end; // of FUNCTION

Function TokenizeJA(Const Sentence:UTF8String):TTokenList;                      //@001+
  begin
    Result := TokenizeJA(UTF8toUTF16(Sentence));
  end;

 Function TokenizeUTF16(Const Lang:TLang; Const Sentence:UnicodeString):TTokenList;  //@001=@003=
 begin
 // We can always check the second character later if there are collissions
   try                                                                          //@001+
     CASE Lang[1] of
       'E': result := TokenizeEN(sentence);  // This should Auto-Convert to UTF8
       'J': result := TokenizeJA(sentence);
     ELSE
       SendDebug('Tokenizer Error: Unsupported Language' + lang);               //@001=
       Raise EUnsupportedLanguage.Create ('Unsupported Language:' + Lang);      //@004+
       assert( 1 = 0, 'TransToken.Tokenize: Unsupported Language' );
     END; // of CASE
   except                                                                       //@001+
     SendDebug('TransToken.Tokenize: Error during processing.');                //@001+
   end;                                                                         //@001+
 end; // of FUNCTION


 Function Tokenize(Const Lang:Tlang; Const Sentence:UTF8String):TTokenList;     //@001+
                                                                   Overload;    //@001+
 //  var
 //   Sentence_utf16:UTF16String;
   Begin
     // Convert UTF16 input into UTF8
  //   sentence_utf8:=UTF16toUTF8(sentence);
     // Output is still a UTF8 StringList
     Result := TokenizeUTF16(Lang, UTF8toUTF16(Sentence));                      //@003=
   end;

  Procedure SL2TS(Const SL:TStringList; Var TS:TTokenStatArray);                //@006+
     var
       i, o:Longint;  // Input, Output row pointers
       InitialSize:Longint; // Initial Size of Array
       LastWord:UTF8String;
     begin
       LastWord := '';
//       InitialSize := Length(ts);
       // Initial safe guess at the size needed
       SetLength(ts, sl.Count); // -1?
       // Set up output pointer (Where next new entry should go)
       o := 0;
       For i := 0 to (sl.Count - 1) do
         begin
           If Lowercase(SL.Strings[i]) = LastWord then   // StringList must be sorted for
                                                         // This check to work!
             begin // Duplicate Item
               if o > 0 then  // Don't ask...
                 TS[o-1].count := TS[o-1].count + 1;
             end
           else
             begin // New Item
               TS[o].Token := LowerCase(SL.Strings[i]);
               TS[o].count := 1;  // init count at 1
               inc(o);
             end;
           LastWord := LowerCase(SL.Strings[i]);
         end;
       // Fix to exact size
       //if o < 0 then
//         showmessage(inttostr(o));
       SetLength(ts, o);
     end;


  Procedure Tuple_1_to_n(Const Lang:TLang;
                         const InputArray:TStringList;                          //@007+
                         var OutputArray:TStringList;
                         const n:Integer);
  // Example Input:
  // [This][is][a][dog][.]
  // Example Output: (N=2)
  // [This is][is a][a dog][dog.]
  // only works for N=2 at the moment
  var
    Token:Integer; // Input token (entry) #
    tmpString:UTF8String;
    j:Integer; // Current token of output entry between 0..n-1
  begin
  //  If n = 1 then // Special Case
  //    begin
  //      OutputArray := InputArray;
  //      Exit;
  //    end;

  //  SetLength(OutputArray, (Length(InputArray)-n+1));
    OutPutArray.Clear;
    for Token := 0 to (InputArray.Count - n) do
      begin
        tmpString := '';
        for j := 0 to n-1 do
         tmpString := TokenConcat(Lang, tmpString, InputArray.Strings[Token+j]);
        OutPutArray.Append(tmpString);
      end;
  end;

  // Returns the offset of the given kana within the range
  // Currentlty the result is quite off, but it is stable, so it works as
  // expected (probably unsigned integer should be used
  Function KanaOffset(Const C:WideChar):LongWord;                                   //@010+
    begin
      If HiraganaCheck(c) then
        Result := LongWord(c) - LongWord(HiraganaMatchLow)
      else if KatakanaCheck(c) then
        Result := LongWord(c) - LongWord(KatakanaMatchLow)
      else
        raise Exception.create('Invalid Kana Input.');
    end;

  // Matches two WideChars.  To return true, they must meet one of the following
  // conditions:
  // 1. They must be the same Unicode Character
  // 2. They must both either hiragana or katakana, and represent the
  //    corrosponding char in the other set.

  // We don't handle
  // 1. Cho-on (Long vowels created by Japanese dash char)
  // 2. Extended katakana (Since there are no equivalent Hirakana)
  // 3. Upper/Lower case romaji equivilence
  // 4. Matching Kanji to kana
  // 5. Non-precomposed chars (i.e. separate dakuon char)
  Function KanaMatchChar(Const c1, c2:WideChar):Boolean;                        //@010+
    var
      Offset1 : LongInt;
      Offset2 : LongInt;
    begin
      // First check for exact match
      If C1 = C2 then Exit(True);
      // No exact match, Check to make sure they are both Kana
      If  Not ( KanaCheck(C1) and KanaCheck(C2) ) then
        // If not, exit with mismatch.
        Exit(False);
      // We Know: Not same char, both kana
      Offset1 := KanaOffset(c1);
      Offset2 := KanaOffset(c1);
      // See if they have the same offset in their range
      If Offset1 = Offset2 then
        Result := True
     else
        Result := False;
    end;

  Function KanaMatch(Const Str1, Str2:UnicodeString):Boolean;                   //@010+
    var
      l1, l2:Longint; // Length of String
      p:Longint; // Current Position in the string
    begin
      l1 := Length(Str1);
      l2 := Length(Str2);
      // Length must match
      If l1 <> l2 then Exit(False);
      // Length matches, check chars
      p := 1;
      While p <= l1 do
       begin
         If not KanaMatchChar(Str1[p], Str2[p]) then
           Exit(False);
         Inc(p);
       end;  // of WHILE
      // If we made it this far, then everything matches
      Result := True;
    end; // of FUNCTION

  Function KanaMatch(Const Str1, Str2:UTF8String):Boolean;                      //@010+
    begin
      Result := KanaMatch(UTF8toUTF16(Str2), UTF8toUTF16(Str2));
    end;

end.

