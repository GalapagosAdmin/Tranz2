unit tdbLegacy;

// Copyright 2009-2012 Noah SILVA
// Change Log
//@001 2011.01.12 Noah SILVA - Added Pattern mode for most functions.
//@002 2011.01.12 Noah SILVA - Modified to use dynamic config directory for DB
//                           - Added more code for Pattern Matching
//@003 2011.??.?? Noah SILVA - Added Pattern unit
//@004 2011.09.06 Noah SILVA - Added Debug Server functionality
//@005 2011.09.10 Noah SILVA - More Debug statements, pattern coding
//@006 2011.09.18 Noah SILVA - Segment Abbreviation db function
//@007 2011.09.18 Noah SILVA - Segment Delimiter db function
//@008 2011.09.19 Noah SILVA - String -> UTF8String
//@009 2011.09.19 Noah SILVA - Glossary Functions
//@010 2011.09.20 Noah SILVA - Parameterization of SQL queries
//                           - Additional Debug statements
//                           - Fix glossary lookups
//                           - Function GetMainTableName
//@011 2011.09.24 Noah SILVA - TextCheck function
//                           - Return Code Constants
//                           - Coverage/TranslateTest Auto mode now use glossary
//                           - Code clean-up and bug fixes for TranslateText
//                           - DBSelfCheck Bug-Fixes
//@012 2011.10.07 Noah SILVA - Added exception support
//@013 2011.11.16 Noah Silva + Adding Project List Support
//                             ResourceString conversions
//@014 2011.11.17 Noah Silva = Use Constants for some Return Codes
//                           - Old code clean-up
//@015 2011.11.18 Noah SILVA + Added Project Entry Details Reading Function
//@016 2011.11.20 Noah SILVA + New resource strings
//@017 2011.11.21 Noah Silva + Reflect additional Project Properties
//@018 2011.11.24 Noah SILVA + New resource string
//                           + Decode function
//@019 2011.11.25 Noah Silva = Bug fix on Windows
//                           = Unicode fixes
//                           + Implementation of FileBlockCRAdd
//@020 2011.11.26 Noah Silva + Begin implementation of FileBlockCRGetList
//@021 2011.11.27 Noah Silva = Bug fixes for HashCRGet
//@022 2011.11.28 Noah Silva + Added depreciated notation where appropriate
//@023 2011.11.30 Noah SILVA = BugFix: Allow zero entries for AbbrGet
//@024 2011.12.05 Noah SILVA = Paramaterization of SQL in various functions
//                           = Rename Generic Database Functions
//                           = Other slight clean-ups
//@025 2011.12.12 Noah SILVA = Decode 5 Selector overload
//@026 2012.01.12 Noah SILVA = Fix to HashCRDelete
//@027 2012.01.15 Noah SILVA + Added LangCodeListGet and SelectXFromY
//@028 2012.01.30 Noah SILVA + Added Segment Statistics Functions
//@029 2012.01.31 Noah SILVA = Speed improvement to Segment Stat Functions
//                           = Update to HashTextAdd
//@030 2012.02.06 Noah SILVA = TextSearch
//@031 2012.02.21 Noah SILVA = Upgrade to TextHashDelete and related functions
//                           = Moved legacy compatibility functions to separate
//                             legacy unit
//                           = HashCRDelete modernization
// -------- tdbLegacy-Specific entries from here --------
//@032 2012.02.24 Noah SILVA + Imported TextAdd compatibility (and other)
//                             overload versions from TransDB


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

// Returns a translated version of the fragment specified in SrcText
// This is low level and does no mix/match, etc.
// Pattern level, searches for the actual pattern itself in the DB
// (i.e. this does no placeholder replacement or searching)

// Added to tdbLegacy                                                           //@032+
  Function TranslatePattern(Const SrcLang:TLang; Const SrcText:UTF8String;      //@008=
                            Const DestLang:TLang):UTF8String;                   //@008=


// Returns a translated version of the fragment specified in SrcText
// This is low level and does no mix/match, etc.
// Obsolete - This one is always fragment mode
  Function TranslateText(const SrcLang:TLang; Const SrcText:UTF8String;         //@008=
                         const DestLang:TLang):UTF8String;                      //@008=
  // Searches Pattern Table
  // Retrieves the second part of a text pair given the following input:
  // Lang1 = Source Language
  // Text  = Source Text
  // Lang2 = The desired destination language

  // Depreciated FRAGMENT LEVEL
  Function TranslateCoverage(const SrcLang:TLang; Const SrcText:UTF8String;     //@008=
                             const DestLang:TLang):Boolean;
  // Depreciated PATTERN LEVEL
  Function PatternCoverage(const SrcLang:TLang; Const SrcText:UTF8String;       //@008=
                             const DestLang:TLang):Boolean;                     //@003+
  // Returns TRUE if there is a translation available for the source text,
  // FALSE if no translation is available for the language pair given.
  // This is meant to be used if a lot of coverage checks need to be done,
  // but not necessarily actual retrieval of the destination text.  (Faster than
  // TranslateText.)
  // Older compatibility versions
  Function TextPairAdd(const lang1:TLang; Const text1:UTF8String;               //@008=
                       const lang2:TLang; Const text2:UTF8String):integer;      //@008=
  Function TextPairAdd(const lang1:TLang; Const text1:UTF8String;               //@001+008=
                       const lang2:TLang; Const text2:UTF8String;               //@001+008=
                       const Pattern_Mode:Boolean):Integer;  Overload;          //@001+

  // Does a hash cross-reference search
  // Lang1 = the language of the string you are looking up (Source language)
  // Hash1 = the hash of the string you are looking up  (Source hash)
  // Lang2 = the language you would like to translate into
  // Result (hash2) = the key to look up the result you want as a string
    // Fragment level (depreciated);
  Function HashCRGet(const SrcLang:TLang; const Hash1:THash;
                     const DestLang:TLang):THash;
  // Pattern Level
//  Function HashCRGet_pattern(const SrcLang:TLang; const Hash1:THash;          //@001+009-
//                     const DestLang:TLang):string;                            //@001+009-

// Adds a new Hash cross-reference when given the following paramaters
// Lang1 = Source Language
// Hash1 = Hash of string in source language (string to translate)
// Lang2 = Destination Language
// Hash2 = Hash of string in destination language
// Returns 0 if no error, or -1 in case of an error.
// depreciated compatibility version
Function HashCRAdd(const lang1:TLang; const Hash1:THash;
                   const lang2:TLang; const Hash2:THash):integer;
Function HashCRAdd(const lang1:TLang; const Hash1:THash;                      //@001+
                   const lang2:TLang; const Hash2:THash;                      //@001+
                   const pattern_mode:Boolean):integer; overload;             //@001+

// Same as HashCRAdd, except that this one calculates the hashes for you
Function BlockCRAdd(const SrcLang:TLang;  const SrcBlock:UTF8String;          //@008=
                    const DestLang:TLang; const DestBlock:UTF8String):integer;//@008=
Function BlockCRAdd(const SrcLang:TLang;  const SrcBlock:UTF8String;          //@001+008=
                    const DestLang:TLang; const DestBlock:UTF8String;         //@001+008=
                    const pattern_mode:boolean):integer;  overload;           //@001+

// Same as HashCRDelete, except that this one calculates the hashes for you
Function BlockCRDelete(const SrcLang:TLang;  const SrcBlock:UTF8String;       //@008=
                       const DestLang:TLang):integer;
Function BlockCRDelete(const SrcLang:TLang;  const SrcBlock:UTF8String;       //@001+008=
                       const DestLang:TLang; const pattern_mode:Boolean)      //@001+
                        :integer;  Overload;                                  //@001+

// Looks up a string in the sentence table from the hash
// This can be used for testing with the source language (hash1), or
// for looking looking up the string for hash2 after retrieving it.
// This doesn't do the cross reference.
// To put it another way, it reverses the hash function.
// Function HashTextGet(const Hash:THash):UTF8String; overload;                //@008=009-
// Function HashTextGet_Pattern(const Hash:THash):UTF8String; overload;        //@001+008=009-
Function HashTextGet(const lang:Tlang; const Hash:THash):
                                                         UTF8String; overload;//@008=

// Adds a new entry (block) to the sentence table using the specified text,
// and it's language and hash key
// Returns 0 if no error, or -1 in case of an error.
// Older compatibility versions
Function HashTextAdd(Const Hash:THash; Const Lang:TLang;
                     Const Text:UTF8String):integer;                          //@008=
Function HashTextAdd(Const Hash:THash; Const Lang:TLang;                      //@001+
                     Const text:UTF8String; Const Pattern_mode:Boolean)       //@001+008=
                        :integer; Overload;                                   //@001+

// Deletes a block from the sentence (or other, as appropriate) table
// Input:
// Lang = Language code of the entry to delete
// Hash = Hash of the string (block) to delete
// Returns 0 of no error, or non-zero in case of an error
Function HashTextDelete(Const Lang:TLang; Const Hash:THash):integer;
Function HashTextDelete(Const Lang:TLang; Const Hash:THash;                   //@001+
                    Const Pattern_Mode:Boolean):integer; overload;            //@001+

// This is just a routine to check whether a block exists in the sentence table
// mainly for debugging use.
// Obsolete versions
Function HashTextCheck(Const Lang:TLang; Const Hash:THash):integer;          //@009+
Function HashPatternCheck(Const Lang:TLang; Const Hash:THash):integer;       //@001

// Adds a new entry, automatically calculating its hash
// Returns 0 if no error, or -1 in case of an error.
// Obsolete versions
Function TextAdd(Const lang:Tlang; Const Text:UTF8String):Integer;            //@008=
//  Function TextAdd_Pattern(const lang:Tlang; const text:string):integer;

// TextCheck checks whether a text block exists in the primary table or not
// No Cross-Reference check is done, so dest. language isn't important.
// Returns TRUE on success, FALSE if no match is found.

// Deletes an existing entry, automatically calculating its hash
// Returns 0 if no error, or non-zero in case of error.
// Obsolete versions
Function TextDelete(const lang:TLang; Const Text:UTF8String):integer;         //@008=
Function TextDelete(const lang:TLang; Const Text:UTF8String;                  //@001+008=
                               const pattern_mode:Boolean):integer; Overload; //@001+

// Returns the list of projects
//  Function ProjectListGet:TStringList; //(Const Username:UTF8String)          //@013+

// Same as TextPairAdd, but removes an entry instead of adding it.
 Function TextPairDelete(Const lang1:TLang; Const text1:UTF8String;            //@008=
                         Const lang2:TLang; Const text2:UTF8String):Integer;   //@008=



implementation

Function TextPairAdd(Const Lang1:TLang; Const Text1:UTF8String;               //@001+008=
                     Const lang2:TLang; Const text2:UTF8String):Integer;      //@001+008=
  deprecated 'Use mode paramater overload of TextPairAdd instead';            //@022+
Begin                                                                         //@001+
 // Call original function in Fragment Mode                                   //@001+
  Result := TextPairAdd(lang1, text1, lang2, text2, False);                   //@001+
end;                                                                          //@001+

Function HashCRAdd(Const Lang1:TLang; Const Hash1:THash;                        //@001+
                   Const Lang2:TLang; Const Hash2:THash):Integer;               //@001+
// compatibility version for fragments
  deprecated 'Use mode paramater overload of HashCRAdd instead';                //@022+
  begin
    // call original function in non-pattern (fragment) mode
    Result := HashCRAdd(Lang1, Hash1, Lang2, Hash2, False);
  end;

Function BlockCRAdd(Const SrcLang:TLang;  Const SrcBlock:UTF8String;            //@001+008=
                    Const DestLang:TLang; Const DestBlock:UTF8String):Integer;  //@001+008=
// This version is for compatibility.
  deprecated 'Use mode paramater overload of BlockCRAdd instead';               //@022+
begin                                                                           //@001+
// call the flexible version in non-pattern (fragment mode)
  result :=  BlockCRAdd(SrcLang, SrcBlock,                                      //@001+
                      DestLang, DestBlock, False);                              //@001+
end;                                                                            //@001+

// Compatibility Version
Function BlockCRDelete(Const SrcLang:TLang; Const SrcBlock:UTF8String;          //@001+008=
                       Const DestLang:TLang):Integer;                           //@001+
  deprecated 'Use mode paramater overload of BlockCRDelete instead';            //@022+
 Begin                                                                          //@001+
 // Call original function in non-pattern (Fragment) mode.                      //@001+
  Result := BlockCRDelete(SrcLang, SrcBlock, DestLang, False);                  //@001+
 end;                                                                           //@001+

Function BlockCRDelete(Const SrcLang:TLang; Const SrcBlock:UTF8String;          //@008=
                       Const DestLang:TLang; Const Pattern_Mode:Boolean):Integer;
  deprecated 'Use mode paramater overload of BlockCRDelete instead';            //@022+
 var
   SrcHash:THash;
 begin
     SrcHash := String2Hash(SrcBlock);
     Result := HashCRDelete(SrcLang, SrcHash, DestLang, Pattern_Mode);          //@001+
 end;

// Deletes a cross-reference Entry
// Legacy versions
Function HashCRDelete(const SrcLang:TLang; const SrcHash:THash;
                      const DestLang:TLang):integer;
Function HashCRDelete(const SrcLang:TLang; const SrcHash:THash;                 //@001+
                      const DestLang:TLang; const Pattern_Mode:Boolean)         //@001+
                      :integer;  Overload;                                      //@001+

// Compatibility Version
Function HashCRDelete(Const SrcLang:TLang; Const SrcHash:THash;
                      Const DestLang:TLang):Integer;
  deprecated 'Use mode paramater overload of HashCRDelete instead';           //@022+
Begin
  // Call original function in fragment mode
  Result := HashCRDelete(SrcLang, SrcHash, DestLang, False);
end;

// Depreciated Compatibility Version
Function TextDelete(Const Lang:TLang; Const Text:UTF8String):Integer;           //@001+008=
 deprecated 'Use mode paramater overload of TextDelete instead';                //@022+
 begin                                                                          //@001+
   Result := TextDelete(Lang, Text, False);                                     //@001+
 end;                                                                           //@001+

Function TextDelete(Const Lang:TLang; Const Text:UTF8String;                    //@008=
                                         Const pattern_mode:Boolean):Integer;   //@001+
  deprecated 'Use mode paramater overload of TextDelete instead';               //@022+
 var
   Hash:THash;
 begin
  hash := string2hash(text);
  Result := HashTextDelete(Lang, Hash, pattern_mode);                           //@001+
 end;

// Depreciated Compatibility Version
Function HashTextDelete(Const Lang:TLang; Const Hash:THash):Integer;            //@001+
  deprecated 'Use mode paramater overload of HashTextDelete instead';           //@022+
  Begin                                                                         //@001+
   // Call Flexible version in non-pattern (fragment) mode.                     //@001+
    Result := HashTextDelete(Lang, Hash, False);                                //@001+
  End;                                                                          //@001+

Function HashTextDelete(Const Lang:TLang; Const Hash:THash;                     //@031=
                        Const Pattern_Mode:Boolean):Integer;
Begin
 Case Pattern_Mode of
  True: Result := HashTextDelete(Lang, Hash, mdPattern);
  False: Result := HashTextDelete(Lang, Hash, mdFragment);
 end;
end;

(*                                                                              //@009-
Function HashCRGet_pattern(const SrcLang:TLang; const Hash1:THash;             //@001+
                   const DestLang:TLang):string;
Var
  QueryString:String;
Begin
     QueryString := 'select hash2 from patterncr where hash1 = "';
     QueryString := QueryString + hash1 + '"';
// Theoretically we shouldn't even need these, but they could speed up the
// search, and reduce the risk of hash collissions
     QueryString := QueryString + ' and lang1 = "';
     QueryString := QueryString + SrcLang + '"';
     QueryString := QueryString + ' and lang2 = "';
     QueryString := QueryString + Destlang + '"';
     query.SQL.Text := QueryString;
 TRY
     query.open;
 // this should only return one result
 //    while not query.EOF do
     if not query.EOF then
       begin
        result := query.FieldByName('hash2').AsString;
//        query.Next;
      end;
      query.Close;
   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        RESULT := '';
      end;
End;
End;
      *)


      Function TextPairDelete(Const Lang1:TLang; Const Text1:UTF8String;        //@031=
                        Const lang2:TLang; Const Text2:UTF8String):Integer;     //@031=
        begin
         Result := TextPairDelete(Lang1, Text1, Lang1, Text2, mdFragment);
        end;

      Function HashCRDelete(Const SrcLang:TLang; Const SrcHash:THash;           //@031+
                            Const DestLang:TLang;
                            Const Pattern_Mode:Boolean):Integer;
        begin
           case Pattern_Mode of
             False: Result := HashCRDelete(SrcLang, SrcHash, DestLang, mdFragment);
             Frue:  Result := HashCRDelete(SrcLang, SrcHash, DestLang, mdPattern);
           end;
         end;
        end;
                                                                                //@032+
Function TextAdd(Const Lang:TLang; Const Text:UTF8String):Integer;              //@001+008=
  deprecated 'Use mode paramater overload of TextAdd instead';                  //@022+
 begin
   Result := TextAdd(lang, Text, mdFragment);
 end;

// Compatible, Currently only works for exact-match fragments                   //@032+
 Function TranslateText(const SrcLang:TLang; Const SrcText:UTF8String;          //@008=
                        const DestLang:TLang):UTF8String;                       //@008=
 deprecated 'Use mode paramater overload of TranslateText instead';             //@022+
  Begin
    Result := TranslateText(SrcLang, SrcText, DestLang, mdFragment);
  End;

// Moved to tdbLegacy                                                           //@032+
   Function TranslatePattern(const SrcLang:TLang; Const SrcText:UTF8String;     //@001+008=
                         const DestLang:TLang):UTF8String;                      //@008=
  // Assumes SrcText is already stripped of placeholders
  deprecated 'Depreciated function? use TranslateText???';                      //@022+
  var
   SrcHash:THash;
   DestHash:THash;
  begin
   SrcHash := string2hash(SrcText);
//   DestHash := HashCRGet_Pattern(SrcLang, SrcHash, DestLang);                 //@009-
   DestHash := HashCRGet(SrcLang, SrcHash, DestLang, mdPattern);                //@009+
   If DestHash <> '' then
     Result := HashTextGet(SrcLang, DestHash, mdPattern);
  end; // of FUNCTION

// Moved to tdbLegacy                                                           //@032+
Function TextPairAdd(Const Lang1:TLang; Const Text1:UTF8String;                 //@009+
                       Const Lang2:TLang; Const Text2:UTF8String;               //@009+
                       Const Pattern_Mode:Boolean):Integer;  Overload;          //@009+
  deprecated 'Use mode paramater overload of TextPairAdd instead';              //@022+
  begin
    Case Pattern_mode of
     False: Result := TextPairAdd(lang1, text1, lang2, text2, mdFragment);
     True: Result := TextPairAdd(lang1, text1, lang2, text2, mdPattern);
    end;
  end;

// Moved to tdbLegacy                                                           //@032+
Function HashCRAdd(Const lang1:TLang; Const Hash1:THash;                        //@009+
                   Const lang2:TLang; Const Hash2:THash;                        //@009+
                   Const Pattern_Mode:Boolean):Integer; Overload;               //@009+
  deprecated 'Use mode paramater overload of HashCRAdd instead';                //@022+
 begin
   Case Pattern_Mode of
    False: Result := HashCRAdd(Lang1, Hash1, Lang2, Hash2, mdFragment);
    True:  Result := HashCRAdd(Lang1, Hash1, Lang2, Hash2, mdPattern);
   end;
 end;

// Moved to tdbLegacy                                                           //@032+
Function BlockCRAdd(Const SrcLang:TLang;  Const SrcBlock:UTF8String;            //@009+
                     Const DestLang:TLang; Const DestBlock:UTF8String;          //@009+
                     Const Pattern_Mode:Boolean):Integer;  Overload;            //@009+010=
  deprecated 'Use mode paramater overload of BlockCRAdd instead';               //@022+
 begin
   Case Pattern_Mode of
    True : Result := BlockCrAdd(SrcLang, SrcBlock,                              //@010=
                               DestLang, DestBlock, mdPattern);                 //@010=
    False: Result := BlockCrAdd(SrcLang, SrcBlock,                              //@010=
                               DestLang, DestBlock, mdFragment);                //@010=
   end;
 end;

// Moved to tdbLegacy                                                           //@032+
// Depreciated compatibility override
Function HashTextAdd(Const Hash:THash; Const Lang:TLang;                        //@001+
                                                 Const Text:UTF8String):Integer;//@001+008=
  deprecated 'Use mode paramater overload of HashTextAdd instead';              //@022+
  begin                                                                         //@001+
    Result := HashTextAdd(Hash, Lang, Text, mdFragment);                        //@001+
  end;                                                                          //@001+

// Moved to tdbLegacy                                                           //@032+
// Depreciated compatibility override
Function HashTextAdd(Const Hash:THash; Const Lang:TLang;                        //@001+
                     Const Text:UTF8String; Const Pattern_Mode:Boolean):Integer;//@001+008=
  deprecated 'Use mode paramater overload of HashTextAdd instead';              //@022+
  begin                                                                         //@001+
   Case Pattern_Mode of                                                         //@001+
      False:Result := HashTextAdd(Hash, Lang, Text, mdFragment);                //@001+
      True:Result := HashTextAdd(Hash, Lang, Text, mdPattern);                  //@001+
    end; // of CASE                                                             //@001+
  end; // of FUNCTION                                                           //@001+

// Moved to tdbLegacy                                                           //@032+
Function HashTextGet(Const Lang:Tlang; Const Hash:THash)
                                               :UTF8String; Overload;           //@008=
  deprecated 'Use mode paramater overload of HashTextGet instead';              //@022+
 begin
   Result := HashTextGet(Lang, Hash, mdFragment);                               //@032=
 end;

// Moved to tdbLegacy                                                           //@032+
// Depreciated Compatibility version
Function HashTextCheck(Const Lang:TLang; Const Hash:THash):Integer;           //@009+
  deprecated 'Use mode paramater overload of HashTextCheck instead';          //@022+
  Begin
    Result := HashTextCheck(Lang, Hash, mdFragment);
  end;

// Moved to tdbLegacy                                                           //@032+
// Depreciated, use HashTextCheck(,,mdPattern) instead
Function HashPatternCheck(Const lang:TLang; Const Hash:THash):Integer;        //@001+
  deprecated 'Use HashTextCheck with with mode = mdPattern instead';          //@022+
//   Var                                                                        //@010-
//    QueryString:String;                                                       //@010-
 begin
  Result := HashTextCheck(Lang, Hash, mdPattern);                             //@010+
  (*                                                                          //@010-
  QueryString := 'select text from pattern where hash = "';
   QueryString := QueryString + hash + '"';
   query.SQL.Text := QueryString;
   TRY
   query.open;
// this should only return one result
//    while not query.EOF do
   if not query.EOF then
     result := 0
    else
     result := 1;
    query.Close;
 except
  on E: Exception do
    begin
      LastErrorMessage := e.message;
      RESULT := 2;
    end;
End;  *)
 end;

// Moved to tdbLegacy                                                           //@032+
Function HashCRGet(Const SrcLang:TLang; Const Hash1:THash;                      //@009+
                   Const DestLang:TLang):THash;
  deprecated 'Use mode paramater overload of HashCRGet instead';                //@022+
Begin
 Result := HashCRGet(SrcLang, Hash1, DestLang, mdFragment);
end;


end.

