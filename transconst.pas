unit TransConst;
// Translation Unit Constants  / Part of the Tranz2 Suite
// Copyright 2009-2012 Noah SILVA
//* Change Log *
//@003 2011.01.13 Noah SILVA - Added new mode constants
//@004 2011.01.19 Noah SILVA - Added Abbreviation List
//@005 2011.09.14 Noah SILVA - Added Function Result Constants
//@005 2011.10.07 Noah SILVA - Added exceptions
//@006 2011.11.16 Noah SILVA - Added Color tag constants for Status Bar
//@007 2011.11.17 Noah SILVA - New Return-Code Constants, exceptions, types
//@008 2011.11.18 Noah SILVA - Record Types for Project, etc.
//@009 2011.11.24 Noah SILVA - New Exception type
//@010 2012.01.30 Noah SILVA - Added Single Quote EN
//@011 2012.01.31 Noah SILVA - Added FancyQuote Constants
//@012 2012.02.06 Noah SILVA - Added further Unicode Constants
//@013 2012.04.18 Noah SILVA - Hiragana/Katakana Match codes
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
 TLang = String[2];
 THash = String[64]; // I think 32 is enough?                                   //@007+
 TTokenList = TStringList;
 TMode = Integer;                                                               //@007+


 Type
   TAbbr_List=Array of UnicodeString;                                           //@004+

 Type                                                                           //@005+
   EUnsupportedLanguage = Class(Exception);
   EInvalidMode = Class(Exception);
   EDatabaseError = Class(Exception);
   ENoSuchProject = Class(Exception);                                           //@007+
   ENoSuchDiskFile = Class(Exception);                                          //@007+
   ENoSuchDocument = Class(Exception);                                          //@009+

   TProjectInfo = Record                                                        //@008+
      ProjectHash:THash;
      ProjectName:UTF8String;
      ProjectDesc:UTF8String;
      BasePath:UTF8String;
      SrcLang:TLang;
      DestLang:TLang;
   end;

   TDocumentInfo = Record                                                       //@008+
      ProjectHash:THash;
      FileName:UTF8String;
      FileDesc:UTF8String;
      SrcLang:TLang;
      DestLang:TLang;
   end;



 Const
  DelimEN = ' ';
  SpaceEN = ' ';
  PeriodEN = '.';
  CommaEN = ',';
  QuestionEN = '?';
  SingleQuoteEn = '''';                                                         //@010+

 mdFragment = 0;                                                                //@001+
 mdPattern  = 1;                                                                //@001+
 mdSentence = 2;                                                                //@001+
 mdWord     = 3;                                                                //@003+
 mdPhrase   = 4;                                                                //@003+
 mdAuto     = 5;                                                                //@003+

 rcNoProblem     = 0;                                                           //@004+
 rcNoResults     = 1;                                                           //@004+
 rcDBException   = 2;                                                           //@004+
 rcZeroRowsAffected = 999;                                                      //@007+
 rcUnknownError  = -1;                                                          //@007+
 rcZeroRows      = 3;                                                           //@007+
 rcNoSuchDiskFile = -4;                                                         //@007+

   // GUI Message Types                                                         //@006+
   tgNormal  = 0;                                                               //@006+
   tgError   = 1;                                                               //@006+
   tgWarning = 2;                                                               //@006+
   tgSuccess = 3;                                                               //@006+
   tgInfo    = 4;                                                               //@006+
   tgStale = 100;

   // StatusBar Panels
   sbpIcon = 0;
   sbpMain = 1;

 CONST                                                                          //@012+
   // Unicode / ASCII Tab
   UnicodeTab = UnicodeString(#$0009);
   // Unicode / ASCII LF
   UnicodeLF = UnicodeString(#$000A);
   // Unicode / ASCII CR
   UnicodeCR = UnicodeString(#$000D);


   // Unicode Info Sep #4 / ASCII File Seperator
   InfoSep4 = UnicodeString(#$001C);
   // Unicode Info Sep #3 / ASCII ???
   InfoSep3 = UnicodeString(#$001D);
   // Unicode Info Sep #2 / ASCII Record Seperator
   InfoSep2 = UnicodeString(#$001E);
   // Unicode Info Sep #1 / ASCII Unit Seperator
   InfoSep1 = UnicodeString(#$001F);

   // Unicode Full Stop / ASCII Period (European/English period)
   UnicodePeriod = UnicodeString(#$002E);

   // Unicode Private Use #1
   PrivateUse1 = UnicodeString(#$0091);
   // Unicode Private Use #2
   PrivateUse2 = UnicodeString(#$0092);


   // Unicode Horiz. Sep.
   HorizontalSep = UnicodeString(#$200A);
   // Zero Width Space - Used to separate words in Japanese, etc.
   ZeroWidthSpace = UnicodeString(#$200B);
   // Zero Width Non-Joiner
   ZeroWidthNonJoiner = UnicodeString(#$200C);
   // Zero Width Joiner
   ZeroWidthJoiner = UnicodeString(#$200D);
   // Word Joiner - Should be used to separate words (Zero Width)
   WordJoiner = UnicodeString(#$2060);

   // Hiragana
   HiraganaLow = UnicodeString(#$3040);
   HiraganaMatchLow = UnicodeString(#$3041); // Small a                         //@013+
   HiraganaHigh = UnicodeString(#$309F);

   // Main Zenkaku (Full Width) Katakana Range
   // Also includes Nakaguro, Chouon, koto
   // Includes Kanji iteration, which could cause us problems
   KatakanaLow = UnicodeString(#$30A0);
   KatakanaMatchLow = UnicodeString(#$30A1); // Small a                         //@013+
   KatakanaHigh = UnicodeString(#$30FF);
   // [Zankaku] Katakana Phonetic Extensions Range
   KatakanaExtLow = UnicodeString(#$31F0);
   KatakanaExtHigh = UnicodeString(#$31FF);

   // Hankaku (Half-Width) Japanese Period
   HKJapanesePeriod = UnicodeString(#$FF61);
   // Hankaku (Half Width) Katakana range
   HKKatakanaLow = UnicodeString(#$FF65);
   HKKatakanaHigh = UnicodeString(#$FF9F);
   // Kana Supplement is basically not supported by anyone yet, so ignoring
   // Kana Supplement is U+1B000 ... U+1B0FF

 CONST                                                                          //@011+
(*
 Name                                     CodePoint     UTF-8 sequence
 ----                                     ---------     --------------
 LEFT SINGLE QUOTATION MARK               U+2018        0xE2 0x80 0x98
 RIGHT SINGLE QUOTATION MARK              U+2019        0xE2 0x80 0x99
 SINGLE LOW-9 QUOTATION MARK              U+201A        0xE2 0x80 0x9A
 SINGLE HIGH-REVERSED-9 QUOTATION MARK    U+201B        0xE2 0x80 0x9B
 LEFT DOUBLE QUOTATION MARK               U+201C        0xE2 0x80 0x9C
 RIGHT DOUBLE QUOTATION MARK              U+201D        0xE2 0x80 0x9D
 DOUBLE LOW-9 QUOTATION MARK              U+201E        0xE2 0x80 0x9E
 DOUBLE HIGH-REVERSED-9 QUOTATION MARK    U+201F        0xE2 0x80 0x9F
*)

   LeftFancyQuote = UnicodeString(#$201C);
   RightFancyQuote = UnicodeString(#$201D);
   LeftFancySingleQuote = UnicodeString(#$2018);
   RightFancySingleQuote = UnicodeString(#$2019);




implementation

end.

