Unit transdb;
// Translation Database Routines
// Functions that Read/Write a SQLite database containing translation phrases
// The functions here require exact matches.  Higher level functions can be
// found in other libraries.

// The DB schema contains two main tables:
// REQUIRES ZEOS component library for SQLite

// SENTENCE : Contains a string, a language code representing the language of
//            the string, and an MD5SUM hash of the string.  The language string
//            isn't really needed, but is included in case of hash collissions.
// SENTENCECR : Cross-Reference for the Sentence table. This table contains a
//              source hash/language code pair, and a destination hash/language
//              code pair.  Again the source language code isn't strictly needed,
//              however the destination language code is important since there
//              might be multiple destination languages available.
// Language code is ISO-639-1 as per
// http://www.sil.org/iso639-3/codes.asp?order=639_1&letter=%25

// PATTERN & PATTERNCR function similarly to SENTENCE and SENTENCECR at the
// database level.
//
// Note:
//            The purpose of the
//            hash is to provide a fixed length identifier that is easy to
//            calculate and always consistant.  (an alternative would be a ROWID,
//            but then we wouldn't be able to determine the ID only from the text
//            for lookups later.
//
// Basic Strategy:
//  To look up a block of text, a hash is calculated from the text, and this source
// hash is looked up in the cross-reference table.  The destination hash for the
// desired language is retrieved, and then the destination text block is looked up
// by the destination hash and retrieved from the text
//
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
//@032 2012.02.24 Noah SILVA = Added exception handling to finalization to
//                             prevent crash due to Zeos 7.0.0 Alpha bug
//                             Moved more deprecated functions to tdbLegacy

// 2011.10.09 Some functions to add for translation project support

// Function LangCodeValidate(Const LC:TLang):Boolean;
// Validate a given language code as supported/unsupported

//2011.11.24 To-Implement functionality
{ TODO 1 -oshiruba -ci18n : Implement Text_Get, Text_Set }
// Uses TEXT table to set/get string information for codes
// Function Text_Set(Const Table_Name:UTF8String; Const FullKey:UTF8String;
//                    Const Lang:TLang; Const Text:UTF8String):Integer;
// Function Text_Get(Const Table_Name:UTF8String; Const FullKey:UTF8String;
//                    Const Lang:TLang;):UTF8String;

//2011.11.25 To-Implement functionality
{ TODO 1 -oshiruba -cfunctionality : Implement read/write to DocBlockCR table }
// Adds a cross-reference entry for the specified Document (Specified by the file
// name, including full path), and the block of text to add.
// Function FileBlockCRDelete(Const FullPath:UTF8String; Const Block:UTF8String):Integer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransConst;

// Returns a translated version of the fragment specified in SrcText
// This is low level and does no mix/match, etc.
// Pattern level, searches for the actual pattern itself in the DB
// (i.e. this does no placeholder replacement or searching)
// Extended version
  Function TranslateText(Const SrcLang:TLang; Const SrcText:UTF8String;         //@002+008=
                         Const DestLang:TLang;                                  //@002+
                         Const lookup_Mode:TMode):UTF8String; Overload;         //@002+008=
  // Searches Pattern Table
  // Retrieves the second part of a text pair given the following input:
  // Lang1 = Source Language
  // Text  = Source Text
  // Lang2 = The desired destination language

  // Extended version
  Function TranslateCoverage(const SrcLang:TLang; Const SrcText:UTF8String;     //@003+008=
                             const DestLang:TLang;                              //@003+
                             Const Mode:TMode):Boolean; Overload;               //@003+
  // Returns TRUE if there is a translation available for the source text,
  // FALSE if no translation is available for the language pair given.
  // This is meant to be used if a lot of coverage checks need to be done,
  // but not necessarily actual retrieval of the destination text.  (Faster than
  // TranslateText.)
  // Newer flexible version
  Function TextPairAdd(Const Lang1:TLang; Const Text1:UTF8String;               //@009+
                       Const Lang2:TLang; Const Text2:UTF8String;               //@009+
                       Const Mode:TMode):Integer;  Overload;                    //@009+
  // Adds a text pair to the sentence and sentencecr, including forward and
  // reverse cross-references.
  // Lang1 = Source text language
  // Text1 = Source Text
  // Lang2 = Destination text language
  // Text2 = Destination Text
  // Returns 0 if no error, or -1 in case of an error.
 // Same as above, but for glossary
 // function below is temporary, use TextPairAdd(,,,,mdWord) instead
 Function TextPairAdd_Glossary(Const lang1:TLang; Const Text1:UTF8String;       //@009+
                               Const lang2:TLang; Const Text2:UTF8String        //@009+
                                  ):Integer;                                    //@009+



 // Same as TextPairAdd, but removes an entry instead of adding it.
  Function TextPairDelete(Const Lang1:TLang; Const text1:UTF8String;            //@008=
                          Const Lang2:TLang; Const text2:UTF8String;            //@008=
                          Const Mode:TMode):Integer;

  // Converts a text string to a 32 byte ASCII representation of the MD5 hash (string)
  // Input: Any string
  // Output: String containing MD5SUM hash
  Function String2Hash(Const Text:UTF8String):THash;                            //@008+@019=

  // Does a test select statement to make sure the DB is working properly
  // Returns 0 if no error, or -1 in case of an error.
  Function DBSelfTest:Integer;

  // Returns the most recent error message as string
  // This is meant to be called after an error occurs in calling one of the
  // other functions
  Function GetLastErrorMessage:UTF8String;                                      //@019=

  // Does a hash cross-reference search
  // Lang1 = the language of the string you are looking up (Source language)
  // Hash1 = the hash of the string you are looking up  (Source hash)
  // Lang2 = the language you would like to translate into
  // Result (hash2) = the key to look up the result you want as a string
  // Fragment, pattern, or word
  Function HashCRGet(Const SrcLang:TLang; Const hash1:THash;                    //@009+
                     Const DestLang:TLang; Const Mode:TMode):THash;             //@009+


  // Adds a new Hash cross-reference when given the following paramaters
  // Lang1 = Source Language
  // Hash1 = Hash of string in source language (string to translate)
  // Lang2 = Destination Language
  // Hash2 = Hash of string in destination language
  // Returns 0 if no error, or -1 in case of an error.
  // Newer  version
  Function HashCRAdd(Const lang1:TLang; Const Hash1:THash;                      //@009+
                     Const lang2:TLang; Const Hash2:THash;                      //@009+
                     Const Mode:TMode):integer; overload;                       //@009+

  // Deletes a cross-reference Entry
  // New Flexible Version
  Function HashCRDelete(Const SrcLang:TLang; Const SrcHash:THash;               //@031+
                        Const DestLang:TLang; Const Mode:TMode)                 //@031+
                        :integer;  Overload;                                    //@031+

  // Same as HashCRAdd, except that this one calculates the hashes for you
  Function BlockCRAdd(const SrcLang:TLang;  const SrcBlock:UTF8String;          //@009+
                      const DestLang:TLang; const DestBlock:UTF8String;         //@009+
                      Const Mode:TMode):integer;  overload;                     //@009+

  // Same as HashCRDelete, except that this one calculates the hashes for you
  Function BlockCRDelete(Const SrcLang:TLang;  Const SrcBlock:UTF8String;       //@009+
                         Const DestLang:TLang; Const Mode:TMode)                //@009+
                          :integer;  Overload;                                  //@009+

  // Looks up a string in the sentence table from the hash
  // This can be used for testing with the source language (hash1), or
  // for looking looking up the string for hash2 after retrieving it.
  // This doesn't do the cross reference.
  // To put it another way, it reverses the hash function.
  Function HashTextGet(const lang:Tlang; const Hash:THash;
                                       Const Mode:TMode):UTF8String; overload;  //@009+


  // Adds a new entry (block) to the sentence table using the specified text,
  // and it's language and hash key
  // Returns 0 if no error, or -1 in case of an error.
  // New flexible version
  Function HashTextAdd(Const Hash:THash; Const Lang:TLang;
                       Const Text:UTF8String; Const Mode:TMode)
                                                             :integer; Overload;//@001+008=


  // Deletes a block from the sentence (or other, as appropriate) table
  // Input:
  // Lang = Language code of the entry to delete
  // Hash = Hash of the string (block) to delete
  // Returns 0 of no error, or non-zero in case of an error
  Function HashTextDelete(Const Lang:TLang; Const Hash:THash;                   //@031+
                      Const Mode:TMode):integer; overload;                      //@031+

  // This is just a routine to check whether a block exists in the sentence table
  // mainly for debugging use.
  // New version
  Function HashTextCheck(Const Lang:TLang; Const Hash:THash;                   //@009=
                                            Const Mode:TMode):integer;        //@009=

  // Adds a new entry, automatically calculating its hash
  // Returns 0 if no error, or -1 in case of an error.
  // New version
  Function TextAdd(Const lang:Tlang; Const Text:UTF8String;                     //@001+008=
                                 Const Mode:TMode):Integer; overload;           //@001+

  // TextCheck checks whether a text block exists in the primary table or not
  // No Cross-Reference check is done, so dest. language isn't important.
  // Returns TRUE on success, FALSE if no match is found.
  Function TextCheck(Const Lang:TLang; Const Text:UTF8String;                   //@011+
                                   Const Mode:TMode):Boolean;

  // Deletes an existing entry, automatically calculating its hash
  // Returns 0 if no error, or non-zero in case of error.
  // New Version
  Function TextDelete(Const lang:TLang; Const Text:UTF8String;                  //@009+
                                 Const Mode:TMode):integer; Overload;           //@009+

  // Populates the passed array with a list of abbreviations that should be
  // considered as exceptions to segment delimiters.
  Function SegmentAbbrGet(Const Lang:TLang; Var Abbr_List:TAbbr_List):Integer;  //@006+
  // Returns a string containing all of the segment delimiters
  Function SegmentDelimGet(Const Lang:TLang):UnicodeString;                     //@007+

  // Returns the list of projects
  Procedure ProjectListGet(Var pl:TStringList); //(Const Username:UTF8String)   //@013+
  // Adds a new project to the project list
  Function ProjectListAdd(Const ProjectName:UTF8String;
                                 Const ProjectDesc:UTF8String;                  //@013+@014=
                                 Const BasePath:UTF8String;                     //@017+
                                 Const SrcLang:TLang;                           //@017+
                                 Const DestLang:Tlang):Integer;                 //@017+

  // Deletes a project from the project list
  Function ProjectListDelete(Const ProjectName:UTF8String):Integer;             //@013+

  // Adds a new file to the given project
  Function ProjectFileAdd(Const ProjectName:UTF8String; FileName:String;
                                 Const FileDesc:UTF8String;
                                 Const SrcLang:TLang;
                                 Const DestLang:TLang):Integer;                 //@013+

  // Returns a list of files for the requested project
  Function ProjectFileListGet(Const ProjectName:UTF8String;                     //@013+
                                 Var FileList:TStringList):Integer;             //@013+

  // Deletes a given file from the given project
  Function ProjectFileDelete(Const ProjectName:UTF8String;
                             Const FileName:UTF8String;
                             Const SrcLang:TLang; Const DestLang:TLang):Integer;//@013+

  // Determines whether or not a project exists in the database
  Function ProjectExists(Const ProjectName:UTF8String):Boolean;                 //@014+

  // Read Project Entry Details and return in a structure                       //@015+
  Function ProjectInfoRead(Const ProjectName:UTF8String):TProjectInfo;          //@015+

  // Checks whether a project exists in the DB -> Exception if not
  Procedure CheckProject(Const ProjectName:UTF8String);                         //@014+

  // Reads information on a particular file registered in the database
  Function FileInfoRead(Const FileName:UTF8String):TDocumentInfo;               //@015+

  Function ProjectInfoReadHash(Const ProjectHash:THash):TProjectInfo;           //@016+

  // Similar to SQL Decode() function
  // Allows one to use CASE with strings
  // 2 selector version
  Function Decode(const inp:string;                                             //@018+
                  const s1:string; const i1:integer;
                  const s2:string; const i2:integer;
                  const default:integer):Integer;
  // 4 selector version
  Function Decode(const inp:string;                                             //@018+
                  const s1:string; const i1:integer;
                  const s2:string; const i2:integer;
                  const s3:string; const i3:integer;
                  const s4:string; const i4:integer;
                  const default:integer):Integer; Overload;

  Function Decode(const inp:string;                                             //@025+
                  const s1:string; const i1:integer;
                  const s2:string; const i2:integer;
                  const s3:string; const i3:integer;
                  const s4:string; const i4:integer;
                  const s5:string; const i5:integer;
                  const default:integer):Integer; Overload;

  Function FileBlockCRAdd(Const FullPath:UTF8String;                            //@019+
                               Const Block:UTF8String):Integer;                 //@019+
  Function FileBlockCRGetList(Const FullPath:UTF8String;                        //@020+
                              Var BlockList:TStrings):Integer;                  //@020+

  // Returns a list of language codes from the database
  Procedure LangCodeListGet(Var LanguageList:TStringList);                      //@027+

  // Updates Statistics for a segment, adding it to the segment
  // table first if required.
  // Inpput Paramaters:
  // Lang = Source Language
  // Text = Surface Token string
  // TknCnt = Number of tokens contained in the text paramater
  // Count = Frequency of the token string observed since last update
  Procedure StatSegAdd(Const Lang:Tlang; Const Text:UTF8String;
                                         Const TknCnt:Integer;
                                         Const Count:Integer);                  //@028+

  // Search for a certain text string, using LIKE
  Function TextSearch(Const Lang:TLang; Const Text:UTF8String;                    //@030+
                                       Const Mode:TMode):UTF8String;              //@030+


ResourceString                                                                  //@013+
  rsZeroEntries      = 'Zero entries';
  rsZeroRowsAffected = '0 rows affected (Should have been 1).';
  rsXRowsAffected    = '%0:s rows affected (should have been 1).';
  rsZeroRowsDeleted  = '0 rows deleted.';
  rsInvalidMode      = 'Invalid mode:';
  rsUnknownDBError   = 'Unknown database error.';
  rsDBCheckFailed    = 'DB self-check failed';
  rsDBNotConnected   = 'Not connected to database.';
  rsDBConnected      = 'Connected successfully to database.';
  rsInitError        = 'Error in initialization:';
  rsException        = 'exception: ';                                           //@014+
  rsXRowsDeleted     = '%0:s rows deleted.';                                    //@014+
  rsNoSuchProject    = 'No such project.';                                      //@014+
  rsNoSuchDiskFile   = 'The requested file could not be located.';              //@014+
  rsDocumentUpdateErr= 'Unable to update document.';                            //@016+
  rsNoSuchDocument   = 'No such document. (For this project).';                 //@018+
  rsSegmentEntryNotExist = 'Segment Entry does not exist (and could not be created)'; //@029+

implementation

uses
     MD5, db, ZConnection, ZDataset,
     pattern,                                                                   //@003+
     dbugintf,                                                                  //@004+
     lclproc,                                                                   //@007+
     DefaultTranslator,                                                         //@013+
     FileUtil;                                                                  //@019+

Var
 Conn: TZConnection;
 Query: TZQuery;
 DataSource : TDataSource;
 LastErrorMessage : String;
 db_path: String;                                                               //@002+
// transaction: TSQLTransaction;    // Not needed for SQLite

Function String2Hash(Const Text:UTF8String):THash;                              //@008=@009=
//VAR output : TMD5Digest;
// The MD5Digest is actually 16 bytes long,
// Which means it should be 32 characters when represented in ASCII
begin
  result := mdprint(md5string(text));
end;

Function GetMainTableName(Const Mode:TMode):String;                             //@010+
  begin
   Case MODE of
      mdFragment : Result := 'sentence';
      mdPattern : Result := 'pattern';
      mdWord: Result := 'glossaries';
      else
        Raise EInvalidMode.Create ('GetMainTableName:' + rsInvalidMode);        //@012+@014=
        Assert(0=1, 'transdb.GetMainTableName:' + rsInvalidMode + IntToStr(Mode)); //@014=
   end;
  end;

Function GetCRTableName(Const Mode:TMode):String;                               //@024+
  begin
   Case MODE of
      mdFragment : Result := 'sentencecr'; //0
      mdPattern :  Result := 'patterncr';   //1
      mdWord:      Result := 'glossariescr';    //3
      else
        Raise EInvalidMode.Create ('GetCRTableName:' + rsInvalidMode);
        Assert(0=1, 'transdb.GetCRTableName:' + rsInvalidMode + IntToStr(Mode));
   end;
  end;

{ TODO 3 -oSHIRUBA -cCode Clean-Up : Redirect this function (Code is redundant) }
Function GlossaryDestGet(Const SrcLang:TLang; Const SrcHash:THash;
                                     Const DestLang:TLang):UTF8String;          //@009+
  Var
    QueryString:String;
Begin
 QueryString := 'select destterm from glossaries where hash = :srchash'         //@024+
// QueryString := 'select destterm from glossaries where hash = "';             //@024-
// QueryString := QueryString + SrcHash + '"';                                  //@024-
// Theoretically we shouldn't even need these, but they could speed up the
// search, and reduce the risk of hash collissions
                 + ' and lang = :lang';                                         //@024+
//     QueryString := QueryString + ' and lang = "';                            //@024-
//     QueryString := QueryString + SrcLang + '"';                              //@024-
     // We'Re not checking the dest lang yet
     //    QueryString := QueryString + ' and lang2 = "';
 //    QueryString := QueryString + Destlang + '"';
     Query.Params.Clear;                                                        //@024+
     Query.SQL.Text := QueryString;
     Query.ParamByName('srchash').AsString := SrcHash;                          //@024+
     Query.ParamByName('lang').AsString := DestLang;                            //@024+
 TRY
     query.open;
 // this should only return one result
 //    while not query.EOF do
     if not query.EOF then
       begin
        result := query.FieldByName('destterm').AsString;
//        query.Next;
       end
        else result := '';
      query.Close;
   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        RESULT := '';
      end;
 End;
End;

  Function TranslateText(Const SrcLang:TLang; Const SrcText:UTF8String;         //@003+008=
                         Const DestLang:TLang;                                  //@003+
                         Const lookup_Mode:TMode):UTF8String; Overload;         //@003+008=
  Var
    SrcHash:THash;
    DestHash:THash;
  Begin
   SrcHash := String2Hash(SrcText);
   CASE lookup_mode of                                                          //@003+
     // Just check sentence table for sentence fragments that match
     mdFragment, mdPattern, mdWord:Begin                                        //@003+@011=
                    DestHash := HashCRGet(SrcLang, SrcHash,
                                          DestLang, lookup_mode);               //@010=@011=
                    If DestHash <> '' then
//                     Result := HashTextGet(SrcLang, DestHash, Lookup_Mode)    //@009=@011-
                      Result := HashTextGet(DestLang, DestHash, Lookup_Mode)    //@011+
                    else                                                        //@011+
                      Result := '';                                             //@011+
                 End;                                                           //@003+
     // Just check pattern table for an exact match, including placeholder
//     mdPattern:Begin                                                          //@003+@011-
//       DestHash := HashCRGet_pattern(SrcLang, SrcHash, DestLang);             //@003+009-@011-
//               DestHash := HashCRGet(SrcLang, SrcHash, DestLang, mdPattern);  /@003+009+@011-
//                    If DestHash <> '' then                                    //@003+@011-
 //                     Result := HashTextGet_pattern(DestHash);                //@003+009-
//                    Result := HashTextGet(SrcLang, DestHash, mdPattern);      //@003+009+@011-
//                End;                                                          //@003+@011-
     //Glossary Mode
//     mdWord:Begin                                                             //@009+@011-
                //  DestHash := string2hash(SrcText);                           //@009+-
//                  Result := HashTextGet_pattern(DestHash, mdPattern);         //@003+009+
//              DestHash := HashCRGet(SrcLang, SrcHash, DestLang, mdWord);      //@010+@011-
//                  If DestHash <> '' then                                      //@003+@011-
//                    Result := HashTextGet(SrcLang, DestHash, mdWord);         //@009+@011-
//                  Result := GlossaryDestGet(SrcLang, SrcHash, DestLang);      //@009+009-
//          End;                                                                //@009+@011-
     // Automatic Mode - High level for application use
     mdAuto:Begin                                                               //@003+
               // Pattern_AutoSearch will make pattern templates and try to fill
               // them automatically.
               Result := TranslateText(SrcLang, SrcText, DestLang, mdFragment);
               // If no exact match is found via fragments, check word glossary
               If result = '' then                                              //@011+
                 Result := TranslateText(SrcLang, SrcText, DestLang, mdWord);   //@011+
               // If no exact match is found via fragments/words, check patterns
               If result = '' then
                 Result := Pattern_AutoSearch(SrcLang, SrcText, DestLang);
            End;                                                                //@003+
     else                                                                       //@003+
      Assert(1=0, 'transdb.TranslateText:'+rsInvalidMode);                      //@003+@011=@014=
   end;                                                                         //@003+
  end; // of FUNCTION




  // new internal version, doesnt know auto type
  Function TranslateCoverage_int(Const SrcLang:TLang; Const SrcText:UTF8String; //@008=
                           Const DestLang:TLang; Const Mode:TMode):Boolean;     //@010=
//  const DestLang:TLang):boolean;                                              //@010-
  // Determine if a fragment translation is available by checking the Hash Cross-
  // reference table.
  Var
   SrcHash:THash;
   DestHash:THash;
  begin
   SrcHash := string2hash(SrcText);
   DestHash := HashCRGet(SrcLang, SrcHash, DestLang, Mode);                     //@010=
   If DestHash <> '' then
     Result := True
   else
     Result := false;
  end; // of FUNCTION

  // compatibility stub
  // OldVersion, Fragment only (Sentence Table)
  Function TranslateCoverage(Const SrcLang:TLang; Const SrcText:UTF8String;     //@010+
                             Const DestLang:TLang):Boolean;
   begin
     Result := TranslateCoverage_Int(SrcLang, SrcText, DestLang, mdFragment);
   end;

  // RAW pattern version
  Function PatternCoverage(Const SrcLang:TLang; Const SrcText:UTF8String;       //@001+008=
                         Const DestLang:TLang):Boolean;
  // Determine if a patterm translation is available by checking the Hash Cross-
  // reference table.
   var
     SrcHash:THash;
     DestHash:THash;
  begin
    SrcHash := string2hash(SrcText);
//    DestHash := HashCRGet_pattern(SrcLang, SrcHash, DestLang);                //@009-
    DestHash := HashCRGet(SrcLang, SrcHash, DestLang, mdPattern);               //@009+
    If DestHash <> '' then
      Result := True
    else
      Result := false;
  end; // of FUNCTION


  Function GlossaryCoverage(Const SrcLang:TLang; Const SrcText:UTF8String;      //@009+
                            Const DestLang:TLang):boolean;
  // Determine if a patterm translation is available by checking the Hash Cross-
  // reference table.
//   var                                                                        //@010-
//    SrcHash:THash;                                                            //@010-
//    DestText:String;                                                          //@010-
  begin
    Result := TranslateCoverage_Int(SrcLang, SrcText, DestLang, mdWord);        //@010+
(*   SrcHash := string2hash(SrcText);                                           //@010-
    DestText := GlossaryDestGet(SrcLang, SrcHash, DestLang);
    If DestText <> '' then
      Result := True
    else
      Result := false; *)
  end; // of FUNCTION

  Function TranslateCoverage(Const SrcLang:TLang; Const SrcText:UTF8String;     //@003+008=
                             Const DestLang:TLang;                              //@003+
                             Const Mode:TMode):Boolean;                         //@003+@010=
  Begin
   Try
    Case mode of
     mdFragment: Result := TranslateCoverage_int(SrcLang, SrcText,
                                                 DestLang, mdFragment);
     mdPattern:  Result := PatternCoverage(SrcLang, SrcText, DestLang);
     mdWord:     Result := TranslateCoverage_Int(SrcLang, SrcText,              //@011+
                                                 DestLang, mdWord);             //@011+
 //    mdWord:     Result := GlossaryCoverage(SrcLang, SrcText, DestLang);      //@009+010-011-
 //     mdWord:     Result := TranslateCoverage(SrcLang, SrcText, DestLang, mdWord);//@009+010=
      mdAuto:Begin
               // It would perhaps be better to try the pattern first, but the
               // lookup is much more performance intensive.
               // try first as a fragment (i.e. translation memory)
               Result := TranslateCoverage(SrcLang, SrcText,
                                           DestLang, mdFragment);
                // If no hits, than try as a word in the general glossary
                If NOT Result then                                               //@011+
                 Result := TranslateCoverage(SrcLang, SrcText,                  //@011+
                                             DestLang, mdWord);                 //@011+
                // If there is still no result, then try a pattern match
                If NOT Result then
 // Searches for Any/all paterns automatically
                   Result := Pattern_Coverage_AutoSearch(SrcLang,
                                                         SrcText, DestLang);
 // Searches for the pattern template only.
 //                 Result := TranslateCoverage(SrcLang, SrcText, DestLang, mdPattern);
             End // of mdAuto
      Else
        Begin
          Assert(1=0, rsInvalidMode);                                           //@014=
          Abort;
        end;
    end; // of CASE
   except
     SendDebug('trans.translateCoverage: '+rsException);                        //@014+
   end;
 End; // of FUNCTION



  { TODO 2 -oSHIRUBA -cCode Clean-Up : Fix this to call TextPairAdd with mdWord }
  Function TextPairAdd_Glossary(Const Lang1:TLang; Const Text1:UTF8String;      //@009+
                                Const Lang2:TLang; Const Text2:UTF8String       //@009+
                                  ):Integer;                                    //@009+
//  const                                                                       //@010-
//    table_name='glossaries';                                                  //@010-
  var
   Hash1:THash;
   QueryString:String;
  Begin
    Result := 0;
    hash1 := String2Hash(Text1);
    Query.Params.Clear;                                                         //@024+
     QueryString := 'INSERT OR REPLACE INTO '
     + GetMainTableName(mdWord)                                                 //@010+
     //+table_name                                                              //@010-
                              + ' ( hash1, lang1, srcterm, lang2, destterm ) '
                 + 'VALUES ( :hash1, :lang1, :text1, :lang2, :text2 )';         //@024+
//     QueryString := QueryString + 'VALUES (';                                 //@024-
//     QueryString := QueryString + '"' + hash1 + '", ';                        //@024-
//     QueryString := QueryString + '"' + lang1 + '", ';                        //@024-
//     QueryString := QueryString + '"' + text1 + '", ';                        //@024-
//     QueryString := QueryString + '"' + lang2 + '", ';                        //@024-
//     QueryString := QueryString + '"' + text2 + '")';                         //@024-
     Query.SQL.Text := QueryString;
     Query.ParamByName('hash1').AsString := Hash1;                              //@024+
     Query.ParamByName('lang1').AsString := Lang1;                              //@024+
     Query.ParamByName('text1').AsString := text1;                              //@024+
     Query.ParamByName('lang2').AsString := Lang2;                              //@024+
     Query.ParamByName('text2').AsString := Text2;                              //@024+
     TRY
       query.execsql;
       case query.RowsAffected of
         0: begin
              Raise EDatabaseError.Create (rsZeroRowsAffected);                 //@012+@013=
              Result := rcZeroRows;                                             //@014=
              LastErrorMessage := rsZeroRowsAffected;                           //@013+
            end;
         1: Result := rcNoProblem;                                              //@014=
        else
          Result := Query.RowsAffected;
          LastErrorMessage := Format(rsXRowsAffected,                           //@013=
                                     [inttostr(Query.RowsAffected)]);           //@013=
       end;  // of CASE

   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        If LastErrorMessage = '' then
          LastErrorMessage := rsUnknownDBError;                                 //@013=
        SendDebug('transdb.TextPairAdd_Glossary '
                                              + rsException + LastErrorMessage);//@010+@014=
        RESULT := rcUnknownError;                                               //@014=
      end; // of ON EXCEPTION

   end;  // of TRY

  end; // of FUNCTION


  Function TextPairAdd(Const Lang1:TLang; Const Text1:UTF8String;               //@008=
                       Const Lang2:TLang; Const Text2:UTF8String;               //@008=
                       Const Mode:TMode):Integer;                               //@001+009=
  var
    Hash1:THash;
    Hash2:THash;
    RC:Integer;
  Begin
    Result := rcNoProblem;                                                      //@014=
    hash1 := String2Hash(Text1);
    Hash2 := string2Hash(Text2);
    // We use HashTextAdd instead of TextAdd because this way we only have to
    // calculate the hash once for the both the sentence and sentenceCR tables.
    TRY
     conn.StartTransaction;
    // We should begin a transaction here
   { TODO 1 -oshiruba -crobustness : check return codes }
    RC := HashTextAdd(Hash1, lang1, text1, mode);                               //@001+009=
    RC := HashTextAdd(Hash2, lang2, text2, mode);                               //@001+009=
    RC := HashCRAdd(lang1, hash1, lang2, hash2, mode);                          //@001+009=
    RC := HashCRAdd(lang2, hash2, lang1, hash1, mode);                          //@001+009=
    // And end the transaction here
    conn.Commit;

   except
    on E: Exception do
      begin
        conn.Rollback;
        LastErrorMessage := e.message;
        SendDebug('transdb.TextPairAdd ' + rsException + e.message);            //@010+@014=
        RESULT := rcUnknownError;                                               //@014=
      end;
  End; // of TRY..EXCEPT
  end;// of FUNCTION


Function FileBlockCRAdd(Const FullPath:UTF8String;                              //@019+
                             Const Block:UTF8String):Integer;                   //@019+
  var
   Hash1:THash; // Document file name hash
   Hash2:THash; // Block hash
   QueryString:UTF8String;                                                      //@032=
  begin
     Hash1 := String2Hash(FullPath);
     Hash2 := String2Hash(Block);
     Query.Params.Clear;                                                        //@024+
     QueryString := 'INSERT OR REPLACE INTO DOCBLOCKCR'
                                + ' ( DOCHASH, BLOCKHASH ) ';
     QueryString := QueryString + 'VALUES ( :hash1, :hash2 )';                  //@024+
//     QueryString := QueryString + 'VALUES (';                                 //@024-
//     QueryString := QueryString + '"' + hash1 + '", ';                        //@024-
//     QueryString := QueryString + '"' + hash2 + '" ) ';                       //@024-
     Query.SQL.Text := QueryString;
     Query.ParamByName('hash1').AsString := Hash1;                              //@024+
     Query.ParamByName('hash2').AsString := Hash2;                              //@024+
     TRY
 // this should only return one result
       Query.execSQL;
   { TODO 1 -oshiruba -cresult-checking : Check the number of rows affected }
       Result := rcNoProblem;
     except
      on E: Exception do
        begin
          LastErrorMessage := e.message;
          if LastErrorMessage = '' then
            LastErrorMessage := rsUnknownDBError;
          RESULT := rcUnknownError;
        end;

     end;  // of TRY
  end; //of FUNCTION

// Beware that this returns the hash, not the actual text
Function FileBlockCRGetList(Const FullPath:UTF8String;                          //@020+
                            Var BlockList:TStrings):Integer;                    //@020+
Var
   QueryString:String;
   ResultCount:Integer;
 Begin
      Query.Params.Clear;                                                       //@024+
      QueryString := 'select BLOCKHASH from DOCBLOCKCR '
//      + 'where DOCHASH = "' + String2Hash(FullPath) + '"';                    //@024-
      + 'where DOCHASH = :dochash';                                             //@024+
      Query.SQL.Text := QueryString;
      Query.ParamByName('dochash').AsString := String2Hash(FullPath);           //@024+
      ResultCount := 0;
  TRY
      Blocklist.Clear;
      query.open;
      While not query.EOF do
        begin
         Inc(ResultCount);
         BlockList.Append(query.FieldByName('BLOCKHASH').AsString);
         query.Next;
       end;
       query.Close;
       If (ResultCount > 0) then result := rcNoProblem else
         begin
          result := rcZeroRows;
          Raise EDatabaseError.Create ('FileBlockCRGetList: ' + rsZeroEntries);
         end;
   except
     on E: Exception do
       begin
         LastErrorMessage := e.message;
         SendDebug('transdb.FileBlockCRGetList ' + rsException + e.message);
         RESULT := rcUnknownError;
       end;
   End;  // of Try..Except
   SendDebug('transdb.FileBlockCRGetList: Loaded ' + IntToStr(ResultCount)
               + ' entries for Document ' + Fullpath + '.');
 end;

Function HashCRAdd(Const lang1:TLang; Const Hash1:THash;
                   Const lang2:TLang; Const Hash2:THash;
                   Const Mode:TMode):Integer;                                   //@001+009=
Var
  QueryString:String;
  table_name:String;                                                            //@001+
Begin
 //    Case pattern_mode of                                                     //@001+009-
    Case Mode of
      mdFragment:table_name := 'sentencecr';                                    //@001=
      mdPattern:table_name := 'patterncr';                                      //@001=
      mdWord:table_name := 'glossariescr';                                      //@009+
    else
      Assert(0=1, 'HashCRAdd: ' + rsInvalidMode + IntToStr(Mode));              //@014=
    end;                                                                        //@001+
     Query.Params.Clear;                                                        //@024+
     QueryString := 'INSERT OR REPLACE INTO ' + table_name                      //@001+
                                + ' ( hash1, lang1, hash2, lang2 ) ';           //@001+
     QueryString := QueryString + 'VALUES ( :hash1, :lang1, :hash2, :lang2 )';  //@024+
//     QueryString := QueryString + 'VALUES (';                                 //@024-
//     QueryString := QueryString + '"' + hash1 + '", ';                        //@024-
//     QueryString := QueryString + '"' + lang1 + '", ';                        //@024-
//     QueryString := QueryString + '"' + hash2 + '", ';                        //@024-
//     QueryString := QueryString + '"' + lang2 + '")';                         //@024-
     Query.SQL.Text := QueryString;
     Query.ParamByName('hash1').AsString:= Hash1;                               //@024+
     Query.ParamByName('lang1').AsString:= Lang1;                               //@024+
     Query.ParamByName('hash2').AsString:= Hash2;                               //@024+
     Query.ParamByName('lang2').AsString:= Lang2;                               //@024+
     TRY
 // this should only return one result
       Query.execSQL;
   { TODO 1 -oshiruba -cresult-checking : Check the number of rows affected }
       Result := rcNoProblem;                                                   //@014=
     except
      on E: Exception do
        begin
          LastErrorMessage := e.message;
          if LastErrorMessage = '' then
            LastErrorMessage := rsUnknownDBError;                               //@014=
          SendDebug('transdb.HashCRAdd ' + rsException + LastErrorMessage);     //@010+
          RESULT := rcUnknownError;                                             //@014=
        end;

     end;  // of TRY
end; // of FUNCTION


Function HashCRDelete(Const SrcLang:TLang; Const SrcHash:THash;
                      Const DestLang:TLang;
                      Const Mode:TMode):Integer;                                //@031+
//                      Const Pattern_Mode:Boolean):Integer;                    //@001+@031-
Var
  QueryString:String;
  Table_Name:String;                                                            //@001+
Begin
  Table_Name := GetCRTableName(Mode);                                           //@031+
  (*                                                                            //@031-
    Case Pattern_Mode of                                                        //@001+
//       True: Table_Name := 'patterncr';                                       //@001+@024-
       True: Table_Name := GetCRTableName(mdPattern);                           //@024+
//       False: Table_Name := 'sentencecr';                                     //@001+@024-
//     False: Table_Name := GetCRTableName(mdSentence);                         //@024+@026-
       False: Table_Name := GetCRTableName(mdFragment);                         //@026+
     end;                                                                       //@001+
   *)
//     QueryString := 'DELETE FROM sentencecr WHERE ';                          //@001-
     Query.Params.Clear;                                                        //@024+
     QueryString := 'DELETE FROM ' + Table_Name + ' WHERE '                     //@001+
      + 'hash1 = :hash1 and lang1 = :lang1 and lang2 = :lang2 ';                //@024-@016=
//     QueryString := QueryString + 'hash1 = "' + SrcHash + '" ';               //@024+
//     QueryString := QueryString + 'and lang1 = "' + SrcLang + '" ';           //@024+
//     QueryString := QueryString + 'and lang2 = "' + DestLang + '" ';          //@024+
     Query.SQL.Text := QueryString;
     Query.ParamByName('hash1').AsString := SrcHash;                            //@024+
     Query.ParamByName('lang1').AsString := SrcLang;                            //@024+
     Query.ParamByName('lang2').AsString := DestLang;                           //@024+
     try
       Query.ExecSQL;
       case query.RowsAffected of
        0:begin
           result := rcZeroRowsAffected;                                        //@014=
           LastErrorMessage := rsZeroRowsAffected;                              //@014=
          end;
        1:result := rcNoProblem;                                                //@014=
        else
         result := query.RowsAffected;
         LastErrorMessage :=
                   Format(rsXRowsDeleted, [inttostr(query.RowsAffected)]);      //@014=
       end; // of CASE

      except
       on E: Exception do
        begin
          LastErrorMessage := e.message;
          SendDebug('transdb.HashCRDelete ' + rsException + e.message);         //@010+
          RESULT := rcUnknownError;                                             //@014=
        end;
     end;  // of TRY..EXCEPT

end;


Function BlockCRAdd(Const SrcLang:TLang;  Const SrcBlock:UTF8String;            //@008=
                      Const DestLang:TLang; Const DestBlock:UTF8String;         //@008=
                      Const Mode:TMode):Integer;                                //@001+010=
                      // In pattern mode the strings should be stripped of place-
                      // holders prior to calling this function
 var
   SrcHash:THash;
   DestHash:THash;
 begin
     SrcHash := String2Hash(SrcBlock);
     DestHash := String2Hash(DestBlock);
     Result := HashCRAdd(SrcLang, SrcHash, DestLang, DestHash, Mode);           //@001-010=
 end;

Function BlockCRDelete(Const SrcLang:TLang;  Const SrcBlock:UTF8String;         //@009+
                       Const DestLang:TLang; Const Mode:TMode)                  //@009+
                        :Integer;  Overload;                                    //@009+
var                                                                             //@031+
  Table_Name:UTF8String;                                                        //@031+
  SrcHash:THash;                                                                //@031+
begin
  Table_Name := GetMainTableName(mode);                                         //@031+
  SrcHash := String2Hash(SrcBlock);                                             //@031+
  Result := HashCRDelete(SrcLang, SrcHash, DestLang, Mode);                     //@031+

(*                                                                              //@031-
  Case Mode of
   mdFragment:Result := BlockCRDelete(SrcLang, SrcBlock, DestLang, False);
   mdPattern: Result := BlockCRDelete(SrcLang, SrcBlock, DestLang, True);
   mdWord:    Result := rcNoProblem; big problem // Ignore for now, since no CR in glossary //@014=
   else
    Raise EInvalidMode.Create ('BlockCRDelete: ' + rsInvalidMode                //@012+@013=
                                                              + IntToStr(Mode));//@012+
    Assert(1=0, 'BlockCRDelete: ' + rsInvalidMode + IntToStr(Mode));            //@013=
  end;    *)

end;


Function TextAdd(Const lang:TLang; Const Text:UTF8String;                       //@008=
                                   Const Mode:TMode):Integer;                   //@001+
  var
   Hash:THash;
 begin
   Hash := String2Hash(Text);
   Result := HashTextAdd(Hash, Lang, Text, Mode);                               //@001+
 end;

Function TextCheck(Const Lang:TLang; Const Text:UTF8String;                     //@011+
                                     Const Mode:TMode):Boolean;               //@011+
 var
  Hash:THash;
 begin
  Hash := String2Hash(Text);
//  SendDebug ('transdb.TextCheck: Hash:' + Hash);
  Result := (HashTextCheck(Lang, Hash, Mode) = 0);
//  IF Result then
//   SendDebug('transdb.TextCheck: Checking ' + text + ': Found')
//  else
//    SendDebug('transdb.TextCheck: Checking ' + text + ': Not Found')
 end;

Function TextSearch(Const Lang:TLang; Const Text:UTF8String;                    //@030+
                                     Const Mode:TMode):UTF8String;              //@030+
Var
 QueryString:String;
 Table_Name:String;
begin
  Table_Name := GetMainTableName(Mode);
  Query.Params.Clear;
  QueryString := 'select text from '+ table_name + ' where text LIKE :text';
  query.SQL.Text := QueryString;
  Query.ParamByName('text').AsString := Text;

  TRY
    query.open;
// this should only return one result
//    while not query.EOF do
    Case Query.EOF of
      False: Result := query.FieldByName('text').AsString; // There are results, return success
      True:  Result := ''; // No results found, return failure
    end; // of CASE
    Query.Close;
except
 on E: Exception do
   begin
     LastErrorMessage := e.message;
     SendDebug('transdb.TextSearch ' + rsException + e.message);
     RESULT := '';
   end;
 End; // of TRY..EXCEPT
end;



Function TextDelete(Const lang:TLang; Const Text:UTF8String;                    //@009+
                               Const Mode:TMode):Integer; Overload;             //@009+
var
  Hash:THash;
 begin
(*
  Case Mode of
    mdFragment:Result := TextDelete(Lang, Text, Mode);
    mdPattern:Result := TextDelete(Lang, Text, Mode);
    mdWord:Result := rcNoProblem;                                               //@014=@031-
    else
     Raise EInvalidMode.Create ('TextDelete: ' + rsInvalidMode                  //@012+//@013=
                                                              + IntToStr(Mode));//@012+
     Assert(1=0, 'TextDelete: ' + rsInvalidMode + IntToStr(Mode));              //@013=
   end; *)
   hash := string2hash(text);                                                   //@031+
   Result := HashTextDelete(Lang, Hash, Mode);                                  //@031+

 end;



Function HashTextDelete(Const Lang:TLang; Const Hash:THash;                     //@031+
                                                  Const Mode:TMode):Integer;    //@001+
//  deprecated 'Use mode paramater overload of HashTextDelete instead';           //@022+@032-
Var
  QueryString:String;
  Table_Name:String;                                                            //@001+
Begin
     Table_Name := GetMainTableName(Mode);                                      //@031+
(*                                                                              //@031-
     Case Pattern_Mode of                                                       //@001+
//      True:Table_name := 'pattern';                                           //@001+@024-
      True: Table_Name := GetMainTableName(mdPattern);                          //@0024+
//      False: Table_name := 'sentence';                                        //@001+@024-
//    False: Table_Name := GetMainTableName(mdSentence);                        //@024+@026-
      False: Table_Name := GetMainTableName(mdFragment);                        //@026+
     end;                                                                       //@001+
*)
     Query.Params.Clear;                                                        //@024+
     QueryString := 'DELETE FROM ' + table_name + ' WHERE ';                    //@001-
     QueryString := QueryString + 'hash = :hash and lang = :lang';              //@024+
//     QueryString := QueryString + 'hash = "' + hash + '" ';                   //@024-
//     QueryString := QueryString + 'and lang = "' + lang + '" ';               //@024-
     Query.SQL.Text := QueryString;
     Query.ParamByName('hash').AsString := Hash;                                //@024+
     Query.ParamByName('lang').AsString := Lang;                                //@024+
     try
       Query.ExecSQL;
       Case Query.RowsAffected of
        0:begin
           result := rcZeroRowsAffected;                                        //@013=
           LastErrorMessage := rsZeroRowsDeleted;                               //@013=
           Raise EDatabaseError.Create ('HashTextDelete: ' + rsZeroRowsDeleted);//@012+@013=
          end;
        1:result := rcNoProblem;                                                //@013=
        else
         result := query.RowsAffected;
         LastErrorMessage :=
                    Format(rsXRowsDeleted, [inttostr(query.RowsAffected)]);     //@014=
       end; // of CASE

      except
       on E: Exception do
        begin
          LastErrorMessage := e.message;
          SendDebug('transdb.HashTextDelete ' + rsException + e.message);       //@010+@014=
          RESULT := rcUnknownError;                                             //@014=
        end;
     end;
end;  // of FUNCTION


Function HashTextAdd(Const Hash:THash; Const Lang:TLang; Const Text:UTF8String; //@008=
                                                      Const Mode:TMode):Integer;//@001+
Var
  QueryString:String;
  Table_Name:String;                                                            //@001+
Begin
   //  Case MODE of                                                             //@001+010-
   //    mdFragment : Table_Name := 'sentence';                                 //@001+010-
   //    mdPattern : Table_Name := 'pattern';                                   //@001+010-
   //    mdWord: Table_Name := 'glossaries';                                    //@009+010-
   //    else                                                                   //@001+010-
        Table_Name := GetMainTableName(Mode);                                   //@010+
        If Table_Name = '' then                                                 //@010+
          begin                                                                 //@001+
            Result := rcUnknownError;                                           //@001+@014=
            Raise EInvalidMode.Create ('HashTextAdd: ' + rsInvalidMode          //@012+@013=
                                                              + IntToStr(Mode));//@012+
            LastErrorMessage := rsInvalidMode;                                  //@001+@013+
            exit;                                                               //@001+
        end;                                                                    //@001+
   //  end;   // of CASE                                                        //@001+010-
     Query.Params.Clear;                                                        //@024+
     QueryString := 'INSERT OR REPLACE INTO ' + table_name                      //@001+
                                        + ' ( hash, lang, text, del ) '         //@001+@029=
     //     QueryString := QueryString + 'VALUES (';                            //@024-
                    + 'VALUES ( :hash, :lang, :text, :del)';                    //@024+@029=
//     QueryString := QueryString + '"' + hash + '", ';                         //@024-
//     QueryString := QueryString + '"' + lang + '", ';                         //@024-
//     QueryString := QueryString + '"' + text + '")';                          //@024-
     query.SQL.Text := QueryString;
     Query.ParamByName('hash').AsString := Hash;                                //@024+
     Query.ParamByName('lang').AsString := Lang;                                //@024+
     Query.ParamByName('text').AsString := Text;                                //@024+
     Query.ParamByName('del').AsString := space(1);                             //@029+
     TRY
//     query.open;
 // this should only return one result
       Query.ExecSQL;
       Case Query.RowsAffected of
         0: begin
              Result := rcZeroRowsAffected;                                     //@014=
              LastErrorMessage := rsZeroRowsAffected;                           //@013=
            end;
         1: Result := rcNoProblem;                                              //@014=
         else
           Result := Query.RowsAffected;
           LastErrorMessage := Format(rsXRowsAffected,                          //@013=
                                      [inttostr(Query.RowsAffected)]);          //@013=
       end;  // of CASE

   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        If LastErrorMessage = '' then
          LastErrorMessage := rsUnknownDBError;                                 //@013+
        SendDebug('transdb.HashTextAdd ' + rsException + LastErrorMessage);     //@010+@014=
        RESULT := rcUnknownError;                                               //@014=
      end;

  end;  // of TRY

end; // of FUNCTION

Function HashTextGet(Const Lang:TLang; Const Hash:THash;
                                                Const Mode:TMode):UTF8String; //@008=009=
Const
  TargetField='text';                                                           //@014+
Var
  QueryString:String;
  Table_Name:String;                                                            //@009+
Begin
 // Case Mode of                                                                //@009+010-
 //   mdFragment:Table_Name := 'sentence';                                      //@009+010-
 //   mdPattern: Table_Name := 'pattern';                                       //@009+010-
 //   mdWord:    Table_Name := 'glossaries';                                    //@009+010-
 //  end;                                                                       //@009+010-
   Table_Name := GetMainTableName(Mode);                                        //@010+
     Query.Params.Clear;                                                        //@010+
     //     QueryString := 'select text from ' + Table_Name + ' where hash = "';//@009=010-
//     QueryString := QueryString + hash + '"';                                 //@010-
     QueryString := 'select ' + TargetField + ' from ' + Table_Name             //@014+
                                                     + ' where hash = :hash';   //@010+
     Query.SQL.Text := QueryString;
     Query.ParamByName('hash').AsString:=hash;                                  //@010+
     TRY
       Query.Open;
 // this should only return one result
       If not Query.EOF then
         Begin
          Result := query.FieldByName(TargetField).AsString;                    //@014=
//          query.Next;
         End;
       Query.Close;
   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        SendDebug('transdb.HashTextGet ' + rsException + e.message);                //@010+
        RESULT := '';
      end;
   End; // of Try
 end;

(*
Function HashTextGet_Pattern(const Hash:THash):UTF8String;                     //@001+008=
Var
  QueryString:String;
Begin
     QueryString := 'select text from pattern where hash = "';
     QueryString := QueryString + hash + '"';
     query.SQL.Text := QueryString;
     TRY
     query.open;
 // this should only return one result
 //    while not query.EOF do
     if not query.EOF then
       begin
        result := query.FieldByName('text').AsString;
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
end;
      *)



//  Function GenericEntryCheck(Const TableName:UTF8String;                      //@024-
  Function SelectXFromYWhereZ_Check(Const TableName:UTF8String;                 //@024+
                             Const KeyFieldName:UTF8String;
                             Const KeyValue:UTF8String;
                             Const SelectField:UTF8String):Boolean;             //@014+
   Var
    QueryString:String;
   begin
     Query.Params.Clear;
 { TODO 1 -oshiruba -cClean-up : Convert this to Format() }
     QueryString := 'select ' + SelectField + ' from '+ TableName
                                + ' where ' + KeyFieldName + '  = :KeyValue';
     Query.SQL.Text := QueryString;
     Query.ParamByName('KeyValue').AsString := KeyValue;
//     TRY   // We let the layer above handle exceptions here since
//           // we return only boolean
     Query.open;
 // this should only return one result
      Result := NOT Query.EOF;
      Query.Close;
//   except
//    on E: Exception do
//      begin
//        LastErrorMessage := e.message;
//        SendDebug('transdb.GenericEntryCheck ' + rsException + e.message);
//        RESULT := rcDBException;
//      end;
//    End; // of TRY..EXCEPT
   end;


  Function HashTextCheck(Const Lang:TLang; Const Hash:THash;
                                            Const Mode:TMode):Integer;          //@009=
   Var
    QueryString:String;
    Table_Name:String;                                                          //@009+
   begin
     Table_Name := GetMainTableName(Mode);                                      //@010+
 //  Case Mode of                                                               //@009+010-
 //     mdFragment:Table_Name := 'sentence';                                    //@009+010-
 //     mdPattern :Table_Name := 'pattern';                                     //@009+010-
 //     mdWord    :Table_Name := 'glossaries';                                  //@009+010-
 //   end;                                                                      //@009+010-
     Query.Params.Clear;                                                        //@010+
 //    QueryString := 'select text from '+ table_name + ' where hash = "';      //@010-
 //    QueryString := QueryString + hash + '"';                                 //@010-
     QueryString := 'select text from '+ table_name + ' where hash = :hash';    //@010+
     query.SQL.Text := QueryString;
     Query.ParamByName('hash').AsString := hash;                                //@010+
     TRY
       query.open;
 // this should only return one result
 //    while not query.EOF do
       Case Query.EOF of                                                        //@011=
         False: Result := rcNoProblem; // There are results, return success     //@011=
         True:  Result := rcNoResults; // No results found, return failure      //@011=
       end; // of CASE                                                          //@011=
       Query.Close;
   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        SendDebug('transdb.HashTextCheck ' + rsException + e.message);          //@010+
        RESULT := rcDBException;                                                //@011=
      end;
    End; // of TRY..EXCEPT
   end;

Function HashCRGet(Const SrcLang:TLang; Const Hash1:THash;
                   Const DestLang:TLang; Const Mode:TMode):THash;               //@009=
Var
  QueryString:String;
  Table_Name:String;                                                            //@009+
Begin
     Table_Name := GetCRTableName(Mode);                                        //@024+
//     Case Mode of                                                               //@009+@024-
//       mdFragment:Table_Name := 'sentencecr';                                   //@009+@024-
//       mdPattern: Table_Name := 'patterncr';                                    //@009+@024-
//       mdWord:    Table_Name := 'glossariescr';                                 //@009+@024-
//     else                                                                     //@009+@024-
//        Raise EInvalidMode.Create ('HashCRGet: ' + rsInvalidMode                //@012+@013=@024-
//                                                              + IntToStr(Mode));//@012+@024-
//        Assert(1=0, 'HashCRGet: ' + rsInvalidMode + IntToStr(Mode));            //@009+@013=@024-
//     end;                                                                       //@009+@024-
     Query.Params.Clear;                                                        //@010+
//     QueryString := 'select hash2 from ' + Table_Name + ' where hash1 = "';   //@010-
//     QueryString := QueryString + hash1 + '"';                                //@010-
     QueryString := 'select hash2 from ' + Table_Name + ' where hash1 = :hash1 '//@010+
// Theoretically we shouldn't even need these, but they could speed up the
// search, and reduce the risk of hash collissions
//QueryString := QueryString + ' and lang1 = "';                                //@010-
//QueryString := QueryString + SrcLang + '"';                                   //@010-
     + ' and lang1 = :srclang'                                                  //@010+
//QueryString := QueryString + ' and lang2 = "';                                //@010-
//QueryString := QueryString + Destlang + '"';                                  //@010-
     + ' and lang2 = :destlang';                                                //@010+
     query.SQL.Text := QueryString;
     Query.ParamByName('hash1').AsString:= Hash1;                               //@010+
     Query.ParamByName('srclang').AsString:= SrcLang;                           //@010+
     Query.ParamByName('destlang').AsString:= DestLang;                         //@010+
 TRY
     Result := '';                                                              //021+
     query.open;
 // this should only return one result
 //    while not query.EOF do
     if not Query.EOF then
//       begin
        Result := Query.FieldByName('hash2').AsString;
//        query.Next;
//      end;
      Query.Close;
   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        SendDebug('transdb.HashCRGet ' + rsException + e.message);              //@010+@014+
        RESULT := '';
      end;
   End; // of TRY..EXCEPT
End;

Function TextPairDelete(Const Lang1:TLang; Const Text1:UTF8String;              //@008=
                        Const lang2:TLang; Const Text2:UTF8String;              //@008=
                        Const Mode:TMode):Integer;                              //@031+
  var
   Hash1:THash;
   Hash2:THash;
   RC:integer;
  Begin
    Result := rcNoProblem;                                                      //@014=
    hash1 := String2Hash(text1);
    Hash2 := string2Hash(text2);
    // We use HashTextAdd instead of TextAdd because this way we only have to
    // calculate the hash once for the both the sentence and sentenceCR tables.
    TRY
      conn.StartTransaction;
      // We should begin a transaction here
      RC := HashCRDelete(lang1, hash1, lang2, Mode);                            //@031=
      RC := HashCRDelete(lang2, hash2, lang1, Mode);                            //@031=
      RC := HashTextDelete(lang1, Hash1, Mode);                                 //@031+
      RC := HashTextDelete(lang2, Hash2, Mode);                                 //@031+
      // And end the transaction here
      conn.Commit;

    except
      on E: Exception do
        begin
          conn.Rollback;
          LastErrorMessage := e.message;
          SendDebug('transdb.TextPairDelete ' + rsException + e.message);       //@010+@014=
          RESULT := rcUnknownError;                                             //@014=
        end;
    End; // of TRY..EXCEPT
  end;// of FUNCTION

Function DBSelfTest:Integer;
VAR
 S : String;
begin
     query.SQL.Text := 'select * from something';
 TRY
  result := rcNoProblem;                                                        //@014=
  query.Open;
      S := '';
      while not query.EOF do
       begin
        inc(Result, 1);
        S := S + query.FieldByName('field2').AsString + #13#10;
        query.Next;
      end;
   // Apps check for zero
   If Result > 0 then Result := rcNoProblem else Result := rcDBException;       //@011+@014=
   except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        RESULT := rcUnknownError;                                               //@014=
        SendDebug('transdb.DBSelfCheck: ' + rsDBCheckFailed + ', message='      //@013=
                                                  + e.message);                 //@005+
      end;
  end;
end;

  Function GetLastErrorMessage:UTF8String;                                      //@019=
   Begin
    Result := LastErrorMessage;
   End;

 Function SegmentAbbrGet(Const Lang:TLang; Var Abbr_List:TAbbr_List):Integer;   //@006+
  Var
    QueryString:String;
    ResultCount:Integer;
  Begin
   { TODO 1 -oshiruba -chardening : Paramaterize this function }
       Query.Params.Clear;                                                      //@024+
       QueryString := 'select abbr from segment_abbr where lang  = :lang';
//       QueryString := 'select abbr from segment_abbr where lang  = "';        //@024-
//       QueryString := QueryString + Lang + '"';                               //@024-
       Query.SQL.Text := QueryString;
       Query.ParamByName('lang').AsString := Lang;                              //@024+
       ResultCount := 0;
   TRY
       Query.Open;
       While not query.EOF do
         begin
          Inc(ResultCount);
          SetLength(Abbr_List, ResultCount);
          Abbr_List[ResultCount-1] := query.FieldByName('abbr').AsString;
          Query.Next;
        end;
        Query.Close;
        Result := rcNoProblem;                                                  //@023+
//        If (ResultCount > 0) then result := rcNoProblem else                  //@014=@023-
//          begin                                                               //@023-
//           result := rcZeroRows;                                              //@014=@023-
//           Raise EDatabaseError.Create ('SegmentAbbrGet: ' + rsZeroEntries);  //@012+@013=@023-
//          end;
    except
      on E: Exception do
        begin
          LastErrorMessage := e.message;
          SendDebug('transdb.SegmentAbbrGet ' + rsException + e.message);       //@010+
          RESULT := rcUnknownError;                                             //@014=
        end;
    End;  // of Try..Except
    SendDebug('transdb.SegmentAbbrGet: Loaded ' + IntToStr(ResultCount)
                + ' entries for language ' + Lang + '.');
  end;


  Function SegmentDelimGet(Const Lang:TLang):UnicodeString;                     //@007+
   Var
     QueryString:String;
     ResultCount:Integer;
     ResultStr:UTF8String;
   Begin
    { TODO 1 -oshiruba -cHardening : Paramaterize this function }
     Query.Params.Clear;;
     QueryString := 'select sm from segment_in where lang  = :lang';            //@024+
//    QueryString := 'select sm from segment_in where lang  = "';               //@024-
//    QueryString := QueryString + Lang + '"';                                  //@024-
     Query.SQL.Text := QueryString;
     Query.ParamByName('lang').AsString := Lang;                                //@024+
     ResultCount := 0;
     Result := '';
     ResultStr := '';
    TRY
        Query.Open;
        While not query.EOF do
          begin
           Inc(ResultCount);
           ResultStr := ResultStr + query.FieldByName('sm').AsString;
           query.Next;
         end;
         Query.Close;
         If (ResultCount = 0) then
          begin
           result := '';
           Raise EDatabaseError.Create ('SegmentDelimGet: ' + rsZeroEntries);   //@012+@013=
          end;
     except
       on E: Exception do
         begin
           LastErrorMessage := e.message;
           SendDebug('transdb.SegmentDelimGet ' + rsException + e.message);         //@010+
           RESULT := '';
         end;
     End;  // of Try..Except
//     SendDebug('transdb.SegmentDelimGet: ' + ResultStr);
     Result:= UTF8toUTF16(ResultStr);
     SendDebug('transdb.SegmentDelimGet: Loaded ' + IntToStr(ResultCount)
                + ' entries for language ' + Lang + '.');
   end;

  Procedure ProjectListGet(Var pl:TStringList); //(Const Username:UTF8String)            //@013+
  // Returns the list of projects
  Var
    QueryString:String;
    ResultCount:Integer;
    ResultStr:UTF8String;
  Begin
       QueryString := 'select projectname from projectlist';
       query.SQL.Text := QueryString;
       ResultCount := 0;
       ResultStr := '';
   TRY
       pl.Clear;
       Query.Open;
       While not query.EOF do
         begin
          Inc(ResultCount);
          ResultStr := query.FieldByName('projectname').AsString;
          pl.Append(ResultStr);
          query.Next;
        end;
        query.Close;
        If (ResultCount = 0) then
         begin
           // do nothing, it's not an error not to have any projects
          SendDebug('transdb.ProjectListGet ' + rsZeroEntries);
         end;
    except
      on E: Exception do
        begin
          LastErrorMessage := e.message;
          SendDebug('transdb.ProjectListGet ' + rsException + e.message);           //@010+
          pl.Clear;
        end;
    End;  // of Try..Except
//    Result:= UTF8toUTF16(ResultStr);
    SendDebug('transdb.ProjectListGet: Loaded ' + IntToStr(ResultCount)
               + ' project entries.');
  end;

  Function ProjectListAdd(Const ProjectName:UTF8String;
                          Const ProjectDesc:UTF8String;                         //@013+@014=
                          Const BasePath:UTF8String;                            //@017+
                          Const SrcLang:TLang;                                  //@017+
                          Const DestLang:Tlang                                  //@017+
                          ):Integer;
  // Adds a new project to the project list
  Const
   Table_Name = 'PROJECTLIST';

  Var
    QueryString:String;
    ProjectHash:THash;
  Begin
  ProjectHash := String2Hash(ProjectName);
  Query.Params.Clear;                                                           //@014+

  QueryString := 'INSERT OR REPLACE INTO ' + table_name
       + ' ( PROJECTHASH, PROJECTNAME, PROJECTDESC, BASEPATH, SRCLANG, DESTLANG ) ';//@017=
  QueryString := QueryString +
   'VALUES ( :projecthash, :projectname, :projectdesc, :basepath, :srclang, :destlang)'; //@014+@017=
//  QueryString := QueryString + 'VALUES (';
//  QueryString := QueryString + '"' + ProjectHash + '", ';
//  QueryString := QueryString + '"' + ProjectName + '", ';
//  QueryString := QueryString + '"' + ProjectDesc + '")';
  Query.SQL.Text := QueryString;
  Query.ParamByName('projecthash').AsString := ProjectHash;                     //@014+
  Query.ParamByName('projectname').AsString := ProjectName;                     //@014+
  Query.ParamByName('projectdesc').AsString := ProjectDesc;                     //@014+
  Query.ParamByName('basepath').AsString     := BasePath;                       //@017+
  Query.ParamByName('srclang').AsString     := SrcLang;                         //@017+
  Query.ParamByName('destlang').AsString     := DestLang;                       //@017+

  TRY
    Query.ExecSQL;
    Case Query.RowsAffected of
     0: begin
         Result := rcZeroRowsAffected;                                          //@014=
         LastErrorMessage := rsZeroRowsAffected;                                //@013=
        end;
     1: Result := rcNoProblem;                                                  //@014=
     else
      Result := Query.RowsAffected;
      LastErrorMessage := Format(rsXRowsAffected,                               //@013=
                                 [inttostr(Query.RowsAffected)]);               //@013=
    end;  // of CASE
  except
    on E: Exception do
     begin
       LastErrorMessage := e.message;
       If LastErrorMessage = '' then
        LastErrorMessage := rsUnknownDBError;                                   //@013+
       SendDebug('transdb.ProjectListAdd ' + rsException + LastErrorMessage);   //@010+
       RESULT := rcUnknownError;                                                //@014=
     end;
  end; // of TRY..EXCEPT
  end; // of FUNCTION

  // Deletes a row from the database using the given table name, key field name,
  // and the key value to delete.
  // This version supports only one key
  // Use with Caution:
  // There is no check to ensure that the key field is actually a database key
//  Function DatabaseGenericDelete(Const TableName:UTF8String;
  Function DeleteFromXWhereY(Const TableName:UTF8String;
                                 Const KeyField:UTF8String;
                                 Const KeyValue:UTF8String):Integer;            //@013+
  // Deletes a project from the project list
  Var
    QueryString:String;
  Begin
       Query.Params.Clear;                                                      //@024+
       QueryString := 'DELETE FROM ' + TableName + ' WHERE ';                   //@001+
//       QueryString := QueryString + KeyField + ' = "' + KeyValue + '" ';      //@024-
       QueryString := QueryString + KeyField + ' = :keyvalue ';                 //@024+
       query.SQL.Text := QueryString;
       Query.ParamByName('keyvalue').AsString := KeyValue;                      //@024+
       try
         Query.ExecSQL;
         case query.RowsAffected of
          0:begin
             result := rcZeroRowsAffected;                                      //@014=
             LastErrorMessage := rsZeroRowsDeleted;                             //@013=
            end;
          1:result := rcNoProblem;                                              //@014+
          else
           result := query.RowsAffected; // Maybe we should have a new error code
           LastErrorMessage := Format(rsXRowsDeleted, [inttostr(query.RowsAffected)]);
         end; // of CASE

        except
         on E: Exception do
          begin
            LastErrorMessage := e.message;
            SendDebug('transdb.DatabaseGenericDelete ' + rsException + e.message);  //@013=
            RESULT := rcUnknownError;                                           //@014=
          end;
       end;  // of TRY..EXCEPT

  end;

Procedure CheckProject(Const ProjectName:UTF8String);                           //@014+
  // Internal function to throw an error if the requested project doesn't exist
  Begin
    If not ProjectExists(ProjectName) then                                      //@014+
      begin                                                                     //@014+
        LastErrorMessage := rsNoSuchProject;                                    //@014+
        Raise ENoSuchProject.Create (rsNoSuchProject);                          //@014=
      end;                                                                      //@014+
  End;

Procedure CheckFilesystemFile(Const FileName:UTF8String);                       //@014+@019+
// Internal function to throw an error if the requested file doesn't exist on disk
Begin
  If not FileExistsUTF8(FileName) then                                          //@014+@019=
    begin                                                                       //@014+
      LastErrorMessage := rsNoSuchDiskFile;                                     //@014+
      Raise ENoSuchDiskFile.Create (rsNoSuchDiskFile);                          //@014=
    end;                                                                        //@014+
End;

Function ProjectListDelete(Const ProjectName:UTF8String):Integer;               //@013+
// Deletes a project from the project list
  Const
    TableName = 'PROJECTLIST';
    KeyName = 'PROJECTHASH';
  Var
    ProjectHash : String;
  Begin
    CheckProject(ProjectName);                                                  //@014+
    ProjectHash := String2Hash(ProjectName);
    Result := DeleteFromXWhereY(TableName, KeyName, ProjectHash);
  End;


Function ProjectFileAdd(Const ProjectName:UTF8String; FileName:String;
                               Const FileDesc:UTF8String;
                               Const SrcLang:TLang;
                               Const DestLang:TLang):Integer;                   //@013+
// Adds a new file to the given project
Const
 Table_Name = 'PROJECTFILE';

Var
  QueryString:String;
  ProjectHash:THash;
  ProjectDesc:UTF8String;
Begin
{ TODO 1 -oshiruba -cErrorChecking : Check if a project exists before adding files to it }
  { TODO 1 -oshiruba -chardening : Paramaterize this function }
 CheckProject(ProjectName);                                                     //@014+
 CheckFilesystemFile(FileName);                                                 //@014+
 ProjectHash := String2Hash(ProjectName);
 Query.Params.Clear;                                                            //@014+
 QueryString := 'INSERT OR REPLACE INTO ' + table_name
                  + ' ( PROJECTHASH, FILENAME, FILEDESC, SRCLANG, DESTLANG, FILEHASH ) ' //@024=
  + 'VALUES ( :projecthash, :FileName, :FileDesc, :SrcLang, :DestLang, :filehash)';  //@024=
// QueryString := QueryString + 'VALUES (';                                     //@014-
//  QueryString := QueryString + '"' + ProjectHash + '", ';                     //@014-
//  QueryString := QueryString + '"' + FileName + '", ';                        //@014-
//  QueryString := QueryString + '"' + FileDesc + '", ';                        //@014-
//  QueryString := QueryString + '"' + SrcLang + '", ';                         //@014-
//  QueryString := QueryString + '"' + DestLang + '")';                         //@014-
  Query.SQL.Text := QueryString;
  Query.ParamByName('projecthash').AsString:=ProjectHash;                       //@014+
  Query.ParamByName('FileName').AsString:=FileName;                             //@014+
  Query.ParamByName('FileDesc').AsString:=FileDesc;                             //@014+
  Query.ParamByName('SrcLang').AsString:=SrcLang;                               //@014+
  Query.ParamByName('DestLang').AsString:=DestLang;                             //@014+
  Query.ParamByName('filehash').AsString := String2Hash(FileName);              //@024+

  TRY
    Query.ExecSQL;
    case query.RowsAffected of
     0: begin
         Result := rcZeroRowsAffected;                                          //@014=
         LastErrorMessage := rsZeroRowsAffected;                                //@013+
        end;
     1: Result := rcNoProblem;                                                  //@014=
     else
      Result := Query.RowsAffected;  // Maybe we should make another error code
      LastErrorMessage := Format(rsXRowsAffected,                               //@013=
                                 [inttostr(Query.RowsAffected)]);               //@013=
    end;  // of CASE
  except
    on E: Exception do
     begin
       LastErrorMessage := e.message;
       If LastErrorMessage = '' then
        LastErrorMessage := rsUnknownDBError;                                   //@013+
        SendDebug('transdb.ProjectFileAdd ' + rsException + LastErrorMessage);  //@010+
       RESULT := rcUnknownError;                                                //@014=
     end;
  end; // of TRY..EXCEPT
end; // of FUNCTION

Function ProjectFileListGet(Const ProjectName:UTF8String;                       //@013+
                               Var FileList:TStringList):Integer;               //@013+
// Returns a list of files for the requested project
Var
  ProjectHash:THash;
  QueryString:String;
  ResultCount:Integer;
  ResultStr:UTF8String;
Begin
     Query.Params.Clear;                                                        //@024+
     ProjectHash := String2Hash(ProjectName);
     QueryString :=
           'select FILENAME from PROJECTFILE where PROJECTHASH = :projecthash'; //@024+
//     QueryString := 'select FILENAME from PROJECTFILE where PROJECTHASH = "'; //@024-
//     QueryString := QueryString + ProjectHash + '"';                          //@024-
     Query.SQL.Text := QueryString;
     Query.ParamByName('projecthash').AsString := ProjectHash;                  //@024+
     ResultCount := 0;
     ResultStr := '';
 TRY
     FileList.Clear;
     Query.open;
     While not Query.EOF do
       begin
        Inc(ResultCount);
        ResultStr := Query.FieldByName('FILENAME').AsString;
        FileList.Append(ResultStr);
        query.Next;
      end;
      query.Close;
      Result := rcNoProblem;                                                    //@014+
      If (ResultCount = 0) then
       begin
         // do nothing, it's not an error not to have any projects
        SendDebug('transdb.ProjectFileListGet '+ rsZeroEntries);                //@013+
       end;
  except
    on E: Exception do
      begin
        LastErrorMessage := e.message;
        SendDebug('transdb.ProjectFileListGet ' + rsException + e.message);     //@010+
        Result := rcUnknownError;                                               //@014+
        FileList.Clear;
      end;
  End;  // of Try..Except
//    Result:= UTF8toUTF16(ResultStr);
  SendDebug('transdb.ProjectListGet: Loaded ' + IntToStr(ResultCount)
             + ' project entries.');
end;

Function ProjectFileDelete(Const ProjectName:UTF8String;
                           Const FileName:UTF8String;
                           Const SrcLang:TLang; Const DestLang:TLang):Integer;  //@013+
// Deletes a given file from the given project
  Const
    TableName = 'PROJECTFILE';
//    KeyName = 'FILENAME';   // Should really be project hash + filename       //@024-
    KeyName = 'FILEHASH';   // Should really be project hash + filename         //@024+
  Var
    FileHash : String;                                                          //@024=
  Begin
    CheckProject(ProjectName);
    FileHash := String2Hash(FileName);                                          //@024=
    Result := DeleteFromXWhereY(TableName, KeyName, FileHash);                  //@024=
 //   Result := DatabaseGenericDelete(TableName, KeyName, ProjectHash, FileName);
  End;

Function ProjectExists(Const ProjectName:UTF8String):Boolean;                   //@014+
// Determines whether or not a project exists in the database
 var
   Hash:THash;
 begin
     Hash := String2Hash(ProjectName);
     Result :=  SelectXFromYWhereZ_Check('PROJECTLIST', 'PROJECTHASH',          //@024=
                           Hash,
                           'PROJECTNAME');                                      //@014+

  end;

Function ProjectInfoRead(Const ProjectName:UTF8String):TProjectInfo;            //@016+
  Begin
    Result := ProjectInfoReadHash(String2Hash(ProjectName));
  end;

Function ProjectInfoReadHash(Const ProjectHash:THash):TProjectInfo;             //@015+//@016=
Const
  TargetField = '*';
  Table_Name = 'PROJECTLIST';
Var
  QueryString:UTF8String;
Begin
     Query.Params.Clear;
     QueryString := 'select ' + TargetField + ' from ' + Table_Name
                              + ' where PROJECTHASH = :projecthash';
     query.SQL.Text := QueryString;
     Query.ParamByName('projecthash').AsString := ProjectHash;
//     TRY // let the exception move up the chain
       Query.Open;
 // this should only return one result
       if not query.EOF then
        With Result do                                                          //@024+
         begin
          ProjectHash := query.FieldByName('PROJECTHASH').AsString;             //@024=
          ProjectName := query.FieldByName('PROJECTNAME').AsString;             //@024=
          ProjectDesc := query.FieldByName('PROJECTDESC').AsString;             //@024=
          BasePath := query.FieldByName('BASEPATH').AsString;                   //@024=
          SrcLang := query.FieldByName('SRCLANG').AsString;                     //@024=
          DestLang := query.FieldByName('DESTLANG').AsString;                   //@024=
         end;
       Query.Close;
//   except
//    on E: Exception do
//      begin
//        LastErrorMessage := e.message;
//        SendDebug('transdb.ProjectInfoRead ' + rsException + e.message);        //@010+
//       RESULT ;
//      end;
//   End; // of Try
 end; // of FUNCTION

Function FileInfoRead(Const FileName:UTF8String):TDocumentInfo;                 //@015+
// Reads information on a particular file registered in the database
Const
  TargetField = '*';
  Table_Name = 'PROJECTFILE';
Var
  QueryString:UTF8String;
Begin
     Query.Params.Clear;
     QueryString := 'select ' + TargetField + ' from ' + Table_Name
                              + ' where FILENAME = :filename';
     query.SQL.Text := QueryString;
     Query.ParamByName('filename').AsString := FileName;
//     TRY // let the exception move up the chain
       Query.Open;
 // this should only return one result
       if not query.EOF then
         begin
          Result.ProjectHash := query.FieldByName('PROJECTHASH').AsString;
          Result.FileName := query.FieldByName('FILENAME').AsString;
          Result.FileDesc := query.FieldByName('FILEDESC').AsString;
          Result.SrcLang := query.FieldByName('SRCLANG').AsString;
          Result.DestLang := query.FieldByName('DESTLANG').AsString;
         end;
       Query.Close;
//   except
//    on E: Exception do
//      begin
//        LastErrorMessage := e.message;
//        SendDebug('transdb.ProjectInfoRead ' + rsException + e.message);        //@010+
//       RESULT ;
//      end;
//   End; // of Try
 end; // of FUNCTION

Function Decode(const inp:string;
                const s1:string; const i1:integer;
                const s2:string; const i2:integer;
                const default:integer):Integer;                                 //@018+
  begin
   if inp = s1 then
    result := i1
   else if inp = s2 then
    result := i2
   else
     result := default;
  end;

Function Decode(const inp:string;                                               //@018+
                const s1:string; const i1:integer;
                const s2:string; const i2:integer;
                const s3:string; const i3:integer;
                const s4:string; const i4:integer;
                const default:integer):Integer; Overload;
begin
 if inp = s1 then
  result := i1
 else if inp = s2 then
  result := i2
 else if inp = s3 then
  result := i3
 else if inp = s4 then
  result := i4
 else
   result := default;
end;

Function Decode(const inp:string;                                               //@025+
                const s1:string; const i1:integer;
                const s2:string; const i2:integer;
                const s3:string; const i3:integer;
                const s4:string; const i4:integer;
                const s5:string; const i5:integer;
                const default:integer):Integer; Overload;
begin
 if inp = s1 then
  result := i1
 else if inp = s2 then
  result := i2
 else if inp = s3 then
  result := i3
 else if inp = s4 then
  result := i4
 else if inp = s5 then
  result := i5
 else
   result := default;
end;

// Open Array Decode Function
// Works like Oracle's PL-SQL Decode
Function Decode(Const inp:String; Const Args : Array of Const):Integer;         //@026+
 var
   loc:Cardinal;
 begin
  // Doc for Array of Const
  // http://www.freepascal.org/docs-html/ref/refsu60.html
  // Args should look like:
  // String1, Integer1
  // String2, Integer2,
  // ...
  // IntegerDefault
  If High(Args)>= 0 then
    Result := Args[High(Args)].VInteger;
  For loc := low(Args) to High(Args)-1 do
     If not Odd(loc) then  // Ignore Odd entries, since they are the output
       If inp = AnsiString(Args[loc].VAnsiString) then
        begin
         Result := Args[Loc+1].VInteger;
         break; // exit loop
        end;

 end;

Procedure SelectXFromY(Const FieldName, TableName:UTF8String; Var List:TStringList);       //@027+
 // Returns the list of entries
// Table Field type should be string compatible
 Var
   QueryString:String;
   ResultCount:Integer;
   ResultStr:UTF8String;
 Begin
      QueryString := 'select ' + FieldName + ' from ' + TableName;
      query.SQL.Text := QueryString;
      ResultCount := 0;
      ResultStr := '';
  TRY
      List.Clear;
      Query.Open;
      // Copy each line of the results into our output
      While not query.EOF do
        begin
         Inc(ResultCount);
         ResultStr := query.FieldByName(FieldName).AsString;
         List.Append(ResultStr);
         query.Next;
       end;
       query.Close;
       If (ResultCount = 0) then
        begin
          // do nothing, it's not an error not to have any entries
         SendDebug('transdb.SelectXFromY ' + rsZeroEntries);
        end;
   except
     on E: Exception do
       begin
         LastErrorMessage := e.message;
         SendDebug('transdb.SelectXFromY ' + rsException + e.message);
         List.Clear;
       end;
   End;  // of Try..Except
   SendDebug('transdb.SelectXFromY: Loaded ' + IntToStr(ResultCount)
              + '  entries.');
 end;

Procedure LangCodeListGet(Var LanguageList:TStringList);                        //@027+
  begin
    SelectXFromY('LANGCODE', 'LANG', LanguageList);
  end;


Function StatSegInsert(Const Lang:Tlang; Const Text:UTF8String;                 //@028+
                                          Const TknCnt:Integer;
                                          Const Count:Integer):Integer;
  Const
    Table_Name = 'STATSEG';
  begin
   Query.Params.Clear;
//   query.SQL.Text := 'INSERT OR REPLACE INTO ' + table_name
// We want it to error if the row exists, since we should be calling update instead
   query.SQL.Text := 'INSERT INTO ' + table_name
                        + ' ( hash, lang, tkncnt, count ) '
                        + 'VALUES ( :hash, :lang, :tkncnt, :count )';

   Query.ParamByName('hash').AsString := String2Hash(Text);
   Query.ParamByName('lang').AsString := Lang;
   Query.ParamByName('tkncnt').AsString := IntToStr(TknCnt);
   Query.ParamByName('count').AsString := IntToStr(Count);

   TRY
         Query.ExecSQL;
         Case Query.RowsAffected of
           0: begin
                Result := rcZeroRowsAffected;
                LastErrorMessage := rsZeroRowsAffected;
              end;
           1: Result := rcNoProblem;
           else
             Result := Query.RowsAffected;
             LastErrorMessage := Format(rsXRowsAffected,
                                        [inttostr(Query.RowsAffected)]);
         end;  // of CASE
     except
      on E: Exception do
        begin
          LastErrorMessage := e.message;
          If LastErrorMessage = '' then
            LastErrorMessage := rsUnknownDBError;
          SendDebug('transdb.StatSegInsert' + rsException + LastErrorMessage);
          RESULT := rcUnknownError;
        end;

    end;  // of TRY

  end; // of FUNCTION

Function StatSegUpdate(Const Lang:Tlang; Const Text:UTF8String;                //@028+
                                          Const TknCnt:Integer;
                                          Const Count:Integer):Integer;
  Const
    Table_Name = 'STATSEG';
  begin
   Query.Params.Clear;
   query.SQL.Text := 'UPDATE ' + table_name
                        + ' SET COUNT = COUNT + :count'
                        + ' WHERE HASH = :hash AND LANG = :lang';

   Query.ParamByName('hash').AsString := String2Hash(Text);
   Query.ParamByName('lang').AsString := Lang;
   Query.ParamByName('count').AsString := IntToStr(Count);

   TRY
         Query.ExecSQL;
         Case Query.RowsAffected of
           0: begin
                Result := rcZeroRowsAffected;
                LastErrorMessage := rsZeroRowsAffected;
              end;
           1: Result := rcNoProblem;
           else
             Result := Query.RowsAffected;
             LastErrorMessage := Format(rsXRowsAffected,
                                        [inttostr(Query.RowsAffected)]);
         end;  // of CASE
     except
      on E: Exception do
        begin
          LastErrorMessage := e.message;
          If LastErrorMessage = '' then
            LastErrorMessage := rsUnknownDBError;
          SendDebug('transdb.StatSegUpdate' + rsException + LastErrorMessage);
          RESULT := rcUnknownError;
        end;

    end;  // of TRY

  end; // of FUNCTION


Procedure StatSegAdd(Const Lang:Tlang; Const Text:UTF8String;
                                       Const TknCnt:Integer;
                                       Const Count:Integer);                    //@028+
  Var
    EntryExists:Boolean;                                                        //@029+
  begin
    // Use mdFragment because mdWord returns GLOSSARIES for now.
    //Result :=
   EntryExists := TextCheck(Lang, Text, mdFragment);                            //@029+
   If not EntryExists then                                                      //@029+
              EntryExists :=
                             (TextAdd(Lang, Text, mdFragment) = rcNoProblem);   //@029=
   If EntryExists then                                                          //@029=
     begin
       if SelectXFromYWhereZ_Check('STATSEG', 'HASH', String2Hash(Text), 'COUNT') then
         StatSegUpdate(Lang, Text, TknCnt, Count)
       else
         StatSegInsert(Lang, Text, TknCnt, Count);  // Should return 0

     end
    else // Segment Entry does not exist and could not be created
     Raise EDatabaseError.Create (rsSegmentEntryNotExist);                      //@029=
   end;

initialization // Unit Constructor
  SendMethodEnter('tranzdb.initialization');                                    //@004+
  LastErrorMessage := '';
  // DB Goes like this
  // Connection -> DataSource -> Query
  conn := TZConnection.Create(nil);
  DataSource := TDataSource.Create(nil);
  query := TZQuery.Create(nil);
//  transaction := TSQLTransaction.Create(nil);
  Try
// Set up the links
   conn.Protocol := 'sqlite-3';

// for testing only
{$IFDEF WIN32}                                                                  //@004+
//   db_path := 'c:\tranz.sqlite3';                                             //@002
   db_path := '..\Maruyaku\maruyaku.sqlite';
{$ELSE}
//   conn.Database := '/Users/shiruba/tranz.sqlite3';                           //@002-
//  db_path := GetAppConfigDir(False) + 'tranz.sqlite3';                        //@002+
// Temporary for testing with Maruyaku
  db_path := '../Maruyaku/maruyaku.sqlite';                                     //@002+@013+
{$ENDIF}                                                                        //@004+
  SendDebug(db_path);                                                           //@004+
  conn.Database := db_path;                                                     //@002+
// Bring the connection up
   Conn.connected := true;
   if not conn.connected = true then
     begin
       SendDebug(rsDBNotConnected);                                             //@013=
       Raise EDatabaseError.Create                                              //@012+
                                ('transdb initialization: ' + rsDBNotConnected);//@012+@013+
     end
   else SendDebug(rsDBConnected);                                               //@002+004=@013=
//   conn.Transaction := transaction;
//   query.DataSource := DataSource;
   query.connection := conn;
   SendDebug('Self-Test result:' + IntToStr(DBSelfTest)) ;                      //@002+004=

   except
    on E: Exception do
      begin                                                                     //@004+
        LastErrorMessage := E.message;
        SendDebug('Unit TranzDB: ' + rsInitError + LastErrorMessage); //@004+
      end;                                                                      //@004+
  end;
SendMethodExit('tranzdb.initialization');                                       //@004+

Finalization  // UNIT Destructor
  try                                                                           //@032+
    Query.Free;
    Conn.Free;
  except                                                                        //@032+
   on E : Exception do                                                          //@032+
    SendDebug('Unit TransDB: Exception during shutdown :' + e.Message);         //@032+
  end;                                                                          //@032+
end. // of UNIT

