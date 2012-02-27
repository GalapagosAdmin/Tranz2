unit pattern;
// @000 2011.01.12 Noah Silva: First Implementation
// @001 2011.01.13 Noah Silva: Pattern_AutoSearch
// @002 2011.09.10 Noah SILVA: Adding debug statements,
//                             Ignore period as a token for patterns
// @003 2011.09.19 Noah SILVA: Additional Debug statements
//                             String -> UTF8String
// @004 2011.09.24 Noah SILVA: Bug fixes for Pattern_AutoSearch
// @005 2011.09.26 Noah SILVA: Short circuit one-word patterns
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TransConst;

// Returns TRUE if the string contains a pattern placeholder
  Function Contains_Pattern(Fragment:UTF8String):Boolean;                       //@003=
// Returns pattern without the placeholder.
  Function Strip_Pattern(Fragment:UTF8String):UTF8String;                       //@003=

 // Temporary for testing
  Function Pattern_AutoSearch(const SrcLang:TLang; const SrcText:UTF8String;    //@001+003=
                             const DestLang:TLang):UTF8String;                  //@001+003=
  Function Pattern_Coverage_AutoSearch(const SrcLang:TLang;                     //@001+
                             const SrcText:UTF8String;                          //@001+003=
                             const DestLang:TLang):Boolean;                     //@001+

implementation

Uses TransToken, TransTokenRemix, transdb,
  dbugintf;                                                                     //@002+

  Function Contains_Pattern(Fragment:UTF8String):Boolean;                       //@003=
    begin
      Result := (pos('@00', Fragment) > 0);
    end;

Function Strip_Pattern(Fragment:UTF8String):UTF8String;                         //@003=
  var
   p:Integer;
  Begin
    If Contains_Pattern(Fragment) then
     Begin
      p := pos('@00', fragment);
      // Get the left side
      Result := LeftStr(Fragment, (p-1));
      // Get the right side
      Result := Result + Trim(Copy(Fragment, p+4, length(Fragment)-(p+4)+2));
     end
    else  // No Placeholder, so we just dump the results out.
     Result := Fragment;
  end; // of FUNCTION


// Takes a template, which contains a placeholder, and the value of the place-
// holder, and combined them into an output string.
//Example:
//Input:
//Template="I went to the @001."
//Placeholder="store";
//Output:
//Result="I went to the store."

function Pattern_Combine(Const template:UTF8String;                             //@003=
                           Const placeholder:UTF8String):UTF8String;            //@003=
  var
   p:Integer;
   hidari_gawa:UTF8String;                                                      //@003=
   migi_gawa:UTF8String;                                                        //@003=
  Begin
    // find position of place holder
      p := pos('@00', template);
      // Get the left side
     // extract string, if any, up to that point, from template and append to result
      hidari_gawa := LeftStr(template, (p-1));
      // Get the right side
     // append placeholder contents into the result
      migi_gawa := Trim(Copy(template, p+4, length(template)-(p+4)+2));
     // append remaining string, if any, from template to result after placeholder
      Result := hidari_gawa + placeholder + migi_gawa;
  end;

// function which works similar to those in TransTokenRemix to be inserted here
// This one will create possible patterns from the given string or token list
// by replacing words with placeholders like this
// Input:
//   I have a kitty
// Output:
//   @001 have a kitty
//   I @001 a kitty
//   I have @001 kitty
//   I have a @001
//
// Next each output can be searched for a match in the pattern table
// When there is a match, the word(s) to be used in place of the placeholder
// will then be looked up to give a final result for each pattern.

  Function Pattern_AutoSearch(Const SrcLang:TLang; Const SrcText:UTF8String;    //@003=
                              Const DestLang:TLang;
                              Const AllowSrcPH:Boolean):UTF8String;             //@003=
   Var
     List:TTokenList;
     Replace, Token:Integer;   // The token to be replaced, and the token being processed
     Fragment:UTF8String;                                                       //@003=
     DBTemplateResult:UTF8String;                                               //@003=
     DBPlaceholderResult:UTF8String;                                            //@003=
     Max_Token:Integer;                                                         //@002+
     Max_Replace:Integer;                                                       //@002+
   begin
    // create the list?
   try
    result := '';
    List := TTokenList.Create;
    List := Tokenize(SrcLang, SrcText);
    If List.Count <= 1 then exit;                                               //@005+
    // Max_Token is the maximum token to be processed.  This should be the last
    // one that exists in the token list.
    Max_Token :=  List.Count -1 ;                                               //@002+
    ASSERT(Max_Token >= 0, 'pattern.pattern_autosearch: Invalid Max token!');   //@005+
    // Max_Replace is the maximum token that should be used as a placeholder,
    // this will be the last token, unless the last token is a period.
    // (Punctiation shouldn't ever be used as a placeholder)
    { TODO 2 -oshiruba -cnice to have : Fix for Japanese period }
    If List.Strings[Max_Token] = '.' then                                       //@002+
      Max_Replace := Max_Token - 1                                              //@002+
    else                                                                        //@002+
      Max_Replace := Max_Token;                                                 //@002+

    For Replace := 0 to Max_Replace do                                          //@002+
      Begin
        Fragment := '';
        For Token  := 0 to Max_Token do
          begin
            If Token = Replace then
              Fragment := TokenConcat(SrcLang, Fragment, '@001')
            else
              Fragment := TokenConcat(SrcLang, Fragment, List.Strings[Token])
          end;  // of FOR token
       // TokenConcat can put spaces, etc. in there, to make sure there are none
       // at the edges before calling anything which will calculate a hash.
       Fragment := Trim(Fragment);
       // periods.
//       SendDebug('Pattern.Pattern_AutoSearch: Testing:' + Fragment);
       // we now have one string where one of the words is replaced with a @001
       // Look it up in the DB
//       write(Replace, ':', Fragment);
//     Coverage := PatternCoverage(SrcLang, Fragment, DestLang);
       // We use pattern mode for the template, because the Tokenizer is already
       // searching for Fragments when it builds the coverage map at a higher
       // level.  Doing it Here would be redundant.

       // Note that the next line is necessary!  Bug in TranslateText?
   //    DBTemplateResult := ''; // just in case                                  //@004+
       DBTemplateResult := TranslateText(SrcLang, Fragment, DestLang, mdPattern);
       If DBTemplateResult <> '' then  // We got a hit, so let's check the
         Begin                         // the placeholder too
//           SendDebug('Pattern.Pattern_AutoSearch: PATTERN MATCH!: '+DBTemplateResult);
           // See if we got a hit on the placeholder
           // We got a match on the pattern (i.e. everything except the variable word)
           // Now we will try for a match on the word itself.
           { TODO 2 -oshiruba -cnice to have : Support for Japanese period }
           If List.Strings[Replace] = '.' Then continue; // this should check for other types of //@002+
//           SendDebug('Placeholder word to be translated: '+List.Strings[Replace]);
           DBPlaceholderResult := // For now force it to fragment mode
           // Though we could use auto to do recursive patterns.
                   TranslateText(SrcLang,
                                 List.Strings[Replace], DestLang, mdFragment);
           // If it isn't there as a TM fragment, try it as a word
           // Note: Avoid recursive auto/pattern mode since this is only one
           // token anyway
           If Length(DBPlaceholderResult) = 0 then                              //@004+
                 DBPlaceholderResult :=                                         //@004+
                    TranslateText(SrcLang,                                      //@004+
                                 List.Strings[Replace], DestLang, mdWord);      //@004+
//           SendDebug('DBPlaceHolderResult:'+DBPlaceHolderResult);
           If DBPlaceholderResult <> '' then // We got a hit on the placeholder too!
             begin
               Result := Pattern_Combine(DBTemplateResult, DBPlaceholderResult);
               SendDebug('Pattern.Pattern_AutoSearch: Success, Result = '
                                                                      + result);
               exit;  // one is enough, we are done (optimize later)
            end // of IF PlaceHolder
           else
             begin // Placeholder word not found                                //@002+
              If not AllowSrcPH then continue;                                  //@002+
              Result := Pattern_Combine(DBTemplateResult,
                                                         List.Strings[Replace]);//@002+
              exit;                                                             //@002+
             end;  // of Placeholder word not found                             //@002+
         end // of IF Template
       else
         // Either the pattern, or the word itself couldn't be translated, so
         // back out to non-pattern methods.
//         SendDebug('Pattern.Pattern_AutoSearch: No Match found.');
      end; // of FOR replace
   finally
    List.Free;
   end;  // of TRY..FINALLY
  end;  // of FUNCTION

  Function Pattern_AutoSearch(Const SrcLang:TLang; Const SrcText:UTF8String;    //@002+003=
                              Const DestLang:TLang):UTF8String;                 //@003=
 begin
   try                                                                          //@005+
 // First we try it in strict mode,
 //requiring that the placeholder words be translated
     Result :=  Pattern_AutoSearch(SrcLang, SrcText, DestLang, False);
 // If that doesn't yield any results, we try again, allowing the
 // placeholder word in the original language to appear in the results.
     If Length(Result) = 0 then
       Result :=  Pattern_AutoSearch(SrcLang, SrcText, DestLang, True);
     except                                                                     //@005+
       SendDebug('pattern.pattern_AutoSearch 3: Error during processing:'       //@005+
                                                              + SrcText);       //@005+
     end;                                                                       //@005+
 end;

  Function Pattern_Coverage_AutoSearch(Const SrcLang:TLang;
                                       Const SrcText:UTF8String;                //@003=
                                       Const DestLang:TLang):Boolean;
    Begin
      // It's not so easy to search for coverage on patterns without actually
      // doing the work, so for now, we will just call the real autosearch
      // function.  Later, I will have a cache table to hold this info.
      // e.x. HASH | RESULT:BOOLEAN
      try                                                                       //@003+
        Result := (Pattern_AutoSearch(SrcLang, SrcText, DestLang) <> '');
      except                                                                    //@003+
        SendDebug('pattern.pattern_coverage_autosearch: Error during processing.');      //@003+
      end;                                                                      //@003+
    end;

end.  // of Unit

