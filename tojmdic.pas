unit toJMDic;
// JMDict Import Unit
// Note that before processing the JMDict XML file, the POS entries must have
// the & mark removed, as it leads to invalid entities.
// Modified LGPL License (Static Linking Allowed)
//@000 2012.02.19 Noah SILVA : Initial Version



{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbugIntf, TransConst,  XMLRead,  DOM, LCLProc,
  comctrls, // TTreeNode
  fileutil; // for FileExistsUTF8


Type
  TJMDicExtractor = Class(TObject)
    Protected
        _FileName:UTF8String;
        _EntryList:TStrings;
 //       _OutputPath:UTF8String;
 //       _ZipSubDir:UTF8String;
 //       _XMLFileNameOnly:UTF8String;
        _XMLFullPath:UTF8String;
        XMLDoc:TXMLDocument;
        _keb:UTF8String; // Kanji for current entry
        _reb:UTF8String; // Reading for current entry
        _pos:UTF8String; // part-of-speach for current sense
        _gloss:UTF8String; // Translation gloss for current sense
        _EntryCount:LongInt; // DB entries Added
    Private
      Function LoadFile:Integer;
      procedure XML2Tree2;
      Function ProcessFile:Integer;
      Function ZipSubDir:UTF8String; Virtual; Abstract;
      Function DocFileName:UTF8String; Virtual; Abstract;
      procedure ExtractEntry(Node:TDOMNode); Virtual;
      Procedure SaveCurrentEntry;
      Function GetEntries:TStrings;
    Public
      Function SetFile(Const FileName:UTF8String):Integer;
      Constructor Create                           ;
      Constructor Create(Const FilePath:UTF8String);
      Destructor Destroy;
    Published
      Property Entries:TStrings Read GetEntries;
  end;


implementation

 uses TransDB;

Procedure TJMDicExtractor.SaveCurrentEntry;
  Const
    Lang = 'JA';
    Mode = mdWord; // use Glossary
  begin
    // First the Kanji
    If _keb <> '' then
      If not TextCheck(Lang, _keb, Mode) then
        begin
          TextAdd(Lang, _keb, Mode);
          inc(_EntryCount);
        end;
    // Then the Reading (if any)
    If _reb <> '' then
      If not TextCheck(Lang, _reb, Mode) then
        begin
         TextAdd(Lang, _reb, Mode);
         inc(_EntryCount);
        end;
    If _EntryCount MOD 100 = 0 then
      SendDebug (IntToStr(_EntryCount) + ' Entries Added');

  end;

Function TJMDicExtractor.LoadFile:Integer;
  var
    i:integer;
  //  ANSIFileName:ANSIString;
  Begin
    // This procedure would unzip the XML file or perform some other preparation
    // In our case, we are using an XML file directly, so we just check that
    // it exists.
    SendDebug(self.ClassName+'.LoadFile Called');
     If not FileExistsUTF8(_Filename) then
         begin
           SendDebug('Error! File input does not exist: '  + _Filename);
           Result := -1;
           raise exception.Create('Input file does not exist: '  + _Filename);
         end;
 //    _XMLFileName := _FileName;
     _XMLFullPath := _FileName;
  end;  // of FUNCTION


Function TJMDicExtractor.SetFile(Const FileName:UTF8String):Integer;
  Begin
    _EntryList.Clear;
    _FileName := FileName;
    if LoadFile <> -1 then
      ProcessFile;
  end;


Function TJMDicExtractor.ProcessFile:Integer;

begin
  SendDebug(self.ClassName+'ProcessFile Called');
  try
      XMLDoc := TXMLDocument.Create;
      ReadXMLFile(XMLDoc, _XMLFullPath) ;
      XML2Tree2;
  finally
    XMLDoc.Free;
  end;
end;



procedure TJMDicExtractor.ExtractEntry(Node:TDOMNode);

  procedure process_entry_tag;
    begin
      // Save the current Entry before moving on
      SaveCurrentEntry;
      // new entry, so we clear all entry level variables
//      SendDebug('entry');
      _reb := '';
      _keb := '';
    end;

  procedure process_keb_tag;
    begin
     _keb :=   UTF16toUTF8(node.firstchild.NodeValue);
//     SendDebug('keb = ' + _keb);
    end;

   procedure process_reb_tag;
    begin
     _reb :=   UTF16toUTF8(node.firstchild.NodeValue);
//     SendDebug('reb = ' + _reb);
    end;

   procedure process_sense_tag;
   begin
    // clear whatever sense related variables
    _pos := '';
   end;

   procedure process_pos_tag;
   begin
     _pos :=   UTF16toUTF8(node.firstchild.NodeValue);
 //    SendDebug('pos = ' + _pos);

   end;

   procedure process_gloss_tag;
   begin
     _gloss :=   UTF16toUTF8(node.firstchild.NodeValue);
//     SendDebug('gloss = ' + _gloss);

   end;


  begin

    if       (Node.NodeName = 'entry') then  process_entry_tag
    else if (Node.NodeName = 'keb') then process_keb_tag
    else if (Node.NodeName = 'reb') then process_reb_tag
    else if (Node.NodeName = 'sense') then process_sense_tag
    else if (Node.NodeName = 'pos') then process_pos_tag
    else if (Node.NodeName = 'gloss') then process_gloss_tag;


    if false
           then
             if Node.ChildNodes.Count > 0 then
              if UTF16toUTF8(Node.firstchild.NodeValue) <> '' then
                _EntryList.append(
                                      UTF16toUTF8(Node.firstchild.NodeValue));
   // there can be more siblings with further text
         { if       (Node.NodeName = 'text:span') then // OpenOffice.org Writer
            if UTF16toUTF8(Node.firstchild.NodeValue) <> '' then
               _EntryList.append(
                                    UTF16toUTF8(Node.firstchild.NodeValue)); }




  end;

procedure TJMDicExtractor.XML2Tree2;

  procedure ParseXML(Node:TDOMNode;  TreeNode: TTreeNode);

  function GetNodeAttributesAsString(tNode:TDOMNode):string;
      var i:integer;
      begin
        Result:='';
        if tNode.HasAttributes then
         for i := 0 to tNode.Attributes.Length -1 do
           Result:=Result+UTF16toUTF8(Node.Attributes[i].NodeName)
                    +'="'+UTF16toUTF8(Node.Attributes[i].NodeValue)+'" ';
        Result:=Trim( UTF16toUTF8(Result));
      end;

    begin
      if Node = nil then Exit; // Stops if reached a leaf
        //Add nodes to TreeView
 //     TreeNode := TreeView.Items.AddChild(TreeNode,
//        Trim(UTF16toUTF8(Node.NodeName)+' '
//            +GetNodeAttributesAsString(Node)+ UTF16toUTF8(Node.NodeValue)));
      //***************
       ExtractEntry(Node);
      //***************
      //Continue the recursion
      Node:=Node.FirstChild;
      while Node <> Nil do
        begin
          ParseXML(Node,TreeNode);
          Node:=Node.NextSibling;
        end;
    end;

  begin
    _EntryList.Clear;
    ParseXML(XMLDoc.DocumentElement, nil);
  end;



Function TJMDicExtractor.GetEntries:TStrings;
  Begin
//    SendDebug(self.ClassName+'GetBlocks Called');
    Result := _EntryList;
  end;


Constructor TJMDicExtractor.Create;
  Begin
    _EntryList := TStringList.Create;
  end;


Constructor TJMDicExtractor.Create(Const FilePath:UTF8String);
  Begin
    _EntryList := TStringList.Create;
    SetFile(FilePath);
  end;


Destructor TJMDicExtractor.Destroy;
   Begin
     // Clean up temp files, etc.
     _EntryList.Free;
   end;

end.

