Unit BlockExtract;

// Modified LGPL License (Static Linking Allowed)
//@000 2011.10.01 Noah SILVA : Initial Version
//@001 2011.11.24 Noah Silva : Windows Compatibility
//@002 2011.11.25 Noah Silva + Additional error checking
//                           = Unicode fixes on windows
//@003 2011.12.12 Noah Silva + Work to add PPTX format
{ TODO 1 -oshiruba -cclean-up : refactor into abstract parent and document-type-specific child-classes }


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLRead,  DOM;                                             //@001=

TYPE
 TDocType=Byte;

CONST
  DT_Raw_XML = 0;
  DT_MS_xlsx = 1;
  DT_MS_PPTX = 2;
  DT_MS_DOCX = 3;
  DT_ODT     = 4;
  DT_ODS     = 5;
  DT_ODP     = 6;
  DT_Numbers = 7;
  DT_Keynote = 8;
  DT_Pages   = 9;
  DT_Unsupported = 255;                                                         //@001+


  {$M+} // Enables Published

  Type
    TBlockExtractor = Class(TObject)
      Protected
          _DocType:TDocType;
          _FileName:UTF8String;
          _BlockList:TStrings;
          _OutputPath:UTF8String;
          _ZipSubDir:UTF8String;                                                //@001+
          _XMLFileNameOnly:UTF8String;                                          //@001+
          _XMLFullPath:UTF8String;
          XMLDoc:TXMLDocument;
   //       ZipSubDir :UTF8String;
   //       DocFileName:UTF8String;
      Private
        Function LoadFile:Integer;
        procedure XML2Tree2;
        Function ProcessFile:Integer;
        Function ZipSubDir:UTF8String; Virtual; Abstract;
        Function DocFileName:UTF8String; Virtual; Abstract;
        procedure ExtractBlock(Node:TDOMNode); Virtual;                         //@001=
        Function GetBlocks:TStrings;
      Public
        Function SetFile(Const FileName:UTF8String):Integer;
        Constructor Create                           ;                          //@001=
        Constructor Create(Const FilePath:UTF8String);                          //@001=
        Destructor Destroy;                                                     //@001=
      Published
        Property Blocks:TStrings Read GetBlocks;                                //@001+
    end;


implementation

Uses

  DbugIntf, TransConst, zipper, ComCtrls, LCLProc,
  transdb, // for DECODE                                                        //@001+
  fileutil; // for FileExistsUTF8                                               //@002+

Function TBlockExtractor.LoadFile:Integer;
  var
    i:integer;
  //  ANSIFileName:ANSIString;
  Begin
    SendDebug(self.ClassName+'.LoadFile Called');
    // First we have to determine the zip firectory, etc.
    With TUnZipper.Create do
     try
       _DocType := DECODE(ExtractFileExt(_FileName),
                         '.docx', DT_MS_DOCX,
                         '.xlsx', DT_MS_XLSX,
                         '.odt', DT_ODT,
                         '.ods', DT_ODS,
                         '.pptx', DT_MS_PPTX,                                   //@003+
                          DT_Unsupported);                                      //@002+
       case _DocType of
         DT_MS_XLSX:begin
                      _ZipSubDir := 'xl';
                      _XMLFileNameOnly := 'sharedStrings.xml'
                    end;
        // DT_ODT:begin
        //        end;
         DT_MS_PPTX:begin                                                       //@003+
                     _ZipSubDir := 'ppt' +  DirectorySeparator + 'slides';      //@003+
                     // There is also slide2.xml, slide3.xml, etc.
                     _XMLFileNameOnly := 'slide1.xml';                          //@003+
                    end                                                         //@003+
         else                                                                   //@002+
          raise exception.Create('Currently unsupported Document Type: '        //@002+
                                                    + IntToStr(_DocType));      //@002+
       end;  // of CASE
       OutputPath := GetTempFileName(SysUtils.GetTempDir, // should use UTF8toAnsi?
                                              self.UnitName + self.ClassName);  //@001=
       _OutputPath := OutputPath; // keep this in sync for later use            //@003+
     //  ANSIFilename := UTF8toANSI(_Filename);                                 //@002+-
       // Make sure the input file (spreadsheet, etc., whatever) is there on the disk.
       If not FileExistsUTF8(_Filename) then                                    //@002+
         begin                                                                  //@002+
           SendDebug('Error! File input does not exist: '  + _Filename);        //@002+
           Result := -1;                                                        //@002+
           raise exception.Create('Input file does not exist: '  + _Filename);  //@002+
         end;

       UnZipAllFiles(UTF8toANSI(_Filename));                                    //@001=@002=
       SendDebug(inttostr(Entries.Count) +  ' files in zip archive.');
       _XMLFullPath := OutputPath + DirectorySeparator + _ZipSubDir             //@001+
                                        + DirectorySeparator + _XMLFileNameOnly;//@001+
       // Make sure the output XML file we are looking for is there
       If not FileExistsUTF8(_XMLFullPath) then                                 //@002=
         begin
           SendDebug('Error! Output file does not exist: '  + _XMLFullPath);
           Result := -1;
           raise exception.Create('File does not exist: '  + _XMLFullPath);     //@001+
         end;
       // we could a list of the temp files to delete...
       // anyway we can't delete them here because they haven't been read yet.
       //for i := 0 to entries.count-1 do SendDebug(entries[i].DiskFileName);
       // deletefile()
       //  removedir()

     Finally
       Free; // the unnamed unzipper object
       //delete the temp files? ...
     end;

  end;  // of FUNCTION

Function TBlockExtractor.SetFile(Const FileName:UTF8String):Integer;
  Begin
    _BlockList.Clear;
    _FileName := FileName;
    if LoadFile <> -1 then
      ProcessFile;
  end;


Function TBlockExtractor.ProcessFile:Integer;

// special handling, because we need to re-read for each slide page
  Procedure Handle_PPTX;                                                        //@003+
    Const
      Max_Slides = 1000;
     var
       SlideNo : Integer;
    begin
      XMLDoc := TXMLDocument.Create;
      For SlideNo := 1 to Max_Slides do
        begin
          _XMLFullPath := _OutputPath + DirectorySeparator + _ZipSubDir
              + DirectorySeparator + 'slide' + IntToStr(SlideNo) + '.xml';
              If not FileExistsUTF8(_XMLFullPath) then
                exit  // we reached the last file, exit silently
              else
                begin // file exists, read and process it.
                  ReadXMLFile(XMLDoc, _XMLFullPath) ;
                  XML2Tree2;
                end;  // of IF XML File exists
        end; // of FOR SlideNo
    end; // of Sub-PROCEDURE Handle_PPTX;

begin
  SendDebug(self.ClassName+'ProcessFile Called');
  try
    Case _DocType of
      DT_MS_PPTX:Handle_PPTX;                                                   //@003+
      DT_Unsupported : result := -1;                                            //@001+
      else // another supported document type                                   //@001+
        XMLDoc := TXMLDocument.Create;                                          //@001+
        ReadXMLFile(XMLDoc, _XMLFullPath) ;
        XML2Tree2;
    end;                                                                        //@001+
  finally
    XMLDoc.Free;
  end;
end;

procedure TBlockExtractor.ExtractBlock(Node:TDOMNode);                          //@001+
  begin
    Case _DocType Of                                                            //@001+
      DT_MS_XLSX:Begin                                                          //@001+
        If (Node.NodeName = 't') then  // MS Excel 2007
          begin
            if not assigned(node.FirstChild) then                               //@002+
              SendDebug(self.ClassName+'.ExtractBlock Node.FirstChild not assigned!') //@002+
            else                                                                //@002+
              If UTF16toUTF8(Node.FirstChild.NodeValue) <> '' then              //@003+
                _BlockList.append(
                                      UTF16toUTF8(Node.FirstChild.NodeValue));
//              SendDebug(UTF16toUTF8(Node.FirstChild.NodeValue));
          end;
        end;  // of xlsx                                                        //@001+
      DT_ODT:Begin                                                              //@001+
          if       (Node.NodeName = 'text:p') // OpenOffice.org Writer
           then
             if Node.ChildNodes.Count > 0 then
              if UTF16toUTF8(Node.firstchild.NodeValue) <> '' then              //@002+
                _BlockList.append(
                                      UTF16toUTF8(Node.firstchild.NodeValue));
   // there can be more siblings with further text
          if       (Node.NodeName = 'text:span') then // OpenOffice.org Writer
            if UTF16toUTF8(Node.firstchild.NodeValue) <> '' then                //@002+
               _BlockList.append(
                                    UTF16toUTF8(Node.firstchild.NodeValue));
          // we should keep checking for more... but how many?  eventually we
          // could have to move the pointer
      end; // of ODT                                                            //@001+
      DT_MS_PPTX:Begin  // Microsoft Powerpoint 2007/2008                       //@003+
                   if not assigned(node.FirstChild) then                               //@002+
                     SendDebug(self.ClassName+'.ExtractBlock Node.FirstChild not assigned!') //@002+
                   else                                                                //@002+
                     if (Node.NodeName = 'a:t') then                                  //@003+
                       IF UTF16toUTF8(Node.firstchild.NodeValue) <> '' then               //@003+
                         _BlockList.append(                                             //@003+
                                      UTF16toUTF8(Node.firstchild.NodeValue));    //@003+
             end;                                                               //@003+
          // still need to handle docx, ods, etc.
    end; // of case                                                             //@001+

  end;

procedure TBlockExtractor.XML2Tree2;

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
       ExtractBlock(Node);
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
    _BlockList.Clear;                                                           //@001+
    ParseXML(XMLDoc.DocumentElement, nil);
  end;

Constructor TBlockExtractor.Create                           ;                  //@001=
  Begin
    _BlockList := TStringList.Create;                                           //@001+
  end;


Constructor TBlockExtractor.Create(Const FilePath:UTF8String);                  //@001=
  Begin
    _BlockList := TStringList.Create;                                           //@001+
    SetFile(FilePath);                                                          //@001+
  end;


Function TBlockExtractor.GetBlocks:TStrings;
  Begin
//    SendDebug(self.ClassName+'GetBlocks Called');
    Result := _BlockList;
  end;

Destructor TBlockExtractor.Destroy;                                    //@001=
   Begin
     // Clean up temp files, etc.
     _BlockList.Free;                                                           //@001+
   end;

end.      // Of Unit

