unit TBAConst;
// Constants unit for Translation Block Analyzer tool
// Part of the Tranz2 suite.

// Copyright (C) 2009-2012 Noah SILVA
// ALL RIGHTS RESERVED
// @001 2011.01.12 Noah Silva : Added mode constants
// @002 2011.09.09 Noah Silva : Changed Company name
// @003 2012.01.12 Noah Silva : Added String Constants
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

ResourceString                                                                  //@003+
  msgError = 'Error';                                                           //@003+
  msgFwdCRDeleted = 'Forward cross-reference entry deleted.';                   //@003+
  msgRevCRDeleted = 'Reverse cross-reference entry deleted.';                   //@003+
  msgDestEntryUpdateOK = 'Destination text entry inserted/updated.';            //@003+
  msgDestEntryDeleteOK = 'Destination block text entry deleted.';               //@003+
  msgSrcEntryUpdateOk = 'Source text entry inserted/updated.';                  //@003+
  msgSrcEntryDeleteOk = 'Source block text entry deleted.';                     //@003+
  msgEntryPairCrUpdateOk =                                                      //@003+
           'Word pair text and bidirectional cross-reference inserted/updated.';//@003+
  msgNoTransAvail = '(no translation available)';                               //@003+
  msgRevCrUpdateOk = 'Reverse cross-reference entry inserted/updated.';         //@003+
  msgFwdCrUpdateOk = 'Forward cross-reference entry inserted/updated.';         //@003+
  msgDbTestOk = 'Database Self-Test OK.';                                       //@003+
  msgDbTestErr = 'Database Self-Test Failed.';                                  //@003+
  msgNoErrMsg = '(No error message)';                                           //@003+
  msgUnsupportedMode = 'Mode not supported.';                                   //@003+
  msgNotImplemented = 'Not Implemented';                                        //@003+

Const
 ApplicationName = 'Translation Block Analyzer';
// CopyrightMessage = 'Copyright 2009 by Takanawa Software';                    //@002-
 CopyrightMessage = 'Copyright 2009-2011 by Galapagos Software';                //@002+

implementation

end.

