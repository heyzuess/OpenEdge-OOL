/*****************************************************************************************
  Copyright © 2019 by Jesse Iberri <jesseiberri@gmail.com>
  Released under the GNU General Public License GPL-3.0-or-later

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>

  Name     : test.p
  Author   : Jesse Iberri
  Date     : 12/12/2019
  Purpose  : OOL XML test (Application)
             This program can be used and modified to test functionality of
             XmlGenericSaxReader class.

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.XML.*.

{OOL/XML/XmlGenericSaxReaderTables.i}

DEFINE VARIABLE o_GenSaxReader AS XmlGenericSaxReader NO-UNDO.

o_GenSaxReader = NEW XmlGenericSaxReader().
o_GenSaxReader:XmlFileType = "GENERIC".
o_GenSaxReader:XmlFileName = "./OOL/XML/test.xml".
o_GenSaxReader:ParseXml({&OUTPUT_TABLE_DATA}).

MESSAGE '  Element Data:' TEMP-TABLE ttElements:HAS-RECORDS   SKIP
        'Attribute Data:' TEMP-TABLE ttAttributes:HAS-RECORDS SKIP
VIEW-AS ALERT-BOX TITLE 'Output Table Data'.

FOR EACH ttElements:
    DISPLAY ttElements.ttPath  FORMAT 'X(20)'
            ttElements.ttValue FORMAT 'X(40)'
    WITH FRAME A DOWN.
    DISPLAY ttElements WITH FRAME A DOWN TITLE 'Elements'.
    DOWN WITH FRAME A.
END.

FOR EACH ttAttributes:
    DISPLAY ttAttributes WITH FRAME B DOWN TITLE 'Attributes'.
    DOWN WITH FRAME B.
END.