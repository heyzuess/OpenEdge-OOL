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

  Name     : XmlGenericSaxReader.cls
  Author   : Jesse Iberri
  Date     : 12/12/2019
  Purpose  : OOL XML XmlGenericSaxReader class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
BLOCK-LEVEL ON ERROR UNDO, THROW.
USING OpenEdge.Core.XML.SaxReader.
USING Progress.Lang.AppError.
USING OOL.XML.*.

CLASS OOL.XML.XmlGenericSaxReader USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.XML.XmlGenericSaxReader
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE TEMP-TABLE ttStack NO-UNDO
        FIELD ttiSequence    AS INTEGER      /* how deep is the node laying (the number of entries in ttcPath), starting at 1 */
        FIELD ttdeParentNode AS DECIMAL      /* identifying sequencenumber for parentnode */
        FIELD ttdeNode       AS DECIMAL      /* identifying sequencenumber for node */
        FIELD ttcLocalName   AS CHARACTER     /* parameter pcLocalName of startelement */
        FIELD ttcQName       AS CHARACTER     /* parameter pcQName of startelement */
        FIELD ttcPath        AS CHARACTER     /* path of the element (for example /Order/Header/BillingAdress/Address/City */
        INDEX ttidxSequence ttiSequence.

    {OOL/XML/XmlGenericSaxReaderTables.i}
/* Tables below are defined in above include and are included here for reference only
=============================================================================================================================
    DEFINE TEMP-TABLE ttElements NO-UNDO /* {&1} BEFORE-TABLE bittElements * OE10 only * */
        FIELD ttPath         AS CHARACTER /* path of the element (for example /Order/Header/BillingAdress/Address/City */
        FIELD ttparentNode   AS DECIMAL   /* identifying sequencenumber for parentnode */
        FIELD ttNode         AS DECIMAL   /* identifying sequencenumber for node */
        FIELD ttvalue        AS CHARACTER /* nodevalue */
        FIELD ttNameSpace    AS CHARACTER
        INDEX puttElements IS PRIMARY UNIQUE ttNode
        INDEX ipath ttPath.

    DEFINE TEMP-TABLE ttAttributes NO-UNDO /* {&1} BEFORE-TABLE bittAttributes * OE10 only * */
        FIELD ttNode         AS DECIMAL
        FIELD ttAttribName   AS CHARACTER
        FIELD ttAttribValue  AS CHARACTER
        INDEX puttAttributes IS PRIMARY UNIQUE ttNode ttAttribName.
=============================================================================================================================
*/

    DEFINE PUBLIC PROPERTY Parser AS SaxReader        GET. PRIVATE SET.
    
    DEFINE PUBLIC PROPERTY XmlFileMemptr AS MEMPTR    GET.
    PUBLIC  SET (INPUT mip_temp AS MEMPTR):
        ASSIGN this:XmlFileMemptr = mip_temp
               this:l_memptrParse = TRUE
               this:l_fileParse   = FALSE.
    END SET.

    DEFINE PUBLIC PROPERTY XmlFileName   AS CHARACTER GET.
    PUBLIC  SET (INPUT cip_temp AS CHARACTER):
        ASSIGN this:XmlFileName   = cip_temp
               this:l_fileParse   = TRUE
               this:l_memptrParse = FALSE.
    END SET.

    DEFINE PUBLIC PROPERTY XmlFileType   AS CHARACTER GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY ContextMode   AS LOGICAL   GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY Node          AS DECIMAL   GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY ParentNode    AS DECIMAL   GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY Sequence      AS INTEGER   GET. PUBLIC  SET.

    DEFINE PRIVATE VARIABLE ElementValue      AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE AttributeValue    AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE NodeStartElement  AS DECIMAL   NO-UNDO.

    DEFINE PRIVATE VARIABLE iPos              AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE iTeller           AS INTEGER   NO-UNDO.

    DEFINE PRIVATE VARIABLE l_fileParse       AS LOGICAL   NO-UNDO.
    DEFINE PRIVATE VARIABLE l_memptrParse     AS LOGICAL   NO-UNDO.

    DEFINE BUFFER xttStack FOR ttStack.
    
    DEFINE PUBLIC PROPERTY Path AS CHARACTER GET.
    PUBLIC SET (INPUT cip_temp AS CHARACTER):
        this:Path = cip_temp.
    END SET.

    DEFINE PUBLIC PROPERTY StackHandle AS HANDLE
    GET ():
        RETURN TEMP-TABLE ttStack:HANDLE.
    END GET.
    PRIVATE SET.

    CONSTRUCTOR XmlGenericSaxReader ():
        SUPER().
        this:ContextMode = TRUE.
    END CONSTRUCTOR.

    DESTRUCTOR XmlGenericSaxReader ():
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL GetContextMode ():
        RETURN this:ContextMode.
    END METHOD.

    METHOD PUBLIC VOID Characters (INPUT phReader AS HANDLE,
                                   INPUT charData AS LONGCHAR,
                                   INPUT numChars AS INTEGER):
        IF this:ContextMode THEN
            ASSIGN this:ElementValue = this:ElementValue + SUBSTRING(charData, 1, LENGTH(charData)).
            // No longer needed as the SaxReader obj cleans up the ampersand issue it seems
            // this:ElementValue = REPLACE(this:ElementValue, "~&", "~|").
    END METHOD.

    METHOD PUBLIC VOID EndDocument (INPUT phReader AS HANDLE):
    END METHOD.

    METHOD PUBLIC VOID EndElement (INPUT phSaxReader     AS HANDLE,
                                   INPUT picNamespaceURI AS CHARACTER,
                                   INPUT picLocalName    AS CHARACTER,
                                   INPUT picQName        AS CHARACTER):
        IF this:ContextMode THEN
        DO:
            ASSIGN iPos        = R-INDEX(this:Path, "/":U)
                   this:Path = IF iPos <= 1 THEN "" ELSE SUBSTRING(this:Path, 1, iPos - 1, "character":U).

            FIND LAST xttStack.
            DELETE xttStack.

            IF this:NodeStartElement = this:Node THEN
            FOR FIRST ttElements
                WHERE ttElements.ttNode = this:NodeStartElement
                AND   ttElements.ttValue = '':
                ttElements.ttValue = TRIM(this:ElementValue).
            END.

            this:ElementValue = "".
        END.
    END METHOD.

    METHOD PUBLIC VOID GetTableData (OUTPUT ttElementsJSON   AS LONGCHAR,
                                     OUTPUT ttAttributesJSON AS LONGCHAR):
        TEMP-TABLE ttElements:WRITE-JSON("LONGCHAR", ttElementsJSON, FALSE).
        TEMP-TABLE ttAttributes:WRITE-JSON("LONGCHAR", ttAttributesJSON, FALSE).
    END METHOD.

    METHOD PUBLIC VOID GetTables (OUTPUT TABLE ttElements,
                                  OUTPUT TABLE ttAttributes):
    END METHOD.

    METHOD PRIVATE VOID SyncEvents (INPUT lip_flag AS LOGICAL):
        IF lip_flag THEN
        DO:
            this:Parser:SaxReaderCharacters:Subscribe(Characters).
            this:Parser:SaxReaderStartDocument:Subscribe(StartDocument).
            this:Parser:SaxReaderEndDocument:Subscribe(EndDocument).
            this:Parser:SaxReaderStartElement:Subscribe(StartElement).
            this:Parser:SaxReaderEndElement:Subscribe(EndElement).
            this:Parser:SaxReaderWarning:Subscribe(Warning).
            this:Parser:SaxReaderError:Subscribe(Error).
            this:Parser:SaxReaderFatalError:Subscribe(FatalError).
        END.
        ELSE
        DO:
            this:Parser:SaxReaderCharacters:Unsubscribe(Characters).
            this:Parser:SaxReaderStartDocument:Unsubscribe(StartDocument).
            this:Parser:SaxReaderEndDocument:Unsubscribe(EndDocument).
            this:Parser:SaxReaderStartElement:Unsubscribe(StartElement).
            this:Parser:SaxReaderEndElement:Unsubscribe(EndElement).
            this:Parser:SaxReaderWarning:Unsubscribe(Warning).
            this:Parser:SaxReaderError:Unsubscribe(Error).
            this:Parser:SaxReaderFatalError:Unsubscribe(FatalError).
        END.
    END METHOD.

    METHOD PUBLIC VOID ParseXml (OUTPUT TABLE ttElements,
                                 OUTPUT TABLE ttAttributes):
        IF NOT this:l_fileParse AND NOT this:l_memptrParse THEN RETURN.

        EMPTY TEMP-TABLE ttElements.
        EMPTY TEMP-TABLE ttAttributes.

        this:Parser = NEW SaxReader().
        this:SyncEvents(TRUE).
        
        IF this:l_fileParse   THEN this:ParseXmlFile().
        ELSE
        IF this:l_memptrParse THEN this:ParseXmlMemptr().

        this:SyncEvents(FALSE).
        IF VALID-OBJECT(this:Parser) THEN DELETE OBJECT this:Parser.
    END METHOD.

    METHOD PRIVATE VOID ParseXmlFile ():
        DO ON ERROR UNDO, THROW:
            this:Parser:ParseFile(this:XmlFileName).
        END.
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    METHOD PRIVATE VOID ParseXmlMemptr ():
        DO ON ERROR UNDO, THROW:
            this:Parser:ParseDocument(this:XmlFileMemptr).
        END.
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    METHOD PUBLIC VOID StartDocument (INPUT phReader AS HANDLE):
        IF this:ContextMode THEN EMPTY TEMP-TABLE ttStack.
    END METHOD.

    METHOD PUBLIC VOID StartElement (INPUT phReader        AS HANDLE,
                                     INPUT picNamespaceURI AS CHARACTER,
                                     INPUT picLocalName    AS CHARACTER,
                                     INPUT picQName        AS CHARACTER,
                                     INPUT pihAttributes   AS HANDLE):
        IF this:ContextMode THEN
        DO:
            FIND LAST xttStack NO-ERROR.
            ASSIGN this:Sequence   = IF NOT AVAILABLE xttStack THEN 1 ELSE xttStack.ttiSequence + 1
                   this:ParentNode = IF NOT AVAILABLE xttStack THEN 0 ELSE xttStack.ttdeNode
                   this:Node       = this:Node + 1
                   this:Path       = IF NOT AVAILABLE xttStack THEN "/":U + picQName ELSE xttStack.ttcPath + "/":U + picQname.

            CREATE xttStack.
            ASSIGN xttStack.ttiSequence    = this:Sequence
                   xttStack.ttdeParentNode = this:ParentNode
                   xttStack.ttdeNode       = this:Node
                   xttStack.ttcLocalName   = picLocalName
                   xttStack.ttcQName       = picQName
                   xttStack.ttcPath        = this:Path.

            CREATE ttElements.
            ASSIGN ttElements.ttNode       = this:Node
                   ttElements.ttParentNode = this:ParentNode
                   ttElements.ttPath       = this:Path
                   ttElements.ttNameSpace  = picLocalName
                   this:NodeStartElement   = ttElements.ttNode.

            IF pihAttributes:NUM-ITEMS > 0 THEN
            DO iTeller = 1 TO pihAttributes:NUM-ITEMS:
                this:AttributeValue = pihAttributes:GET-VALUE-BY-QNAME(pihAttributes:GET-LOCALNAME-BY-INDEX(iTeller)).
                IF this:AttributeValue = ? THEN this:AttributeValue = pihAttributes:GET-VALUE-BY-INDEX(iTeller).

                CREATE ttAttributes.
                ASSIGN ttAttributes.ttNode        = this:Node
                       ttAttributes.ttAttribName  = pihAttributes:GET-LOCALNAME-BY-INDEX(iTeller)
                       ttAttributes.ttAttribValue = this:AttributeValue.
            END.
        END.
    END METHOD.

    METHOD PUBLIC VOID Proc_MsgParse_Status (INPUT ipc_code        AS CHARACTER,
                                             INPUT ipc_description AS CHARACTER,
                                             INPUT ipc_datetime    AS CHARACTER,
                                             INPUT ipi_sysno       AS INTEGER):
    END METHOD.

    METHOD PUBLIC VOID Warning (INPUT phReader AS HANDLE,
                                INPUT cip_error AS CHARACTER):
        UNDO, THROW NEW AppError("Warning: " + cip_error, 1).
    END METHOD.

    METHOD PUBLIC VOID Error (INPUT phReader AS HANDLE,
                              INPUT cip_error AS CHARACTER):
        UNDO, THROW NEW AppError("Error: " + cip_error, 2).
    END METHOD.

    METHOD PUBLIC VOID FatalError (INPUT phReader AS HANDLE,
                                   INPUT cip_error AS CHARACTER):
        UNDO, THROW NEW AppError("Fatal Error: " + cip_error, 3).
    END METHOD.
END CLASS.