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

  Name     : XmlGenericSaxReaderTables.i
  Author   : Jesse Iberri
  Date     : 12/12/2019
  Purpose  : OOL XML XmlGenericSaxReaderTables (Include)

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
&IF DEFINED(XmlGenericSaxReader_Tables) = 0 &THEN
&GLOBAL-DEFINE XmlGenericSaxReader_Tables

    &IF DEFINED(OUTPUT_TABLE_DATA) = 0 &THEN
    &GLOBAL-DEFINE OUTPUT_TABLE_DATA OUTPUT TABLE ttElements, OUTPUT TABLE ttAttributes
    &ENDIF

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
&ENDIF