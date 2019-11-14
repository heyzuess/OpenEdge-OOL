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

  Name     : ByteHelper.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL NET ByteHelper Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.NET.*.

&IF DEFINED(INT_BYTE_SIZE) = 0 &THEN
&SCOPED-DEFINE INT_BYTE_SIZE 32
&ENDIF

&IF DEFINED(INT64_BYTE_SIZE) = 0 &THEN
&SCOPED-DEFINE INT64_BYTE_SIZE 64
&ENDIF

CLASS OOL.NET.ByteHelper USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.NET.ByteHelper
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PROTECTED PROPERTY Value AS INTEGER GET. PROTECTED SET.

    DEFINE PRIVATE VARIABLE i_bitSize AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE c_string  AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_temp    AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_temp2   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_val     AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_a       AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_b       AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_i       AS INTEGER   NO-UNDO.

    CONSTRUCTOR ByteHelper ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR ByteHelper ():
    END DESTRUCTOR.

    METHOD PUBLIC CHARACTER ByteString (INPUT iip_value AS INTEGER):
        c_string = System.Convert:ToString(iip_value, 2).
        IF LENGTH(c_string) < i_bitSize THEN ASSIGN c_string = FILL("0", i_bitSize - LENGTH(c_string)) + c_string.
        RETURN c_string.
    END METHOD.

    METHOD PUBLIC CHARACTER ByteString (INPUT iip_value AS INT64):
        c_string = System.Convert:ToString(iip_value, 2).
        IF LENGTH(c_string) < i_bitSize THEN ASSIGN c_string = FILL("0", i_bitSize - LENGTH(c_string)) + c_string.
        RETURN c_string.
    END METHOD.

    METHOD PUBLIC INTEGER ToInt32 (INPUT cip_value AS CHARACTER):
        RETURN System.Convert:ToInt32(cip_value, 2).
    END METHOD.

    METHOD PUBLIC INT64 ToInt64 (INPUT cip_value AS CHARACTER):
        RETURN System.Convert:ToInt64(cip_value, 2).
    END METHOD.

    METHOD PUBLIC CHARACTER ComplementByteString (INPUT iip_value AS INTEGER):
        i_bitSize = {&INT_BYTE_SIZE}.
        c_val   = "".
        c_temp2 = this:ByteString(iip_value).
        DO i_i = 1 TO LENGTH(c_temp2):
            ASSIGN c_temp = SUBSTRING(c_temp2, i_i, 1).

            IF c_temp = "1" THEN
                ASSIGN c_val = c_val + "0".
            ELSE
            IF c_temp = "0" THEN
                ASSIGN c_val = c_val + "1".
        END.

        RETURN c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER ComplementByteString (INPUT iip_value AS INT64):
        i_bitSize = {&INT64_BYTE_SIZE}.
        c_val   = "".
        c_temp2 = this:ByteString(iip_value).
        DO i_i = 1 TO LENGTH(c_temp2):
            ASSIGN c_temp = SUBSTRING(c_temp2, i_i, 1).

            IF c_temp = "1" THEN
                ASSIGN c_val = c_val + "0".
            ELSE
            IF c_temp = "0" THEN
                ASSIGN c_val = c_val + "1".
        END.

        RETURN c_val.
    END METHOD.

    METHOD PUBLIC INTEGER Complement (INPUT iip_value AS INTEGER):
        RETURN this:ToInt32(this:ComplementByteString(iip_value)).
    END METHOD.

    METHOD PUBLIC INT64 Complement64 (INPUT iip_value AS INT64):
        RETURN this:ToInt64(this:ComplementByteString(iip_value)).
    END METHOD.

    METHOD PUBLIC CHARACTER NotByteString (INPUT iip_value AS INTEGER):
        RETURN this:ComplementByteString(iip_value).
    END METHOD.

    METHOD PUBLIC CHARACTER NotByteString (INPUT iip_value AS INT64):
        RETURN this:ComplementByteString(iip_value).
    END METHOD.

    METHOD PUBLIC INTEGER Not (INPUT iip_value AS INTEGER):
        RETURN this:Complement(iip_value).
    END METHOD.

    METHOD PUBLIC INT64 Not64 (INPUT iip_value AS INT64):
        RETURN this:Complement64(iip_value).
    END METHOD.

    METHOD PUBLIC CHARACTER AndByteString (INPUT iip_val1 AS INTEGER,
                                           INPUT iip_val2 AS INTEGER):
        i_bitSize = {&INT_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val1)
               c_temp2 = this:ByteString(iip_val2).

        DO i_i = 1 TO LENGTH(c_temp):
            ASSIGN i_a   = INTEGER(SUBSTRING(c_temp,  i_i, 1))
                   i_b   = INTEGER(SUBSTRING(c_temp2, i_i, 1))
                   c_val = c_val + STRING(i_a * i_b).
        END.
        RETURN c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER AndByteString (INPUT iip_val1 AS INT64,
                                           INPUT iip_val2 AS INT64):
        i_bitSize = {&INT64_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val1)
               c_temp2 = this:ByteString(iip_val2).

        DO i_i = 1 TO LENGTH(c_temp):
            ASSIGN i_a   = INTEGER(SUBSTRING(c_temp,  i_i, 1))
                   i_b   = INTEGER(SUBSTRING(c_temp2, i_i, 1))
                   c_val = c_val + STRING(i_a * i_b).
        END.
        RETURN c_val.
    END METHOD.

    METHOD PUBLIC INTEGER And (INPUT iip_val1 AS INTEGER,
                               INPUT iip_val2 AS INTEGER):
        RETURN this:ToInt32(this:AndByteString(iip_val1, iip_val2)).
    END METHOD.

    METHOD PUBLIC INT64 And64 (INPUT iip_val1 AS INT64,
                               INPUT iip_val2 AS INT64):
        RETURN this:ToInt64(this:AndByteString(iip_val1, iip_val2)).
    END METHOD.

    METHOD PUBLIC CHARACTER OrByteString (INPUT iip_val1 AS INTEGER,
                                          INPUT iip_val2 AS INTEGER):
        i_bitSize = {&INT_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val1)
               c_temp2 = this:ByteString(iip_val2).

        DO i_i = 1 TO LENGTH(c_temp):
            ASSIGN i_a   = INTEGER(SUBSTRING(c_temp,  i_i, 1))
                   i_b   = INTEGER(SUBSTRING(c_temp2, i_i, 1)).

            c_val = c_val + IF i_a = 0 AND i_b = 0 THEN "0" ELSE "1".
        END.
        RETURN c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER OrByteString (INPUT iip_val1 AS INT64,
                                          INPUT iip_val2 AS INT64):
        i_bitSize = {&INT64_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val1)
               c_temp2 = this:ByteString(iip_val2).

        DO i_i = 1 TO LENGTH(c_temp):
            ASSIGN i_a   = INTEGER(SUBSTRING(c_temp,  i_i, 1))
                   i_b   = INTEGER(SUBSTRING(c_temp2, i_i, 1)).

            c_val = c_val + IF i_a = 0 AND i_b = 0 THEN "0" ELSE "1".
        END.
        RETURN c_val.
    END METHOD.

    METHOD PUBLIC INTEGER Or (INPUT iip_val1 AS INTEGER,
                              INPUT iip_val2 AS INTEGER):
        RETURN this:ToInt32(this:OrByteString(iip_val1, iip_val2)).
    END METHOD.

    METHOD PUBLIC INT64 Or64 (INPUT iip_val1 AS INT64,
                              INPUT iip_val2 AS INT64):
        RETURN this:ToInt64(this:OrByteString(iip_val1, iip_val2)).
    END METHOD.

    METHOD PUBLIC CHARACTER XOrByteString (INPUT iip_val1 AS INTEGER,
                                           INPUT iip_val2 AS INTEGER):
        i_bitSize = {&INT_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val1)
               c_temp2 = this:ByteString(iip_val2).

        DO i_i = 1 TO LENGTH(c_temp):
            ASSIGN i_a = INTEGER(SUBSTRING(c_temp,  i_i, 1))
                   i_b = INTEGER(SUBSTRING(c_temp2, i_i, 1)).

            c_val = c_val + IF i_a <> i_b THEN "1" ELSE "0".
        END.
        RETURN c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER XOrByteString (INPUT iip_val1 AS INT64,
                                           INPUT iip_val2 AS INT64):
        i_bitSize = {&INT64_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val1)
               c_temp2 = this:ByteString(iip_val2).

        DO i_i = 1 TO LENGTH(c_temp):
            ASSIGN i_a = INTEGER(SUBSTRING(c_temp,  i_i, 1))
                   i_b = INTEGER(SUBSTRING(c_temp2, i_i, 1)).

            c_val = c_val + IF i_a <> i_b THEN "1" ELSE "0".
        END.
        RETURN c_val.
    END METHOD.

    METHOD PUBLIC INTEGER XOr (INPUT iip_val1 AS INTEGER,
                               INPUT iip_val2 AS INTEGER):
        RETURN this:ToInt32(this:XOrByteString(iip_val1, iip_val2)).
    END METHOD.

    METHOD PUBLIC INT64 XOr64 (INPUT iip_val1 AS INT64,
                               INPUT iip_val2 AS INT64):
        RETURN this:ToInt64(this:XOrByteString(iip_val1, iip_val2)).
    END METHOD.

    METHOD PUBLIC CHARACTER LeftShiftByteString (INPUT iip_val AS INTEGER,
                                                 INPUT iip_pos AS INTEGER):
        i_bitSize = {&INT_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val)
               c_temp2 = FILL(",", 7).

        i_a = 1.
        DO i_i = 1 TO iip_pos:
            i_a = i_a - 1.
            IF i_a = 0 THEN ASSIGN i_a = i_bitSize.
        END.

        DO i_i = 1 TO i_bitSize:
            ASSIGN ENTRY(i_a, c_temp2) = SUBSTRING(c_temp, i_i, 1).
            i_a = i_a + 1.
            IF i_a > i_bitSize THEN ASSIGN i_a = 1.
        END.

        ASSIGN c_val = REPLACE(c_temp2, ",", "").

        RETURN c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER LeftShiftByteString (INPUT iip_val AS INT64,
                                                 INPUT iip_pos AS INT64):
        i_bitSize = {&INT64_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val)
               c_temp2 = FILL(",", 7).

        i_a = 1.
        DO i_i = 1 TO iip_pos:
            i_a = i_a - 1.
            IF i_a = 0 THEN ASSIGN i_a = i_bitSize.
        END.

        DO i_i = 1 TO i_bitSize:
            ASSIGN ENTRY(i_a, c_temp2) = SUBSTRING(c_temp, i_i, 1).
            i_a = i_a + 1.
            IF i_a > i_bitSize THEN ASSIGN i_a = 1.
        END.

        ASSIGN c_val = REPLACE(c_temp2, ",", "").

        RETURN c_val.
    END METHOD.

    METHOD PUBLIC INTEGER LeftShift (INPUT iip_val AS INTEGER,
                                     INPUT iip_pos AS INTEGER):
        RETURN this:ToInt32(this:LeftShiftByteString(iip_val, iip_pos)).
    END METHOD.

    METHOD PUBLIC INT64 LeftShift64 (INPUT iip_val AS INT64,
                                     INPUT iip_pos AS INT64):
        RETURN this:ToInt64(this:LeftShiftByteString(iip_val, iip_pos)).
    END METHOD.

    METHOD PUBLIC CHARACTER RightShiftByteString (INPUT iip_val AS INTEGER,
                                                  INPUT iip_pos AS INTEGER):
        i_bitSize = {&INT_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val)
               c_temp2 = FILL(",", 7).

        i_a = 1.
        DO i_i = 1 TO iip_pos:
            ASSIGN i_a = i_a + 1.
            IF i_a > i_bitSize THEN ASSIGN i_a = 1.
        END.

        DO i_i = 1 TO i_bitSize:
            ASSIGN ENTRY(i_a, c_temp2) = SUBSTRING(c_temp, i_i, 1).
            i_a = i_a + 1.
            IF i_a > i_bitSize THEN ASSIGN i_a = 1.
        END.

        ASSIGN c_val = REPLACE(c_temp2, ",", "").

        RETURN c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER RightShiftByteString (INPUT iip_val AS INT64,
                                                  INPUT iip_pos AS INT64):
        i_bitSize = {&INT64_BYTE_SIZE}.
        c_val = "".
        ASSIGN c_temp  = this:ByteString(iip_val)
               c_temp2 = FILL(",", 7).

        i_a = 1.
        DO i_i = 1 TO iip_pos:
            ASSIGN i_a = i_a + 1.
            IF i_a > i_bitSize THEN ASSIGN i_a = 1.
        END.

        DO i_i = 1 TO i_bitSize:
            ASSIGN ENTRY(i_a, c_temp2) = SUBSTRING(c_temp, i_i, 1).
            i_a = i_a + 1.
            IF i_a > i_bitSize THEN ASSIGN i_a = 1.
        END.

        ASSIGN c_val = REPLACE(c_temp2, ",", "").

        RETURN c_val.
    END METHOD.

    METHOD PUBLIC INTEGER RightShift (INPUT iip_val AS INTEGER,
                                      INPUT iip_pos AS INTEGER):
        RETURN this:ToInt32(this:RightShiftByteString(iip_val, iip_pos)).
    END METHOD.

    METHOD PUBLIC INT64 RightShift64 (INPUT iip_val AS INT64,
                                      INPUT iip_pos AS INT64):
        RETURN this:ToInt64(this:RightShiftByteString(iip_val, iip_pos)).
    END METHOD.
END CLASS.