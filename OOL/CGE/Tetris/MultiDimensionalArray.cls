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

  Name     : MultiDimensionalArray.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris MultiDimensionalArray class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.MultiDimensionalArray INHERITS System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.MultiDimensionalArray
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Count AS INTEGER GET. PRIVATE SET.

    DEFINE TEMP-TABLE arrayTable
        FIELD i_idx   AS INTEGER
        FIELD c_array AS CHARACTER.

    DEFINE PRIVATE VARIABLE i_i   AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE delim AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE temp  AS CHARACTER NO-UNDO.

    CONSTRUCTOR MultiDimensionalArray ():
        SUPER().
        this:delim = CHR(10).
    END CONSTRUCTOR.

    DESTRUCTOR MultiDimensionalArray ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID SetValue (INPUT iip_x   AS INTEGER,
                                 INPUT iip_y   AS INTEGER,
                                 INPUT cip_val AS CHARACTER):
        FOR FIRST arrayTable
            WHERE arrayTable.i_idx = iip_x:
        END.
        IF NOT AVAILABLE arrayTable THEN
        DO:
            CREATE arrayTable.
            ASSIGN this:Count = this:Count + 1.
        END.

        ASSIGN arrayTable.i_idx = iip_x.

        IF NUM-ENTRIES(arrayTable.c_array, delim) < iip_y THEN
        DO i_i = 1 TO iip_y - NUM-ENTRIES(arrayTable.c_array, delim):
            ASSIGN arrayTable.c_array = arrayTable.c_array + delim + "".
        END.

        ASSIGN ENTRY(iip_y, arrayTable.c_array, delim) = cip_val.
    END METHOD.

    METHOD PUBLIC VOID SetValue (INPUT iip_x   AS INTEGER,
                                 INPUT iip_y   AS INTEGER,
                                 INPUT iip_val AS INTEGER):
        this:SetValue(iip_x, iip_y, STRING(iip_val)).
    END METHOD.

    METHOD PUBLIC VOID SetValue (INPUT iip_x   AS INTEGER,
                                 INPUT iip_y   AS INTEGER,
                                 INPUT dip_val AS DECIMAL):
        this:SetValue(iip_x, iip_y, STRING(dip_val)).
    END METHOD.

    METHOD PUBLIC VOID SetValue (INPUT iip_x   AS INTEGER,
                                 INPUT iip_y   AS INTEGER,
                                 INPUT lip_val AS LOGICAL):
        this:SetValue(iip_x, iip_y, STRING(lip_val)).
    END METHOD.

    METHOD PUBLIC CHARACTER GetCharValue (INPUT iip_x AS INTEGER,
                                          INPUT iip_y AS INTEGER):
        FOR FIRST arrayTable
            WHERE arrayTable.i_idx = iip_x:
        END.
        IF NOT AVAILABLE arrayTable THEN RETURN ?.

        IF NUM-ENTRIES(arrayTable.c_array, delim) < iip_y THEN RETURN ?.

        RETURN ENTRY(iip_y, arrayTable.c_array, delim).
    END METHOD.

    METHOD PUBLIC INTEGER   GetIntValue (INPUT iip_x AS INTEGER,
                                         INPUT iip_y AS INTEGER):
        DO ON ERROR UNDO, THROW:
            RETURN INTEGER(this:GetCharValue(iip_x, iip_y)).
        END.
        CATCH e AS Progress.Lang.Error:
            RETURN ?.
        END CATCH.
    END METHOD.

    METHOD PUBLIC LOGICAL   GetLogValue (INPUT iip_x AS INTEGER,
                                         INPUT iip_y AS INTEGER):
        DO ON ERROR UNDO, THROW:
            RETURN LOGICAL(this:GetCharValue(iip_x, iip_y)).
        END.
        CATCH e AS Progress.Lang.Error:
            RETURN ?.
        END CATCH.
    END METHOD.

    METHOD PUBLIC DECIMAL   GetDecValue (INPUT iip_x AS INTEGER,
                                         INPUT iip_y AS INTEGER):
        DO ON ERROR UNDO, THROW:
            RETURN DECIMAL(this:GetCharValue(iip_x, iip_y)).
        END.
        CATCH e AS Progress.Lang.Error:
            RETURN ?.
        END CATCH.
    END METHOD.

    METHOD PUBLIC INTEGER GetLength (INPUT iip_idx AS INTEGER):
        FOR FIRST arrayTable
            WHERE arrayTable.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arrayTable THEN RETURN ?.

        RETURN NUM-ENTRIES(arrayTable.c_array, delim).
    END METHOD.

    METHOD PUBLIC CHARACTER GetArray (INPUT iip_idx AS INTEGER):
        FOR FIRST arrayTable
            WHERE arrayTable.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arrayTable THEN RETURN ?.

        RETURN "~{" + REPLACE(arrayTable.c_array, delim, ",") + "~}".
    END METHOD.

    METHOD PUBLIC OVERRIDE CHARACTER ToString ():
        temp = "".

        FOR EACH arrayTable:
            temp = temp + this:GetArray(arrayTable.i_idx) + ",".
        END.
        IF temp > "" THEN
        DO:
            ASSIGN temp = SUBSTRING(temp, 1, LENGTH(temp) - 1)
                   temp = "~{" + temp + "~}".
        END.

        RETURN temp.
    END METHOD.

    METHOD PUBLIC INTEGER GetArraySize (INPUT iip_idx AS INTEGER,
                                        INPUT iip_y   AS INTEGER):
        CASE iip_idx:
            WHEN 1 THEN RETURN this:Count.
            WHEN 2 THEN
            DO:
                FOR FIRST arrayTable
                    WHERE arrayTable.i_idx = iip_y:
                    
                    RETURN NUM-ENTRIES(arrayTable.c_array, delim).
                END.
            END.
            OTHERWISE RETURN ?.
        END CASE.
        RETURN 0.
    END METHOD.
END CLASS.