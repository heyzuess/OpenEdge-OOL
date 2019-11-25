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

    DEFINE TEMP-TABLE arrayTable
        FIELD i_x   AS INTEGER
        FIELD i_y   AS INTEGER
        FIELD c_val AS CHARACTER.

    DEFINE PRIVATE VARIABLE i_i AS INTEGER NO-UNDO.

    CONSTRUCTOR MultiDimensionalArray ():
    END CONSTRUCTOR.

    DESTRUCTOR MultiDimensionalArray ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID SetValue (INPUT iip_x   AS INTEGER,
                                 INPUT iip_y   AS INTEGER,
                                 INPUT cip_val AS CHARACTER):
        FOR FIRST arrayTable
            WHERE arrayTable.i_x = iip_x
            AND   arrayTable.i_y = iip_y:
        END.
        IF NOT AVAILABLE arrayTable THEN CREATE arrayTable.

        ASSIGN arrayTable.i_x   = iip_x
               arrayTable.i_y   = iip_y
               arrayTable.c_val = cip_val.
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
            WHERE arrayTable.i_x = iip_x
            AND   arrayTable.i_y = iip_y:
        END.
        IF NOT AVAILABLE arrayTable THEN
        DO:
            CREATE arrayTable.
            ASSIGN arrayTable.i_x   = iip_x
                   arrayTable.i_y   = iip_y
                   arrayTable.c_val = '0'.
        END.
        RETURN arrayTable.c_val.
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
        i_i = 0.
        CASE iip_idx:
            WHEN 0 THEN
            DO:
            END.
            WHEN 1 THEN
            DO:
            END.
            OTHERWISE RETURN ?.
        END CASE.
    END METHOD.
END CLASS.