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

  Name     : Tetromino.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris Tetromino class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.Tetromino INHERITS System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.Tetromino
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Body       AS MultiDimensionalArray GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE newFigure AS MultiDimensionalArray NO-UNDO.
    DEFINE PRIVATE VARIABLE i_row     AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE i_col     AS INTEGER               NO-UNDO.

    DEFINE PUBLIC PROPERTY Width AS INTEGER
    GET ():
        RETURN this:Body:GetArraySize(1, ?).
    END GET.
    PUBLIC SET.

    DEFINE PUBLIC PROPERTY Height AS INTEGER
    GET ():
        RETURN this:Body:GetArraySize(2, 1).
    END GET.
    PUBLIC SET.

    CONSTRUCTOR Tetromino ():
        SUPER().
    END CONSTRUCTOR.

    CONSTRUCTOR Tetromino (INPUT oip_md AS MultiDimensionalArray):
        SUPER().
        this:Body = oip_md.
    END CONSTRUCTOR.

    DESTRUCTOR Tetromino ():
    END DESTRUCTOR.

    METHOD PUBLIC Object ToObject ():
        RETURN CAST(this, Object).
    END METHOD.

    METHOD PUBLIC Tetromino  GetRotate ():
        this:newFigure = NEW MultiDimensionalArray().
        DO i_row = 1 TO this:Width:
            DO i_col = 1 TO this:Height:
                this:newFigure:SetValue(i_col,
                                        this:Width - i_row + 1,
                                        this:Body:GetLogValue(i_row, i_col)).
            END.
        END.

        RETURN NEW Tetromino(this:newFigure).
    END METHOD.
END CLASS.