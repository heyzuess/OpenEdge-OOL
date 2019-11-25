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

  Name     : TetrominoHelper.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris Program class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.TetrominoHelper INHERITS System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.TetrominoHelper
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE newFigure AS MultiDimensionalArray NO-UNDO.
    DEFINE PRIVATE VARIABLE i_row     AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE i_col     AS INTEGER               NO-UNDO.

    CONSTRUCTOR TetrominoHelper ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR TetrominoHelper ():
    END DESTRUCTOR.

    METHOD PUBLIC Tetromino GetRotate (INPUT oip_tetro AS Tetromino):
        this:newFigure = NEW MultiDimensionalArray().
        DO i_row = 1 TO oip_tetro:Width:
            DO i_col = 1 TO oip_tetro:Height:
                this:newFigure:SetValue(i_col - 1, 
                                        oip_tetro:Width - i_row - 1, 
                                        oip_tetro:GetCharValue(i_row - 1, i_col - 1)).
            END.
        END.

        RETURN NEW Tetromino(this:newFigure).
    END METHOD.
END CLASS.