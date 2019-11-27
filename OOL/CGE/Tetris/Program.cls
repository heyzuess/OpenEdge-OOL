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

  Name     : Program.cls
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
USING System.*                     FROM ASSEMBLY.
USING System.Collections.Generic.* FROM ASSEMBLY.
USING Progress.Util.*              FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.Program USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.Program
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE TetrisRows          AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE TetrisCols          AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE TetrisFigures       AS "System.Collections.Generic.List<System.Object>" NO-UNDO.
    DEFINE PRIVATE VARIABLE State               AS TetrisGameState       NO-UNDO.

    DEFINE PRIVATE VARIABLE tempTetromino       AS Tetromino             NO-UNDO.
    DEFINE PRIVATE VARIABLE tempMDArray         AS MultiDimensionalArray NO-UNDO.

    DEFINE PRIVATE VARIABLE ScorePerLines       AS Array                 NO-UNDO.
    DEFINE PRIVATE VARIABLE scoreManager        AS ScoreManager          NO-UNDO.
    DEFINE PRIVATE VARIABLE musicPlayer         AS MusicPlayer           NO-UNDO.
    DEFINE PRIVATE VARIABLE tetrisConsoleWriter AS TetrisConsoleWriter   NO-UNDO.

    CONSTRUCTOR Program ():
        SUPER().
        this:Main().
    END CONSTRUCTOR.

    DESTRUCTOR Program ():
    END DESTRUCTOR.

    METHOD PRIVATE Array IntArray (INPUT iip_idx AS INTEGER):
        RETURN Array:CreateInstance(TypeHelper:GetType("System.Int32[]"), iip_idx).
    END METHOD.

    METHOD PRIVATE VOID Main ():
        this:TetrisFigures = NEW "System.Collections.Generic.List<System.Object>"().

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, TRUE).
        this:tempMDArray:SetValue(1, 2, TRUE).
        this:tempMDArray:SetValue(1, 3, TRUE).
        this:tempMDArray:SetValue(1, 4, TRUE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, TRUE).
        this:tempMDArray:SetValue(1, 2, TRUE).
        this:tempMDArray:SetValue(2, 1, TRUE).
        this:tempMDArray:SetValue(2, 2, TRUE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, FALSE).
        this:tempMDArray:SetValue(1, 2, TRUE).
        this:tempMDArray:SetValue(1, 3, FALSE).
        this:tempMDArray:SetValue(2, 1, TRUE).
        this:tempMDArray:SetValue(2, 2, TRUE).
        this:tempMDArray:SetValue(2, 3, TRUE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, FALSE).
        this:tempMDArray:SetValue(1, 2, TRUE).
        this:tempMDArray:SetValue(1, 3, TRUE).
        this:tempMDArray:SetValue(2, 1, TRUE).
        this:tempMDArray:SetValue(2, 2, TRUE).
        this:tempMDArray:SetValue(2, 3, FALSE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, TRUE).
        this:tempMDArray:SetValue(1, 2, TRUE).
        this:tempMDArray:SetValue(1, 3, FALSE).
        this:tempMDArray:SetValue(2, 1, FALSE).
        this:tempMDArray:SetValue(2, 2, TRUE).
        this:tempMDArray:SetValue(2, 3, TRUE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, TRUE).
        this:tempMDArray:SetValue(1, 2, FALSE).
        this:tempMDArray:SetValue(1, 3, FALSE).
        this:tempMDArray:SetValue(2, 1, TRUE).
        this:tempMDArray:SetValue(2, 2, TRUE).
        this:tempMDArray:SetValue(2, 3, TRUE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:tempMDArray = NEW MultiDimensionalArray().
        this:tempMDArray:SetValue(1, 1, FALSE).
        this:tempMDArray:SetValue(1, 2, FALSE).
        this:tempMDArray:SetValue(1, 3, TRUE).
        this:tempMDArray:SetValue(2, 1, TRUE).
        this:tempMDArray:SetValue(2, 2, TRUE).
        this:tempMDArray:SetValue(2, 3, TRUE).
        this:tempTetromino = NEW Tetromino(this:tempMDArray).
        this:TetrisFigures:Add(this:tempTetromino:ToObject()).

        this:ScorePerLines = this:IntArray(5).
        this:ScorePerLines:SetValue(0, 0).
        this:ScorePerLines:SetValue(1, 40).
        this:ScorePerLines:SetValue(2, 100).
        this:ScorePerLines:SetValue(3, 300).
        this:ScorePerLines:SetValue(4, 1200).

        this:State = NEW TetrisGameState(this:TetrisRows, this:TetrisCols).
        this:scoreManager = NEW ScoreManager("scores.txt").
        this:musicPlayer = NEW MusicPlayer().
        this:musicPlayer:Play().
    END METHOD.
END CLASS.