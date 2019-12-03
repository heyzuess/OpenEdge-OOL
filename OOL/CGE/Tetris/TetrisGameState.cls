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

  Name     : TetrisGameState.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris TetrisGameState class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.TetrisGameState USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.TetrisGameState
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Frame              AS INTEGER               GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Level              AS INTEGER               GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY FramesToMoveFigure AS INTEGER               GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY CurrentFigure      AS Tetromino             GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY CurrentFigureRow   AS INTEGER               GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY CurrentFigureCol   AS INTEGER               GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY TetrisField        AS MultiDimensionalArray GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_i AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_j AS INTEGER NO-UNDO.

    CONSTRUCTOR TetrisGameState (INPUT tetrisRows    AS INTEGER,
                                 INPUT tetrisColumns AS INTEGER):
        SUPER().
        this:Frame = 0.
        this:Level = 1.
        this:FramesToMoveFigure = 15.
        this:CurrentFigure = ?.
        this:CurrentFigureRow = 0.
        this:CurrentFigureCol = 0.
        this:TetrisField = NEW MultiDimensionalArray().

        DO i_i = 1 TO tetrisRows:
            DO i_j = 1 TO tetrisColumns:
                this:TetrisField:SetValue(i_i, i_j, FALSE).
            END.
        END.
    END CONSTRUCTOR.

    DESTRUCTOR TetrisGameState ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID UpdateLevel (INPUT score AS INTEGER):
        IF score <= 0 THEN
        DO:
            this:Level = 1.
            RETURN.
        END.

        this:Level = INTEGER(Math:Log10(score)) - 1.
        IF this:Level < 1 THEN
            this:Level = 1.

        IF this:Level > 10 THEN
            this:Level = 10.
    END METHOD.
END CLASS.