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
USING System.Text.*                FROM ASSEMBLY.
USING System.Threading.*           FROM ASSEMBLY.
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

    DEFINE PRIVATE VARIABLE ScorePerLines       AS INTEGER               NO-UNDO EXTENT 5.
    DEFINE PRIVATE VARIABLE scoreManager        AS ScoreManager          NO-UNDO.
    DEFINE PRIVATE VARIABLE musicPlayer         AS MusicPlayer           NO-UNDO.
    DEFINE PRIVATE VARIABLE tetrisConsoleWriter AS TetrisConsoleWriter   NO-UNDO.
    DEFINE PRIVATE VARIABLE random              AS Random                NO-UNDO.
    DEFINE PRIVATE VARIABLE key                 AS ConsoleKeyInfo        NO-UNDO.
    DEFINE PRIVATE VARIABLE newFigure           AS Tetromino             NO-UNDO.
    DEFINE PRIVATE VARIABLE lines               AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE linesTemp           AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE rowIsFull           AS LOGICAL               NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i                 AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE i_j                 AS INTEGER               NO-UNDO.
    DEFINE PRIVATE VARIABLE i_k                 AS INTEGER               NO-UNDO.

    CONSTRUCTOR Program ():
        SUPER().
        DO ON ERROR UNDO, THROW:
            this:Main().
        END.
        CATCH e AS System.Exception:
            Console:Write("An issue occurred running application").
            Console:Write(e:Message).
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR Program ():
    END DESTRUCTOR.

    METHOD PRIVATE Array IntArray (INPUT iip_idx AS INTEGER):
        RETURN Array:CreateInstance(TypeHelper:GetType("System.Int32"), iip_idx).
    END METHOD.

    METHOD PRIVATE VOID Main ():
        this:TetrisRows = 20.
        this:tetrisCols = 10.
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
        this:ScorePerLines[1] = 0.
        this:ScorePerLines[2] = 40.
        this:ScorePerLines[3] = 100.
        this:ScorePerLines[4] = 300.
        this:ScorePerLines[5] = 1200.

        this:State = NEW TetrisGameState(this:TetrisRows, this:TetrisCols).
        this:scoreManager = NEW ScoreManager("scores.txt").
        this:musicPlayer = NEW MusicPlayer().
        //this:musicPlayer:Play().

        this:tetrisConsoleWriter = NEW TetrisConsoleWriter(this:TetrisRows, this:TetrisCols).
        this:random = NEW Random().

        this:tempTetromino = CAST(this:TetrisFigures:Item[this:random:Next(0, this:TetrisFigures:Count)], Tetromino).
        this:State:CurrentFigure = this:tempTetromino.

        DO WHILE TRUE:
            this:State:Frame = this:State:Frame + 1.
            this:State:UpdateLevel(this:scoreManager:Score).

            IF Console:KeyAvailable THEN
            DO:
                this:key = Console:ReadKey().
                IF this:key:Key = ConsoleKey:Escape THEN
                DO:
                    RETURN.
                END.
                IF this:key:Key = ConsoleKey:LeftArrow OR
                   this:key:Key = ConsoleKey:A         THEN
                DO:
                    IF this:State:CurrentFigureCol >= 1 THEN
                    DO:
                        this:State:CurrentFigureCol = this:State:CurrentFigureCol - 1.
                    END.
                END.
                IF this:key:Key = ConsoleKey:RightArrow OR
                   this:key:Key = ConsoleKey:D          THEN
                DO:
                    IF this:State:CurrentFigureCol < this:TetrisCols - this:State:CurrentFigure:Height THEN
                    DO:
                        this:State:CurrentFigureCol = this:State:CurrentFigureCol + 1.
                    END.
                END.
                IF this:key:Key = ConsoleKey:DownArrow OR
                   this:key:Key = ConsoleKey:S         THEN
                DO:
                    this:State:Frame = 1.
                    this:scoreManager:AddToScore(this:State:Level).
                    this:State:CurrentFigureRow = this:State:CurrentFigureRow + 1.
                END.
                IF this:key:Key = ConsoleKey:Spacebar OR
                   this:key:Key = ConsoleKey:UpArrow  OR
                   this:key:Key = ConsoleKey:W        THEN
                DO:
                    this:newFigure = this:State:CurrentFigure:GetRotate().
                    IF NOT this:Collision(this:newFigure) THEN
                    DO:
                        this:State:CurrentFigure = this:newFigure.
                    END.
                END.
            END.

            IF this:State:Frame MODULO (this:State:FramesToMoveFigure - this:State:Level) = 0 THEN
            DO:
                this:State:CurrentFigureRow = this:State:CurrentFigureRow + 1.
                this:State:Frame = 0.
            END.

            IF this:Collision(this:State:CurrentFigure) THEN
            DO:
                this:AddCurrentFigure().
                this:lines = this:CheckForFullLines().
                this:scoreManager:AddToScore(this:ScorePerLines[this:lines] * this:State:Level).
                this:tempTetromino = CAST(this:TetrisFigures:Item[this:random:Next(0, this:TetrisFigures:Count)], Tetromino).
                this:State:CurrentFigure = this:tempTetromino.
                this:State:CurrentFigureRow = 0.
                this:State:CurrentFigureCol = 0.

                IF this:Collision(this:State:CurrentFigure) THEN
                DO:
                    this:scoreManager:AddToHighScore().
                    this:tetrisConsoleWriter:DrawAll(this:State, this:scoreManager).
                    this:tetrisConsoleWriter:WriteGameOver(this:scoreManager:Score).
                    Thread:Sleep(100000).
                    RETURN.
                END.
            END.

            this:tetrisConsoleWriter:DrawAll(this:State, this:scoreManager).

            Thread:Sleep(40).
        END.
    END METHOD.

    METHOD PRIVATE INTEGER CheckForFullLines ():
        this:linesTemp = 0.
        DO i_i = 1 TO this:State:TetrisField:GetArraySize(1, ?):
            this:rowIsFull = TRUE.

            CHECK-BLK:
            DO i_j = 1 TO this:State:TetrisField:GetArraySize(2, 1):
                IF NOT this:State:TetrisField:GetLogValue(i_i, i_j) THEN
                DO:
                    this:rowIsFull = FALSE.
                    LEAVE CHECK-BLK.
                END.
            END.

            IF this:rowIsFull THEN
            DO:
                i_j = i_i.
                REPEAT WHILE i_j >= 1:
                    DO i_k = 1 TO this:State:TetrisField:GetArraySize(2, 1):
                        this:State:TetrisField:SetValue(i_j, i_k, this:State:TetrisField:GetLogValue(i_j - 1, i_k)).
                    END.

                    i_j = i_j - 1.
                    IF i_j = 1 THEN LEAVE.
                END.

                this:linesTemp = this:linesTemp + 1.
            END.
        END.

        RETURN this:linesTemp + 1.
    END METHOD.

    METHOD PRIVATE VOID AddCurrentFigure ():
        DO i_i = 1 TO this:State:CurrentFigure:Width:
            DO i_j = 1 TO this:State:CurrentFigure:Height:

                IF this:State:CurrentFigure:Body:GetLogValue(i_i, i_j) THEN
                DO:
                    this:State:TetrisField:SetValue(this:State:CurrentFigureRow + i_i,
                                                    this:State:CurrentFigureCol + i_j,
                                                    TRUE).
                END.
            END.
        END.
    END METHOD.

    METHOD PRIVATE LOGICAL Collision (INPUT figure AS Tetromino):
        IF this:State:CurrentFigureCol > this:TetrisCols - figure:Height THEN RETURN TRUE.

        IF this:State:CurrentFigureRow + figure:Width = this:TetrisRows  THEN RETURN TRUE.

        DO i_i = 1 TO figure:Width:
            DO i_j = 1 TO figure:Height:
                IF figure:Body:GetLogValue(i_i, i_j) AND
                   this:State:TetrisField:GetLogValue(this:State:CurrentFigureRow + i_i + 1,
                                                      this:State:CurrentFigureCol + i_j) THEN
                DO:
                    RETURN TRUE.
                END.
            END.
        END.

        RETURN FALSE.
    END METHOD.
END CLASS.