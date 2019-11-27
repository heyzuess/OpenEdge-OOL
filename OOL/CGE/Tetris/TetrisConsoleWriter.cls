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

  Name     : TetrisConsoleWriter.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris TetrisConsoleWriter class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.TetrisConsoleWriter USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.TetrisConsoleWriter
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE tetrisRows     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE tetrisColumns  AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE infoColumns    AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE consoleRows    AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE consoleColumns AS INTEGER   NO-UNDO.

    DEFINE PRIVATE VARIABLE line           AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE middleLine     AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE endLine        AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i            AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_j            AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE tempValue      AS LOGICAL   NO-UNDO.

    CONSTRUCTOR TetrisConsoleWriter (INPUT iip_rows AS INTEGER,
                                     INPUT iip_cols AS INTEGER,
                                     INPUT iip_info AS INTEGER):
        SUPER().
        IF iip_info = ? THEN ASSIGN iip_info = 11.
        this:tetrisRows = iip_rows.
        this:tetrisColumns = iip_cols.
        this:infoColumns = iip_info.
        this:Main().
    END CONSTRUCTOR.

    CONSTRUCTOR TetrisConsoleWriter (INPUT iip_rows AS INTEGER,
                                     INPUT iip_cols AS INTEGER):
        SUPER().
        this:tetrisRows = iip_rows.
        this:tetrisColumns = iip_cols.
        this:infoColumns = 11.
        this:Main().
    END CONSTRUCTOR.

    DESTRUCTOR TetrisConsoleWriter ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID Main ():
        this:consoleRows = this:tetrisRows + 2.
        this:consoleColumns = this:tetrisColumns + this:infoColumns + 3.

        Console:WindowHeight = this:consoleRows + 1.
        Console:WindowWidth = this:consoleColumns.
        Console:BufferHeight = this:consoleRows + 1.
        Console:BufferWidth = this:consoleColumns.
        Console:ForegroundColor = ConsoleColor:Black.
        Console:Title = "Tetris".
        Console:CursorVisible = FALSE.
    END METHOD.

    METHOD PUBLIC VOID DrawAll (INPUT state        AS TetrisGameState,
                                INPUT scoreManager AS ScoreManager):
        this:DrawBorder().
        this:DrawGameState(this:tetrisColumns + 3, state, scoreManager).
        this:DrawTetrisField(state:TetrisField).
        this:DrawCurrentFigure(state:CurrentFigure, state:CurrentFigureRow, state:CurrentFigureCol).
    END METHOD.

    METHOD PUBLIC VOID DrawBorder ():
        Console:SetCursorPosition(0, 0).
        this:line = "╔".
        this:line = this:line + FILL('=', this:tetrisColumns).
        this:line = this:line + "╦".
        this:line = this:line + FILL('=', this:infoColumns).
        this:line = this:line + "╗".
        Console:Write(this:line).

        DO i_i = 1 TO this:tetrisRows:
            this:middleLine = "║".
            this:middleLine = this:middleLine + FILL(' ', this:tetrisColumns).
            this:middleLine = this:middleLine + "║".
            this:middleLine = this:middleLine + FILL(' ', this:infoColumns).
            this:middleLine = this:middleLine + "║".
            Console:Write(this:middleLine).
        END.

        this:endLine = "╚".
        this:endLine = this:endLine + FILL('=', this:tetrisColumns).
        this:endLine = this:endLine + "╩".
        this:endLine = this:endLine + FILL('=', this:infoColumns).
        this:endLine = this:endLine + "╝".
        Console:Write(this:endLine).
    END METHOD.

    METHOD PUBLIC VOID DrawGameState (INPUT startColumn  AS INTEGER,
                                      INPUT state        AS TetrisGameState,
                                      INPUT scoreManager AS ScoreManager):
        this:Write("Level:", 1, startColumn).
        this:Write(STRING(state:Level), 2, startColumn).
        this:Write("Score:", 4, startColumn).
        this:Write(STRING(scoreManager:Score), 5, startColumn).
        this:Write("Best:", 7, startColumn).
        this:Write(STRING(scoreManager:HighScore), 8, startColumn).
        this:Write("Frame:", 10, startColumn).
        this:Write(STRING(state:Frame) + " ~/ " + STRING(state:FramesToMoveFigure - state:Level), 11, startColumn).
        this:Write("Position:", 13, startColumn).
        this:Write(SUBSTITUTE("&1, &2", state:CurrentFigureRow, state:CurrentFigureCol), 14, startColumn).
        this:Write("Keys:", 16, startColumn).
        this:Write("  ^ ", 18, startColumn).
        this:Write("<   >", 19, startColumn).
        this:Write("  v ", 20, startColumn).
    END METHOD.

    METHOD PUBLIC VOID DrawTetrisField (INPUT tetrisField AS MultiDimensionalArray):
        DO i_i = 1 TO tetrisField:GetLength(1):
            this:line = "".
            DO i_j = 1 TO tetrisField:GetLength(2):
                this:tempValue = tetrisField:GetLogValue(i_i, i_j).

                IF this:tempValue THEN
                DO:
                    this:line = this:line + "*".
                END.
                ELSE
                DO:
                    this:line = this:line + " ".
                END.
            END.

            this:Write(this:line, i_i + 1, 1).
        END.
    END METHOD.

    METHOD PUBLIC VOID DrawCurrentFigure (INPUT currentFigure    AS Tetromino,
                                          INPUT currentFigureRow AS INTEGER,
                                          INPUT currentFigureCol AS INTEGER):
        DO i_i = 1 TO currentFigure:Width:
            DO i_j = 1 TO currentFigure:Height:
                this:tempValue = currentFigure:Body:GetLogValue(i_i, i_j).

                IF this:tempValue THEN
                DO:
                    this:Write("*", i_i + 1 + currentFigureRow, 1 + currentFigureColumn + i_j).
                END.
            END.
        END.
    END METHOD.

    METHOD PRIVATE VOID Write (INPUT cip_text AS CHARACTER,
                               INPUT iip_row  AS INTEGER,
                               INPUT iip_col  AS INTEGER):
        Console:SetCursorPosition(iip_col, iip_row).
        Console:Write(cip_text).
    END METHOD.
END CLASS.