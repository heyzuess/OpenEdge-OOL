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
  Date     : 12/4/2019
  Purpose  : OOL CGE Tron Program class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*           FROM ASSEMBLY.
USING System.Threading.* FROM ASSEMBLY.
USING OOL.CGE.*.
USING OOL.CGE.Tron.*.

CLASS OOL.CGE.Tron.Program USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS Program
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE STATIC VARIABLE myApp AS Program.

    DEFINE PRIVATE VARIABLE left                  AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE right                 AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE up                    AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE down                  AS INTEGER             NO-UNDO.

    DEFINE PRIVATE VARIABLE firstPlayerScore      AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE firstPlayerDirection  AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE firstPlayerColumn     AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE firstPlayerRow        AS INTEGER             NO-UNDO.
    
    DEFINE PRIVATE VARIABLE secondPlayerScore     AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerDirection AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerColumn    AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerRow       AS INTEGER             NO-UNDO.

    DEFINE PRIVATE VARIABLE isUsed                AS TwoDimensionalArray NO-UNDO.

    DEFINE PRIVATE VARIABLE heading               AS CHARACTER           NO-UNDO.
    DEFINE PRIVATE VARIABLE longestString         AS CHARACTER           NO-UNDO.
    DEFINE PRIVATE VARIABLE cursorLeft            AS INTEGER             NO-UNDO.

    DEFINE PRIVATE VARIABLE key                   AS ConsoleKeyInfo      NO-UNDO.

    DEFINE PRIVATE VARIABLE firstPlayerLoses      AS LOGICAL             NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerLoses     AS LOGICAL             NO-UNDO.
    DEFINE PRIVATE VARIABLE gameMessage           AS CHARACTER           NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i                   AS INTEGER             NO-UNDO.
    
    CONSTRUCTOR Program ():
        SUPER().

        DO ON ERROR UNDO, THROW:
            this:InitializeComponents().
        END.
        CATCH e AS Progress.Lang.Error:
            Console:Clear().
            Console:SetCursorPosition(0, 0).
            Console:ForegroundColor = ConsoleColor:Red.
            DO i_i = 1 TO e:NumMessages:
                Console:WriteLine(e:GetMessage(i_i)).
            END.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR Program ():
    END DESTRUCTOR.

    METHOD PUBLIC STATIC VOID Main ():
        Main("").
    END METHOD.

    METHOD PUBLIC STATIC VOID Main (INPUT args AS CHARACTER):
        myApp = NEW Program().
    END METHOD.

    METHOD PRIVATE VOID InitializeComponents ():
        this:left = 0.
        this:right = 1.
        this:up = 2.
        this:down = 3.
        this:firstPlayerScore = 0.
        this:firstPlayerDirection = this:right.
        this:firstPlayerColumn = 0.
        this:firstPlayerRow = 0.
        this:secondPlayerScore = 0.
        this:secondPlayerDirection = this:left.
        this:secondPlayerColumn = 40.
        this:secondPlayerRow = 5.

        this:SetGameField().
        this:StartupScreen().

        this:isUsed = NEW TwoDimensionalArray("LOGICAL", Console:WindowWidth, Console:WindowHeight).

        DO WHILE TRUE:
            IF Console:KeyAvailable THEN
            DO:
                this:key = Console:ReadKey(TRUE).
                IF this:key:Key = ConsoleKey:Escape THEN Environment:Exit(0).
                this:ChangePlayerDirection(this:key).
            END.

            this:MovePlayers().

            this:firstPlayerLoses = this:DoesPlayerLose(this:firstPlayerRow, this:firstPlayerColumn).
            this:secondPlayerLoses = this:DoesPlayerLose(this:secondPlayerRow, this:secondPlayerColumn).
        
            IF this:firstPlayerLoses OR this:secondPlayerLoses THEN
            DO:
                IF this:firstPlayerLoses AND this:secondPlayerLoses THEN
                DO:
                    this:firstPlayerScore  = this:firstPlayerScore  + 1.
                    this:secondPlayerScore = this:secondPlayerScore + 1.
                    this:gameMessage = "Draw game!!!".
                END.
                IF this:firstPlayerLoses THEN
                DO:
                   this:secondPlayerScore = this:secondPlayerScore + 1.
                   this:gameMessage = "Second player wins!!!".
                END.
                IF this:secondPlayerLoses THEN
                DO:
                    this:firstPlayerScore = this:firstPlayerScore + 1.
                    this:gameMessage = "First player wins!!!".
                END.

                Console:WriteLine().
                Console:WriteLine("Game Over").
                Console:WriteLine(this:gameMessage).
                Console:WriteLine(SUBSTITUTE("Current score: &1 - &2", this:firstPlayerScore, this:secondPlayerScore)).
                this:ResetGame().
            END.

            this:isUsed:SetValue(this:firstPlayerColumn, this:firstPlayerRow, TRUE).
            this:isUsed:SetValue(this:secondPlayerColumn, this:secondPlayerRow, TRUE).

            this:WriteOnPosition(this:firstPlayerColumn, this:firstPlayerRow, "*", ConsoleColor:Yellow).
            this:WriteOnPosition(this:secondPlayerColumn, this:secondPlayerRow, "*", ConsoleColor:Cyan).

            Thread:Sleep(100).
        END.
    END METHOD.

    METHOD PRIVATE VOID WriteOnPosition (INPUT iip_x     AS INTEGER,
                                         INPUT iip_y     AS INTEGER,
                                         INPUT cip_ch    AS CHARACTER,
                                         INPUT oip_color AS ConsoleColor):
        Console:ForegroundColor = oip_color.
        Console:SetCursorPosition(iip_x, iip_y).
        Console:Write(cip_ch).
    END METHOD.

    METHOD PRIVATE VOID ResetGame ():
        this:isUsed = NEW TwoDimensionalArray("LOGICAL", Console:WindowWidth, Console:WindowHeight).
        this:SetGameField().
        this:firstPlayerDirection = this:right.
        this:secondPlayerDirection = this:left.
        Console:WriteLine("Press any key to start again...").
        Console:ReadKey().
        Console:Clear().
        this:MovePlayers().
    END METHOD.

    METHOD PRIVATE LOGICAL DoesPlayerLose (INPUT iip_row AS INTEGER,
                                           INPUT iip_col AS INTEGER):
        IF iip_row < 0 THEN
            RETURN TRUE.
        IF iip_col < 0 THEN
            RETURN TRUE.
        IF iip_row >= Console:WindowHeight THEN
            RETURN TRUE.
        IF iip_col >= Console:WindowWidth  THEN
            RETURN TRUE.

        IF this:isUsed:GetLogValue(iip_col, iip_row) THEN
            RETURN TRUE.

        RETURN FALSE.
    END METHOD.

    METHOD PRIVATE VOID MovePlayers ():
        IF this:firstPlayerDirection = this:right THEN
            this:firstPlayerColumn = this:firstPlayerColumn + 1.
        IF this:firstPlayerDirection = this:left  THEN
            this:firstPlayerColumn = this:firstPlayerColumn - 1.
        IF this:firstPlayerDirection = this:up    THEN
            this:firstPlayerRow = this:firstPlayerRow - 1.
        IF this:firstPlayerDirection = this:down  THEN
            this:firstPlayerRow = this:firstPlayerRow + 1.

        IF this:secondPlayerDirection = this:right THEN
            this:secondPlayerColumn = this:secondPlayerColumn + 1.
        IF this:secondPlayerDirection = this:left  THEN
            this:secondPlayerColumn = this:secondPlayerColumn - 1.
        IF this:secondPlayerDirection = this:up    THEN
            this:secondPlayerRow = this:secondPlayerRow - 1.
        IF this:secondPlayerDirection = this:down  THEN
            this:secondPlayerRow = this:secondPlayerRow + 1.
    END METHOD.

    METHOD PRIVATE VOID ChangePlayerDirection (INPUT iip_key AS ConsoleKeyInfo):
        IF iip_key:Key = ConsoleKey:W AND this:firstPlayerDirection <> this:down  THEN
            this:firstPlayerDirection = this:up.
        IF iip_key:Key = ConsoleKey:A AND this:firstPlayerDirection <> this:right THEN
            this:firstPlayerDirection = this:left.
        IF iip_key:Key = ConsoleKey:D AND this:firstPlayerDirection <> this:left  THEN
            this:firstPlayerDirection = this:right.
        IF iip_key:Key = ConsoleKey:S AND this:firstPlayerDirection <> this:up    THEN
            this:firstPlayerDirection = this:down.

        IF iip_key:Key = ConsoleKey:UpArrow    AND this:secondPlayerDirection <> this:down  THEN
            this:secondPlayerDirection = this:up.
        IF iip_key:Key = ConsoleKey:LeftArrow  AND this:secondPlayerDirection <> this:right THEN
            this:secondPlayerDirection = this:left.
        IF iip_key:Key = ConsoleKey:RightArrow AND this:secondPlayerDirection <> this:left  THEN
            this:secondPlayerDirection = this:right.
        IF iip_key:Key = ConsoleKey:DownArrow  AND this:secondPlayerDirection <> this:up    THEN
            this:secondPlayerDirection = this:down.
    END METHOD.

    METHOD PRIVATE VOID SetGameField ():
        Console:WindowHeight = 30.
        Console:BufferHeight = 30.

        Console:WindowWidth = 100.
        Console:BufferWidth = 100.

        this:firstPlayerColumn = 0.
        this:firstPlayerRow = Console:WindowHeight / 2.

        this:secondPlayerColumn = Console:WindowWidth - 1.
        this:secondPlayerRow = Console:WindowHeight / 2.

        Console:CursorVisible = FALSE.
    END METHOD.

    METHOD PRIVATE VOID StartupScreen ():
        this:heading = "A simple tron-like game".
        Console:CursorLeft = Console:BufferWidth / 2 - LENGTH(this:heading) / 2.
        Console:WriteLine(this:heading).

        Console:ForegroundColor = ConsoleColor:Yellow.
        Console:WriteLine("Player 1's controls:\n").
        Console:WriteLine("W - Up").
        Console:WriteLine("A - Left").
        Console:WriteLine("S - Down").
        Console:WriteLine("D - Right").

        this:longestString = "Player 2's controls:".
        this:cursorLeft = Console:BufferWidth - LENGTH(this:longestString).

        Console:CursorTop = 1.
        Console:ForegroundColor = ConsoleColor:Cyan.
        Console:CursorLeft = this:cursorLeft.
        Console:WriteLine(this:longestString).
        Console:CursorLeft = this:cursorLeft.
        Console:WriteLine("Up Arrow - Up").
        Console:CursorLeft = this:cursorLeft.
        Console:WriteLine("Left Arrow - Left").
        Console:CursorLeft = this:cursorLeft.
        Console:WriteLine("Down Arrow - Down").
        Console:CursorLeft = this:cursorLeft.
        Console:WriteLine("Right Arrow - Right").

        Console:ReadKey().
        Console:Clear().
    END METHOD.
END CLASS.