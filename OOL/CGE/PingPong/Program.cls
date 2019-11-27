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
  Purpose  : OOL CGE PingPong Program class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*           FROM ASSEMBLY.
USING System.Threading.* FROM ASSEMBLY.
USING OOL.CGE.PingPong.*.

CLASS OOL.CGE.PingPong.Program USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.PingPong.Program
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE firstPlayerPadSize   AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerPadSize  AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE ballPositionX        AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE ballPositionY        AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE ballDirectionUp      AS LOGICAL        NO-UNDO.
    DEFINE PRIVATE VARIABLE ballDirectionRight   AS LOGICAL        NO-UNDO.
    DEFINE PRIVATE VARIABLE firstPlayerPosition  AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerPosition AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE firstPlayerResult    AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE secondPlayerResult   AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE rng                  AS Random         NO-UNDO.

    DEFINE PRIVATE VARIABLE randomNumber         AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE keyInfo              AS ConsoleKeyInfo NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i                  AS INTEGER        NO-UNDO.

    CONSTRUCTOR Program ():
        this:Main().
    END CONSTRUCTOR.

    DESTRUCTOR Program ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID Main ():
        this:firstPlayerPadSize = 4.
        this:secondPlayerPadSize = 4.
        this:ballPositionX = 0.
        this:ballPositionY = 0.
        this:ballDirectionUp = TRUE.
        this:ballDirectionRight = FALSE.
        this:firstPlayerPosition = 0.
        this:secondPlayerPosition = 0.
        this:firstPlayerResult = 0.
        this:secondPlayerResult = 0.
        this:rng = NEW Random().

        this:RemoveScrollBars().
        this:SetInitialPositions().
        DO WHILE TRUE:
            IF Console:KeyAvailable THEN
            DO:
                this:keyInfo = Console:ReadKey().
                IF this:keyInfo:Key = ConsoleKey:UpArrow THEN
                    this:MoveFirstPlayerUp().
                IF this:keyInfo:Key = ConsoleKey:DownArrow THEN
                    this:MoveFirstPlayerDown().
            END.
            this:SecondPlayerAIMove().
            this:MoveBall().
            Console:Clear().
            this:DrawFirstPlayer().
            this:DrawSecondPlayer().
            this:DrawBall().
            this:PrintResult().
            Thread:Sleep(60).
        END.
    END METHOD.

    METHOD PRIVATE VOID RemoveScrollBars ():
        Console:ForegroundColor = ConsoleColor:Yellow.
        Console:BufferHeight = Console:WindowHeight.
        Console:BufferWidth = Console:WindowWidth.
    END METHOD.

    METHOD PRIVATE VOID DrawFirstPlayer ():
        DO i_i = this:firstPlayerPosition TO this:firstPlayerPosition + this:firstPlayerPadSize - 1:
            this:PrintAtPosition(0, this:i_i, "|").
            this:PrintAtPosition(1, this:i_i, "|").
        END.
    END METHOD.

    METHOD PRIVATE VOID DrawSecondPlayer ():
        DO i_i = this:secondPlayerPosition TO this:secondPlayerPosition + this:secondPlayerPadSize - 1:
            this:PrintAtPosition(Console:WindowWidth - 1, this:i_i, "|").
            this:PrintAtPosition(Console:WindowWidth - 2, this:i_i, "|").
        END.
    END METHOD.

    METHOD PRIVATE VOID PrintAtPosition (INPUT x      AS INTEGER,
                                         INPUT y      AS INTEGER,
                                         INPUT symbol AS CHARACTER):
        Console:SetCursorPosition(x, y).
        Console:Write(symbol).
    END METHOD.

    METHOD PRIVATE VOID SetInitialPositions ():
        this:firstPlayerPosition = Console:WindowHeight / 2 - this:firstPlayerPadSize / 2.
        this:secondPlayerPosition = Console:WindowHeight / 2 - this:secondPlayerPadSize / 2.
        this:SetBall().
    END METHOD.

    METHOD PRIVATE VOID SetBall ():
        this:ballPositionX = Console:WindowWidth / 2.
        this:ballPositionY = Console:WindowHeight / 2.
    END METHOD.

    METHOD PRIVATE VOID DrawBall ():
        this:PrintAtPosition(this:ballPositionX, this:ballPositionY, '@').
    END METHOD.

    METHOD PRIVATE VOID PrintResult ():
        Console:SetCursorPosition(INTEGER(Console:WindowWidth / 2 - 1), 0).
        Console:Write(SUBSTITUTE("&1-&2", this:firstPlayerResult, this:secondPlayerResult)).
    END METHOD.

    METHOD PRIVATE VOID MoveFirstPlayerDown ():
        IF this:firstPlayerPosition < Console:WindowHeight - this:firstPlayerPadSize THEN
            this:firstPlayerPosition = this:firstPlayerPosition + 1.
    END METHOD.

    METHOD PRIVATE VOID MoveFirstPlayerUp ():
        IF this:firstPlayerPosition > 0 THEN
            this:firstPlayerPosition = this:firstPlayerPosition - 1.
    END METHOD.

    METHOD PRIVATE VOID MoveSecondPlayerDown ():
        IF this:secondPlayerPosition < Console:WindowHeight - this:secondPlayerPadSize THEN
            this:secondPlayerPosition = this:secondPlayerPosition + 1.
    END METHOD.

    METHOD PRIVATE VOID MoveSecondPlayerUp ():
        IF this:secondPlayerPosition > 0 THEN
            this:secondPlayerPosition = this:secondPlayerPosition - 1.
    END METHOD.

    METHOD PRIVATE VOID SecondPlayerAIMove ():
        this:randomNumber = this:rng:Next(1, 101).
        IF this:randomNumber <= 70 THEN
        DO:
            IF this:ballDirectionUp THEN
                this:MoveSecondPlayerUp().
            ELSE
                this:MoveSecondPlayerDown().
        END.
    END METHOD.

    METHOD PRIVATE VOID MoveBall ():
        IF this:ballPositionY = 0 THEN
            this:ballDirectionUp = FALSE.
        IF this:ballPositionY = Console:WindowHeight - 1 THEN
            this:ballDirectionUp = TRUE.
        IF this:ballPositionX = Console:WindowWidth - 1 THEN
        DO:
            this:SetBall().
            this:ballDirectionRight = FALSE.
            this:ballDirectionUp = TRUE.
            this:firstPlayerResult = this:firstPlayerResult + 1.
            Console:SetCursorPosition(INTEGER(Console:WindowWidth / 2), INTEGER(Console:WindowHeight / 2)).
            Console:WriteLine("First player wins!").
            Console:ReadKey().
        END.
        IF THIS:ballPositionX = 0 THEN
        DO:
            this:SetBall().
            this:ballDirectionRight = TRUE.
            this:ballDirectionUp = TRUE.
            this:secondPlayerResult = this:secondPlayerResult + 1.
            Console:SetCursorPosition(INTEGER(Console:WindowWidth / 2), INTEGER(Console:WindowHeight / 2)).
            Console:WriteLine("Second player wins!").
            Console:ReadKey().
        END.
        IF this:ballPositionX < 3 THEN
        DO:
            IF this:ballPositionY >= this:firstPlayerPosition                           AND
               this:ballPositionY <  this:firstPlayerPosition + this:firstPlayerPadSize THEN
            DO:
                this:ballDirectionRight = TRUE.
            END.
        END.
        IF this:ballPositionX >= Console:WindowWidth - 4 THEN
        DO:
            IF this:ballPositionY >= this:secondPlayerPosition                            AND
               this:ballPositionY <  this:secondPlayerPosition + this:secondPlayerPadSize THEN
            DO:
                this:ballDirectionRight = FALSE.
            END.
        END.
        
        IF this:ballDirectionUp THEN
            this:ballPositionY = this:ballPositionY - 1.
        ELSE
            this:ballPositionY = this:ballPositionY + 1.

        IF this:ballDirectionRight THEN
            this:ballPositionX = this:ballPositionX + 1.
        ELSE
            this:ballPositionX = this:ballPositionX - 1.
    END METHOD.
END CLASS.