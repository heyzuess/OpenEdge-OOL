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
  Date     : 11/21/2019
  Purpose  : OOL CGE Snake Program class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*                     FROM ASSEMBLY.
USING System.Linq.*                FROM ASSEMBLY.
USING System.Threading.*           FROM ASSEMBLY.
USING System.Collections.*         FROM ASSEMBLY.
USING System.Collections.Generic.* FROM ASSEMBLY.
USING Progress.Util.*              FROM ASSEMBLY.
USING OOL.CGE.Snake.*.

CLASS OOL.CGE.Snake.Program USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Snake.Program
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE right             AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE left              AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE down              AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE up                AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE lastFoodTime      AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE foodDissapearTime AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE negativePoints    AS INTEGER        NO-UNDO.

    DEFINE PRIVATE VARIABLE directions        AS Position       NO-UNDO EXTENT 4.
    DEFINE PRIVATE VARIABLE sleepTime         AS DECIMAL        NO-UNDO.
    DEFINE PRIVATE VARIABLE direction         AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE rng               AS Random         NO-UNDO.

    DEFINE PRIVATE VARIABLE obstacles         AS "System.Collections.Generic.List<System.Object>"  NO-UNDO.
    DEFINE PRIVATE VARIABLE snakeElements     AS "System.Collections.Generic.Queue<System.Object>" NO-UNDO.
    DEFINE PRIVATE VARIABLE obstacle          AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE food              AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE position          AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE userInput         AS ConsoleKeyInfo NO-UNDO.
    DEFINE PRIVATE VARIABLE snakeHead         AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE nextDirection     AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE snakeNewHead      AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE userPoints        AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE last              AS Position       NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i               AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE i_contains        AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE i_while           AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE tempR             AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE tempC             AS INTEGER        NO-UNDO.
    DEFINE PRIVATE VARIABLE tempColor         AS ConsoleColor   NO-UNDO.
    DEFINE PRIVATE VARIABLE tempPosition      AS Position       NO-UNDO.
    DEFINE PRIVATE VARIABLE replay            AS LOGICAL        NO-UNDO.

    CONSTRUCTOR Program ():
        this:Main().
    END CONSTRUCTOR.

    DESTRUCTOR Program ():
    END DESTRUCTOR.

    METHOD PRIVATE Object GetQueueObject (INPUT oip_queue AS "System.Collections.Generic.Queue<System.Object>",
                                          INPUT iip_idx   AS INTEGER):
        RETURN oip_queue:ToArray():GetValue(iip_idx).
    END METHOD.

    METHOD PRIVATE Object Last (INPUT oip_queue AS "System.Collections.Generic.Queue<System.Object>"):
        RETURN this:GetQueueObject(oip_queue, oip_queue:Count - 1).
    END METHOD.

    METHOD PRIVATE LOGICAL Contains (INPUT oip_queue AS "System.Collections.Generic.Queue<System.Object>",
                                     INPUT oip_pos   AS Position):
        IF NOT VALID-OBJECT(oip_pos) THEN RETURN FALSE.
        DO i_contains = 1 TO oip_queue:Count:
            this:tempPosition = CAST(this:GetQueueObject(oip_queue, i_contains - 1), Position).
            IF NOT VALID-OBJECT(this:tempPosition) THEN NEXT.

            IF this:tempPosition:col = oip_pos:col AND
               this:tempPosition:row = oip_pos:row THEN RETURN TRUE.
        END.
        RETURN FALSE.
    END METHOD.
    
    METHOD PRIVATE LOGICAL Contains (INPUT oip_list  AS "System.Collections.Generic.List<System.Object>",
                                     INPUT oip_pos   AS Position):
        IF NOT VALID-OBJECT(oip_pos) THEN RETURN FALSE.
        DO i_contains = 1 TO oip_list:Count:
            this:tempPosition = CAST(oip_list:Item[i_contains - 1], Position).
            IF NOT VALID-OBJECT(this:tempPosition) THEN NEXT.

            IF this:tempPosition:col = oip_pos:col AND
               this:tempPosition:row = oip_pos:row THEN RETURN TRUE.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC VOID Main ():
        this:right = 0.
        this:left = 1.
        this:down = 2.
        this:up = 3.
        this:lastFoodTime = 0.
        this:foodDissapearTime = 8000.
        this:negativePoints = 0.

        this:directions[1] = NEW Position(0, 1).  // right
        this:directions[2] = NEW Position(0, -1). // left
        this:directions[3] = NEW Position(1, 0).  // down
        this:directions[4] = NEW Position(-1, 0). // up

        this:sleepTime = 100.
        this:direction = this:right.
        this:rng = NEW Random().
        Console:BufferHeight = Console:WindowHeight.
        this:lastFoodTime = Environment:TickCount.

        this:obstacles = NEW "System.Collections.Generic.List<System.Object>" ().
        this:obstacles:Add(NEW Position(12, 12)).
        this:obstacles:Add(NEW Position(14, 20)).
        this:obstacles:Add(NEW Position(7, 7)).
        this:obstacles:Add(NEW Position(19, 19)).
        this:obstacles:Add(NEW Position(6, 9)).

        DO i_i = 1 TO this:obstacles:Count:
            this:obstacle = CAST(this:obstacles:Item[i_i - 1], Position).
            Console:ForegroundColor = ConsoleColor:Cyan.
            Console:SetCursorPosition(this:obstacle:col, this:obstacle:row).
            Console:Write("=").
        END.

        this:snakeElements = NEW "System.Collections.Generic.Queue<System.Object>" ().
        DO i_i = 0 TO 5:
            this:snakeElements:Enqueue(NEW Position(0, i_i)).
        END.

        i_while = 0.
        DO WHILE (this:Contains(this:snakeElements, this:food) OR
                  this:Contains(this:obstacles, this:food))    OR
                 i_while = 0:
            
            this:food = NEW Position(this:rng:Next(0, Console:WindowHeight),
                                     this:rng:Next(0, Console:WindowWidth)).
            i_while = i_while + 1.
        END.

        Console:SetCursorPosition(this:food:col, this:food:row).
        Console:ForegroundColor = ConsoleColor:Yellow.
        Console:Write("@").

        DO i_i = 1 TO this:snakeElements:Count:
            this:position = CAST(this:GetQueueObject(this:snakeElements, i_i - 1), Position).
            Console:SetCursorPosition(this:position:col, this:position:row).
            Console:ForegroundColor = ConsoleColor:DarkGray.
            Console:Write("*").
        END.

        ANIM-BLK:
        DO WHILE TRUE:
            this:negativePoints = this:negativePoints + 1.

            IF Console:KeyAvailable THEN
            DO:
                this:userInput = Console:ReadKey().
                IF this:userInput:Key = ConsoleKey:LeftArrow  THEN
                DO:
                    IF this:direction <> this:right THEN this:direction = this:left.
                END.
                IF this:userInput:Key = ConsoleKey:RightArrow THEN
                DO:
                    IF this:direction <> this:left  THEN this:direction = this:right.
                END.
                IF this:userInput:Key = ConsoleKey:UpArrow    THEN
                DO:
                    IF this:direction <> this:down  THEN this:direction = this:up.
                END.
                IF this:userInput:Key = ConsoleKey:DownArrow  THEN
                DO:
                    IF this:direction <> this:up    THEN this:direction = this:down.
                END.
                IF this:userInput:Key = ConsoleKey:Escape THEN
                DO:
                    LEAVE ANIM-BLK.
                END.
                IF this:userInput:Key = ConsoleKey:Spacebar THEN
                DO:
                    this:tempC = Console:CursorLeft.
                    this:tempR = Console:CursorTop.
                    this:tempColor = Console:ForegroundColor.
                    Console:SetCursorPosition(INTEGER(Console:WindowWidth / 2 - 6), 
                                              INTEGER(Console:WindowHeight / 2)).
                    Console:ForegroundColor = ConsoleColor:White.
                    Console:Write("- PAUSED -").
                    WAIT-BLK:
                    REPEAT:
                        this:userInput = Console:ReadKey().
                        IF this:userInput:Key = ConsoleKey:Spacebar THEN LEAVE WAIT-BLK.
                    END.
                    Console:SetCursorPosition(INTEGER(Console:WindowWidth / 2 - 6), 
                                              INTEGER(Console:WindowHeight / 2)).
                    Console:Write("          ").
                    Console:SetCursorPosition(this:tempC, this:tempR).
                    Console:ForegroundColor = this:tempColor.
                    this:lastFoodTime = Environment:TickCount.
                END.
            END.

            this:snakeHead = CAST(this:Last(this:snakeElements), Position).
            this:nextDirection = this:directions[this:direction + 1].
            
            this:snakeNewHead = NEW Position(this:snakeHead:row + this:nextDirection:row,
                                             this:snakeHead:col + this:nextDirection:col).

            IF this:snakeNewHead:col < 0 THEN this:snakeNewHead:col = Console:WindowWidth  - 1.
            IF this:snakeNewHead:row < 0 THEN this:snakeNewHead:row = Console:WindowHeight - 1.
            IF this:snakeNewHead:row >= Console:WindowHeight THEN this:snakeNewHead:row = 0.
            IF this:snakeNewHead:col >= Console:WindowWidth  THEN this:snakeNewHead:col = 0.

            IF this:Contains(this:snakeElements, this:snakeNewHead) OR
               this:Contains(this:obstacles, this:snakeNewHead)     THEN
            DO:
                Console:SetCursorPosition(0, 0).
                Console:ForegroundColor = ConsoleColor:Red.
                Console:WriteLine("Game Over!").
                this:userPoints = (this:snakeElements:Count - 6) * 100 - this:negativePoints.
                this:userPoints = MAX(this:userPoints, 0).
                Console:WriteLine("Your points are: " + STRING(this:userPoints)).
                Console:ReadKey().
                RETURN.
            END.

            Console:SetCursorPosition(this:snakeHead:col, this:snakeHead:row).
            Console:ForegroundColor = ConsoleColor:DarkGray.
            Console:Write("*").

            this:snakeElements:Enqueue(this:snakeNewHead).
            Console:SetCursorPosition(this:snakeNewHead:col, this:snakeNewHead:row).
            Console:ForegroundColor = ConsoleColor:Gray.
            IF this:direction = this:right THEN Console:Write(">").
            IF this:direction = this:left  THEN Console:Write("<").
            IF this:direction = this:up    THEN Console:Write("^").
            IF this:direction = this:down  THEN Console:Write("v").

            IF this:snakeNewHead:col = this:food:col AND
               this:snakeNewHead:row = this:food:row THEN
            DO:
                i_while = 0.
                DO WHILE (this:Contains(this:snakeElements, this:food) OR
                          this:Contains(this:obstacles, this:food))    OR
                         i_while = 0:
                    
                    this:food = NEW Position(this:rng:Next(0, Console:WindowHeight),
                                             this:rng:Next(0, Console:WindowWidth)).
                    i_while = i_while + 1.
                END.
                this:lastFoodTime = Environment:TickCount.
                Console:SetCursorPosition(this:food:col, this:food:row).
                Console:ForegroundColor = ConsoleColor:Yellow.
                Console:Write("@").
                this:sleepTime = this:sleepTime - 1.

                this:obstacle = NEW Position().
                i_while = 0.
                DO WHILE (this:Contains(this:snakeElements, this:obstacle) OR
                          this:Contains(this:obstacles, this:obstacle)     OR
                         (this:food:row <> this:obstacle:row         AND 
                          this:food:col <> this:obstacle:col))       OR
                         i_while = 0:
                    
                    this:obstacle = NEW Position(this:rng:Next(0, Console:WindowHeight),
                                                 this:rng:Next(0, Console:WindowWidth)).
                    i_while = i_while + 1.
                END.
                this:obstacles:Add(this:obstacle).
                Console:SetCursorPosition(this:obstacle:col, this:obstacle:row).
                Console:ForegroundColor = ConsoleColor:Cyan.
                Console:Write("=").
            END.
            ELSE
            DO:
                this:last = CAST(this:snakeElements:Dequeue(), Position).
                Console:SetCursorPosition(this:last:col, this:last:row).
                Console:Write(" ").
            END.

            IF Environment:TickCount - this:lastFoodTime >= this:foodDissapearTime THEN
            DO:
                this:negativePoints = this:negativePoints + 50.
                Console:SetCursorPosition(this:food:col, this:food:row).
                Console:Write(" ").

                i_while = 0.
                DO WHILE (this:Contains(this:snakeElements, this:food) OR
                          this:Contains(this:obstacles, this:food)) OR
                         i_while = 0:

                    this:food = NEW Position(this:rng:Next(0, Console:WindowHeight),
                                             this:rng:Next(0, Console:WindowWidth)).
                    i_while = i_while + 1.
                END.
                this:lastFoodTime = Environment:TickCount.
            END.

            Console:SetCursorPosition(this:food:col, this:food:row).
            Console:ForegroundColor = ConsoleColor:Yellow.
            Console:Write("@").

            this:sleepTime = this:sleepTime - 0.01.
            Thread:Sleep(INTEGER(this:sleepTime)).
        END.
    END METHOD.
END CLASS.