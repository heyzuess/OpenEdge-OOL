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
  Date     : 11/22/2019
  Purpose  : OOL CGE Car Program class

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
USING System.Collections.Generic.* FROM ASSEMBLY.
USING Progress.Util.*              FROM ASSEMBLY.
USING OOL.CGE.Cars.*.

CLASS OOL.CGE.Cars.Program USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Cars.Program
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE speed          AS DECIMAL             NO-UNDO.
    DEFINE PRIVATE VARIABLE acceleration   AS DECIMAL             NO-UNDO.
    DEFINE PRIVATE VARIABLE playfieldWidth AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE livesCount     AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE userCar        AS OOL.CGE.Cars.Object NO-UNDO.
    DEFINE PRIVATE VARIABLE rng            AS Random              NO-UNDO.
    DEFINE PRIVATE VARIABLE objects        AS "System.Collections.Generic.List<System.Object>" NO-UNDO.
    DEFINE PRIVATE VARIABLE hitted         AS LOGICAL             NO-UNDO.
    DEFINE PRIVATE VARIABLE chance         AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE newObject      AS OOL.CGE.Cars.Object NO-UNDO.
    DEFINE PRIVATE VARIABLE newCar         AS OOL.CGE.Cars.Object NO-UNDO.
    DEFINE PRIVATE VARIABLE pressedKey     AS ConsoleKeyInfo      NO-UNDO.
    DEFINE PRIVATE VARIABLE newList        AS "System.Collections.Generic.List<System.Object>" NO-UNDO.
    DEFINE PRIVATE VARIABLE i_i            AS INTEGER             NO-UNDO.
    DEFINE PRIVATE VARIABLE oldCar         AS OOL.CGE.Cars.Object NO-UNDO.
    DEFINE PRIVATE VARIABLE car            AS OOL.CGE.Cars.Object NO-UNDO.

    CONSTRUCTOR Program ():
        SUPER().
        this:Main().
    END CONSTRUCTOR.

    DESTRUCTOR Program ():
    END DESTRUCTOR.

    METHOD PRIVATE OOL.CGE.Cars.Object SysToObj (INPUT oip_obj AS System.Object):
        RETURN CAST(oip_obj, OOL.CGE.Cars.Object).
    END METHOD.

    METHOD PRIVATE VOID Main ():
        this:speed = 100.0.
        this:acceleration = 0.5.
        this:playfieldWidth = 5.
        this:livesCount = 5.
        Console:WindowHeight = 20.
        Console:BufferHeight = 20.
        Console:WindowWidth = 30.
        Console:BufferWidth = 30.
        this:userCar = NEW OOL.CGE.Cars.Object().
        this:userCar:x = 2.
        this:userCar:y = Console:WindowHeight - 1.
        this:userCar:c = '@'.
        this:userCar:color = ConsoleColor:Yellow.
        this:rng = NEW Random().
        this:objects = NEW "System.Collections.Generic.List<System.Object>"().

        DO WHILE TRUE:
            this:speed = this:speed + this:acceleration.
            IF this:speed > 400 THEN
            DO:
                this:speed = 400.
            END.

            this:hitted = FALSE.
            DO:
                this:chance = this:rng:Next(0, 100).

                IF this:chance < 10 THEN
                DO:
                    this:newObject = NEW OOL.CGE.Cars.Object().
                    this:newObject:color = ConsoleColor:Cyan.
                    this:newObject:c = "-".
                    this:newObject:x = this:rng:Next(0, this:playfieldWidth).
                    this:newObject:y = 0.
                    this:objects:Add(this:newObject).
                END.
                ELSE
                IF this:chance < 20 THEN
                DO:
                    this:newObject = NEW OOL.CGE.Cars.Object().
                    this:newObject:color = ConsoleColor:Cyan.
                    this:newObject:c = '*'.
                    this:newObject:x = this:rng:Next(0, this:playfieldWidth).
                    this:newObject:y = 0.
                    this:objects:Add(this:newObject).
                END.
                ELSE
                IF this:chance < 30 THEN
                DO:
                    this:newObject = NEW OOL.CGE.Cars.Object().
                    this:newObject:color = ConsoleColor:Magenta.
                    this:newObject:c = '^'.
                    this:newObject:x = this:rng:Next(0, this:playfieldWidth).
                    this:newObject:y = 0.
                    this:objects:Add(this:newObject).
                END.
                ELSE
                DO:
                    this:newCar = NEW OOL.CGE.Cars.Object().
                    this:newCar:color = ConsoleColor:Green.
                    this:newCar:x = this:rng:Next(0, this:playfieldWidth).
                    this:newCar:y = 0.
                    this:newCar:c = '#'.
                    this:objects:Add(this:newCar).
                END.
            END.

            DO WHILE Console:KeyAvailable:
                this:pressedKey = Console:ReadKey(TRUE).

                IF this:pressedKey:Key = ConsoleKey:LeftArrow  THEN
                DO:
                    IF this:userCar:x - 1 >= 0 THEN
                    DO:
                        this:userCar:x = this:userCar:x - 1.
                    END.
                END.
                ELSE
                IF this:pressedKey:Key = ConsoleKey:RightArrow THEN
                DO:
                    IF this:userCar:x + 1 < this:playfieldWidth THEN
                    DO:
                        this:userCar:x = this:userCar:x + 1.
                    END.
                END.
            END.
            this:newList = NEW "System.Collections.Generic.List<System.Object>"().
            DO i_i = 1 TO this:objects:Count:
                this:oldCar = this:SysToObj(this:objects:Item[i_i - 1]).
                this:newObject = NEW OOL.CGE.Cars.Object().
                this:newObject:x = this:oldCar:x.
                this:newObject:y = this:oldCar:y + 1.
                this:newObject:c = this:oldCar:c.
                this:newObject:color = this:oldCar:color.
                IF this:newObject:c = '*'            AND 
                   this:newObject:y = this:userCar:y AND
                   this:newObject:x = this:userCar:x THEN
                DO:
                    this:speed = this:speed - 20.
                END.
                IF this:newObject:c = '-'            AND
                   this:newObject:y = this:userCar:y AND
                   this:newObject:x = this:userCar:x THEN
                DO:
                    this:livesCount = this:livesCount + 1.
                END.
                IF this:newObject:c = '^'            AND
                   this:newObject:y = this:userCar:y AND
                   this:newObject:x = this:userCar:x THEN
                DO:
                    this:acceleration = this:acceleration + 1.5.
                END.
                IF this:newObject:c = '#'            AND
                   this:newObject:y = this:userCar:y AND
                   this:newObject:x = this:userCar:x THEN
                DO:
                    this:livesCount = this:livesCount - 1.
                    this:hitted = TRUE.
                    this:speed = 100.
                    this:acceleration = 0.5.
                    IF this:speed > 400 THEN
                    DO:
                        this:speed = 400.
                    END.
                    IF this:livesCount <= 0 THEN
                    DO:
                        this:PrintOnPosition(8, 10, "GAME OVER!!!", ConsoleColor:Red).
                        this:PrintOnPosition(8, 12, "Press [enter] to exit", ConsoleColor:Red).
                        Console:ReadLine().
                        Environment:Exit(0).
                    END.
                END.
                IF newObject:y < Console:WindowHeight THEN
                DO:
                    this:newList:Add(this:newObject).
                END.
            END.
            this:objects = this:newList.
            Console:Clear().
            IF this:hitted THEN
            DO:
                this:objects:Clear().
                this:PrintOnPosition(this:userCar, 'X', ConsoleColor:Red).
            END.
            ELSE
            DO:
                this:PrintOnPosition(this:userCar).
            END.
            DO i_i = 1 TO this:objects:Count:
                this:PrintOnPosition(this:SysToObj(this:objects:Item[i_i - 1])).
            END.

            this:PrintOnPosition(8, 4, "Lives: " + STRING(this:livesCount), ConsoleColor:White).
            this:PrintOnPosition(8, 5, "Speed: " + STRING(this:speed), ConsoleColor:White).
            this:PrintOnPosition(8, 6, "Acceleration: " + STRING(this:acceleration), ConsoleColor:White).
            Thread:Sleep(INTEGER(500 - this:speed)).
        END.
    END METHOD.

    METHOD PRIVATE VOID PrintOnPosition (INPUT x AS INTEGER,
                                         INPUT y AS INTEGER,
                                         INPUT c AS CHARACTER,
                                         INPUT d AS ConsoleColor):
        IF NOT VALID-OBJECT(d) OR d = ? THEN ASSIGN d = ConsoleColor:Gray.
        Console:SetCursorPosition(x, y).
        Console:ForegroundColor = d.
        Console:Write(c).
    END METHOD.

    METHOD PRIVATE VOID PrintOnPosition (INPUT oip_obj AS OOL.CGE.Cars.Object):
        IF NOT VALID-OBJECT(oip_obj) THEN RETURN.
        Console:SetCursorPosition(oip_obj:x, oip_obj:y).
        Console:ForegroundColor = oip_obj:color.
        Console:Write(oip_obj:c).
    END METHOD.

    METHOD PRIVATE VOID PrintOnPosition (INPUT oip_obj AS OOL.CGE.Cars.Object,
                                         INPUT c AS CHARACTER,
                                         INPUT d AS ConsoleColor):
        IF NOT VALID-OBJECT(oip_obj) THEN RETURN.
        IF NOT VALID-OBJECT(d) OR d = ? THEN ASSIGN d = ConsoleColor:Gray.
        Console:SetCursorPosition(oip_obj:x, oip_obj:y).
        Console:ForegroundColor = d.
        Console:Write(c).
    END METHOD.
END CLASS.