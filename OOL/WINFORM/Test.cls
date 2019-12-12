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

  Name     : Test.cls
  Author   : Jesse Iberri
  Date     : 12/11/2019
  Purpose  : OOL WINFORM Test Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.WINFORM.*.

CLASS OOL.WINFORM.Test USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.WINFORM.Test
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE STATIC VARIABLE testApp AS OOL.WINFORM.Test NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i      AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_j      AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE c_temp   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_line   AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE o_grid   AS Grid      NO-UNDO.
    DEFINE PRIVATE VARIABLE o_sprite AS Sprite    NO-UNDO.
    DEFINE PRIVATE VARIABLE o_ctrl   AS Tile      NO-UNDO.

    DEFINE PRIVATE VARIABLE key      AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE c_disp   AS CHARACTER VIEW-AS EDITOR SIZE 40 BY 20.

    DEFINE FRAME A
        c_disp NO-LABEL
    WITH NO-BOX.

    CONSTRUCTOR Test ():
        SUPER().
        DO ON ERROR UNDO, THROW:
            this:InitializeComponents().
        END.
        CATCH e AS Progress.Lang.Error:
            DO i_i = 1 TO e:NumMessages:
                MESSAGE e:GetMessage(i_i)
                VIEW-AS ALERT-BOX ERROR.
            END.
        END CATCH.
    END CONSTRUCTOR.

    METHOD PUBLIC STATIC VOID Main ():
        testApp = NEW Test().
    END METHOD.

    METHOD PRIVATE VOID InitializeComponents ():
        this:o_grid = NEW Grid(20, 40).
        this:InitGrid().

        this:o_sprite = NEW Sprite(this:o_grid).
        this:c_disp = this:o_sprite:Body.

        this:o_ctrl = this:o_sprite:Grid:GetTile(this:o_sprite:Grid:Width - 1, this:o_sprite:Grid:Height - 1).
        this:o_ctrl:Value = "A".
        this:o_sprite:Grid:SetTile(this:o_ctrl).

        PAUSE 0 BEFORE-HIDE.

        MAIN-BLK:
        REPEAT:
            ASSIGN this:key = "".

            PUT CURSOR OFF.
            READKEY PAUSE 0.
            IF LASTKEY > 0 THEN this:key = IF LASTKEY = 32 THEN 'SPACE' ELSE KEYLABEL(LASTKEY).

            CASE this:key:
                WHEN "W" OR WHEN "CURSOR-UP"    THEN
                DO:
                    IF this:o_ctrl:X > 2 THEN
                    DO:
                        this:o_sprite:Grid:SetTile(this:o_ctrl:X, this:o_ctrl:Y, " ").
                        this:o_ctrl:X = this:o_ctrl:X - 1.
                    END.
                END.
                WHEN "S" OR WHEN "CURSOR-DOWN"  THEN
                DO:
                    IF this:o_ctrl:X < this:o_sprite:Grid:Width - 1 THEN
                    DO:
                        this:o_sprite:Grid:SetTile(this:o_ctrl:X, this:o_ctrl:Y, " ").
                        this:o_ctrl:X = this:o_ctrl:X + 1.
                    END. 
                END.
                WHEN "D" OR WHEN "CURSOR-RIGHT" THEN
                DO:
                    IF this:o_ctrl:Y < this:o_sprite:Grid:Height - 1 THEN
                    DO:
                        this:o_sprite:Grid:SetTile(this:o_ctrl:X, this:o_ctrl:Y, " ").
                        this:o_ctrl:Y = this:o_ctrl:Y + 1.
                    END.
                END.
                WHEN "A" OR WHEN "CURSOR-LEFT"  THEN
                DO:
                    IF this:o_ctrl:Y > 2 THEN
                    DO:
                        this:o_sprite:Grid:SetTile(this:o_ctrl:X, this:o_ctrl:Y, " ").
                        this:o_ctrl:Y = this:o_ctrl:Y - 1.
                    END.
                END.
            END CASE.

            o_sprite:Grid:SetTile(this:o_ctrl).
            this:c_disp = o_sprite:Body.
            DISPLAY c_disp WITH FRAME A.
            VIEW FRAME A.
        END.
    END METHOD.

    METHOD PRIVATE VOID InitGrid ():
        c_temp = c_temp + "+" + FILL("-", 38) + "+" + CHR(10).
        
        DO i_i = 1 TO 18:
            c_temp = c_temp + "|" + FILL(" ", 38) + "|" + CHR(10).
        END.

        c_temp = c_temp + "+" + FILL("-", 38) + "+" + CHR(10).

        DO i_i = 1 TO o_grid:Width:
            c_line = ENTRY(i_i, c_temp, CHR(10)).

            DO i_j = 1 TO o_grid:Height:
                o_grid:Body:SetValue(i_i, i_j, SUBSTRING(c_line, i_j, 1)).
            END.
        END.
    END METHOD.
END CLASS.