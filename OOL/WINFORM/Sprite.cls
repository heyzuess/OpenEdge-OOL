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

  Name     : Sprite.cls
  Author   : Jesse Iberri
  Date     : 12/10/2019
  Purpose  : OOL WINFORM Sprite class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.WINFORM.*.

CLASS OOL.WINFORM.Sprite USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.WINFORM.Sprite
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Grid AS Grid      GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY Body AS CHARACTER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_temp AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_i    AS INTEGER   NO-UNDO.

    CONSTRUCTOR Sprite (INPUT oip_grid AS Grid):
        SUPER().
        DO ON ERROR UNDO, THROW:
            this:Grid = oip_grid.
            this:Grid:TileUpdate:Subscribe(LoadTile).
            this:InitializeComponents().
        END.
        CATCH e AS Progress.Lang.Error:
            DO i_i = 1 TO e:NumMessages:
                MESSAGE e:GetMessage(i_i)
                VIEW-AS ALERT-BOX ERROR.
            END.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR Sprite ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponents ():
        this:LoadGrid().
    END METHOD.

    METHOD PUBLIC VOID ClearGrid ():
        this:Body = "".
    END METHOD.

    METHOD PUBLIC VOID LoadGrid ():
        this:ClearGrid().
        this:Grid:ForEach:Subscribe(SetGrid).
        this:Grid:ForEach().
        this:Grid:ForEach:Unsubscribe(SetGrid).
    END METHOD.

    METHOD PRIVATE VOID SetGrid (INPUT iip_x AS INTEGER,
                                 INPUT iip_y AS INTEGER,
                                 INPUT cip_c AS CHARACTER):
        this:Body = this:Body + SUBSTRING(cip_c, 1, 1).
        IF iip_y = this:Grid:Height THEN this:Body = this:Body + CHR(10).
    END METHOD.

    METHOD PRIVATE VOID LoadTile (INPUT oip_tile AS Tile):
        this:c_temp = "".
        this:c_temp = ENTRY(oip_tile:X, this:Body, CHR(10)).
        SUBSTRING(this:c_temp, oip_tile:Y, 1) = oip_tile:Value.
        ENTRY(oip_tile:X, this:Body, CHR(10)) = this:c_temp.
    END METHOD.
END CLASS.