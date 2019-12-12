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

  Name     : Grid.cls
  Author   : Jesse Iberri
  Date     : 12/10/2019
  Purpose  : OOL WINFORM Grid Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.WINFORM.*.

CLASS OOL.WINFORM.Grid USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.WINFORM.Grid
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Width  AS INTEGER             GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY Height AS INTEGER             GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY Body   AS TwoDimensionalArray GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_i AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_j AS INTEGER NO-UNDO.

    DEFINE PUBLIC EVENT TileUpdate SIGNATURE VOID (INPUT oip_tile AS Tile).

    DEFINE PUBLIC EVENT ForEach SIGNATURE VOID (INPUT iip_x AS INTEGER,
                                                INPUT iip_y AS INTEGER,
                                                INPUT cip_c AS CHARACTER).

    CONSTRUCTOR Grid (INPUT iip_w AS INTEGER,
                      INPUT iip_h AS INTEGER):
        SUPER().
        DO ON ERROR UNDO, THROW:
            this:Width = iip_w.
            this:Height = iip_h.
            this:InitializeComponents().
        END.
        CATCH e AS Progress.Lang.Error:
            DO i_i = 1 TO e:NumMessages:
                MESSAGE e:GetMessage(i_i)
                VIEW-AS ALERT-BOX ERROR.
            END.
        END CATCH.
    END CONSTRUCTOR.

    CONSTRUCTOR Grid (INPUT oip_array AS TwoDimensionalArray):
        SUPER().
        DO ON ERROR UNDO, THROW:
            this:Width = oip_array:GetArraySize(1, ?).
            this:Height = oip_array:GetArraySize(2, 1).
            this:Body = oip_array.
            this:InitializeComponents().
        END.
        CATCH e AS Progress.Lang.Error:
            DO i_i = 1 TO e:NumMessages:
                MESSAGE e:GetMessage(i_i)
                VIEW-AS ALERT-BOX ERROR.
            END.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR Grid ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponents ():
        IF NOT VALID-OBJECT(this:Body) THEN
        DO:
            this:Body = NEW TwoDimensionalArray().

            DO i_i = 1 TO this:Width:
                DO i_j = 1 TO this:Height:
                    this:Body:SetValue(i_i, i_j, " ").
                END.
            END.
        END.
    END METHOD.

    METHOD PUBLIC VOID ForEach ():
        DO i_i = 1 TO this:Width:
            DO i_j = 1 TO this:Height:
                this:ForEach:Publish(i_i, i_j, this:Body:GetCharValue(i_i, i_j)).
            END.
        END.
    END METHOD.

    METHOD PUBLIC Tile GetTile (INPUT iip_x AS INTEGER,
                                INPUT iip_y AS INTEGER):
        IF NOT this:Body:HasValue(iip_x, iip_y) THEN RETURN ?.
        RETURN NEW Tile(iip_x, iip_y, this:Body:GetCharValue(iip_x, iip_y)).
    END METHOD.

    METHOD PUBLIC VOID SetTile (INPUT oip_tile AS Tile):
        this:Body:SetValue(oip_tile:X, oip_tile:Y, oip_tile:Value).
        this:TileUpdate:Publish(oip_tile).
    END METHOD.

    METHOD PUBLIC VOID SetTile (INPUT iip_x AS INTEGER,
                                INPUT iip_y AS INTEGER,
                                INPUT cip_c AS CHARACTER):
        this:Body:SetValue(iip_x, iip_y, cip_c).
        this:TileUpdate:Publish(NEW Tile(iip_x, iip_y, cip_c)).
    END METHOD.
END CLASS.