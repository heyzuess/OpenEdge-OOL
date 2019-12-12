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

  Name     : Tile.cls
  Author   : Jesse Iberri
  Date     : 12/11/2019
  Purpose  : OOL WINFORM Tile Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.WINFORM.*.

CLASS OOL.WINFORM.Tile USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.WINFORM.Tile
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY X     AS INTEGER   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Y     AS INTEGER   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Value AS CHARACTER GET. PUBLIC SET.

    DEFINE PRIVATE VARIABLE i_i AS INTEGER NO-UNDO.

    CONSTRUCTOR Tile ():
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

    CONSTRUCTOR Tile (INPUT iip_x AS INTEGER,
                      INPUT iip_y AS INTEGER,
                      INPUT cip_c AS CHARACTER):
        SUPER().
        DO ON ERROR UNDO, THROW:
            this:X = iip_x.
            this:Y = iip_y.
            this:Value = cip_c.
            this:InitializeComponents().
        END.
        CATCH e AS Progress.Lang.Error:
            DO i_i = 1 TO e:NumMessages:
                MESSAGE e:GetMessage(i_i)
                VIEW-AS ALERT-BOX ERROR.
            END.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR Tile ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponents ():
        
    END METHOD.
END CLASS.