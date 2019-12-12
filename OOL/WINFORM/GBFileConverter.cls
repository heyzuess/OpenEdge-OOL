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

  Name     : GBFileConverter.cls
  Author   : Jesse Iberri
  Date     : 12/11/2019
  Purpose  : OOL WINFORM GBFileConverter Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.WINFORM.*.

CLASS OOL.WINFORM.GBFileConverter USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.WINFORM.GBFileConverter
    GET ():
        RETURN THIS-OBJECT. 
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE STATIC VARIABLE tempArray AS TwoDimensionalArray NO-UNDO.
    DEFINE PRIVATE STATIC VARIABLE tempGrid  AS Grid                NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i AS INTEGER NO-UNDO.

    DEFINE STATIC TEMP-TABLE gridBlock
        FIELD i_idx  AS INTEGER
        FIELD i_x    AS INTEGER
        FIELD i_y    AS INTEGER
        FIELD c_val  AS CHARACTER
        FIELD c_loc  AS CHARACTER
        FIELD c_data AS CHARACTER
        FIELD l_fill AS LOGICAL.

    CONSTRUCTOR GBFileConverter ():
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

    DESTRUCTOR GBFileConverter ():
    END DESTRUCTOR.

    METHOD PUBLIC STATIC Grid FileToGrid (INPUT cip_file AS CHARACTER):
        IF SEARCH(cip_file) = ? THEN RETURN ?.

        TEMP-TABLE gridBlock:READ-JSON("FILE", cip_file, "EMPTY").
        IF NOT TEMP-TABLE gridBlock:HAS-RECORDS THEN RETURN ?.

        tempArray = NEW TwoDimensionalArray().

        FOR EACH gridBlock BY gridBlock.i_idx:
            tempArray:SetValue(gridBlock.i_x, gridBlock.i_y, gridBlock.c_val).
        END.

        RETURN NEW Grid(tempArray).
    END METHOD.

    METHOD PUBLIC STATIC Sprite FileToSprite (INPUT cip_file AS CHARACTER):
        IF SEARCH(cip_file) = ? THEN RETURN ?.

        tempGrid = FileToGrid(cip_file).
        IF NOT VALID-OBJECT(tempGrid) THEN RETURN ?.

        RETURN NEW Sprite(tempGrid).
    END METHOD.

    METHOD PRIVATE VOID InitializeComponents ():
    END METHOD.
END CLASS.