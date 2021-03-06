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

  Name     : WINTEXT.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL WINTEXT Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
CLASS OOL.WINTEXT INHERITS OOL.WINOBJ:
    DEFINE PUBLIC PROPERTY VALUE        AS CHARACTER GET.
    PUBLIC  SET (cip_value AS CHARACTER):
        ASSIGN THIS-OBJECT:VALUE = cip_value.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:SCREEN-VALUE = THIS-OBJECT:VALUE.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY WIDTH  AS INTEGER   GET. 
    PUBLIC SET (iip_tempWidth AS INTEGER):
        ASSIGN THIS-OBJECT:WIDTH = iip_tempWidth.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:WIDTH-CHARS = THIS-OBJECT:WIDTH
                   THIS-OBJECT:HANDLE:FORMAT      = SUBSTITUTE("X(&1)", THIS-OBJECT:WIDTH).
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY HEIGHT AS INTEGER GET.
    PUBLIC SET (iip_tempHeight AS INTEGER):
        ASSIGN THIS-OBJECT:HEIGHT = iip_tempHeight.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:HEIGHT = THIS-OBJECT:HEIGHT.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE EVENT FOCUS-OFF SIGNATURE VOID (aip_data AS ARRAY).

    CONSTRUCTOR WINTEXT ():
        SUPER().
        THIS-OBJECT:REF-ONLY = TRUE.
    END CONSTRUCTOR.

    CONSTRUCTOR WINTEXT (INPUT oip_parent AS WINOBJ,
                         INPUT cip_name   AS CHARACTER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):
        SUPER().

        ASSIGN THIS-OBJECT:PARENT        = oip_parent
               THIS-OBJECT:NAME          = cip_name
               THIS-OBJECT:WIDTH         = MAX(iip_w, 1)
               THIS-OBJECT:HEIGHT        = MAX(iip_h, 1)
               THIS-OBJECT:X             = MAX(iip_x, 1)
               THIS-OBJECT:Y             = MAX(iip_y, 1).

        THIS-OBJECT:REF-ONLY = TRUE.
        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    DESTRUCTOR WINTEXT ():
    END DESTRUCTOR.

    METHOD PUBLIC OVERRIDE VOID ENABLE ():
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID WAIT ():
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:
        END.

        CREATE TEXT THIS-OBJECT:HANDLE ASSIGN
            NAME         = THIS-OBJECT:NAME
            FRAME        = THIS-OBJECT:PARENT:HANDLE
            WIDTH-CHARS  = THIS-OBJECT:WIDTH
            HEIGHT-CHARS = THIS-OBJECT:HEIGHT
            FORMAT       = SUBSTITUTE("X(&1)", THIS-OBJECT:WIDTH)
            COL          = THIS-OBJECT:X
            ROW          = THIS-OBJECT:Y
            SCREEN-VALUE = THIS-OBJECT:VALUE
            DCOLOR       = THIS-OBJECT:COLOR.

        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).
        
        IF THIS-OBJECT:LIVE                        AND
           VALID-HANDLE(THIS-OBJECT:PARENT:HANDLE) THEN
        DO:
            THIS-OBJECT:PARENT:ADD-CHILD(THIS-OBJECT).
            THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.