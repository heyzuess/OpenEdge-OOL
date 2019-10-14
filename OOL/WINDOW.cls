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

  Name     : WINDOW.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL WINDOW Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
CLASS OOL.WINDOW INHERITS OOL.WINOBJ:
    DEFINE PUBLIC PROPERTY TITLE        AS CHARACTER GET.
    PUBLIC SET (cip_title AS CHARACTER):
        ASSIGN THIS-OBJECT:TITLE = cip_title.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:TITLE = THIS-OBJECT:TITLE.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY MAX-WIDTH    AS INTEGER   GET.
    PUBLIC SET (iip_mWidth AS INTEGER):
        ASSIGN THIS-OBJECT:MAX-WIDTH = MAX(iip_mWidth, THIS-OBJECT:WIDTH).

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:MAX-WIDTH = THIS-OBJECT:MAX-WIDTH.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY MAX-HEIGHT   AS INTEGER   GET.
    PUBLIC SET (iip_mHeight AS INTEGER):
        ASSIGN THIS-OBJECT:MAX-HEIGHT = MAX(iip_mHeight, THIS-OBJECT:HEIGHT).

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:MAX-HEIGHT = THIS-OBJECT:MAX-HEIGHT.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY MESSAGE-AREA AS LOGICAL   GET.
    PUBLIC SET (lip_on AS LOGICAL):
        ASSIGN THIS-OBJECT:MESSAGE-AREA = lip_on.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:MESSAGE-AREA = THIS-OBJECT:MESSAGE-AREA.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY STATUS-AREA  AS LOGICAL   GET.
    PUBLIC SET (lip_on AS LOGICAL):
        ASSIGN THIS-OBJECT:STATUS-AREA = lip_on.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:STATUS-AREA = THIS-OBJECT:STATUS-AREA.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY WIDTH AS INTEGER GET.
    PUBLIC SET (iip_width AS INTEGER):
        ASSIGN THIS-OBJECT:WIDTH = MIN(iip_width, THIS-OBJECT:MAX-WIDTH).

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:WIDTH-CHARS = THIS-OBJECT:WIDTH.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY HEIGHT AS INTEGER GET.
    PUBLIC SET (iip_height AS INTEGER):
        ASSIGN THIS-OBJECT:HEIGHT = MIN(iip_height, THIS-OBJECT:MAX-HEIGHT).

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:HEIGHT-CHARS = THIS-OBJECT:HEIGHT.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE EVENT FOCUS-OFF SIGNATURE VOID (aip_data AS ARRAY).

    CONSTRUCTOR WINDOW ():
        SUPER().

    END CONSTRUCTOR.

    CONSTRUCTOR WINDOW (INPUT cip_name  AS CHARACTER,
                        INPUT cip_title AS CHARACTER,
                        INPUT iip_w     AS INTEGER,
                        INPUT iip_h     AS INTEGER,
                        INPUT iip_mW    AS INTEGER,
                        INPUT iip_mH    AS INTEGER,
                        INPUT iip_x     AS INTEGER,
                        INPUT iip_y     AS INTEGER,
                        INPUT lip_mess  AS LOGICAL,
                        INPUT lip_stat  AS LOGICAL):
        SUPER().

        ASSIGN THIS-OBJECT:NAME         = cip_name
               THIS-OBJECT:TITLE        = cip_title
               THIS-OBJECT:WIDTH        = iip_w
               THIS-OBJECT:HEIGHT       = iip_h
               THIS-OBJECT:MAX-WIDTH    = iip_mW
               THIS-OBJECT:MAX-HEIGHT   = iip_mH
               THIS-OBJECT:X            = iip_x
               THIS-OBJECT:Y            = iip_y
               THIS-OBJECT:MESSAGE-AREA = lip_mess
               THIS-OBJECT:STATUS-AREA  = lip_stat.
    END CONSTRUCTOR.

    DESTRUCTOR WINDOW ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID FOCUS ():
        IF THIS-OBJECT:LIVE THEN
            ASSIGN CURRENT-WINDOW = THIS-OBJECT:HANDLE.
    END METHOD.

    METHOD PUBLIC VOID FOCUS (INPUT lip_waitfor AS LOGICAL):
        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN CURRENT-WINDOW = THIS-OBJECT:HANDLE.
            IF lip_waitfor THEN
                WAIT-FOR "WINDOW-CLOSE" OF CURRENT-WINDOW.
        END.
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID ENABLE ():
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID WAIT ():
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:

        END.

        CREATE WINDOW THIS-OBJECT:HANDLE ASSIGN
            NAME              = THIS-OBJECT:NAME
            TITLE             = THIS-OBJECT:TITLE
            WIDTH-PIXELS      = THIS-OBJECT:WIDTH
            HEIGHT-PIXELS     = THIS-OBJECT:HEIGHT
            MAX-WIDTH-PIXELS  = THIS-OBJECT:MAX-WIDTH
            MAX-HEIGHT-PIXELS = THIS-OBJECT:MAX-HEIGHT
            X                 = THIS-OBJECT:X
            Y                 = THIS-OBJECT:Y
            MESSAGE-AREA      = THIS-OBJECT:MESSAGE-AREA
            STATUS-AREA       = THIS-OBJECT:STATUS-AREA.

        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).

        IF THIS-OBJECT:LIVE THEN
        DO:
            IF VALID-HANDLE(THIS-OBJECT:PARENT:HANDLE) THEN
            DO:
                THIS-OBJECT:PARENT:ADD-CHILD(THIS-OBJECT:HANDLE).
                THIS-OBJECT:HANDLE:PARENT = THIS-OBJECT:PARENT:HANDLE.
            END.
            
            THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.