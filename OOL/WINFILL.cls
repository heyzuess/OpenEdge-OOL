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

  Name     : WINFILL.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL WINFILL Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
CLASS OOL.WINFILL INHERITS OOL.WINOBJ:
    DEFINE PUBLIC PROPERTY LABEL        AS CHARACTER GET. 
    PUBLIC SET (INPUT cip_label AS CHARACTER):
        IF VALID-OBJECT(THIS-OBJECT:LABEL-OBJECT) THEN
            ASSIGN THIS-OBJECT:LABEL-OBJECT:VALUE = cip_label.
        ELSE
        IF VALID-HANDLE(THIS-OBJECT:LABEL-HANDLE) THEN
            ASSIGN THIS-OBJECT:LABEL-HANDLE:SCREEN-VALUE = cip_label.
    END SET.

    DEFINE PUBLIC PROPERTY LABEL-OBJECT AS WINTEXT   GET. 
    PUBLIC SET (INPUT oip_label AS WINTEXT):
        IF THIS-OBJECT:SELF-REF THEN
        DO:
            ASSIGN THIS-OBJECT:LABEL-OBJECT = oip_label.
            RETURN.
        END.

        IF NOT THIS-OBJECT:PARENT:IS-CHILD(oip_label) THEN RETURN.

        ASSIGN THIS-OBJECT:LABEL-OBJECT = oip_label
               THIS-OBJECT:LABEL-HANDLE = oip_label:HANDLE.
    END SET.
    
    DEFINE PUBLIC PROPERTY LABEL-HANDLE AS HANDLE    GET. 
    PUBLIC SET (INPUT hip_label AS HANDLE):
        IF THIS-OBJECT:SELF-REF THEN
        DO:
            RETURN.
        END.

        IF NOT THIS-OBJECT:PARENT:IS-CHILD(hip_label) THEN RETURN.

        ASSIGN THIS-OBJECT:LABEL-HANDLE = hip_label
               THIS-OBJECT:LABEL        = hip_label:SCREEN-VALUE.

        IF THIS-OBJECT:LIVE THEN
        DO:
            IF VALID-HANDLE(THIS-OBJECT:LABEL-HANDLE) THEN
                ASSIGN THIS-OBJECT:HANDLE:SIDE-LABEL-HANDLE = THIS-OBJECT:LABEL-HANDLE.
        END.
    END SET.
    
    DEFINE PUBLIC PROPERTY VALUE        AS CHARACTER GET.
    PUBLIC  SET (cip_value AS CHARACTER):
        ASSIGN THIS-OBJECT:VALUE = cip_value.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:SCREEN-VALUE = THIS-OBJECT:VALUE.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY FORMAT AS CHARACTER GET.
    PUBLIC SET (cip_form AS CHARACTER):
        ASSIGN THIS-OBJECT:FORMAT = cip_form.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:FORMAT = THIS-OBJECT:FORMAT.
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

    DEFINE PUBLIC OVERRIDE PROPERTY HEIGHT AS INTEGER    GET.
    PUBLIC SET (iip_tempHeight AS INTEGER):
        ASSIGN THIS-OBJECT:HEIGHT = iip_tempHeight.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:HEIGHT-CHARS = THIS-OBJECT:HEIGHT.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE EVENT FOCUS-OFF SIGNATURE VOID (aip_data  AS ARRAY).

    DEFINE PRIVATE VARIABLE a_tempData AS ARRAY.

    CONSTRUCTOR WINFILL ():
        SUPER().

    END CONSTRUCTOR.

    CONSTRUCTOR WINFILL (INPUT oip_parent AS WINOBJ,
                         INPUT oip_label  AS WINTEXT,
                         INPUT cip_name   AS CHARACTER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):
        SUPER().

        ASSIGN THIS-OBJECT:PARENT        = oip_parent
               THIS-OBJECT:LABEL-OBJECT  = oip_label
               THIS-OBJECT:NAME          = cip_name
               THIS-OBJECT:WIDTH         = MAX(iip_w, 1)
               THIS-OBJECT:HEIGHT        = MAX(iip_h, 1)
               THIS-OBJECT:X             = MAX(iip_x, 1)
               THIS-OBJECT:Y             = MAX(iip_y, 1).

        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    CONSTRUCTOR WINFILL (INPUT oip_parent AS WINOBJ,
                         INPUT hip_label  AS HANDLE,
                         INPUT cip_name   AS CHARACTER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):
        SUPER().

        ASSIGN THIS-OBJECT:PARENT        = oip_parent
               THIS-OBJECT:LABEL-HANDLE  = hip_label
               THIS-OBJECT:NAME          = cip_name
               THIS-OBJECT:WIDTH         = MAX(iip_w, 1)
               THIS-OBJECT:HEIGHT        = MAX(iip_h, 1)
               THIS-OBJECT:X             = MAX(iip_x, 1)
               THIS-OBJECT:Y             = MAX(iip_y, 1).

        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    CONSTRUCTOR WINFILL (INPUT oip_parent AS WINOBJ,
                         INPUT cip_label  AS CHARACTER,
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

        THIS-OBJECT:ADD-LABEL(cip_label).
        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    DESTRUCTOR WINFILL ():
        IF VALID-OBJECT(THIS-OBJECT:LABEL-OBJECT) THEN
        DO:
            ASSIGN THIS-OBJECT:LABEL-OBJECT:VISIBLE = FALSE.
            DELETE OBJECT THIS-OBJECT:LABEL-OBJECT:HANDLE.
        END.
        ELSE
        IF VALID-HANDLE(THIS-OBJECT:LABEL-HANDLE) THEN
        DO:
            ASSIGN THIS-OBJECT:LABEL-HANDLE:VISIBLE = FALSE.
            DELETE OBJECT THIS-OBJECT:LABEL-HANDLE.
        END.
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL ADD-LABEL (INPUT cip_text AS CHARACTER):
        IF VALID-OBJECT(THIS-OBJECT:LABEL-OBJECT) OR
           VALID-HANDLE(THIS-OBJECT:LABEL-HANDLE) THEN
        RETURN FALSE.

        IF cip_text = "" THEN RETURN FALSE.
        
        THIS-OBJECT:SELF-REF = TRUE.
        THIS-OBJECT:LABEL-OBJECT = NEW WINTEXT().
        THIS-OBJECT:SELF-REF = FALSE.
        THIS-OBJECT:Y = THIS-OBJECT:Y + 1.

        ASSIGN THIS-OBJECT:LABEL-OBJECT:PARENT  = THIS-OBJECT:PARENT
               THIS-OBJECT:LABEL-OBJECT:VALUE   = cip_text
               THIS-OBJECT:LABEL-OBJECT:X       = THIS-OBJECT:X
               THIS-OBJECT:LABEL-OBJECT:Y       = THIS-OBJECT:Y - 1
               THIS-OBJECT:LABEL-OBJECT:WIDTH   = LENGTH(cip_text)
               THIS-OBJECT:LABEL-OBJECT:HEIGHT  = 1
               THIS-OBJECT:LABEL-OBJECT:VISIBLE = THIS-OBJECT:VISIBLE
               THIS-OBJECT:LABEL-OBJECT:COLOR   = THIS-OBJECT:COLOR.
        
        THIS-OBJECT:LABEL-OBJECT:REGISTER().

        IF NOT THIS-OBJECT:LABEL-OBJECT:LIVE THEN
            ASSIGN THIS-OBJECT:Y = THIS-OBJECT:Y - 1.

        IF THIS-OBJECT:LABEL-OBJECT:LIVE THEN
            ASSIGN THIS-OBJECT:LABEL-HANDLE = THIS-OBJECT:LABEL-OBJECT:HANDLE.

        RETURN THIS-OBJECT:LABEL-OBJECT:LIVE.
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID WAIT ():
        /* THIS-OBJECT:HANDLE causes an issue here for some reason */
        WAIT-FOR "GO","PF1","F1","TAB","ENTER","RETURN" OF HANDLE FOCUS HANDLE.
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID ENABLE ():
        IF NOT THIS-OBJECT:LIVE THEN RETURN.

        IF NOT THIS-OBJECT:VISIBLE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:VISIBLE = TRUE.
        END.

        IF NOT THIS-OBJECT:SENSITIVE THEN
            ASSIGN THIS-OBJECT:HANDLE:SENSITIVE = TRUE.

        PUT CURSOR THIS-OBJECT:X THIS-OBJECT:Y.

        THIS-OBJECT:WAIT().
        THIS-OBJECT:VALUE = THIS-OBJECT:HANDLE:SCREEN-VALUE.

        a_tempData = NEW ARRAY().
        a_tempData:PUSH-BACK(THIS-OBJECT:VALUE).
        THIS-OBJECT:FOCUS-OFF:PUBLISH(a_tempData).

        PUT CURSOR OFF.

        IF NOT THIS-OBJECT:SENSITIVE THEN
            ASSIGN THIS-OBJECT:HANDLE:SENSITIVE = FALSE.

        IF NOT THIS-OBJECT:VISIBLE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:VISIBLE = FALSE.
        END.
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:
        END.

        CREATE FILL-IN THIS-OBJECT:HANDLE ASSIGN
            NAME              = THIS-OBJECT:NAME
            FRAME             = THIS-OBJECT:PARENT:HANDLE
            WIDTH-CHARS       = THIS-OBJECT:WIDTH
            HEIGHT-CHARS      = THIS-OBJECT:HEIGHT
            FORMAT            = SUBSTITUTE("X(&1)", THIS-OBJECT:WIDTH)
            COL               = THIS-OBJECT:X
            ROW               = THIS-OBJECT:Y
            SCREEN-VALUE      = THIS-OBJECT:VALUE
            DCOLOR            = THIS-OBJECT:COLOR
            SIDE-LABEL-HANDLE = THIS-OBJECT:LABEL-HANDLE
            SENSITIVE         = THIS-OBJECT:SENSITIVE.

        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).
        
        IF THIS-OBJECT:LIVE                        AND
           VALID-HANDLE(THIS-OBJECT:PARENT:HANDLE) THEN
        DO:
            THIS-OBJECT:PARENT:ADD-CHILD(THIS-OBJECT).
            THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
            IF VALID-HANDLE(THIS-OBJECT:LABEL-HANDLE) THEN THIS-OBJECT:LABEL = THIS-OBJECT:LABEL-HANDLE:SCREEN-VALUE.
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.