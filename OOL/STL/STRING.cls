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

  Name     : STRING.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL STL STRING Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
USING OOL.STL.*.
CLASS OOL.STL.STRING INHERITS OOL.STL.WINVAR:
    DEFINE PUBLIC PROPERTY STRING AS CHARACTER GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY VALUE  AS CHARACTER GET. PUBLIC SET.

    CONSTRUCTOR STRING ():
    END CONSTRUCTOR.

    DESTRUCTOR STRING ():
    END DESTRUCTOR.

    METHOD PRIVATE LOGICAL VALUE_TO_LOG ():
        IF LOOKUP(THIS-OBJECT:VALUE, "0,1") > 0 THEN
            RETURN LOGICAL(ENTRY(LOOKUP(THIS-OBJECT:VALUE, "0,1"), c_BoolDef)).
        ELSE
        IF LOOKUP(THIS-OBJECT:VALUE, c_BoolDef) > 0 THEN
            RETURN LOGICAL(THIS-OBJECT:VALUE).
        ELSE
            RETURN ?.
    END METHOD.

    METHOD PUBLIC OVERRIDE CHARACTER ToString():
        RETURN THIS-OBJECT:VALUE.
    END METHOD.

    METHOD PUBLIC OVERRIDE CHARACTER ToStr ():
        RETURN THIS-OBJECT:VALUE.
    END METHOD.

    METHOD PUBLIC CHARACTER ToStr (INPUT cip_string AS CHARACTER):
        ASSIGN THIS-OBJECT:VALUE = cip_string.
        RETURN THIS-OBJECT:ToStr().
    END METHOD.

    METHOD PUBLIC CHARACTER ToStr (INPUT iip_int AS INTEGER):
        ASSIGN THIS-OBJECT:VALUE = THIS-OBJECT:CAST_STRING(iip_int).
        RETURN THIS-OBJECT:ToStr().
    END METHOD.

    METHOD PUBLIC CHARACTER ToStr (INPUT iip_dec AS DECIMAL):
        ASSIGN THIS-OBJECT:VALUE = THIS-OBJECT:CAST_STRING(iip_dec).
        RETURN THIS-OBJECT:ToStr().
    END METHOD.

    METHOD PUBLIC CHARACTER ToStr (INPUT dip_dte AS DATE):
        ASSIGN THIS-OBJECT:VALUE = THIS-OBJECT:CAST_STRING(dip_dte).
        RETURN THIS-OBJECT:ToStr().
    END METHOD.

    METHOD PUBLIC CHARACTER ToStr (INPUT lip_log AS LOGICAL):
        ASSIGN THIS-OBJECT:VALUE = THIS-OBJECT:CAST_STRING(lip_log).
        RETURN THIS-OBJECT:ToStr().
    END METHOD.

    METHOD PUBLIC CHARACTER ToStr (INPUT hip_hnd AS HANDLE):
        ASSIGN THIS-OBJECT:VALUE = THIS-OBJECT:CAST_STRING(hip_hnd).
        RETURN THIS-OBJECT:ToStr().
    END METHOD.

    METHOD PUBLIC OVERRIDE INTEGER ToInt ():
        IF THIS-OBJECT:NaN(THIS-OBJECT:VALUE) THEN RETURN ?.
        RETURN INTEGER(THIS-OBJECT:VALUE).
    END METHOD.

    METHOD PUBLIC OVERRIDE DECIMAL ToDec ():
        IF THIS-OBJECT:NaN(THIS-OBJECT:VALUE) THEN RETURN ?.
        RETURN DECIMAL(THIS-OBJECT:VALUE).
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL ToLog ().
        IF THIS-OBJECT:NaL(THIS-OBJECT:VALUE) THEN RETURN ?.
        RETURN LOGICAL(THIS-OBJECT:VALUE_TO_LOG()).
    END METHOD.

    METHOD PUBLIC OVERRIDE DATE ToDate ():
        IF THIS-OBJECT:NaDt(THIS-OBJECT:VALUE) THEN RETURN ?.
        RETURN DATE(THIS-OBJECT:VALUE).
    END METHOD.

    METHOD PUBLIC OVERRIDE HANDLE ToHandle ():
        IF NOT VALID-HANDLE(HANDLE(THIS-OBJECT:VALUE)) THEN RETURN ?.
        RETURN HANDLE(THIS-OBJECT:VALUE).
    END METHOD.
END CLASS.