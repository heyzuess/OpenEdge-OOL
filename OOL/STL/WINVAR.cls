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

  Name     : WINVAR.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL STL WINVAR Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
USING OOL.STL.*.
CLASS OOL.STL.WINVAR ABSTRACT:
    DEFINE PROTECTED VARIABLE c_BoolDef AS CHARACTER.
    DEFINE PRIVATE   VARIABLE d_date    AS DATE.
    DEFINE PRIVATE   VARIABLE i_int     AS INTEGER.

    CONSTRUCTOR WINVAR ():
        ASSIGN c_BoolDef = "FALSE,TRUE,YES,NO,0,1".
    END CONSTRUCTOR.

    DESTRUCTOR WINVAR ():
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL NaN (INPUT cip_string AS CHARACTER):
        ASSIGN i_int = 0
               i_int = INTEGER(cip_string) NO-ERROR.

        RETURN ERROR-STATUS:ERROR.
    END METHOD.

    METHOD PUBLIC LOGICAL NaL (INPUT cip_string AS CHARACTER):
        RETURN LOOKUP(cip_string, c_BoolDef) = 0.
    END METHOD.

    METHOD PUBLIC LOGICAL NaDc (INPUT cip_string AS CHARACTER):
        IF INDEX(cip_string, ".") = 0 THEN RETURN FALSE.

        IF THIS-OBJECT:NaN(ENTRY(1,cip_string,".")) THEN RETURN FALSE.

        IF THIS-OBJECT:NaN(ENTRY(2,cip_string,".")) THEN RETURN FALSE.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL NaDt (INPUT cip_string AS CHARACTER):
        ASSIGN d_date = ?
               d_date = DATE(cip_string) NO-ERROR.
        
        RETURN ERROR-STATUS:ERROR.
    END METHOD.

    METHOD PROTECTED CHARACTER CAST_STRING (INPUT iip_int AS INTEGER):
        RETURN STRING(iip_int).
    END METHOD.

    METHOD PROTECTED CHARACTER CAST_STRING (INPUT dip_dec AS DECIMAL):
        RETURN STRING(dip_dec).
    END METHOD.

    METHOD PROTECTED CHARACTER CAST_STRING (INPUT dip_dte AS DATE):
        RETURN STRING(dip_dte).
    END METHOD.

    METHOD PROTECTED CHARACTER CAST_STRING (INPUT lip_log AS LOGICAL):
        RETURN STRING(lip_log).
    END METHOD.

    METHOD PROTECTED CHARACTER CAST_STRING (INPUT hip_hnd AS HANDLE):
        RETURN STRING(hip_hnd).
    END METHOD.

    METHOD PUBLIC ABSTRACT CHARACTER ToStr ().

    METHOD PUBLIC ABSTRACT INTEGER ToInt ().

    METHOD PUBLIC ABSTRACT DECIMAL ToDec ().

    METHOD PUBLIC ABSTRACT LOGICAL ToLog ().

    METHOD PUBLIC ABSTRACT DATE ToDate ().

    METHOD PUBLIC ABSTRACT HANDLE ToHandle ().
END CLASS.