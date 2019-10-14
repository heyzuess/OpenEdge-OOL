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

  Name     : MATH.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL MATH Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
CLASS OOL.MATH:
    DEFINE PRIVATE VARIABLE c_char AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_int  AS INTEGER.
    DEFINE PRIVATE VARIABLE d_sum  AS DECIMAL.

    CONSTRUCTOR MATH ():
    END CONSTRUCTOR.

    DESTRUCTOR MATH ():
    END DESTRUCTOR.

    METHOD PUBLIC INTEGER FLOOR (INPUT dip_decimal AS DECIMAL):
        RETURN INTEGER(ENTRY(1, STRING(dip_decimal), ".")).
    END METHOD.

    METHOD PUBLIC INTEGER FLOOR (INPUT iip_int1 AS INTEGER,
                                 INPUT iip_int2 AS INTEGER):
        RETURN THIS-OBJECT:FLOOR(DECIMAL(iip_int1 / iip_int2)).
    END METHOD.

    METHOD PUBLIC DECIMAL AVERAGE (INPUT dip_int1 AS DECIMAL,
                                   INPUT dip_int2 AS DECIMAL):
        RETURN DECIMAL((dip_int1 + dip_int2) / 2).
    END METHOD.

    METHOD PUBLIC DECIMAL AVERAGE (INPUT aip_array AS ARRAY):
        ASSIGN d_sum = 0.
        IF NOT VALID-OBJECT(aip_array) THEN RETURN 0.

        aip_array:FOR-EACH:SUBSCRIBE(AVERAGE_ARRAY).
        aip_array:FOR-EACH().
        aip_array:FOR-EACH:UNSUBSCRIBE(AVERAGE_ARRAY).

        ASSIGN d_sum = d_sum / aip_array:LENGTH.

        RETURN d_sum.
    END METHOD.

    METHOD PUBLIC DECIMAL AVERAGE (INPUT cip_list AS CHARACTER):
        ASSIGN d_sum = 0.
        DO i_int = 1 TO NUM-ENTRIES(cip_list):
            ASSIGN c_char = TRIM(ENTRY(i_int, cip_list)).
            IF c_char          = "" THEN ASSIGN c_char = "0".
            IF DECIMAL(c_char) = ?  THEN ASSIGN c_char = "0".

            ASSIGN d_sum = d_sum + DECIMAL(c_char) NO-ERROR.
        END.

        ASSIGN d_sum = d_sum / NUM-ENTRIES(cip_list).

        RETURN d_sum.
    END METHOD.

    METHOD PRIVATE VOID AVERAGE_ARRAY (INPUT aip_data AS ARRAY):
        ASSIGN d_sum = d_sum + aip_data:INDEX-DEC(aip_data:THIS).
    END METHOD.
END CLASS.