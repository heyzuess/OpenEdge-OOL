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

  Name     : HEX.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL HEX Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING OOL.*.
CLASS OOL.HEX:
    DEFINE PRIVATE VARIABLE math        AS MATH.

    DEFINE PRIVATE VARIABLE c_hexVals   AS CHARACTER INIT
    "0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F".

    DEFINE PRIVATE VARIABLE c_temp      AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_temp      AS INTEGER.
    DEFINE PRIVATE VARIABLE i_remainder AS INTEGER.
    DEFINE PRIVATE VARIABLE i_return    AS INTEGER.

    CONSTRUCTOR HEX ():
        SUPER().
        math = NEW MATH().
    END CONSTRUCTOR.

    DESTRUCTOR HEX () :
    END DESTRUCTOR.

    METHOD PUBLIC CHARACTER HexToString (INPUT iip_integer AS INTEGER):
        ASSIGN c_temp      = ""
               i_temp      = 0
               i_remainder = 0.
        
        REPEAT:
            ASSIGN i_temp      = math:Floor(iip_integer / 16)
                   i_remainder = iip_integer MODULO 16
                   c_temp      = IF i_remainder >= 10 THEN
                                    CHR(ASC('A') + i_remainder - 10) + c_temp
                                 ELSE
                                    STRING(i_remainder) + c_temp.

            IF i_temp = 0 THEN LEAVE.

            ASSIGN iip_integer = i_temp.
        END.

        ASSIGN c_temp = "0x" + c_temp.

        RETURN c_temp.
    END METHOD.

    METHOD PUBLIC CHARACTER HexToHash (INPUT iip_integer AS INTEGER):
        ASSIGN c_temp = ""
               i_temp = 0
               c_temp = THIS-OBJECT:HexToString(iip_integer).

        ASSIGN c_temp = REPLACE(c_temp,"0x","")
               i_temp = 6 - LENGTH(c_temp)
               c_temp = "#" + FILL("0", i_temp) + c_temp.

        RETURN c_temp.
    END METHOD.

    METHOD PUBLIC INTEGER StringToHex (INPUT cip_string AS CHARACTER):
        ASSIGN c_temp      = ""
               i_temp      = 0
               i_remainder = 0
               i_return    = 0.

        IF cip_string BEGINS "0x" THEN
            ASSIGN cip_string = SUBSTRING(cip_string, 3, LENGTH(cip_string)).
        ELSE
        IF cip_string BEGINS "#"  THEN
            ASSIGN cip_string = SUBSTRING(cip_string, 2, LENGTH(cip_string)).
        ELSE
            RETURN ?.

        DO i_temp = 1 TO LENGTH(cip_string):
            ASSIGN c_temp      = SUBSTRING(cip_string, i_temp, 1)
                   i_remainder = IF c_temp >= "A" THEN
                                     LOOKUP(c_temp, c_hexVals) - 1
                                 ELSE
                                     INTEGER(c_temp)
                   i_remainder = IF i_temp = LENGTH(cip_string) THEN
                                     i_remainder
                                 ELSE
                                     EXP(16, LENGTH(cip_string) - i_temp) * i_remainder
                   i_return    = i_return + i_remainder.
        END.

        RETURN i_return.
    END METHOD.
END CLASS.