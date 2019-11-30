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

  Name     : Debug.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Debug class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.*.

CLASS OOL.CGE.Debug USE-WIDGET-POOL:
    CONSTRUCTOR Debug ():
    END CONSTRUCTOR.

    DESTRUCTOR Debug ():
    END DESTRUCTOR.

    DEFINE PRIVATE STATIC VARIABLE tempColor AS ConsoleColor NO-UNDO.

    METHOD PUBLIC STATIC VOID Message (INPUT cip_char AS CHARACTER):
        tempcolor = Console:ForegroundColor.
        Console:ForegroundColor = ConsoleColor:Yellow.
        Console:WriteLine(cip_char).
        Console:ForegroundColor = tempcolor.
        Console:ReadKey().
    END METHOD.

    METHOD PUBLIC STATIC VOID Message (INPUT iip_int AS INTEGER):
        Message(STRING(iip_int)).
    END METHOD.

    METHOD PUBLIC STATIC VOID Message (INPUT dip_dec AS DECIMAL):
        Message(STRING(dip_dec)).
    END METHOD.

    METHOD PUBLIC STATIC VOID Message (INPUT lip_log AS LOGICAL):
        Message(STRING(lip_log)).
    END METHOD.
END CLASS.