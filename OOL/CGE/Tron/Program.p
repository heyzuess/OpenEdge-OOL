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

  Name     : Program.p
  Author   : Jesse Iberri
  Date     : 12/4/2019
  Purpose  : OOL CGE Tron Program (application)

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tron.*.

DEFINE VARIABLE i_i AS INTEGER NO-UNDO.

DO ON ERROR UNDO, THROW:
    Program:Main().
END.
CATCH e AS Progress.Lang.Error:
    Console:Clear().
    Console:SetCursorPosition(0, 0).
    Console:ForegroundColor = ConsoleColor:Red.
    DO i_i = 1 TO e:NumMessages:
        Console:WriteLine(e:GetMessage(i_i)).
    END.
END CATCH.
FINALLY: QUIT. END.