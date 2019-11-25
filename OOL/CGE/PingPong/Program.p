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
  Date     : 11/25/2019
  Purpose  : OOL CGE PingPong Program (Application)

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.PingPong.*.

DEFINE VARIABLE i_i AS INTEGER.

DO ON ERROR UNDO, THROW:
    DEFINE VARIABLE myApp AS Program.
    myApp = NEW Program().

    CATCH e AS Progress.Lang.Error:
        DO i_i = 1 to e:NumMessages:
            Console:WriteLine(e:GetMessage(i_i)).
        END.
        Console:ReadKey().
    END CATCH.
END.
FINALLY:
    QUIT.
END.