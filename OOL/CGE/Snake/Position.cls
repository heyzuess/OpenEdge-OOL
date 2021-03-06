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

  Name     : Position.cls
  Author   : Jesse Iberri
  Date     : 11/21/2019
  Purpose  : OOL CGE Snake Position class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Snake.*.

CLASS OOL.CGE.Snake.Position Inherits System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Snake.Position
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY row AS INTEGER GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY col AS INTEGER GET. PUBLIC SET.

    CONSTRUCTOR Position ():
        SUPER().
    END CONSTRUCTOR.

    CONSTRUCTOR Position (INPUT iip_row AS INTEGER,
                          INPUT iip_col AS INTEGER):
        SUPER().
        this:row = iip_row.
        this:col = iip_col.
    END CONSTRUCTOR.

    DESTRUCTOR Position ():
    END DESTRUCTOR.
END CLASS.