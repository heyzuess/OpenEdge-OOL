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

  Name     : Object.cls
  Author   : Jesse Iberri
  Date     : 11/22/2019
  Purpose  : OOL CGE Car Object class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.ConsoleColor FROM ASSEMBLY.
USING OOL.CGE.Cars.*.

CLASS OOL.CGE.Cars.Object INHERITS System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Cars.Object
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY x     AS INTEGER   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY y     AS INTEGER   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY c     AS CHARACTER GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY color AS ConsoleColor GET. PUBLIC SET.

    CONSTRUCTOR Object ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR Object ():
    END DESTRUCTOR.
END CLASS.