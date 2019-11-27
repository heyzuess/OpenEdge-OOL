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

  Name     : Tetromino.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris Tetromino class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.Tetromino INHERITS System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.Tetromino
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Body   AS MultiDimensionalArray GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY Helper AS TetrominoHelper       GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY Width AS INTEGER
    GET ():
        RETURN this:Body:GetLength(0).
    END GET.
    PUBLIC SET.

    DEFINE PUBLIC PROPERTY Height AS INTEGER
    GET ():
        RETURN this:Body:GetLength(1).
    END GET.
    PUBLIC SET.

    CONSTRUCTOR Tetromino ():
        SUPER().
        this:Helper = NEW TetrominoHelper().
    END CONSTRUCTOR.

    CONSTRUCTOR Tetromino (INPUT oip_md AS MultiDimensionalArray):
        SUPER().
        this:Helper = NEW TetrominoHelper().
        this:Body   = oip_md.
    END CONSTRUCTOR.

    DESTRUCTOR Tetromino ():
    END DESTRUCTOR.

    METHOD PUBLIC Object ToObject ():
        RETURN CAST(this, Object).
    END METHOD.
END CLASS.