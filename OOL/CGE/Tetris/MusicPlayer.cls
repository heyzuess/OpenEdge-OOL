/*****************************************************************************************
  Copyright © 2019 by Jesse Iberri <jesseiberri@gmail.com>
  Released under the GNU General Public License GPL-3.0-or-later

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY. without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>

  Name     : MusicPlayer.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris MusicPlayer class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*           FROM ASSEMBLY.
USING System.Threading.* FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.MusicPlayer INHERITS System.Object USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.MusicPlayer
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE soundLength AS INTEGER NO-UNDO.

    CONSTRUCTOR MusicPlayer ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR MusicPlayer ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID Play ():
        this:PlayMusic().
    END METHOD.

    METHOD PRIVATE VOID PlayMusic ():
        DO WHILE TRUE:
            this:soundLength = 100.
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(990, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 2).
            Console:Beep(1320, this:soundLength).
            Console:Beep(1188, this:soundLength).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(990, this:soundLength * 2).
            Console:Beep(880, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1188, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(990, this:soundLength * 6).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 4).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1056, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Thread:Sleep(this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 4).
            Console:Beep(1408, this:soundLength * 2).
            Console:Beep(1760, this:soundLength * 4).
            Console:Beep(1584, this:soundLength * 2).
            Console:Beep(1408, this:soundLength * 2).
            Console:Beep(1320, this:soundLength * 6).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1188, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(990, this:soundLength * 4).
            Console:Beep(990, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 4).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1056, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Thread:Sleep(this:soundLength * 4).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(990, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 2).
            Console:Beep(1320, this:soundLength).
            Console:Beep(1188, this:soundLength).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(990, this:soundLength * 2).
            Console:Beep(880, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1188, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(990, this:soundLength * 6).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 4).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1056, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Thread:Sleep(this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 4).
            Console:Beep(1408, this:soundLength * 2).
            Console:Beep(1760, this:soundLength * 4).
            Console:Beep(1584, this:soundLength * 2).
            Console:Beep(1408, this:soundLength * 2).
            Console:Beep(1320, this:soundLength * 6).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1188, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(990, this:soundLength * 4).
            Console:Beep(990, this:soundLength * 2).
            Console:Beep(1056, this:soundLength * 2).
            Console:Beep(1188, this:soundLength * 4).
            Console:Beep(1320, this:soundLength * 4).
            Console:Beep(1056, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 4).
            Thread:Sleep(this:soundLength * 4).
            Console:Beep(660, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 8).
            Console:Beep(594, this:soundLength * 8).
            Console:Beep(495, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 8).
            Console:Beep(440, this:soundLength * 8).
            Console:Beep(419, this:soundLength * 8).
            Console:Beep(495, this:soundLength * 8).
            Console:Beep(660, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 8).
            Console:Beep(594, this:soundLength * 8).
            Console:Beep(495, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 4).
            Console:Beep(660, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 8).
            Console:Beep(838, this:soundLength * 16).
            Console:Beep(660, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 8).
            Console:Beep(594, this:soundLength * 8).
            Console:Beep(495, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 8).
            Console:Beep(440, this:soundLength * 8).
            Console:Beep(419, this:soundLength * 8).
            Console:Beep(495, this:soundLength * 8).
            Console:Beep(660, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 8).
            Console:Beep(594, this:soundLength * 8).
            Console:Beep(495, this:soundLength * 8).
            Console:Beep(528, this:soundLength * 4).
            Console:Beep(660, this:soundLength * 4).
            Console:Beep(880, this:soundLength * 8).
            Console:Beep(838, this:soundLength * 16).
        END.
    END METHOD.
END CLASS.