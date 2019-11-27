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

  Name     : ScoreManager.cls
  Author   : Jesse Iberri
  Date     : 11/25/2019
  Purpose  : OOL CGE Tetris ScoreManager class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*                         FROM ASSEMBLY.
USING System.IO.*                      FROM ASSEMBLY.
USING System.Collections.Generic.*     FROM ASSEMBLY.
USING System.Text.RegularExpressions.* FROM ASSEMBLY.
USING Progress.Util.*                  FROM ASSEMBLY.
USING OOL.CGE.Tetris.*.

CLASS OOL.CGE.Tetris.ScoreManager USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY this AS OOL.CGE.Tetris.ScoreManager
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Score     AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY HighScore AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE highScoreFile AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE tempList      AS "System.Collections.Generic.List<System.String>" NO-UNDO.
    DEFINE PRIVATE VARIABLE hs            AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE allScores     AS Array     NO-UNDO.
    DEFINE PRIVATE VARIABLE sc            AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE match         AS Match     NO-UNDO.

    DEFINE PRIVATE VARIABLE i_i           AS INTEGER   NO-UNDO.

    CONSTRUCTOR ScoreManager (INPUT cip_file AS CHARACTER):
        this:highScoreFile = cip_file.
        this:HighScore = this:GetHighScore().
    END CONSTRUCTOR.

    DESTRUCTOR ScoreManager ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID AddToScore (INPUT addToScore AS INTEGER):
        this:Score = this:Score + addToScore.
        IF this:Score > this:HighScore THEN
            this:HighScore = this:Score.
    END METHOD.

    METHOD PUBLIC VOID AddToHighScore ():
        this:tempList = NEW "System.Collections.Generic.List<System.String>"().
        this:tempList:Add(SUBSTITUTE("~[&1~] &2 => &3",
                                     STRING(DateTime:Now),
                                     Environment:UserName,
                                     this:Score)).
        File:AppendAllLines(this:highScoreFile, this:tempList).
    END METHOD.

    METHOD PRIVATE INTEGER GetHighScore ():
        this:hs = 0.
        IF File:Exists(this:highScoreFile) THEN
        DO:
            this:allScores = CAST(File:ReadAllLines(this:highScoreFile), Array).

            DO i_i = 1 TO this:allScores:Length:
                this:sc = this:allScores:GetValue(i_i - 1).
                this:match = Regex:Match(this:sc, " => (?<score>[0-9]+").
                this:hs = Math:Max(this:hs, INTEGER(this:match:Groups["score"]:Value)).
            END.
        END.

        RETURN this:hs.
    END METHOD.
END CLASS.