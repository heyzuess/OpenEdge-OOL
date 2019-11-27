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

  Name     : POLY_TEST.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL NET POLY_TEST Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*               FROM ASSEMBLY.
USING System.Drawing.*       FROM ASSEMBLY.
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Progress.Util.*        FROM ASSEMBLY.
USING Progress.Windows.Form  FROM ASSEMBLY.
USING OOL.*.
USING OOL.NET.*.

CLASS OOL.NET.POLY_TEST INHERITS Form USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE components AS  System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE Graphics   AS  Graphics                         NO-UNDO.
    DEFINE PRIVATE VARIABLE Pen        AS  Pen                              NO-UNDO.
    DEFINE PRIVATE VARIABLE Point      AS  Point EXTENT 4                   NO-UNDO.
    DEFINE PRIVATE VARIABLE Points     AS "Point[]"                         NO-UNDO.

    DEFINE PRIVATE VARIABLE InputText  AS  TextBox                          NO-UNDO.

    DEFINE PRIVATE VARIABLE i_int      AS  INTEGER                          NO-UNDO.

    CONSTRUCTOR POLY_TEST ():
        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        WAIT-FOR System.Windows.Forms.Application:Run (THIS-OBJECT).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR POLY_TEST ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent():
        THIS-OBJECT:SuspendLayout().
        
        THIS-OBJECT:InputText = NEW TextBox().
        THIS-OBJECT:InputText:AcceptsReturn = TRUE.
        THIS-OBJECT:InputText:AcceptsTab = TRUE.
        THIS-OBJECT:InputText:Dock = DockStyle:Fill.
        THIS-OBJECT:InputText:Multiline = TRUE.
        THIS-OBJECT:InputText:ScrollBars = ScrollBars:Vertical.
        THIS-OBJECT:InputText:KeyPress:Subscribe(InputText_KeyPress).
        
        THIS-OBJECT:Name = "Main".
        THIS-OBJECT:Text = "Main".

        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(300, 300).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:InputText).
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    METHOD PROTECTED OVERRIDE VOID OnPaint (INPUT e AS PaintEventArgs):
        THIS-OBJECT:Graphics = THIS-OBJECT:CreateGraphics().
        IF NOT VALID-OBJECT(THIS-OBJECT:Graphics) THEN
        DO:
            UNDO, THROW NEW Progress.Lang.AppError("Graphics Device Handle is null", 001).
        END.

        THIS-OBJECT:Pen = NEW Pen(Color:Black, 10).
        THIS-OBJECT:Point[1] = NEW Point(100, 100).
        THIS-OBJECT:Point[2] = NEW Point(100, 200).
        THIS-OBJECT:Point[3] = NEW Point(200, 200).
        THIS-OBJECT:Point[4] = NEW Point(200, 100).
        THIS-OBJECT:Points   = THIS-OBJECT:Point.
        THIS-OBJECT:Graphics:FillPolygon(Brushes:Yellow, THIS-OBJECT:Points).
        THIS-OBJECT:Graphics:DrawLines(THIS-OBJECT:Pen, THIS-OBJECT:Points).
    END METHOD.

    METHOD PRIVATE VOID InputText_KeyPress (INPUT sender AS System.Object,
                                            INPUT e      AS KeyPressEventArgs):
        THIS-OBJECT:InputText:AppendText(SUBSTITUTE("Key Press: &1" + CHR(13) + CHR(10), e:KeyChar)).
        ASSIGN e:Handled = TRUE.
    END METHOD.
END CLASS.