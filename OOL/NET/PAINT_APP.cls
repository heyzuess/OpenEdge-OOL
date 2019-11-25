/*****************************************************************************************
  Copyright © 2019 by Jesse Iberri <jesseiberri@gmail.com>
  Released under the GNU General Public License GPL-3.0-or-later

  This is a recreation of a Simple Paint Application in C# by Anoop Kr Sharma.
  It is intended soley as a demonstration of OpenEdge .NET OOP using the 
  C# DLL Libraries. Source project and code is licensed under CPOL License 1.02.
  Source code, project, and license information can be found online at:
  https://www.codeproject.com/Tips/811495/Simple-Paint-Application-in-Csharp

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

  Name     : PAINT_APP.cls
  Author   : Jesse Iberri
  Date     : 10/14/2019
  Purpose  : Small paint application using C# DLL Libraries in OpenEdge OOP.

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.*                FROM ASSEMBLY.
USING System.Array.*          FROM ASSEMBLY.
USING System.Drawing.*        FROM ASSEMBLY.
USING System.Windows.Forms.*  FROM ASSEMBLY.
USING System.ComponentModel.* FROM ASSEMBLY.
USING Progress.Util.*         FROM ASSEMBLY.
USING Progress.Windows.Form   FROM ASSEMBLY.

USING OOL.*.
USING OOL.NET.*.

CLASS OOL.NET.PAINT_APP INHERITS Form USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY THIS AS OOL.NET.PAINT_APP
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE graphics        AS Graphics   NO-UNDO.
    DEFINE PRIVATE VARIABLE components      AS IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE panel1          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel2          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel3          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel4          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel5          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel6          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel7          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE panel8          AS Panel      NO-UNDO.
    DEFINE PRIVATE VARIABLE pnl_Draw        AS Panel      NO-UNDO.

    DEFINE PRIVATE VARIABLE label1          AS Label      NO-UNDO.
    DEFINE PRIVATE VARIABLE label2          AS Label      NO-UNDO.
    DEFINE PRIVATE VARIABLE label3          AS Label      NO-UNDO.
    DEFINE PRIVATE VARIABLE label4          AS Label      NO-UNDO.

    DEFINE PRIVATE VARIABLE txt_ShapeSize   AS TextBox    NO-UNDO.

    DEFINE PRIVATE VARIABLE button1         AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE button2         AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE btn_Rectangle   AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE btn_Square      AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE btn_Circle      AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE btn_CanvasColor AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE btn_PenColor    AS Button     NO-UNDO.
    DEFINE PRIVATE VARIABLE btn_ClearCanvas AS Button     NO-UNDO.

    DEFINE PRIVATE VARIABLE comboBox1       AS ComboBox   NO-UNDO.
    DEFINE PRIVATE VARIABLE cmb_PenSize     AS ComboBox   NO-UNDO.

    DEFINE PRIVATE VARIABLE menuStrip1      AS MenuStrip  NO-UNDO.
    
    DEFINE PRIVATE VARIABLE fileToolStripMenuItem           AS ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE newToolStripMenuItem            AS ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE exitToolStripMenuItem           AS ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE helpToolStripMenuItem           AS ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE aboutMiniPaintToolStripMenuItem AS ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE tempObject                      AS Object                                 NO-UNDO.
    DEFINE PRIVATE VARIABLE tempObjectL                     AS "System.Object[]"                      NO-UNDO.
    DEFINE PRIVATE VARIABLE tempStripL                      AS "System.Windows.Forms.ToolStripItem[]" NO-UNDO.

    DEFINE PRIVATE VARIABLE startPaint                      AS LOGICAL           NO-UNDO.
    DEFINE PRIVATE VARIABLE drawSquare                      AS LOGICAL           NO-UNDO.
    DEFINE PRIVATE VARIABLE drawRectangle                   AS LOGICAL           NO-UNDO.
    DEFINE PRIVATE VARIABLE drawCircle                      AS LOGICAL           NO-UNDO.
    DEFINE PRIVATE VARIABLE cDialog                         AS ColorDialog       NO-UNDO.
    DEFINE PRIVATE VARIABLE sb                              AS SolidBrush        NO-UNDO.
    DEFINE PRIVATE VARIABLE p                               AS Pen               NO-UNDO.
    DEFINE PRIVATE VARIABLE initX                           AS INTEGER           NO-UNDO.
    DEFINE PRIVATE VARIABLE initY                           AS INTEGER           NO-UNDO.

    DEFINE PRIVATE VARIABLE i_int                           AS INTEGER           NO-UNDO.

    CONSTRUCTOR PAINT_APP ():
        SUPER().
        InitializeComponent().
        THIS:graphics = THIS:pnl_Draw:CreateGraphics().

        THIS:ComponentsCollection:Add(THIS:components).
        WAIT-FOR Application:Run(THIS).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR PAINT_APP ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent ():
        THIS:SuspendLayout().

        THIS:panel1 = NEW Panel().
        THIS:panel2 = NEW Panel().
        THIS:panel3 = NEW Panel().
        THIS:panel4 = NEW Panel().
        THIS:panel5 = NEW Panel().
        THIS:panel6 = NEW Panel().
        THIS:panel7 = NEW Panel().
        THIS:panel8 = NEW Panel().
        THIS:pnl_Draw = NEW Panel().
        THIS:label1 = NEW Label().
        THIS:label2 = NEW Label().
        THIS:label3 = NEW Label().
        THIS:label4 = NEW Label().
        THIS:txt_ShapeSize = NEW TextBox().
        THIS:btn_Circle = NEW Button().
        THIS:btn_Rectangle = NEW Button().
        THIS:btn_Square = NEW Button().
        THIS:btn_CanvasColor = NEW Button().
        THIS:button1 = NEW Button().
        THIS:button2 = NEW Button().
        THIS:btn_PenColor = NEW Button().
        THIS:btn_ClearCanvas = NEW Button().
        THIS:comboBox1 = NEW ComboBox().
        THIS:cmb_PenSize = NEW ComboBox().
        THIS:menuStrip1 = NEW MenuStrip().
        THIS:fileToolStripMenuItem = NEW ToolStripMenuItem().
        THIS:newToolStripMenuItem = NEW ToolStripMenuItem().
        THIS:exitToolStripMenuItem = NEW ToolStripMenuItem().
        THIS:helpToolStripMenuItem = NEW ToolStripMenuItem().
        THIS:aboutMiniPaintToolStripMenuItem = NEW ToolStripMenuItem().

        THIS:panel1:BackColor = Color:Silver.
        THIS:panel1:Controls:Add(THIS:panel7).
        THIS:panel1:Controls:Add(THIS:panel4).
        THIS:panel1:Controls:Add(THIS:panel2).
        THIS:panel1:Location = NEW Point(0, 21).
        THIS:panel1:Name = "panel1".
        THIS:panel1:Size = NEW Size(140, 433).
        THIS:panel1:TabIndex = 0.

        THIS:panel2:BackColor = Color:PaleGoldenrod.
        THIS:panel2:BorderStyle = BorderStyle:FixedSingle.
        THIS:panel2:Controls:Add(THIS:panel6).
        THIS:panel2:Controls:Add(THIS:panel3).
        THIS:panel2:Controls:Add(THIS:btn_PenColor).
        THIS:panel2:Controls:Add(THIS:cmb_PenSize).
        THIS:panel2:Location = NEW Point(5, 6).
        THIS:panel2:Name = "panel2".
        THIS:panel2:Size = NEW Size(130, 100).
        THIS:panel2:TabIndex = 1.

        THIS:panel3:BackColor = Color:DarkGray.
        THIS:panel3:BorderStyle = BorderStyle:FixedSingle.
        THIS:panel3:Controls:Add(THIS:button1).
        THIS:panel3:Controls:Add(THIS:comboBox1).
        THIS:panel3:Controls:Add(THIS:button2).
        THIS:panel3:Location = NEW Point(-1, 126).
        THIS:panel3:Name = "panel3".
        THIS:panel3:Size = NEW Size(130, 100).
        THIS:panel3:TabIndex = 3.

        THIS:panel4:BackColor = Color:PaleGoldenrod.
        THIS:panel4:BorderStyle = BorderStyle:FixedSingle.
        THIS:panel4:Controls:Add(THIS:btn_CanvasColor).
        THIS:panel4:Controls:Add(THIS:btn_ClearCanvas).
        THIS:panel4:Controls:Add(THIS:panel5).
        THIS:panel4:Location = NEW Point(5, 112).
        THIS:panel4:Name = "panel4".
        THIS:panel4:Size = NEW Size(130, 110).
        THIS:panel4:TabIndex = 2.

        THIS:panel5:BackColor = Color:DimGray.
        THIS:panel5:Controls:Add(THIS:label1).
        THIS:panel5:Location = NEW Point(-1, -1).
        THIS:panel5:Name = "panel5".
        THIS:panel5:Size = NEW Size(130, 27).
        THIS:panel5:TabIndex = 0.

        THIS:panel6:BackColor = Color:DimGray.
        THIS:panel6:Controls:Add(THIS:label2).
        THIS:panel6:Location = NEW Point(0, -1).
        THIS:panel6:Name = "panel6".
        THIS:panel6:Size = NEW Size(130, 27).
        THIS:panel6:TabIndex = 2.

        THIS:panel7:BackColor = Color:PaleGoldenrod.
        THIS:panel7:BorderStyle = BorderStyle:FixedSingle.
        THIS:panel7:Controls:Add(THIS:txt_ShapeSize).
        THIS:panel7:Controls:Add(THIS:label4).
        THIS:panel7:Controls:Add(THIS:btn_Circle).
        THIS:panel7:Controls:Add(THIS:btn_Rectangle).
        THIS:panel7:Controls:Add(THIS:btn_Square).
        THIS:panel7:Controls:Add(THIS:panel8).
        THIS:panel7:Location = NEW Point(5, 228).
        THIS:panel7:Name = "panel7".
        THIS:panel7:Size = NEW Size(130, 194).
        THIS:panel7:TabIndex = 3.

        THIS:panel8:BackColor = Color:DimGray.
        THIS:panel8:Controls:Add(THIS:label3).
        THIS:panel8:Location = NEW Point(-1, -1).
        THIS:panel8:Name = "panel8".
        THIS:panel8:Size = NEW Size(130, 27).
        THIS:panel8:TabIndex = 1.

        THIS:pnl_Draw:BackColor = SystemColors:ButtonHighlight.
        THIS:pnl_Draw:Location = NEW Point(140, 23).
        THIS:pnl_Draw:Name = "pnl_Draw".
        THIS:pnl_Draw:Size = NEW Size(604, 431).
        THIS:pnl_Draw:TabIndex = 2.
        THIS:pnl_Draw:MouseDown:Subscribe(pnl_Draw_MouseDown).
        THIS:pnl_Draw:MouseMove:Subscribe(pnl_Draw_MouseMove).
        THIS:pnl_Draw:MouseUp:Subscribe(pnl_Draw_MouseUp).

        THIS:label1:AutoSize = TRUE.
        THIS:label1:Font = NEW Font("Microsoft Sans Serif", 9.75, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label1:ForeColor = Color:White.
        THIS:label1:Location = NEW Point(15, 5).
        THIS:label1:Name = "label1".
        THIS:label1:Size = NEW Size(101, 16).
        THIS:label1:TabIndex = 1.
        THIS:label1:Text = "Canvas Color".

        THIS:label2:AutoSize = TRUE.
        THIS:label2:Font = NEW Font("Microsoft Sans Serif", 9.75, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label2:ForeColor = Color:White.
        THIS:label2:Location = NEW Point(27, 5).
        THIS:label2:Name = "label2".
        THIS:label2:Size = NEW Size(71, 16).
        THIS:label2:TabIndex = 1.
        THIS:label2:Text = "Pen Tool".

        THIS:label3:AutoSize = TRUE.
        THIS:label3:Font = NEW Font("Microsoft Sans Serif", 9.75, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label3:ForeColor = Color:White.
        THIS:label3:Location = NEW Point(15, 5).
        THIS:label3:Name = "label3".
        THIS:label3:Size = NEW Size(100, 16).
        THIS:label3:TabIndex = 1.
        THIS:label3:Text = "Draw Shapes".

        THIS:label4:AutoSize = TRUE.
        THIS:label4:Font = NEW Font("Microsoft Sans Serif", 9.75, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label4:Location = NEW Point(6, 147).
        THIS:label4:Name = "label4".
        THIS:label4:Size = NEW Size(71, 13).
        THIS:label4:TabIndex = 8.
        THIS:label4:Text = "Shape Size".

        THIS:txt_ShapeSize:Location = NEW Point(3, 163).
        THIS:txt_ShapeSize:Name = "txt_ShapeSize".
        THIS:txt_ShapeSize:Size = NEW Size(122, 20).
        THIS:txt_ShapeSize:TabIndex = 9.
        THIS:txt_ShapeSize:Text = "10".

        THIS:btn_Circle:BackColor = SystemColors:Control.
        THIS:btn_Circle:Font = NEW Font("Microsoft Sans Serif", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:btn_Circle:Location = NEW Point(3, 100).
        THIS:btn_Circle:Name = "btn_Circle".
        THIS:btn_Circle:Size = NEW Size(122, 28).
        THIS:btn_Circle:TabIndex = 7.
        THIS:btn_Circle:Text = "Circle".
        THIS:btn_Circle:UseVisualStyleBackColor = FALSE.
        THIS:btn_Circle:Click:Subscribe(btn_Circle_Click).

        THIS:btn_Rectangle:BackColor = SystemColors:Control.
        THIS:btn_Rectangle:Font = NEW Font("Microsoft Sans Serif", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:btn_Rectangle:Location = NEW Point(3, 66).
        THIS:btn_Rectangle:Name = "btn_Rectangle".
        THIS:btn_Rectangle:Size = NEW Size(122, 28).
        THIS:btn_Rectangle:TabIndex = 6.
        THIS:btn_Rectangle:Text = "Rectangle".
        THIS:btn_Rectangle:UseVisualStyleBackColor = FALSE.
        THIS:btn_Rectangle:Click:Subscribe(btn_Rectangle_Click).

        THIS:btn_Square:BackColor = SystemColors:Control.
        THIS:btn_Square:Font = NEW Font("Microsoft Sans Serif", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:btn_Square:Location = NEW Point(3, 32).
        THIS:btn_Square:Name = "btn_Square".
        THIS:btn_Square:Size = NEW Size(122, 28).
        THIS:btn_Square:TabIndex = 5.
        THIS:btn_Square:Text = "Square".
        THIS:btn_Square:UseVisualStyleBackColor = FALSE.
        THIS:btn_Square:Click:Subscribe(btn_Square_Click).

        THIS:btn_CanvasColor:BackColor = SystemColors:Control.
        THIS:btn_CanvasColor:Location = NEW Point(3, 34).
        THIS:btn_CanvasColor:Name = "btn_CanvasColor".
        THIS:btn_CanvasColor:Size = NEW Size(122, 28).
        THIS:btn_CanvasColor:TabIndex = 4.
        THIS:btn_CanvasColor:UseVisualStyleBackColor = FALSE.
        THIS:btn_CanvasColor:Click:Subscribe(btn_CanvasColor_Click).

        THIS:button1:BackColor = Color:Black.
        THIS:button1:Location = NEW Point(3, 65).
        THIS:button1:Name = "button1".
        THIS:button1:Size = NEW Size(122, 28).
        THIS:button1:TabIndex = 2.
        THIS:button1:UseVisualStyleBackColor = FALSE.

        THIS:button2:Image = Image:FromFile("C:\OpenEdge\WRK\OOL\NET\Resources\pencil.png").
        THIS:button2:Location = NEW Point(4, 4).
        THIS:button2:Name = "button2".
        THIS:button2:Size = NEW Size(122, 28).
        THIS:button2:TabIndex = 0.
        THIS:button2:Text = "Pen Tool".
        THIS:button2:TextImageRelation = TextImageRelation:ImageBeforeText.
        THIS:button2:UseVisualStyleBackColor = TRUE.

        THIS:btn_PenColor:BackColor = Color:Black.
        THIS:btn_PenColor:Location = NEW Point(3, 65).
        THIS:btn_PenColor:Name = "btn_PenColor".
        THIS:btn_PenColor:Size = NEW Size(122, 28).
        THIS:btn_PenColor:TabIndex = 2.
        THIS:btn_PenColor:UseVisualStyleBackColor = FALSE.
        THIS:btn_PenColor:Click:Subscribe(btn_PenColor_Click).

        THIS:btn_ClearCanvas:BackColor = SystemColors:Control.
        THIS:btn_ClearCanvas:Font = NEW Font("Microsoft Sans Serif", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:btn_ClearCanvas:Location = NEW Point(3, 74).
        THIS:btn_ClearCanvas:Name = "btn_ClearCanvas".
        THIS:btn_ClearCanvas:Size = NEW Size(122, 28).
        THIS:btn_ClearCanvas:TabIndex = 5.
        THIS:btn_ClearCanvas:Text = "Clear".
        THIS:btn_ClearCanvas:UseVisualStyleBackColor = FALSE.
        THIS:btn_ClearCanvas:Click:Subscribe(btn_ClearCanvas_Click).

        THIS:tempObjectL = CAST(Array:CreateInstance(TypeHelper:GetType("System.Object"), 4), "System.Object[]").
        DO i_int = 1 TO 4:
            THIS:tempObject = STRING(i_int).
            THIS:tempObjectL:SetValue(THIS:tempObject, i_int - 1).
        END.

        THIS:comboBox1:FormattingEnabled = TRUE.
        THIS:comboBox1:Items:AddRange(THIS:tempObjectL).
        THIS:comboBox1:Location = NEW Point(4, 38).
        THIS:comboBox1:Name = "comboBox1".
        THIS:comboBox1:Size = NEW Size(122, 21).
        THIS:comboBox1:TabIndex = 1.
        THIS:comboBox1:Text = "1".

        THIS:cmb_PenSize:FormattingEnabled = TRUE.
        THIS:cmb_PenSize:Items:AddRange(THIS:tempObjectL).
        THIS:cmb_PenSize:Location = NEW Point(4, 38).
        THIS:cmb_PenSize:Name = "cmb_PenSize".
        THIS:cmb_PenSize:Size = NEW Size(122, 21).
        THIS:cmb_PenSize:TabIndex = 1.
        THIS:cmb_PenSize:Text = "1".

        THIS:tempStripL = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 2),
                               "System.Windows.Forms.ToolStripItem[]").
        THIS:tempStripL:SetValue(THIS:fileToolStripMenuItem, 0).
        THIS:tempStripL:SetValue(THIS:helpToolStripMenuItem, 1).
        
        THIS:menuStrip1:Items:AddRange(THIS:tempStripL).
        THIS:menuStrip1:Location = NEW Point(0, 0).
        THIS:menuStrip1:Name = "menuStrip1".
        THIS:menuStrip1:Size = NEW Size(744, 24).
        THIS:menuStrip1:TabIndex = 1.
        THIS:menuStrip1:Text = "menuStrip1".

        THIS:tempStripL:SetValue(THIS:newToolStripMenuItem, 0).
        THIS:tempStripL:SetValue(THIS:exitToolStripMenuItem, 1).

        THIS:fileToolStripMenuItem:DropDownItems:AddRange(THIS:tempStripL).
        THIS:fileToolStripMenuItem:Name = "fileToolStripMenuItem".
        THIS:fileToolStripMenuItem:Size = NEW Size(37, 20).
        THIS:fileToolStripMenuItem:Text = "File".

        THIS:newToolStripMenuItem:Name = "newToolStripMenuItem".
        THIS:newToolStripMenuItem:Size = NEW Size(98, 22).
        THIS:newToolStripMenuItem:Text = "New".
        THIS:newToolStripMenuItem:Click:Subscribe(newToolStripMenuItem_Click).

        THIS:exitToolStripMenuItem:Name = "exitToolStripMenuItem".
        THIS:exitToolStripMenuItem:Size = NEW Size(98, 22).
        THIS:exitToolStripMenuItem:Text = "Exit".
        THIS:exitToolStripMenuItem:Click:Subscribe(exitToolStripMenuItem_Click).

        THIS:tempStripL = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.ToolStripMenuItem"), 1),
                               "System.Windows.Forms.ToolStripMenuItem[]").
        THIS:tempStripL:SetValue(THIS:aboutMiniPaintToolStripMenuItem, 0).

        THIS:helpToolStripMenuItem:DropDownItems:AddRange(THIS:tempStripL).
        THIS:helpToolStripMenuItem:Name = "helpToolStripMenuItem".
        THIS:helpToolStripMenuItem:Size = NEW Size(44, 20).
        THIS:helpToolStripMenuItem:Text = "Help".

        THIS:aboutMiniPaintToolStripMenuItem:Name = "aboutMiniPaintToolStripMenuItem".
        THIS:aboutMiniPaintToolStripMenuItem:Size = NEW Size(164, 22).
        THIS:aboutMiniPaintToolStripMenuItem:Text = "About Mini Paint".
        THIS:aboutMiniPaintToolStripMenuItem:Click:Subscribe(aboutMiniPaintToolStripMenuItem_Click).

        THIS:AutoScaleDimensions = NEW SizeF(6, 13).
        THIS:AutoScaleMode = AutoScaleMode:Font.
        THIS:ClientSize = NEW Size(744, 451).
        THIS:Controls:Add(THIS:pnl_Draw).
        THIS:Controls:Add(THIS:panel1).
        THIS:Controls:Add(THIS:menuStrip1).
        THIS:MaximizeBox = FALSE.
        THIS:Name = "Form1".
        THIS:StartPosition = FormStartPosition:CenterScreen.
        THIS:Text = "Mini Paint".

        THIS:ResumeLayout(FALSE).
        THIS:PerformLayout().
    END METHOD.

    METHOD PRIVATE VOID pnl_Draw_MouseDown (INPUT sender AS Object,
                                            INPUT e      AS MouseEventArgs):
        THIS:startPaint = TRUE.
        IF THIS:drawSquare THEN
        DO:
            THIS:sb = NEW SolidBrush(THIS:btn_PenColor:BackColor).
            THIS:graphics:FillRectangle(THIS:sb, e:X, e:Y, INTEGER(THIS:txt_ShapeSize:Text), INTEGER(THIS:txt_ShapeSize:Text)).
            THIS:startPaint = FALSE.
            THIS:drawSquare = FALSE.
        END.
        IF THIS:drawRectangle THEN
        DO:
            THIS:sb = NEW SolidBrush(THIS:btn_PenColor:BackColor).
            THIS:graphics:FillRectangle(THIS:sb, e:X, e:Y, 2 * INTEGER(THIS:txt_ShapeSize:Text), INTEGER(THIS:txt_ShapeSize:Text)).
            THIS:startPaint = FALSE.
            THIS:drawRectangle = FALSE.
        END.
        IF THIS:drawCircle THEN
        DO:
            THIS:sb = NEW SolidBrush(THIS:btn_PenColor:BackColor).
            THIS:graphics:FillEllipse(THIS:sb, e:X, e:Y, INTEGER(THIS:txt_ShapeSize:Text), INTEGER(THIS:txt_ShapeSize:Text)).
            THIS:startPaint = FALSE.
            THIS:drawCircle = FALSE.
        END.
    END METHOD.

    METHOD PRIVATE VOID pnl_Draw_MouseMove (INPUT sender AS Object,
                                            INPUT e      AS MouseEventArgs):
        IF THIS:startPaint THEN
        DO:
            THIS:p = NEW Pen(THIS:btn_PenColor:BackColor, DECIMAL(THIS:cmb_PenSize:Text)).
            THIS:graphics:DrawLine(THIS:p,
                                   NEW Point(IF THIS:initX = ? THEN e:X ELSE THIS:initX,
                                             IF THIS:initY = ? THEN e:Y ELSE THIS:initY),
                                   NEW Point(e:X, e:Y)).
            THIS:initX = e:X.
            THIS:initY = e:Y.
        END.
    END METHOD.

    METHOD PRIVATE VOID pnl_Draw_MouseUp (INPUT sender AS Object,
                                          INPUT e      AS MouseEventArgs):
        THIS:startPaint = FALSE.
        THIS:initX = ?.
        THIS:initY = ?.
    END METHOD.

    METHOD PRIVATE VOID btn_Circle_Click (INPUT sender AS Object,
                                          INPUT e      AS EventArgs):
        THIS:drawCircle = TRUE.
    END METHOD.

    METHOD PRIVATE VOID btn_Rectangle_Click (INPUT sender AS Object,
                                             INPUT e      AS EventArgs):
        THIS:drawRectangle = TRUE.
    END METHOD.

    METHOD PRIVATE VOID btn_Square_Click (INPUT sender AS Object,
                                          INPUT e      AS EventArgs):
        THIS:drawSquare = TRUE.
    END METHOD.

    METHOD PRIVATE VOID btn_CanvasColor_Click (INPUT sender AS Object,
                                               INPUT e      AS EventArgs):
        THIS:cDialog = NEW ColorDialog().
        WAIT-FOR THIS:cDialog:ShowDialog().
        
        DO:
            THIS:pnl_Draw:BackColor = THIS:cDialog:Color.
            THIS:btn_CanvasColor:BackColor = THIS:cDialog:Color.
        END.
    END METHOD.

    METHOD PRIVATE VOID btn_PenColor_Click (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
        THIS:cDialog = NEW ColorDialog().
        WAIT-FOR THIS:cDialog:ShowDialog().

        DO:
            THIS:btn_PenColor:BackColor = cDialog:Color.
        END.
    END METHOD.

    METHOD PRIVATE VOID btn_ClearCanvas_Click (INPUT sender AS Object,
                                               INPUT e      AS EventArgs):
        IF MessageBox:Show("Are you sure you want to clear canvas?",
                           "Warning",
                           MessageBoxButtons:YesNo,
                           MessageBoxIcon:Warning) = DialogResult:Yes THEN
        DO:
            THIS:graphics:Clear(THIS:pnl_Draw:BackColor).
        END.
    END METHOD.

    METHOD PRIVATE VOID newToolStripMenuItem_Click (INPUT sender AS Object,
                                                    INPUT e      AS EventArgs):
        THIS:graphics:Clear(THIS:pnl_Draw:BackColor).
        THIS:pnl_Draw:BackColor = Color:White.
        THIS:btn_CanvasColor:BackColor = Color:White.
    END METHOD.

    METHOD PRIVATE VOID exitToolStripMenuItem_Click (INPUT sender AS Object,
                                                     INPUT e      AS EventArgs):
        IF MessageBox:Show("Do you want to Exit?", 
                           "Exit", 
                           MessageBoxButtons:YesNo, 
                           MessageBoxIcon:Information) = DialogResult:Yes THEN
        DO:
            Application:Exit().
        END.
    END METHOD.

    METHOD PRIVATE VOID aboutMiniPaintToolStripMenuItem_Click (INPUT sender AS Object,
                                                               INPUT e      AS EventArgs):
        MessageBox:Show("This is a recreation of a Simple Paint Application in C# by Anoop Kr Sharma." + CHR(13) + CHR(10) +
                        "This is a demonstration of OpenEdge .NET OOP using the C# DLL Libraries."     + CHR(13) + CHR(10) +
                        "Source project can be found online at:"                                       + CHR(13) + CHR(10) +
                        "'https://www.codeproject.com/Tips/811495/Simple-Paint-Application-in-Csharp'", "About", MessageBoxButtons:Ok, MessageBoxIcon:Information).
    END METHOD.
END CLASS.