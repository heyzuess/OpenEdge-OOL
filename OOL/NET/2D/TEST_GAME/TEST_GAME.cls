/*****************************************************************************************
  Copyright © 2019 by Jesse Iberri <jesseiberri@gmail.com>
  Released under the GNU General Public License GPL-3.0-or-later

  This is a recreation of a C# Simple 2D Game Physics Application by Hydeen.
  It is intended soley as a demonstration of OpenEdge .NET OOP using the 
  C# DLL Libraries. Source project and code is licensed under CPOL License 1.02.
  Source code, project, and license information can be found online at:
  https://www.codeproject.com/Tips/881397/Csharp-Simple-D-Game-Physics-Gravity-Jumping-Movem

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

  This program is unfinished and therefore will not run. This is published so as
  to share progress with project.

  Name     : TEST_GAME.cls
  Author   : Jesse Iberri
  Date     : 10/15/2019
  Purpose  : Openedge NET application showing a simple 2D game with physics.

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

USING OOL.NET.2D.TEST_GAME.*.

CLASS TEST_GAME INHERITS Form USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY THIS AS TEST_GAME
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE graphics               AS Graphics                               NO-UNDO.
    DEFINE PRIVATE VARIABLE components             AS IContainer                             NO-UNDO.
    DEFINE PRIVATE VARIABLE resources              AS ComponentResourceManager               NO-UNDO.

    DEFINE PRIVATE VARIABLE WorldFrame             AS Panel                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE contextMenuStrip1      AS ContextMenuStrip                       NO-UNDO.
    DEFINE PRIVATE VARIABLE debugToolStripMenuItem AS ToolStripMenuItem                      NO-UNDO.
    DEFINE PRIVATE VARIABLE onToolStripMenuItem    AS ToolStripMenuItem                      NO-UNDO.
    DEFINE PRIVATE VARIABLE offToolStripMenuItem   AS ToolStripMenuItem                      NO-UNDO.
    DEFINE PRIVATE VARIABLE pb_NPC2                AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE pb_NPC1                AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE label_Dead             AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE pb_Block1              AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE pb_Block2              AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE pb_Pipe                AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE label2                 AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE label1                 AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE label_Score            AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE pb_Player              AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE timer_Gravity          AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE timer_Jump             AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE timer_Anim             AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE timer_Randombomb       AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE timer_BombFailsafe     AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE timer_Sec              AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE timerBoomRemove        AS Timer                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE WorldFloor             AS Panel                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_MSpeed           AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_Log              AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_PSpeed           AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_Godmode          AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_MJump            AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_NoBombs          AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_PJump            AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_PGravity         AS Label                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE debug_MGravity         AS Label                                  NO-UNDO.

    DEFINE PRIVATE VARIABLE tempToolStr            AS "System.Windows.Forms.ToolStripItem[]" NO-UNDO.
    DEFINE PRIVATE VARIABLE tempPictureBox         AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE temp1                  AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE tempControl            AS Control                                NO-UNDO.
    DEFINE PRIVATE VARIABLE tempSF                 AS SaveFileDialog                         NO-UNDO.
    DEFINE PRIVATE VARIABLE tempDR                 AS DialogResult                           NO-UNDO.
    DEFINE PRIVATE VARIABLE tempSW                 AS System.IO.StreamWriter                 NO-UNDO.
    DEFINE PRIVATE VARIABLE i_int                  AS INTEGER                                NO-UNDO.

    DEFINE PRIVATE VARIABLE Bombs                  AS "System.Windows.Forms.PictureBox[]"    NO-UNDO.
    DEFINE PRIVATE VARIABLE Explosives             AS "System.Windows.Forms.PictureBox[]"    NO-UNDO.
    DEFINE PRIVATE VARIABLE WorldObjects           AS "System.Windows.Forms.PictureBox[]"    NO-UNDO.
    DEFINE PRIVATE VARIABLE DebugMenu              AS "System.Windows.Forms.Control[]"       NO-UNDO.
    DEFINE PRIVATE VARIABLE NPC                    AS "System.Windows.Forms.PictureBox[]"    NO-UNDO.
    DEFINE PRIVATE VARIABLE rng                    AS Random                                 NO-UNDO.
    DEFINE PRIVATE VARIABLE Player_Jump            AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Player_Left            AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Player_Right           AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE LastDirRight           AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE GameOn                 AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE GodMode                AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Debug                  AS LOGICAL                                NO-UNDO.
    DEFINE PRIVATE VARIABLE DebugLog               AS CHARACTER                              NO-UNDO.
    DEFINE PRIVATE VARIABLE Gravity                AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Anim                   AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Force                  AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE BombSize               AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Speed_Movement         AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Speed_Jump             AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Speed_Fall             AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Score                  AS INTEGER                                NO-UNDO.

    CONSTRUCTOR TEST_GAME ():
        SUPER().
        InitializeComponent().
        THIS:ComponentsCollection:Add(THIS:components).

        WAIT-FOR Application:Run(THIS).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR TEST_GAME ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent ():
        THIS:SuspendLayout().

        THIS:components = NEW Container().
        THIS:resources  = NEW ComponentResourceManager(TypeHelper:GetType("System.Windows.Forms.Form")).

        THIS:WorldFrame             = NEW Panel().
        THIS:contextMenuStrip1      = NEW ContextMenuStrip(THIS:components).
        THIS:debugToolStripMenuItem = NEW ToolStripMenuItem().
        THIS:onToolStripMenuItem    = NEW ToolStripMenuItem().
        THIS:offToolStripMenuItem   = NEW ToolStripMenuItem().
        THIS:pb_NPC2                = NEW PictureBox().
        THIS:pb_NPC1                = NEW PictureBox().
        THIS:label_Dead             = NEW Label().
        THIS:pb_Block1              = NEW PictureBox().
        THIS:pb_Block2              = NEW PictureBox().
        THIS:pb_Pipe                = NEW PictureBox().
        THIS:label2                 = NEW Label().
        THIS:label1                 = NEW Label().
        THIS:label_Score            = NEW Label().
        THIS:pb_Player              = NEW PictureBox().
        THIS:timer_Gravity          = NEW Timer(THIS:components).
        THIS:timer_Jump             = NEW Timer(THIS:components).
        THIS:timer_Anim             = NEW Timer(THIS:components).
        THIS:timer_Randombomb       = NEW Timer(THIS:components).
        THIS:timer_BombFailsafe     = NEW Timer(THIS:components).
        THIS:timer_Sec              = NEW Timer(THIS:components).
        THIS:timerBoomRemove        = NEW Timer(THIS:components).
        THIS:WorldFloor             = NEW Panel().
        THIS:debug_MSpeed           = NEW Label().
        THIS:debug_Log              = NEW Label().
        THIS:debug_PSpeed           = NEW Label().
        THIS:debug_Godmode          = NEW Label().
        THIS:debug_MJump            = NEW Label().
        THIS:debug_NoBombs          = NEW Label().
        THIS:debug_PJump            = NEW Label().
        THIS:debug_PGravity         = NEW Label().
        THIS:debug_MGravity         = NEW Label().

        CAST(THIS:pb_NPC2, ISupportInitialize):BeginInit().
        CAST(THIS:pb_NPC1, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Block1, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Block2, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Pipe, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Player, ISupportInitialize):BeginInit().

        THIS:WorldFrame:BackColor = Color:SkyBlue.
        THIS:WorldFrame:BackgroundImageLayout = ImageLayout:Stretch.
        THIS:WorldFrame:ContextMenuStrip = THIS:contextMenuStrip1.
        THIS:WorldFrame:Controls:Add(THIS:pb_NPC2).
        THIS:WorldFrame:Controls:Add(THIS:pb_NPC1).
        THIS:WorldFrame:Controls:Add(THIS:label_Dead).
        THIS:WorldFrame:Controls:Add(THIS:pb_Block1).
        THIS:WorldFrame:Controls:Add(THIS:pb_Block2).
        THIS:WorldFrame:Controls:Add(THIS:pb_Pipe).
        THIS:WorldFrame:Controls:Add(THIS:label2).
        THIS:WorldFrame:Controls:Add(THIS:label1).
        THIS:WorldFrame:Controls:Add(THIS:label_Score).
        THIS:WorldFrame:Controls:Add(THIS:pb_Player).
        THIS:WorldFrame:Dock = DockStyle:Top.
        THIS:WorldFrame:Location = NEW Point(0, 0).
        THIS:WorldFrame:Size = NEW Size(656, 241).
        THIS:WorldFrame:TabIndex = 0.

        THIS:tempToolStr = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 1),
                                "System.Windows.Forms.ToolStripItem[]").
        THIS:tempToolStr:SetValue(THIS:debugToolStripMenuItem, 0).

        THIS:contextMenuStrip1:Items:AddRange(THIS:tempToolStr).
        THIS:contextMenuStrip1:Name = "contextMenuStrip1".
        THIS:contextMenuStrip1:Size = NEW Size(110, 26).

        THIS:tempToolStr = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 2),
                                "System.Windows.Forms.ToolStripItem[]").
        THIS:tempToolStr:SetValue(THIS:onToolStripMenuItem, 0).
        THIS:tempToolStr:SetValue(THIS:offToolStripMenuItem, 1).

        THIS:debugToolStripMenuItem:DropDownItems:AddRange(THIS:tempToolStr).
        THIS:debugToolStripMenuItem:Name = "debugToolStripMenuItem".
        THIS:debugToolStripMenuItem:Size = NEW Size(109, 22).
        THIS:debugToolStripMenuItem:Text = "Debug".

        THIS:onToolStripMenuItem:Name = "onToolStripMenuItem".
        THIS:onToolStripMenuItem:Size = NEW Size(91, 22).
        THIS:onToolStripMenuItem:Text = "On".
        THIS:onToolStripMenuItem:Click:Subscribe(onToolStripMenuItem_Click).

        THIS:offToolStripMenuItem:Name = "offToolStripMenuItem".
        THIS:offToolStripMenuItem:Size = NEW Size(91, 22).
        THIS:offToolStripMenuItem:Text = "Off".
        THIS:offToolStripMenuItem:Click:Subscribe(offToolStripMenuItem_Click).

        THIS:pb_NPC2:Image = Image:FromFile("").
        THIS:pb_NPC2:Location = NEW Point(38, 64).
        THIS:pb_NPC2:Name = "pb_NPC2".
        THIS:pb_NPC2:Size = NEW Size(20, 20).
        THIS:pb_NPC2:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:pb_NPC2:TabIndex = 10.
        THIS:pb_NPC2:TabStop = FALSE.

        THIS:pb_NPC1:Image = Image:FromFile("").
        THIS:pb_NPC1:Location = NEW Point(12, 64).
        THIS:pb_NPC1:Name = "pb_NPC1".
        THIS:pb_NPC1:Size = NEW Size(20, 20).
        THIS:pb_NPC1:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:pb_NPC1:TabIndex = 9.
        THIS:pb_NPC1:TabStop = FALSE.

        THIS:label_Dead:AutoSize = TRUE.
        THIS:label_Dead:BackColor = Color:Transparent.
        THIS:label_Dead:Font = NEW Font("Consolas", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label_Dead:Location = NEW Point(236, 113).
        THIS:label_Dead:Name = "label_Dead".
        THIS:label_Dead:Size = NEW Size(187, 13).
        THIS:label_Dead:TabIndex = 1.
        THIS:label_Dead:Text = "You died, press Space to start".
        THIS:label_Dead:Visible = FALSE.

        THIS:pb_Block1:BackColor = Color:Gray.
        THIS:pb_Block1:BackgroundImage = Image:FromFile("").
        THIS:pb_Block1:BorderStyle = BorderStyle:FixedSingle.
        THIS:pb_Block1:Location = NEW Point(451, 182).
        THIS:pb_Block1:Name = "pb_Block1".
        THIS:pb_Block1:Size = NEW Size(151, 24).
        THIS:pb_Block1:TabIndex = 8.
        THIS:pb_Block1:TabStop = FALSE.

        THIS:pb_Block2:BackColor = Color:Gray.
        THIS:pb_Block2:BackgroundImage = Image:FromFile("").
        THIS:pb_Block2:BorderStyle = BorderStyle:FixedSingle.
        THIS:pb_Block2:Location = NEW Point(38, 157).
        THIS:pb_Block2:Name = "pb_Block2".
        THIS:pb_Block2:Size = NEW Size(135, 24).
        THIS:pb_Block2:TabIndex = 7.
        THIS:pb_Block2:TabStop = FALSE.

        THIS:pb_Pipe:BackgroundImage = Image:FromFile("").
        THIS:pb_Pipe:Location = NEW Point(250, 196).
        THIS:pb_Pipe:Name = "pb_Pipe".
        THIS:pb_Pipe:Size = NEW Size(35, 45).
        THIS:pb_Pipe:TabIndex = 5.
        THIS:pb_Pipe:TabStop = FALSE.

        THIS:label2:AutoSize = TRUE.
        THIS:label2:BackColor = Color:Transparent.
        THIS:label2:Font = NEW Font("Consolas", 8.25, Fontstyle:Bold).
        THIS:label2:Location = NEW Point(4, 22).
        THIS:label2:Name = "label2".
        THIS:label2:Size = NEW Size(79, 13).
        THIS:label2:TabIndex = 3.
        THIS:label2:Text = "Highscore: 0".

        THIS:label1:AutoSize = TRUE.
        THIS:label1:BackColor = Color:Transparent.
        THIS:label1:Font = NEW Font("Consolas", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label1:Location = NEW Point(4, 35).
        THIS:label1:Name = "label1".
        THIS:label1:Size = NEW Size(49, 13).
        THIS:label1:TabIndex = 3.
        THIS:label1:Text = "Bombs: ".

        THIS:label_Score:AutoSize = TRUE.
        THIS:label_Score:BackColor = Color:Transparent.
        THIS:label_Score:Font = NEW Font("Consolas", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label_Score:Location = NEW Point(4, 9).
        THIS:label_Score:Name = "label_Score".
        THIS:label_Score:Size = NEW Size(55, 13).
        THIS:label_Score:TabIndex = 2.
        THIS:label_Score:Text = "Score: 0".

        THIS:pb_Player:BackColor = Color:Transparent.
        THIS:pb_Player:Image = Image:FromFile("").
        THIS:pb_Player:Location = NEW Point(12, 94).
        THIS:pb_Player:Name = "pb_Player".
        THIS:pb_Player:Size = NEW Size(16, 32).
        THIS:pb_Player:TabIndex = 0.
        THIS:pb_Player:TabStop = FALSE.

        THIS:timer_Gravity:Enabled = TRUE.
        THIS:timer_Gravity:Interval = 1.
        THIS:timer_Gravity:Tick:Subscribe(timer_Gravity_Tick).

        THIS:timer_Jump:Enabled = TRUE.
        THIS:timer_Jump:Interval = 1.
        THIS:timer_Jump:Tick:Subscribe(timer_Jump_Tick).

        THIS:timer_Anim:Enabled = TRUE.
        THIS:timer_Anim:Interval = 1.
        THIS:timer_Anim:Tick:Subscribe(timer_Anim_Tick).

        THIS:timer_Randombomb:Enabled = TRUE.
        THIS:timer_Randombomb:Interval = 800.
        THIS:timer_Randombomb:Tick:Subscribe(timer_Randombomb_Tick).

        THIS:timer_BombFailsafe:Enabled = TRUE.
        THIS:timer_BombFailsafe:Interval = 3000.
        THIS:timer_BombFailsafe:Tick:Subscribe(timer_BombFailsafe_Tick).

        THIS:timer_Sec:Enabled = TRUE.
        THIS:timer_Sec:Interval = 1000.
        THIS:timer_Sec:Tick:Subscribe(timer_Sec_Tick).

        THIS:timerBoomRemove:Enabled = TRUE.
        THIS:timerBoomRemove:Tick:Subscribe(timerBoomRemove_Tick).

        THIS:WorldFloor:BackgroundImage = Image:FromFile("").
        THIS:WorldFloor:Controls:Add(THIS:debug_MSpeed).
        THIS:WorldFloor:Controls:Add(THIS:debug_Log).
        THIS:WorldFloor:Controls:Add(THIS:debug_PSpeed).
        THIS:WorldFloor:Controls:Add(THIS:debug_Godmode).
        THIS:WorldFloor:Controls:Add(THIS:debug_MJump).
        THIS:WorldFloor:Controls:Add(THIS:debug_NoBombs).
        THIS:WorldFloor:Controls:Add(THIS:debug_PJump).
        THIS:WorldFloor:Controls:Add(THIS:debug_PGravity).
        THIS:WorldFloor:Controls:Add(THIS:debug_MGravity).
        THIS:WorldFloor:Dock = DockStyle:Bottom.
        THIS:WorldFloor:Location = NEW Point(0, 240).
        THIS:WorldFloor:Name = "WorldFloor".
        THIS:WorldFloor:Size = NEW Size(656, 32).
        THIS:WorldFloor:TabIndex = 1.

        THIS:debug_MSpeed:AutoSize = TRUE.
        THIS:debug_MSpeed:BackColor = Color:Transparent.
        THIS:debug_MSpeed:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_MSpeed:ForeColor = Color:Black.
        THIS:debug_MSpeed:Location = NEW Point(584, 13).
        THIS:debug_MSpeed:Name = "debug_MSpeed".
        THIS:debug_MSpeed:Size = NEW Size(61, 13).
        THIS:debug_MSpeed:TabIndex = 8.
        THIS:debug_MSpeed:Text = "[- Speed]".
        THIS:debug_MSpeed:Visible = FALSE.
        THIS:debug_MSpeed:Click:Subscribe(debug_MSpeed_Click).

        THIS:debug_Log:AutoSize = TRUE.
        THIS:debug_Log:BackColor = Color:Transparent.
        THIS:debug_Log:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_Log:ForeColor = Color:Black.
        THIS:debug_Log:Location = NEW Point(1, 13).
        THIS:debug_Log:Name = "debug_Log".
        THIS:debug_Log:Size = NEW Size(73, 13).
        THIS:debug_Log:TabIndex = 0.
        THIS:debug_Log:Text = "[Debug log]".
        THIS:debug_Log:Visible = FALSE.
        THIS:debug_Log:Click:Subscribe(debug_Log_Click).

        THIS:debug_PSpeed:AutoSize = TRUE.
        THIS:debug_PSpeed:BackColor = Color:Transparent.
        THIS:debug_PSpeed:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_PSpeed:ForeColor = Color:Black.
        THIS:debug_PSpeed:Location = NEW Point(522, 13).
        THIS:debug_PSpeed:Name = "debug_PSpeed".
        THIS:debug_PSpeed:Size = NEW Size(61, 13).
        THIS:debug_PSpeed:TabIndex = 7.
        THIS:debug_PSpeed:Text = "[+ Speed]".
        THIS:debug_PSpeed:Visible = FALSE.
        THIS:debug_PSpeed:Click:Subscribe(debug_PSpeed_Click).

        THIS:debug_Godmode:AutoSize = TRUE.
        THIS:debug_Godmode:BackColor = Color:Transparent.
        THIS:debug_Godmode:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_Godmode:ForeColor = Color:Black.
        THIS:debug_Godmode:Location = NEW Point(82, 13).
        THIS:debug_Godmode:Name = "debug_Godmode".
        THIS:debug_Godmode:Size = NEW Size(103, 13).
        THIS:debug_Godmode:TabIndex = 1.
        THIS:debug_Godmode:Text = "[Toggle Godmode]".
        THIS:debug_Godmode:Visible = FALSE.
        THIS:debug_Godmode:Click:Subscribe(debug_Godmode_Click).

        THIS:debug_MJump:AutoSize = TRUE.
        THIS:debug_MJump:BackColor = Color:Transparent.
        THIS:debug_MJump:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_MJump:ForeColor = Color:Black.
        THIS:debug_MJump:Location = NEW Point(464, 13).
        THIS:debug_MJump:Name = "debug_MJump".
        THIS:debug_MJump:Size = NEW Size(55, 13).
        THIS:debug_MJump:TabIndex = 6.
        THIS:debug_MJump:Text = "[- Jump]".
        THIS:debug_MJump:Visible = FALSE.
        THIS:debug_MJump:Click:Subscribe(debug_MJump_Click).

        THIS:debug_NoBombs:AutoSize = TRUE.
        THIS:debug_NoBombs:BackColor = Color:Transparent.
        THIS:debug_NoBombs:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_NoBombs:ForeColor = Color:Black.
        THIS:debug_NoBombs:Location = NEW Point(189, 13).
        THIS:debug_NoBombs:Name = "debug_NoBombs".
        THIS:debug_NoBombs:Size = NEW Size(67, 13).
        THIS:debug_NoBombs:TabIndex = 2.
        THIS:debug_NoBombs:Text = "[No bombs]".
        THIS:debug_NoBombs:Visible = FALSE.
        THIS:debug_NoBombs:Click:Subscribe(debug_NoBombs_Click).

        THIS:debug_PJump:AutoSize = TRUE.
        THIS:debug_PJump:BackColor = Color:Transparent.
        THIS:debug_PJump:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_PJump:ForeColor = Color:Black.
        THIS:debug_PJump:Location = NEW Point(410, 13).
        THIS:debug_PJump:Name = "debug_PJump".
        THIS:debug_PJump:Size = NEW Size(55, 13).
        THIS:debug_PJump:TabIndex = 5.
        THIS:debug_PJump:Text = "[+ Jump]".
        THIS:debug_PJump:Visible = FALSE.
        THIS:debug_PJump:Click:Subscribe(debug_PJump_Click).

        THIS:debug_PGravity:AutoSize = TRUE.
        THIS:debug_PGravity:BackColor = Color:Transparent.
        THIS:debug_PGravity:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_PGravity:ForeColor = Color:Black.
        THIS:debug_PGravity:Location = NEW Point(261, 13).
        THIS:debug_PGravity:Name = "debug_PGravity".
        THIS:debug_PGravity:Size = NEW Size(73, 13).
        THIS:debug_PGravity:TabIndex = 3.
        THIS:debug_PGravity:Text = "[+ Gravity]".
        THIS:debug_PGravity:Visible = FALSE.
        THIS:debug_PGravity:Click:Subscribe(debug_PGravity_Click).

        THIS:debug_MGravity:AutoSize = TRUE.
        THIS:debug_MGravity:BackColor = Color:Transparent.
        THIS:debug_MGravity:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:debug_MGravity:ForeColor = Color:Black.
        THIS:debug_MGravity:Location = NEW Point(334, 13).
        THIS:debug_MGravity:Name = "debug_MGravity".
        THIS:debug_MGravity:Size = NEW Size(73, 13).
        THIS:debug_MGravity:TabIndex = 4.
        THIS:debug_MGravity:Text = "[- Gravity]".
        THIS:debug_MGravity:Visible = FALSE.
        THIS:debug_MGravity:Click:Subscribe(debug_MGravity_Click).

        THIS:AutoScaleDimensions = NEW SizeF(6, 13).
        THIS:AutoScaleMode = AutoScaleMode:Font.
        THIS:ClientSize = NEW Size(656, 272).
        THIS:Controls:Add(THIS:WorldFloor).
        THIS:Controls:Add(THIS:WorldFrame).
        THIS:FormBorderStyle = FormBorderStyle:FixedToolWindow.
        THIS:Icon = Icon:ExtractAssociatedIcon("").
        THIS:MaximizeBox = FALSE.
        THIS:MinimizeBox = FALSE.
        THIS:Name = "Main".
        THIS:Text = "Test Game".
        THIS:StartPosition = FormStartPosition:CenterScreen.
        THIS:Load:Subscribe(TEST_GAME_Load).

        CAST(THIS:pb_NPC2, ISupportInitialize):EndInit().
        CAST(THIS:pb_NPC1, ISupportInitialize):EndInit().
        CAST(THIS:pb_Block1, ISupportInitialize):EndInit().
        CAST(THIS:pb_Block2, ISupportInitialize):EndInit().
        CAST(THIS:pb_Pipe, ISupportInitialize):EndInit().
        CAST(THIS:pb_Player, ISupportInitialize):EndInit().

        THIS:ResumeLayout(FALSE).
        THIS:PerformLayout().

        THIS:Bombs        = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.PictureBox"), 10),
                                 "System.Windows.Forms.PictureBox[]").
        THIS:Explosives   = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.PictureBox"), 10),
                                 "System.Windows.Forms.PictureBox[]").
        THIS:WorldObjects = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.PictureBox"), 10),
                                 "System.Windows.Forms.PictureBox[]").

        THIS:DebugMenu    = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.Control"), 9),
                                 "System.Windows.Forms.Control[]").

        THIS:NPC          = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.PictureBox"), 2),
                                 "System.Windows.Forms.PictureBox[]").

        THIS:rng = NEW Random().
        THIS:Player_Jump  = FALSE.
        THIS:Player_Left  = FALSE.
        THIS:Player_Right = FALSE.
        THIS:LastDirRight = TRUE.
        THIS:GameOn       = FALSE.
        THIS:GodMode      = FALSE.
        THIS:Debug        = FALSE.

        THIS:DebugLog = "STARTED: " + STRING(DateTime:Now) + CHR(13) + CHR(10).

        THIS:Gravity        = 20.
        THIS:Anim           = 0.
        THIS:Force          = 0.
        THIS:BombSize       = 16.
        THIS:Speed_Movement = 3.
        THIS:Speed_Jump     = 3.
        THIS:Speed_Fall     = 3.
        THIS:Score          = 0.

    END METHOD.

    METHOD PRIVATE VOID onToolStripMenuItem_Click (INPUT sender AS Object,
                                                   INPUT e      AS EventArgs):
        DO i_int = 1 TO THIS:DebugMenu:Length:
            THIS:tempControl = CAST(THIS:DebugMenu:GetValue(i_int), Control).
            THIS:tempControl:Visible = TRUE.
        END.
    END METHOD.

    METHOD PRIVATE VOID offToolStripMenuItem_Click (INPUT sender AS Object,
                                                    INPUT e      AS EventArgs):
        DO i_int = 1 TO THIS:DebugMenu:Length:
            THIS:tempControl = CAST(THIS:DebugMenu:GetValue(i_int), Control).
            THIS:tempControl:Visible = FALSE.
        END.
    END METHOD.

    METHOD PRIVATE VOID timer_Gravity_Tick (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID timer_Jump_Tick (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID timer_Anim_Tick (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID timer_Randombomb_Tick (INPUT sender AS Object,
                                               INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID timer_BombFailsafe_Tick (INPUT sender AS Object,
                                                 INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID timer_Sec_Tick (INPUT sender AS Object,
                                        INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID timerBoomRemove_Tick (INPUT sender AS Object,
                                              INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID debug_Log_Click (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
        THIS:tempSF = NEW SaveFileDialog().
        THIS:tempSF:DefaultExt = "txt".

        WAIT-FOR THIS:tempSF:ShowDialog() SET THIS:tempDR.
        IF THIS:tempDR = DialogResult:OK THEN
        DO:
            THIS:tempSW = NEW System.IO.StreamWriter(THIS:tempSF:FileName).
            THIS:tempSW:WriteLine(THIS:DebugLog).
        END.
    END METHOD.

    METHOD PRIVATE VOID debug_Godmode_Click (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
        IF NOT THIS:GodMode THEN
        DO:
            THIS:GodMode = TRUE.
            THIS:pb_Player:BorderStyle = BorderStyle:FixedSingle.
        END.
        ELSE
        DO:
            THIS:GodMode = FALSE.
            THIS:pb_Player:BorderStyle = BorderStyle:None.
        END.
    END METHOD.

    METHOD PRIVATE VOID debug_NoBombs_Click (INPUT sender AS Object,
                                             INPUT e      AS EventArgs):
        IF NOT THIS:Debug THEN
        DO:
            THIS:Debug = TRUE.
            THIS:timer_Randombomb:Enabled = FALSE.
            THIS:timer_Sec:Enabled = FALSE.
        END.
        ELSE
        DO:
            THIS:Debug = FALSE.
            THIS:timer_Randombomb:Enabled = TRUE.
            THIS:timer_Sec:Enabled = TRUE.
        END.
    END METHOD.

    METHOD PRIVATE VOID debug_PJump_Click (INPUT sender AS Object,
                                           INPUT e      AS EventArgs):
        THIS:Speed_Jump = THIS:Speed_Jump + 1.
    END METHOD.

    METHOD PRIVATE VOID debug_MJump_Click (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
        THIS:Speed_Jump = THIS:Speed_Jump - 1.
    END METHOD.

    METHOD PRIVATE VOID debug_PGravity_Click (INPUT sender AS Object,
                                              INPUT e      AS EventArgs):
        THIS:Gravity = THIS:Gravity + 1.
    END METHOD.

    METHOD PRIVATE VOID debug_MGravity_Click (INPUT sender AS Object,
                                              INPUT e      AS EventArgs):
        THIS:Gravity = THIS:Gravity - 1.
    END METHOD.

    METHOD PRIVATE VOID debug_PSpeed_Click (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
        THIS:Speed_Movement = THIS:Speed_Movement + 1.
    END METHOD.

    METHOD PRIVATE VOID debug_MSpeed_Click (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
        THIS:Speed_Movement = THIS:Speed_Movement - 1.
    END METHOD.

    METHOD PRIVATE VOID TEST_GAME_Load (INPUT sender AS Object,
                                        INPUT e      AS EventArgs):
    END METHOD.

    METHOD PUBLIC LOGICAL InAirCollision (INPUT tar AS PictureBox):
        IF NOT THIS:OutSideWorldFrame(tar) THEN
        DO i_int = 1 TO THIS:WorldObjects:Length:
            THIS:tempPictureBox = CAST(THIS:WorldObjects:GetValue(i_int), PictureBox).

            IF NOT tar:Bounds:IntersectsWith(THIS:tempPictureBox:Bounds) THEN
            DO:
                IF tar:Location:Y < THIS:WorldFrame:Width THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL OutsideWorldFrame (INPUT tar AS PictureBox):
        IF tar:Location:X < 0 THEN
            RETURN TRUE.
        IF tar:Location:X > THIS:WorldFrame:Width THEN
            RETURN TRUE.
        IF tar:Location:Y + tar:Height > THIS:WorldFrame:Height - 3 THEN
            RETURN TRUE.

        DO i_int = 1 TO THIS:WorldObjects:Length:
            ASSIGN THIS:tempPictureBox = CAST(THIS:WorldObjects:GetValue(i_int), PictureBox).
            IF VALID-OBJECT(THIS:tempPictureBox) THEN
            DO:
                IF tar:Bounds:IntersectsWith(THIS:tempPictureBox:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Top (INPUT tar AS PictureBox):
        DO i_int = 1 TO THIS:WorldObjects:Length:
            THIS:tempPictureBox = CAST(THIS:WorldObjects:GetValue(i_int), PictureBox).

            IF VALID-OBJECT(THIS:tempPictureBox) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:tempPictureBox:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X - 3, THIS:temp1:Location:Y - 1, THIS:temp1:Width + 6, 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Bottom (INPUT tar AS PictureBox):
        DO i_int = 1 TO THIS:WorldObjects:Length:
            THIS:tempPictureBox = CAST(THIS:WorldObjects:GetValue(i_int), PictureBox).

            IF VALID-OBJECT(THIS:tempPictureBox) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:tempPictureBox:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X, THIS:temp1:Location:Y + THIS:temp1:Height, THIS:temp1:Width, 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Left (INPUT tar AS PictureBox):
        DO i_int = 1 TO THIS:WorldObjects:Length:
            ASSIGN THIS:tempPictureBox = CAST(THIS:WorldObjects:GetValue(i_int), PictureBox).

            IF VALID-OBJECT(THIS:tempPictureBox) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:tempPictureBox:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X - 1, THIS:temp1:Location:Y + 1, 1, THIS:temp1:Height - 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Right (INPUT tar AS PictureBox):
        DO i_int = 1 TO THIS:WorldObjects:Length:
            THIS:tempPictureBox = CAST(THIS:WorldObjects:GetValue(i_int), PictureBox).

            IF VALID-OBJECT(THIS:tempPictureBox) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:tempPictureBox:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X + THIS:temp1:Width, THIS:temp1:Location:Y + 1, 1, THIS:temp1:Height - 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC VOID Dead ():
        IF NOT THIS:GodMode THEN
        DO:
            THIS:pb_Player:Visible = FALSE.
            THIS:label_Dead:Visible = TRUE.
            THIS:GameOn = FALSE.
        END.
    END METHOD.
END CLASS.