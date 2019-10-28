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

CLASS OOL.NET.2D.TEST_GAME.TEST_GAME INHERITS Form USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY THIS AS OOL.NET.2D.TEST_GAME.TEST_GAME
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PROTECTED PROPERTY DebugFile  AS CHARACTER GET.
    PRIVATE SET.

    DEFINE PROTECTED PROPERTY DebugLog   AS CHARACTER GET.
    PRIVATE SET(INPUT cip_longchar AS CHARACTER):
        OUTPUT TO VALUE(THIS:DebugFile) APPEND.
            PUT UNFORMATTED cip_longchar.
        OUTPUT CLOSE.
        THIS:DebugLog = "".
    END SET.

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
    DEFINE PRIVATE VARIABLE tempArray              AS Array                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE tempSF                 AS SaveFileDialog                         NO-UNDO.
    DEFINE PRIVATE VARIABLE tempDR                 AS DialogResult                           NO-UNDO.
    DEFINE PRIVATE VARIABLE tempSW                 AS System.IO.StreamWriter                 NO-UNDO.
    DEFINE PRIVATE VARIABLE i_int                  AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE i_count                AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE i_set                  AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE c_temp                 AS CHARACTER                              NO-UNDO.

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
    //DEFINE PRIVATE VARIABLE DebugLog               AS LONGCHAR                               NO-UNDO.
    DEFINE PRIVATE VARIABLE Gravity                AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Anim                   AS INT64                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE Force                  AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE BombSize               AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Speed_Movement         AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Speed_Jump             AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Speed_Fall             AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE Score                  AS INTEGER                                NO-UNDO.

    DEFINE PRIVATE VARIABLE r                      AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE NextSpot               AS INTEGER                                NO-UNDO.
    DEFINE PRIVATE VARIABLE npcTemp                AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE Bomb                   AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE Boom                   AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE Obj1                   AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE Obj2                   AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE pb                     AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE Coin                   AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE pbR                    AS PictureBox                             NO-UNDO.
    DEFINE PRIVATE VARIABLE pbL                    AS PictureBox                             NO-UNDO.

    DEFINE PRIVATE VARIABLE Character              AS Actor                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE World                  AS World                                  NO-UNDO.
    DEFINE PRIVATE VARIABLE Enemy                  AS Enemy                                  NO-UNDO.

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
        THIS:DebugFile = "C:\OpenEdge\WRK\OOL\NET\2D\TEST_GAME\Debug\TEST_GAME.cls-" + STRING(TIME) + "_" + STRING(ETIME) + ".txt".

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
        //
        // Character Resources
        //
        THIS:Character = NEW Actor().
        THIS:Character:ResourceLocation = "C:\OpenEdge\WRK\OOL\NET\2D\TEST_GAME\Resources\".
        THIS:Character:jump_l  = THIS:Character:LoadFromDir("jump_l.png").
        THIS:Character:jump_r  = THIS:Character:LoadFromDir("jump_r.png").
        THIS:Character:stand_l = THIS:Character:LoadFromDir("stand_l.png").
        THIS:Character:stand_r = THIS:Character:LoadFromDir("stand_r.png").
        THIS:Character:walk_l  = THIS:Character:LoadFromDir("walk_l.gif").
        THIS:Character:walk_r  = THIS:Character:LoadFromDir("walk_r.gif").
        //
        // World Resources
        //
        THIS:World = NEW World().
        THIS:World:ResourceLocation = THIS:Character:ResourceLocation.
        THIS:World:Boom     = THIS:World:LoadFromDir("Boom.gif").
        THIS:World:Bush     = THIS:World:LoadFromDir("Bush.png").
        THIS:World:Clouds   = THIS:World:LoadFromDir("Clouds.png").
        THIS:World:Coin     = THIS:World:LoadFromDir("Coin.png").
        THIS:World:Floor    = THIS:World:LoadFromDir("floor.png").
        THIS:World:Pipe     = THIS:World:LoadFromDir("Pipe.png").
        THIS:World:Platform = THIS:World:LoadFromDir("Platform.png").
        //
        // Enemy Resources
        //
        THIS:Enemy = NEW Enemy().
        THIS:Enemy:ResourceLocation = THIS:Character:ResourceLocation.
        THIS:Enemy:Bomb        = THIS:Enemy:LoadFromDir("Bomb.png").
        THIS:Enemy:Enemy_left  = THIS:Enemy:LoadFromDir("Enemy-left.png").
        THIS:Enemy:Enemy_right = THIS:Enemy:LoadFromDir("Enemy-right.png").
        THIS:Enemy:Rocket_L    = THIS:Enemy:LoadFromDir("Rocket_L.png").
        THIS:Enemy:Rocket_R    = THIS:Enemy:LoadFromDir("Rocket_R.png").

        CAST(THIS:pb_NPC2, ISupportInitialize):BeginInit().
        CAST(THIS:pb_NPC1, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Block1, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Block2, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Pipe, ISupportInitialize):BeginInit().
        CAST(THIS:pb_Player, ISupportInitialize):BeginInit().
        //
        // WorldFrame
        //
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
        THIS:WorldFrame:Name = "World Frame".
        THIS:WorldFrame:Size = NEW Size(656, 241).
        THIS:WorldFrame:TabIndex = 0.
        //
        // Creating an array instance to populate menus with tool strip items
        // since the AddRange method only accepts ToolStripItem[] datatype
        //
        THIS:tempToolStr = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 1),
                                "System.Windows.Forms.ToolStripItem[]").
        THIS:tempToolStr:SetValue(THIS:debugToolStripMenuItem, 0).
        //
        // contextMenuStrip1
        //
        THIS:contextMenuStrip1:Items:AddRange(THIS:tempToolStr).
        THIS:contextMenuStrip1:Name = "contextMenuStrip1".
        THIS:contextMenuStrip1:Size = NEW Size(110, 26).
        //
        // Array for AddRange method
        //
        THIS:tempToolStr = CAST(Array:CreateInstance(TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 2),
                                "System.Windows.Forms.ToolStripItem[]").
        THIS:tempToolStr:SetValue(THIS:onToolStripMenuItem, 0).
        THIS:tempToolStr:SetValue(THIS:offToolStripMenuItem, 1).
        //
        // debugToolStripMenuItem
        //
        THIS:debugToolStripMenuItem:DropDownItems:AddRange(THIS:tempToolStr).
        THIS:debugToolStripMenuItem:Name = "debugToolStripMenuItem".
        THIS:debugToolStripMenuItem:Size = NEW Size(109, 22).
        THIS:debugToolStripMenuItem:Text = "Debug".
        //
        // onToolStripMenuItem
        //
        THIS:onToolStripMenuItem:Name = "onToolStripMenuItem".
        THIS:onToolStripMenuItem:Size = NEW Size(91, 22).
        THIS:onToolStripMenuItem:Text = "On".
        THIS:onToolStripMenuItem:Click:Subscribe(onToolStripMenuItem_Click).
        //
        // offToolStripMenuItem
        //
        THIS:offToolStripMenuItem:Name = "offToolStripMenuItem".
        THIS:offToolStripMenuItem:Size = NEW Size(91, 22).
        THIS:offToolStripMenuItem:Text = "Off".
        THIS:offToolStripMenuItem:Click:Subscribe(offToolStripMenuItem_Click).
        //
        // pb_NPC2
        //
        THIS:pb_NPC2:Image = THIS:Enemy:Enemy_left.
        THIS:pb_NPC2:Location = NEW Point(38, 64).
        THIS:pb_NPC2:Name = "pb_NPC2".
        THIS:pb_NPC2:Size = NEW Size(20, 20).
        THIS:pb_NPC2:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:pb_NPC2:TabIndex = 10.
        THIS:pb_NPC2:TabStop = FALSE.
        //
        // pb_NPC1
        //
        THIS:pb_NPC1:Image = THIS:Enemy:Enemy_right.
        THIS:pb_NPC1:Location = NEW Point(12, 64).
        THIS:pb_NPC1:Name = "pb_NPC1".
        THIS:pb_NPC1:Size = NEW Size(20, 20).
        THIS:pb_NPC1:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:pb_NPC1:TabIndex = 9.
        THIS:pb_NPC1:TabStop = FALSE.
        //
        // label_Dead
        //
        THIS:label_Dead:AutoSize = TRUE.
        THIS:label_Dead:BackColor = Color:Transparent.
        THIS:label_Dead:Font = NEW Font("Consolas", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label_Dead:Location = NEW Point(236, 113).
        THIS:label_Dead:Name = "label_Dead".
        THIS:label_Dead:Size = NEW Size(187, 13).
        THIS:label_Dead:TabIndex = 1.
        THIS:label_Dead:Text = "You died, press Space to start".
        THIS:label_Dead:Visible = FALSE.
        //
        // pb_Block1
        //
        THIS:pb_Block1:BackColor = Color:Gray.
        THIS:pb_Block1:BackgroundImage = THIS:World:Platform.
        THIS:pb_Block1:BorderStyle = BorderStyle:FixedSingle.
        THIS:pb_Block1:Location = NEW Point(451, 182).
        THIS:pb_Block1:Name = "pb_Block1".
        THIS:pb_Block1:Size = NEW Size(151, 24).
        THIS:pb_Block1:TabIndex = 8.
        THIS:pb_Block1:TabStop = FALSE.
        //
        // pb_Block2
        //
        THIS:pb_Block2:BackColor = Color:Gray.
        THIS:pb_Block2:BackgroundImage = THIS:World:Platform.
        THIS:pb_Block2:BorderStyle = BorderStyle:FixedSingle.
        THIS:pb_Block2:Location = NEW Point(38, 157).
        THIS:pb_Block2:Name = "pb_Block2".
        THIS:pb_Block2:Size = NEW Size(135, 24).
        THIS:pb_Block2:TabIndex = 7.
        THIS:pb_Block2:TabStop = FALSE.
        //
        // pb_Pipe
        //
        THIS:pb_Pipe:BackgroundImage = THIS:World:Pipe.
        THIS:pb_Pipe:Location = NEW Point(250, 196).
        THIS:pb_Pipe:Name = "pb_Pipe".
        THIS:pb_Pipe:Size = NEW Size(35, 45).
        THIS:pb_Pipe:TabIndex = 5.
        THIS:pb_Pipe:TabStop = FALSE.
        //
        // label2
        //
        THIS:label2:AutoSize = TRUE.
        THIS:label2:BackColor = Color:Transparent.
        THIS:label2:Font = NEW Font("Consolas", 8.25, Fontstyle:Bold).
        THIS:label2:Location = NEW Point(4, 22).
        THIS:label2:Name = "label2".
        THIS:label2:Size = NEW Size(79, 13).
        THIS:label2:TabIndex = 3.
        THIS:label2:Text = "Highscore: 0".
        //
        // label1
        //
        THIS:label1:AutoSize = TRUE.
        THIS:label1:BackColor = Color:Transparent.
        THIS:label1:Font = NEW Font("Consolas", 8.25, FontStyle:Bold).
        THIS:label1:Location = NEW Point(4, 38).
        THIS:label1:Name = "label1".
        THIS:label1:Size = NEW Size(49, 13).
        THIS:label1:TabIndex = 3.
        THIS:label1:Text = "Bombs: ".
        //
        // label_Score
        //
        THIS:label_Score:AutoSize = TRUE.
        THIS:label_Score:BackColor = Color:Transparent.
        THIS:label_Score:Font = NEW Font("Consolas", 8.25, FontStyle:Bold, GraphicsUnit:Point).
        THIS:label_Score:Location = NEW Point(4, 9).
        THIS:label_Score:Name = "label_Score".
        THIS:label_Score:Size = NEW Size(55, 13).
        THIS:label_Score:TabIndex = 2.
        THIS:label_Score:Text = "Score: 0".
        //
        // pb_Player
        //
        THIS:pb_Player:BackColor = Color:Transparent.
        THIS:pb_Player:Image = THIS:Character:stand_r.
        THIS:pb_Player:Location = NEW Point(12, 94).
        THIS:pb_Player:Name = "pb_Player".
        THIS:pb_Player:Size = NEW Size(16, 32).
        THIS:pb_Player:TabIndex = 0.
        THIS:pb_Player:TabStop = FALSE.
        //
        // timer_Gravity
        //
        THIS:timer_Gravity:Enabled = TRUE.
        THIS:timer_Gravity:Interval = 1.
        THIS:timer_Gravity:Tick:Subscribe(timer_Gravity_Tick).
        //
        // timer_Jump
        //
        THIS:timer_Jump:Enabled = TRUE.
        THIS:timer_Jump:Interval = 1.
        THIS:timer_Jump:Tick:Subscribe(timer_Jump_Tick).
        //
        // timer_Anim
        //
        THIS:timer_Anim:Enabled = TRUE.
        THIS:timer_Anim:Interval = 1.
        THIS:timer_Anim:Tick:Subscribe(timer_Anim_Tick).
        //
        // timer_Randombomb
        //
        THIS:timer_Randombomb:Enabled = TRUE.
        THIS:timer_Randombomb:Interval = 800.
        THIS:timer_Randombomb:Tick:Subscribe(timer_Randombomb_Tick).
        //
        // timer_BombFailsafe
        //
        THIS:timer_BombFailsafe:Enabled = TRUE.
        THIS:timer_BombFailsafe:Interval = 3000.
        THIS:timer_BombFailsafe:Tick:Subscribe(timer_BombFailsafe_Tick).
        //
        // timer_Sec
        //
        THIS:timer_Sec:Enabled = TRUE.
        THIS:timer_Sec:Interval = 1000.
        THIS:timer_Sec:Tick:Subscribe(timer_Sec_Tick).
        //
        // timerBoomRemove
        //
        THIS:timerBoomRemove:Enabled = TRUE.
        THIS:timerBoomRemove:Tick:Subscribe(timerBoomRemove_Tick).
        //
        // WorldFloor
        //
        THIS:WorldFloor:BackgroundImage = THIS:World:Floor.
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
        //
        // debug_MSpeed
        //
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
        //
        // debug_Log
        //
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
        //
        // debug_PSpeed
        //
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
        //
        // debug_Godmode
        //
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
        //
        // debug_MJump
        //
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
        //
        // debug_NoBombs
        //
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
        //
        // debug_PJump
        //
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
        //
        // debug_PGravity
        //
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
        //
        // debug_MGravity
        //
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
//        THIS:Icon = Icon:ExtractAssociatedIcon("").
        THIS:MaximizeBox = FALSE.
        THIS:MinimizeBox = FALSE.
        THIS:Name = "Main".
        THIS:Text = "Test Game".
        THIS:StartPosition = FormStartPosition:CenterScreen.
        THIS:Load:Subscribe(TEST_GAME_Load).
        THIS:KeyDown:Subscribe(TEST_GAME_KeyDown).
        THIS:KeyUp:Subscribe(TEST_GAME_KeyUp).

        CAST(THIS:pb_NPC2, ISupportInitialize):EndInit().
        CAST(THIS:pb_NPC1, ISupportInitialize):EndInit().
        CAST(THIS:pb_Block1, ISupportInitialize):EndInit().
        CAST(THIS:pb_Block2, ISupportInitialize):EndInit().
        CAST(THIS:pb_Pipe, ISupportInitialize):EndInit().
        CAST(THIS:pb_Player, ISupportInitialize):EndInit().

        THIS:ResumeLayout(FALSE).
        THIS:PerformLayout().

        // Defining some global control array variables here They will hold instances of objects that are
        // created periodically throughout the game
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
            THIS:tempControl = CAST(THIS:DebugMenu:GetValue(i_int - 1), Control).
            THIS:tempControl:Visible = TRUE.
        END.
    END METHOD.

    METHOD PRIVATE VOID offToolStripMenuItem_Click (INPUT sender AS Object,
                                                    INPUT e      AS EventArgs):
        DO i_int = 1 TO THIS:DebugMenu:Length:
            THIS:tempControl = CAST(THIS:DebugMenu:GetValue(i_int - 1), Control).
            THIS:tempControl:Visible = FALSE.
        END.
    END METHOD.

    METHOD PRIVATE VOID timer_Gravity_Tick (INPUT sender AS Object,
                                            INPUT e      AS EventArgs):
        IF NOT THIS:Player_Jump                                                           AND 
           THIS:pb_Player:Location:Y + THIS:pb_Player:Height < THIS:WorldFrame:Height - 2 AND
           NOT THIS:Collision_Top(THIS:pb_Player)                                         THEN
        DO:
            THIS:pb_Player:Top = THIS:pb_Player:Top + THIS:Speed_Fall.
        END.

        IF NOT THIS:Player_Jump                                                           AND
           THIS:pb_Player:Location:Y + THIS:pb_Player:Height > THIS:WorldFrame:Height - 1 THEN
        DO:
            THIS:pb_Player:Top = THIS:pb_Player:Top - 1.
        END.

        i_count = 0.
        IF THIS:GameOn THEN
        DO:
            DO i_int = 1 TO THIS:Bombs:Length:
                ASSIGN THIS:Bomb = CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox).

                IF VALID-OBJECT(THIS:Bomb) THEN
                DO:
                    IF NOT THIS:OutsideWorldFrame(THIS:Bomb) THEN
                    DO:
                        IF THIS:Bomb:Name = "pb" OR THIS:Bomb:Name = "Coin" THEN
                        DO:
                            THIS:Bomb:Top = THIS:Bomb:Top + 3.
                        END.
                        ELSE
                        IF THIS:Bomb:Name = "pbR" THEN
                        DO:
                            THIS:Bomb:Left = THIS:Bomb:Left + 3.
                        END.
                        ELSE
                        IF THIS:Bomb:Name = "pbL" THEN
                        DO:
                            THIS:Bomb:Left = THIS:Bomb:Left - 3.
                        END.
                        ELSE
                        DO:
                            THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1 Error: Unkown bomb name '&2'&3",
                                                                       DateTime:Now,
                                                                       THIS:Bomb:Name,
                                                                       CHR(13) + CHR(10)).
                        END.
                    END.
                    ELSE
                    DO:
                        IF THIS:OutsideWorldFrame(THIS:Bomb) THEN
                        DO:
                            THIS:Bomb:Dispose().
                            THIS:Bombs:SetValue(?, i_count).
                            THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Removed rocket at &2&3",
                                                                       DateTime:Now,
                                                                       i_count,
                                                                       CHR(13) + CHR(10)).
                        END.
                        IF THIS:OutsideWorldFrame(THIS:Bomb) THEN
                        DO:
                            THIS:Bomb:Dispose().
                            THIS:Bombs:SetValue(?, i_count).
                            THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Removed rocket at &2&3",
                                                                       DateTime:Now,
                                                                       i_count,
                                                                       CHR(13) + CHR(10)).
                        END.
                        THIS:Bomb:Dispose().
                        THIS:Bombs:SetValue(?, i_count).
                        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Removed bomb at &2&3",
                                                                   DateTime:Now,
                                                                   i_count,
                                                                   CHR(13) + CHR(10)).
                        
                        IF VALID-OBJECT(CAST(THIS:Explosives:GetValue(0), PictureBox)) THEN
                        DO:
                            CAST(THIS:Explosives:GetValue(0), PictureBox):Dispose().
                            THIS:Explosives:SetValue(?, 0).
                        END.
                        THIS:CreateBoom(THIS:Bomb:Location:X, THIS:Bomb:Location:Y).
                    END.
                END.

                i_count = i_count + 1.
                IF i_count >= 10 THEN i_count = 0.
            END.
        END.
    END METHOD.

    METHOD PRIVATE VOID timer_Jump_Tick (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
        IF THIS:GameOn THEN
        DO:
            IF THIS:Player_Right                                 AND
               THIS:pb_Player:Right <= THIS:WorldFrame:Width - 3 AND
               NOT THIS:Collision_Left(THIS:pb_Player)           THEN
            DO:
                THIS:pb_Player:Left = THIS:pb_Player:Left + THIS:Speed_Movement.
            END.

            IF THIS:Player_Left                         AND
               THIS:pb_Player:Location:X >= 3           AND
               NOT THIS:Collision_Right(THIS:pb_Player) THEN
            DO:
                THIS:pb_Player:Left = THIS:pb_Player:Left - THIS:Speed_Movement.
            END.
        END.
        ELSE
        DO:
            THIS:Player_Right = FALSE.
            THIS:Player_Left  = FALSE.
        END.

        IF THIS:Force > 0 THEN
        DO:
            IF THIS:Collision_Bottom(THIS:pb_Player) THEN
            DO:
                THIS:Force = 0.
            END.
            ELSE
            DO:
                THIS:Force = THIS:Force - 1.
                THIS:pb_Player:Top = THIS:pb_Player:Top - THIS:Speed_Jump.
            END.
        END.
        ELSE
        DO:
            THIS:Player_Jump = FALSE.
        END.
    END METHOD.

    METHOD PRIVATE VOID timer_Anim_Tick (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
        THIS:Anim = THIS:Anim + 1.
        THIS:label1:Text = "Bombs: " + STRING(THIS:GetBombsNum(THIS:Bombs)).
        THIS:label2:Text = "Highscore: " + STRING(123456789).

        IF THIS:Player_Right AND THIS:Anim MODULO 15 = 0 THEN
        DO:
            THIS:pb_Player:Image = THIS:Character:walk_r.
        END.
        IF THIS:Player_Left  AND THIS:Anim MODULO 15 = 0 THEN
        DO:
            THIS:pb_Player:Image = THIS:Character:walk_l.
        END.

        DO i_int = 1 TO THIS:Bombs:Length:
            THIS:Bomb = CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox).

            IF VALID-OBJECT(THIS:Bomb) THEN
            DO:
                IF THIS:pb_Player:Bounds:IntersectsWith(THIS:Bomb:Bounds) THEN
                DO:
                    IF THIS:Bomb:Name = "Coin" THEN
                    DO:
                        THIS:Score = THIS:Score + 1.
                        THIS:Bomb:Dispose().
                    END.
                    ELSE
                    DO:
                        THIS:Dead().
                        THIS:Bomb:Dispose().
                    END.
                END.
            END.
        END.

        DO i_int = 1 TO THIS:NPC:Length:
            THIS:npcTemp = CAST(THIS:NPC:GetValue(i_int - 1), PictureBox).

            IF THIS:npcTemp:Bounds:IntersectsWith(THIS:pb_Player:Bounds) THEN
            DO:
                THIS:Dead().
            END.
            ELSE
            DO:
                IF THIS:npcTemp:Location:X > THIS:pb_Player:Location:X AND
                   THIS:npcTemp:Location:X < THIS:WorldFrame:Width     AND
                   NOT THIS:Collision_Right(THIS:npcTemp)              AND
                   THIS:GameOn                                                THEN
                DO:
                    THIS:npcTemp:Left  = THIS:npcTemp:Left - 1.
                    THIS:npcTemp:Image = THIS:Enemy:Enemy_left.
                END.

                IF THIS:npcTemp:Location:X < THIS:pb_Player:Location:X AND
                   THIS:npcTemp:Location:X > 0                         AND
                   NOT THIS:Collision_Left(THIS:npcTemp)               AND
                   THIS:GameOn                                                THEN
                DO:
                    THIS:npcTemp:Left  = THIS:npcTemp:Left + 1.
                    THIS:npcTemp:Image = THIS:Enemy:Enemy_right.
                END.
            END.
        END.
    END METHOD.

    METHOD PRIVATE VOID timer_Randombomb_Tick (INPUT sender AS Object,
                                               INPUT e      AS EventArgs):
        THIS:rng = NEW Random().
        IF THIS:GameOn OR (NOT THIS:GameOn AND NOT THIS:label_Dead:Visible) THEN
        DO:
            IF THIS:GetBombsNum(THIS:Bombs) = 10 THEN
            DO:
                THIS:timer_BombFailsafe:Enabled = TRUE.
            END.
            ELSE
            DO:
                THIS:timer_BombFailsafe:Enabled = FALSE.
                THIS:r = 2.
                THIS:NextSpot = THIS:NextBomb(THIS:Bombs).
                IF THIS:Score > 20 AND THIS:Score < 40 THEN THIS:r = 12.
                IF THIS:Score > 40 AND THIS:Score < 80 THEN THIS:r = 13.
                IF THIS:Score > 80                     THEN THIS:r = 14.
                CASE THIS:rng:Next(1, THIS:r):
                    WHEN 1 OR
                    WHEN 2 OR
                    WHEN 3 OR
                    WHEN 4 OR
                    WHEN 5 OR
                    WHEN 6 OR
                    WHEN 7 OR
                    WHEN 8 THEN
                    DO:
                        THIS:NextSpot = THIS:NextBomb(THIS:Bombs).
                        THIS:pb = NEW PictureBox().
                        THIS:pb:Name = "pb".
                        THIS:pb:BackColor = Color:Transparent.
                        THIS:pb:SizeMode = PictureBoxSizeMode:StretchImage.
                        THIS:pb:Size = NEW Size(16, 16).
                        THIS:pb:Image = THIS:Enemy:Bomb.
                        IF THIS:Score > 20 THEN
                        DO:
                            THIS:pb:Location = NEW Point(THIS:rng:Next(THIS:pb_Player:Location:X - 10, THIS:pb_Player:Location:X + 10), 0).
                        END.
                        ELSE
                        DO:
                            THIS:pb:Location = NEW Point(THIS:rng:Next(0, THIS:WorldFrame:Width), 0).
                        END.
                        THIS:WorldFrame:Controls:Add(THIS:pb).
                        THIS:Bombs:SetValue(THIS:pb, THIS:NextBomb(THIS:Bombs)).
                        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Added bomb at &2&3",
                                                                   DateTime:Now,
                                                                   THIS:NextSpot,
                                                                   CHR(13) + CHR(10)).
                    END.
                    WHEN 9  OR
                    WHEN 10 OR
                    WHEN 11 THEN
                    DO:
                        THIS:NextSpot = THIS:NextBomb(THIS:Bombs).
                        THIS:Coin = NEW PictureBox().
                        THIS:Coin:Name = "Coin".
                        THIS:Coin:BackColor = Color:Transparent.
                        THIS:Coin:SizeMode = PictureBoxSizeMode:Zoom.
                        THIS:Coin:Size = NEW Size(20, 29).
                        THIS:Coin:Image = THIS:World:Coin.
                        THIS:Coin:Location = NEW Point(THIS:rng:Next(0, THIS:WorldFrame:Width), 0).
                        THIS:WorldFrame:Controls:Add(THIS:Coin).
                        THIS:Bombs:SetValue(THIS:Coin, THIS:NextBomb(THIS:Bombs)).
                        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Added coin at &2&3",
                                                                   DateTime:Now,
                                                                   THIS:NextSpot,
                                                                   CHR(13) + CHR(10)).
                    END.
                    WHEN 12 THEN
                    DO:
                        THIS:NextSpot = THIS:NextBomb(THIS:Bombs).
                        THIS:pbR = NEW PictureBox().
                        THIS:pbR:Name = "pbR".
                        THIS:pbR:BackColor = Color:Transparent.
                        THIS:pbR:SizeMode = PictureBoxSizeMode:StretchImage.
                        THIS:pbR:Size = NEW Size(30, 20).
                        THIS:pbR:Image = THIS:Enemy:Rocket_R.
                        IF THIS:rng:Next(1, 3) = 1 THEN
                        DO:
                            THIS:pbR:Location = NEW Point(1, 205).
                        END.
                        ELSE
                        DO:
                            THIS:pbR:Location = NEW Point(1, 124).
                        END.
                        THIS:WorldFrame:Controls:Add(THIS:pbR).
                        THIS:Bombs:SetValue(THIS:pbR, THIS:NextBomb(THIS:Bombs)).
                        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Added bomb at &2&3",
                                                                   DateTime:Now,
                                                                   THIS:NextSpot,
                                                                   CHR(13) + CHR(10)).
                    END.
                    WHEN 13 THEN
                    DO:
                        THIS:NextSpot = THIS:NextBomb(THIS:Bombs).
                        THIS:pbL = NEW PictureBox().
                        THIS:pbL:Name = "pbL".
                        THIS:pbL:BackColor = Color:Transparent.
                        THIS:pbL:SizeMode = PictureBoxSizeMode:StretchImage.
                        THIS:pbL:Size = NEW Size(30, 20).
                        THIS:pbL:Image = THIS:Enemy:Rocket_L.
                        IF THIS:rng:Next(1, 3) = 1 THEN
                        DO:
                            THIS:pbL:Location = NEW Point(THIS:WorldFrame:Width + 30, 205).
                        END.
                        ELSE
                        DO:
                            THIS:pbL:Location = NEW Point(THIS:WorldFrame:Width + 30, 151).
                        END.
                        THIS:WorldFrame:Controls:Add(THIS:pbL).
                        THIS:Bombs:SetValue(THIS:pbL, THIS:NextBomb(THIS:Bombs)).
                        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Added bomb at &2&3",
                                                                   DateTime:Now,
                                                                   THIS:NextSpot,
                                                                   CHR(13) + CHR(10)).
                    END.
                END CASE.
            END.
        END.
    END METHOD.

    METHOD PRIVATE VOID timer_BombFailsafe_Tick (INPUT sender AS Object,
                                                 INPUT e      AS EventArgs):
        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Bombs - &2&3",
                                                   DateTime:Now,
                                                   THIS:BombDebug(),
                                                   CHR(13) + CHR(10)).

        DO i_int = 1 TO 10:
            IF VALID-OBJECT(CAST(THIS:Explosives:GetValue(i_int - 1), PictureBox)) THEN
            DO:
                CAST(THIS:Explosives:GetValue(i_int - 1), PictureBox):Dispose().
                THIS:Explosives:SetValue(?, i_int - 1).
            END.
            IF VALID-OBJECT(CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox)) THEN
            DO:
                CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox):Dispose().
                THIS:Bombs:SetValue(?, i_int - 1).
            END.
        END.
        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Had to hard Failsafe&2",
                                                   DateTime:Now, CHR(13) + CHR(10)).
        THIS:DebugLog = THIS:DebugLog + SUBSTITUTE("&1: Bombs - &2&3",
                                                   DateTime:Now,
                                                   THIS:BombDebug(),
                                                   CHR(13) + CHR(10)).
    END METHOD.

    METHOD PRIVATE VOID timer_Sec_Tick (INPUT sender AS Object,
                                        INPUT e      AS EventArgs):
        DO i_int = 1 TO 10:
            IF VALID-OBJECT(CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox)) AND
               CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox):IsDisposed    THEN
            DO:
                THIS:Bombs:SetValue(?, i_int - 1).
            END.
        END.        
        THIS:label_Score:Text = "Score: " + STRING(THIS:Score).
        IF NOT THIS:label_Dead:Visible THEN
        DO:
            THIS:Score = THIS:Score + 1.
            THIS:BombSize = THIS:BombSize + 1.
            IF THIS:timer_Randombomb:Interval > 1 THEN
            DO:
                THIS:timer_Randombomb:Interval = THIS:timer_Randombomb:Interval - 1.
            END.
            IF THIS:Score > 123456789 THEN
            DO:
                /*Set Highscore here*/
            END.
        END.
    END METHOD.

    METHOD PRIVATE VOID timerBoomRemove_Tick (INPUT sender AS Object,
                                              INPUT e      AS EventArgs):
        /*
        THIS:tempArray = CAST(THIS:Controls, Array).
        DO i_int = 1 TO THIS:tempArray:Length:
            THIS:tempControl = CAST(THIS:tempArray:GetValue(i_int - 1), Control).

            IF THIS:tempControl:GetType() = TypeHelper:GetType("System.Windows.Forms.PictureBox") THEN
            DO:
                IF THIS:tempControl:Name = "Boom" THEN
                DO:
                    THIS:tempControl:Dispose().
                END.
            END.
        END.
        */
        DO i_int = 1 TO THIS:Explosives:Length:
            IF VALID-OBJECT(CAST(THIS:Explosives:GetValue(i_int - 1), PictureBox)) THEN
            DO:
                CAST(THIS:Explosives:GetValue(i_int - 1), PictureBox):Dispose().
            END.
        END.
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
        THIS:Reset().
        THIS:WorldObjects:SetValue(THIS:pb_Pipe, 0).
        THIS:WorldObjects:SetValue(THIS:pb_Block1, 1).
        THIS:WorldObjects:SetValue(THIS:pb_Block2, 2).
        THIS:DebugMenu:SetValue(THIS:debug_Log, 0).
        THIS:DebugMenu:SetValue(THIS:debug_Godmode, 1).
        THIS:DebugMenu:SetValue(THIS:debug_NoBombs, 2).
        THIS:DebugMenu:SetValue(THIS:debug_PGravity, 3).
        THIS:DebugMenu:SetValue(THIS:debug_MGravity, 4).
        THIS:DebugMenu:SetValue(THIS:debug_PJump, 5).
        THIS:DebugMenu:SetValue(THIS:debug_MJump, 6).
        THIS:DebugMenu:SetValue(THIS:debug_PSpeed, 7).
        THIS:DebugMenu:SetValue(THIS:debug_MSpeed, 8).
        THIS:NPC:SetValue(THIS:pb_NPC1, 0).
        THIS:NPC:SetValue(THIS:pb_NPC2, 1).
    END METHOD.

    METHOD PUBLIC LOGICAL InAirNoCollision (INPUT tar AS PictureBox):
        IF NOT THIS:OutSideWorldFrame(tar) THEN
        DO i_set = 1 TO THIS:WorldObjects:Length:
            THIS:Obj1 = CAST(THIS:WorldObjects:GetValue(i_set - 1), PictureBox).

            IF NOT tar:Bounds:IntersectsWith(THIS:Obj1:Bounds) THEN
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

        DO i_set = 1 TO THIS:WorldObjects:Length:
            ASSIGN THIS:Obj2 = CAST(THIS:WorldObjects:GetValue(i_set - 1), PictureBox).
            IF VALID-OBJECT(THIS:Obj2) THEN
            DO:
                IF tar:Bounds:IntersectsWith(THIS:Obj2:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Top (INPUT tar AS PictureBox):
        DO i_set = 1 TO THIS:WorldObjects:Length:
            THIS:Obj1 = CAST(THIS:WorldObjects:GetValue(i_set - 1), PictureBox).

            IF VALID-OBJECT(THIS:Obj1) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:Obj1:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X - 3, THIS:temp1:Location:Y - 1, THIS:temp1:Width + 6, 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Bottom (INPUT tar AS PictureBox):
        DO i_set = 1 TO THIS:WorldObjects:Length:
            THIS:Obj1 = CAST(THIS:WorldObjects:GetValue(i_set - 1), PictureBox).

            IF VALID-OBJECT(THIS:Obj1) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:Obj1:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X, THIS:temp1:Location:Y + THIS:temp1:Height, THIS:temp1:Width, 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Left (INPUT tar AS PictureBox):
        DO i_set = 1 TO THIS:WorldObjects:Length:
            ASSIGN THIS:Obj1 = CAST(THIS:WorldObjects:GetValue(i_set - 1), PictureBox).

            IF VALID-OBJECT(THIS:Obj1) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:Obj1:Bounds.
                THIS:temp1:SetBounds(THIS:temp1:Location:X - 1, THIS:temp1:Location:Y + 1, 1, THIS:temp1:Height - 1).

                IF tar:Bounds:IntersectsWith(THIS:temp1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL Collision_Right (INPUT tar AS PictureBox):
        DO i_set = 1 TO THIS:WorldObjects:Length:
            THIS:Obj1 = CAST(THIS:WorldObjects:GetValue(i_set - 1), PictureBox).

            IF VALID-OBJECT(THIS:Obj1) THEN
            DO:
                THIS:temp1 = NEW PictureBox().
                THIS:temp1:Bounds = THIS:Obj1:Bounds.
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

    METHOD PUBLIC VOID RemovePictureBoxAt (INPUT x AS INTEGER,
                                           INPUT y AS INTEGER):
        DO i_int = 1 TO THIS:Explosives:Length:
            THIS:Boom = CAST(THIS:Explosives:GetValue(i_int - 1), PictureBox).

            IF VALID-OBJECT(THIS:Boom) THEN
            DO:
                IF THIS:Boom:Location:X = x AND
                   THIS:Boom:Location:Y = y THEN THIS:Boom:Dispose().
            END.
        END.
    END METHOD.

    METHOD PUBLIC VOID Reset ():
        THIS:label_Dead:Visible = FALSE.

        DO i_int = 1 TO THIS:Bombs:Length:
            THIS:Bomb = CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox).

            IF VALID-OBJECT(THIS:Bomb) THEN
            DO:
                THIS:Bomb:Dispose().
                THIS:Bomb = ?.
            END.
        END.

        DO i_int = 1 TO THIS:Explosives:Length:
            THIS:Boom = CAST(THIS:Explosives:GetValue(i_int - 1), PictureBox).

            IF VALID-OBJECT(THIS:Boom) THEN
            DO:
                THIS:Boom:Dispose().
                THIS:Boom = ?.
            END.
        END.

        THIS:pb_Player:Visible = TRUE.
        THIS:pb_Player:Location = NEW Point(167, THIS:WorldFrame:Size:Height - 10 - THIS:pb_Player:Height).
        THIS:pb_NPC1:Location = NEW Point(1, THIS:WorldFrame:Size:Height - 1 - THIS:pb_NPC1:Height).
        THIS:pb_NPC2:Location = NEW Point(THIS:WorldFrame:Width - 10, THIS:WorldFrame:Size:Height - 1 - THIS:pb_NPC2:Height).
        THIS:pb_Player:Image = THIS:Character:stand_r.
        THIS:Score = 0.
        THIS:BombSize = 16.
        THIS:GameOn = TRUE.
    END METHOD.

    METHOD PUBLIC VOID CreateBoom (INPUT x AS INTEGER,
                                   INPUT y AS INTEGER):
        THIS:tempPictureBox = NEW PictureBox().
        THIS:tempPictureBox:Name = "Boom".
        THIS:tempPictureBox:BackColor = Color:Transparent.
        THIS:tempPictureBox:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:tempPictureBox:Size = NEW Size(THIS:BombSize, THIS:BombSize).
        THIS:tempPictureBox:Image = THIS:World:Boom.
        THIS:tempPictureBox:Location = NEW Point(x, y).
        THIS:WorldFrame:Controls:Add(THIS:tempPictureBox).
        THIS:Explosives:SetValue(THIS:tempPictureBox, 0).
    END METHOD.

    METHOD PUBLIC VOID CreateCloud (INPUT x AS INTEGER,
                                    INPUT y AS INTEGER):
        IF y = ? THEN ASSIGN y = 20.

        THIS:tempPictureBox = NEW PictureBox().
        THIS:tempPictureBox:Name = "Cloud".
        THIS:tempPictureBox:BackColor = Color:Transparent.
        THIS:tempPictureBox:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:tempPictureBox:Size = NEW Size(THIS:BombSize, THIS:BombSize).
        THIS:tempPictureBox:Image = THIS:World:Clouds.
        THIS:tempPictureBox:Location = NEW Point(x, y).
        THIS:WorldFrame:Controls:Add(THIS:tempPictureBox).
    END METHOD.

    METHOD PUBLIC VOID PaintBox (INPUT x AS INTEGER,
                                 INPUT y AS INTEGER,
                                 INPUT w AS INTEGER,
                                 INPUT h AS INTEGER,
                                 INPUT c AS Color):
        THIS:tempPictureBox = NEW PictureBox().
        THIS:tempPictureBox:BackColor = c.
        THIS:tempPictureBox:Size = NEW Size(w, h).
        THIS:tempPictureBox:Location = NEW Point(x, y).
        THIS:WorldFrame:Controls:Add(THIS:tempPictureBox).
    END METHOD.

    METHOD PUBLIC VOID CreatePipe (INPUT x AS INTEGER):
        i_int = THIS:WorldFrame:Height - 45.
        THIS:tempPictureBox = NEW PictureBox().
        THIS:tempPictureBox:Name = "Pipe".
        THIS:tempPictureBox:BackColor = Color:Transparent.
        THIS:tempPictureBox:SizeMode = PictureBoxSizeMode:StretchImage.
        THIS:tempPictureBox:Size = NEW Size(35, 45).
        THIS:tempPictureBox:Image = THIS:World:Pipe.
        THIS:tempPictureBox:Location = NEW Point(x, i_int).
        THIS:WorldFrame:Controls:Add(THIS:tempPictureBox).
    END METHOD.

    METHOD PUBLIC VOID TEST_GAME_KeyDown (INPUT sender AS Object,
                                          INPUT e      AS KeyEventArgs):
        CASE e:KeyCode:
            WHEN Keys:Z THEN
            DO:
                THIS:Score = THIS:Score + 120.
            END.
            WHEN Keys:X THEN
            DO:
                THIS:pb_Player:Top = THIS:pb_Player:Top + 3.
            END.
            WHEN Keys:P THEN
            DO:
                IF THIS:GameOn THEN
                DO:
                    THIS:GameOn = FALSE.
                    THIS:label_Dead:Text = "Paused, press P to Continue".
                    THIS:label_Dead:Visible = TRUE.
                END.
                ELSE
                DO:
                    THIS:GameOn = TRUE.
                    THIS:label_Dead:Text = "You died, press Space to start".
                    THIS:label_Dead:Visible = FALSE.
                END.
            END.
            WHEN Keys:Left THEN
            DO:
                IF THIS:GameOn THEN
                DO:
                    THIS:LastDirRight = FALSE.
                    THIS:Player_Left = TRUE.
                END.
            END.
            WHEN Keys:Right THEN
            DO:
                IF THIS:GameOn THEN
                DO:
                    THIS:LastDirRight = TRUE.
                    THIS:Player_Right = TRUE.
                END.
            END.
            WHEN Keys:Space THEN
            DO:
                IF THIS:label_Dead:Visible                   AND
                   INDEX(THIS:label_Dead:Text, "Paused") = 0 THEN
                DO:
                    THIS:Reset().
                END.
                ELSE
                DO:
                    IF NOT THIS:Player_Jump AND NOT THIS:InAirNoCollision(THIS:pb_Player) THEN
                    DO:
                        IF THIS:LastDirRight THEN
                            THIS:pb_Player:Image = THIS:Character:jump_r.
                        ELSE
                            THIS:pb_Player:Image = THIS:Character:jump_l.

                        THIS:pb_Player:Top = THIS:pb_Player:Top - THIS:Speed_Jump.
                        THIS:Force = THIS:Gravity.
                        THIS:Player_Jump = TRUE.
                    END.
                END.
            END.
        END CASE.
    END METHOD.

    METHOD PUBLIC VOID TEST_GAME_KeyUp (INPUT sender AS Object,
                                        INPUT e      AS KeyEventArgs):
        IF THIS:GameOn THEN
        DO:
            CASE e:KeyCode:
                WHEN Keys:Left THEN
                DO:
                    THIS:pb_Player:Image = THIS:Character:stand_l.
                    THIS:LastDirRight = FALSE.
                    THIS:Player_Left = FALSE.
                END.
                WHEN Keys:Right THEN
                DO:
                    THIS:pb_Player:Image = THIS:Character:stand_r.
                    THIS:LastDirRight = TRUE.
                    THIS:Player_Right = FALSE.
                END.
            END CASE.
        END.
    END METHOD.

    METHOD PUBLIC INTEGER GetBombsNum (INPUT Arr AS "System.Windows.Forms.PictureBox[]"):
        i_count = 0.
        DO i_int = 1 TO Arr:Length:
            THIS:Bomb = CAST(Arr:GetValue(i_int - 1), PictureBox).
            IF VALID-OBJECT(THIS:Bomb) THEN ASSIGN i_count = i_count + 1.
        END.
        RETURN i_count.
    END METHOD.

    METHOD PUBLIC LOGICAL NoCollision (INPUT tar AS PictureBox):
        DO i_int = 1 TO THIS:WorldObjects:Length:
            THIS:Obj1 = CAST(THIS:WorldObjects:GetValue(i_int - 1), PictureBox).

            IF VALID-OBJECT(THIS:Obj1) THEN
            DO:
                IF tar:Bounds:IntersectsWith(THIS:Obj1:Bounds) THEN RETURN TRUE.
            END.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC INTEGER NextBomb (INPUT Arr AS "System.Windows.Forms.PictureBox[]"):
        IF THIS:GetBombsNum(Arr) < 10 THEN
        DO:
            DO i_int = 1 TO 10:
                IF NOT VALID-OBJECT(CAST(Arr:GetValue(i_int - 1), PictureBox)) THEN
                DO:
                    RETURN i_int - 1.
                END.
            END.
        END.
        THIS:Bombs:SetValue(?, 0).
        RETURN 0.
    END METHOD.

    METHOD PUBLIC CHARACTER BombDebug ():
        c_temp = "".
        DO i_int = 1 TO 10:
            IF NOT VALID-OBJECT(CAST(THIS:Bombs:GetValue(i_int - 1), PictureBox)) THEN
            DO:
                c_temp = c_temp + "0".
            END.
            ELSE
            DO:
                c_temp = c_temp + "1".
            END.
        END.

        RETURN c_temp.
    END METHOD.
END CLASS.