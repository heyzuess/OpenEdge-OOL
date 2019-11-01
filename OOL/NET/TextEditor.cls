USING System.*                FROM ASSEMBLY.
USING System.Array.*          FROM ASSEMBLY.
USING System.Drawing.*        FROM ASSEMBLY.
USING System.Windows.Forms.*  FROM ASSEMBLY.
USING System.ComponentModel.* FROM ASSEMBLY.
USING Progress.Util.*         FROM ASSEMBLY.
USING Progress.Windows.Form   FROM ASSEMBLY.

USING OOL.NET.*.

CLASS OOL.NET.TextEditor INHERITS Form USE-WIDGET-POOL:
    DEFINE PUBLIC PROPERTY THIS AS OOL.NET.TextEditor
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PRIVATE VARIABLE components            AS IContainer               NO-UNDO.
    DEFINE PRIVATE VARIABLE resources             AS ComponentResourceManager NO-UNDO.
    DEFINE PRIVATE VARIABLE mainMenu              AS MenuStrip                NO-UNDO.
    DEFINE PRIVATE VARIABLE mM_File               AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE file_New              AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE file_Open             AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator11  AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE file_Save             AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator13  AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE file_Exit             AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE mM_Edit               AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE edit_Undo             AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE edit_Redo             AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator14  AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE edit_Cut              AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE edit_Copy             AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE edit_Paste            AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator15  AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE edit_SelectAll        AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE mM_Tools              AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE tools_Customise       AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE Tools                 AS ToolStrip                NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_New                AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Open               AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Save               AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator    AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Cut                AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Copy               AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Paste              AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator1   AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Bold               AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Italic             AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_UnderLine          AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Strike             AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator3   AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_AlignLeft          AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_AlignCenter        AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_AlignRight         AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator7   AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_UpperCase          AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_LowerCase          AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator8   AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_ZoomIn             AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_ZoomOut            AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator9   AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_Font               AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_FontSize           AS ToolStripButton          NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator2   AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE Status                AS StatusStrip              NO-UNDO.
    DEFINE PRIVATE VARIABLE charCount             AS ToolStripStatusLabel     NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripStatusLabel2 AS ToolStripStatusLabel     NO-UNDO.
    DEFINE PRIVATE VARIABLE status_ZoomFactor     AS ToolStripStatusLabel     NO-UNDO.
    DEFINE PRIVATE VARIABLE Document              AS RichTextBox              NO-UNDO.
    DEFINE PRIVATE VARIABLE rcMenu                AS ContextMenuStrip         NO-UNDO.
    DEFINE PRIVATE VARIABLE rc_Undo               AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE rc_Redo               AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator10  AS ToolStripSeparator       NO-UNDO.
    DEFINE PRIVATE VARIABLE rc_Cut                AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE rc_Copy               AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE rc_Paste              AS ToolStripMenuItem        NO-UNDO.
    DEFINE PRIVATE VARIABLE Timer                 AS Timer                    NO-UNDO.
    DEFINE PRIVATE VARIABLE openWork              AS OpenFileDialog           NO-UNDO.
    DEFINE PRIVATE VARIABLE saveWork              AS SaveFileDialog           NO-UNDO.

    DEFINE PRIVATE VARIABLE toolStripItemList     AS "ToolStripItem[]"        NO-UNDO.
    DEFINE PRIVATE VARIABLE tempEnum              AS Enum                     NO-UNDO.

    CONSTRUCTOR TextEditor ():
        SUPER().
        InitializeComponent().
        THIS:ComponentsCollection:Add(THIS:components).
    END CONSTRUCTOR.

    DESTRUCTOR TextEditor ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent ():
        THIS:components            = NEW Container().
        THIS:resources             = NEW ComponentResourceManager(TypeHelper:GetType("TextEditor")).
        THIS:mainMenu              = NEW MenuStrip().
        THIS:mM_File               = NEW ToolStripMenuItem().
        THIS:file_New              = NEW ToolStripMenuItem().
        THIS:file_Open             = NEW ToolStripMenuItem().
        THIS:toolStripSeparator11  = NEW ToolStripSeparator().
        THIS:file_Save             = NEW ToolStripMenuItem().
        THIS:toolStripSeparator13  = NEW ToolStripSeparator().
        THIS:file_Exit             = NEW ToolStripMenuItem().
        THIS:mM_Edit               = NEW ToolStripMenuItem().
        THIS:edit_Undo             = NEW ToolStripMenuItem().
        THIS:edit_Redo             = NEW ToolStripMenuItem().
        THIS:toolStripSeparator14  = NEW ToolStripSeparator().
        THIS:edit_Cut              = NEW ToolStripMenuItem().
        THIS:edit_Copy             = NEW ToolStripMenuItem().
        THIS:edit_Paste            = NEW ToolStripMenuItem().
        THIS:toolStripSeparator15  = NEW ToolStripSeparator().
        THIS:edit_SelectAll        = NEW ToolStripMenuItem().
        THIS:mM_Tools              = NEW ToolStripMenuItem().
        THIS:tools_Customise       = NEW ToolStripMenuItem().
        THIS:Tools                 = NEW ToolStrip().
        THIS:tb_New                = NEW ToolStripButton().
        THIS:tb_Open               = NEW ToolStripButton().
        THIS:tb_Save               = NEW ToolStripButton().
        THIS:toolStripSeparator    = NEW ToolStripSeparator().
        THIS:tb_Cut                = NEW ToolStripButton().
        THIS:tb_Copy               = NEW ToolStripButton().
        THIS:tb_Paste              = NEW ToolStripButton().
        THIS:toolStripSeparator1   = NEW ToolStripSeparator().
        THIS:tb_Bold               = NEW ToolStripButton().
        THIS:tb_Italic             = NEW ToolStripButton().
        THIS:tb_UnderLine          = NEW ToolStripButton().
        THIS:tb_Strike             = NEW ToolStripButton().
        THIS:toolStripSeparator3   = NEW ToolStripSeparator().
        THIS:tb_AlignLeft          = NEW ToolStripButton().
        THIS:tb_AlignCenter        = NEW ToolStripButton().
        THIS:tb_AlignRight         = NEW ToolStripButton().
        THIS:toolStripSeparator7   = NEW ToolStripSeparator().
        THIS:tb_UpperCase          = NEW ToolStripButton().
        THIS:tb_LowerCase          = NEW ToolStripButton().
        THIS:toolStripSeparator8   = NEW ToolStripSeparator().
        THIS:tb_ZoomIn             = NEW ToolStripButton().
        THIS:tb_ZoomOut            = NEW ToolStripButton().
        THIS:toolStripSeparator9   = NEW ToolStripSeparator().
        THIS:tb_Font               = NEW ToolStripButton().
        THIS:tb_FontSize           = NEW ToolStripButton().
        THIS:toolStripSeparator2   = NEW ToolStripSeparator().
        THIS:Status                = NEW StatusStrip().
        THIS:charCount             = NEW ToolStripStatusLabel().
        THIS:toolStripStatusLabel2 = NEW ToolStripStatusLabel().
        THIS:status_ZoomFactor     = NEW ToolStripStatusLabel().
        THIS:Document              = NEW RichTextBox().
        THIS:rcMenu                = NEW ContextMenuStrip(THIS:components).
        THIS:rc_Undo               = NEW ToolStripMenuItem().
        THIS:rc_Redo               = NEW ToolStripMenuItem().
        THIS:toolStripSeparator10  = NEW ToolStripSeparator().
        THIS:rc_Cut                = NEW ToolStripMenuItem().
        THIS:rc_Copy               = NEW ToolStripMenuItem().
        THIS:rc_Paste              = NEW ToolStripMenuItem().
        THIS:Timer                 = NEW Timer(THIS:components).
        THIS:openWork              = NEW OpenFileDialog().
        THIS:saveWork              = NEW SaveFileDialog().

        THIS:SuspendLayout().
        //
        // mainMenu
        //
        THIS:toolStripItemList = CAST(Array:CreateInstance(TypeHelper:GetType("ToolStripItem"), 3),
                                      "ToolStripItem[]").
        THIS:toolStripItemList:SetValue(THIS:mM_File, 0).
        THIS:toolStripItemList:SetValue(THIS:mM_Edit, 1).
        THIS:toolStripItemList:SetValue(THIS:mM_Tools, 2).
        THIS:mainMenu:Items:AddRange(THIS:toolStripItemList).
        THIS:mainMenu:Location = NEW Point(0, 0).
        THIS:mainMenu:Name = "mainMenu".
        THIS:mainMenu:Size = NEW Size(732, 24).
        THIS:mainMenu:TabIndex = 0.
        THIS:mainMenu:Text = "menuStrip1".
        //
        // mM_File
        //
        THIS:toolStripItemList = CAST(Array:CreateInstance(TypeHelper:GetType("ToolStripItem"), 6),
                                     "ToolStripItem[]").
        THIS:toolStripItemList:SetValue(THIS:file_New, 0).
        THIS:toolStripItemList:SetValue(THIS:file_Open, 1).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator11, 2).
        THIS:toolStripItemList:SetValue(THIS:file_Save, 3).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator13, 4).
        THIS:toolStripItemList:SetValue(THIS:file_Exit, 5).
        THIS:mM_File:DropDownItems:AddRange(THIS:toolStripItemList).
        THIS:mM_File:Name = "mM_File".
        THIS:mM_File:Size = NEW Size(37, 20).
        THIS:mM_File:Text = "&File".
        //
        // file_New
        //
        THIS:file_New:Image = CAST(THIS:resources:GetObject("file_New:Image"), Image).
        THIS:file_New:ImageTransparentColor = Color:Magenta.
        THIS:file_New:Name = "file_New".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:N).
        THIS:file_New:ShortCutKeys = CAST(THIS:tempEnum, Keys).
        THIS:file_New:Size = NEW Size(146, 22).
        THIS:file_New:Text = "&New".
        THIS:file_New:Click:Subscribe(file_New_Click).
        //
        // file_Open
        //
        THIS:file_Open:Image = CAST(THIS:resources:GetObject("file_Open:Image"), Image).
        THIS:file_Open:ImageTransparentColor = Color:Magenta.
        THIS:file_Open:Name = "file_Open".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:O).
        THIS:file_Open:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:file_Open:Size = NEW Size(146, 22).
        THIS:file_Open:Text = "&Open".
        THIS:file_Open:Click:Subscribe(file_Open_Click).
        //
        // toolStripSeparator11
        //
        THIS:toolStripSeparator11:Name = "toolStripSeparator11".
        THIS:toolStripSeparator11:Size = NEW Size(143, 6).
        //
        // file_Save
        //
        THIS:file_Save:Image = CAST(THIS:resources:GetObject("file_Save:Image"), Image).
        THIS:file_Save:ImageTransparentColor = Color:Magenta.
        THIS:file_Save:Name = "file_Save".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:S).
        THIS:file_Save:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:file_Save:Size = NEW Size(146, 22).
        THIS:file_Save:Text = "&Save".
        THIS:file_Save:Click:Subscribe(file_Save_Click).
        //
        // toolStripSeparator13
        //
        THIS:toolStripSeparator13:Name = "toolStripSeparator13".
        THIS:toolStripSeparator13:Size = NEW Size(143, 6).
        //
        // mM_Edit
        //
        THIS:toolStripItemList = CAST(Array:CreateInstance(TypeHelper:GetType("ToolStripItem"), 8),
                                      "ToolStripItem[]").
        THIS:toolStripItemList:SetValue(THIS:edit_Undo, 0).
        THIS:toolStripItemList:SetValue(THIS:edit_Redo, 1).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator14, 2).
        THIS:toolStripItemList:SetValue(THIS:edit_Cut, 3).
        THIS:toolStripItemList:SetValue(THIS:edit_Copy, 4).
        THIS:toolStripItemList:SetValue(THIS:edit_Paste, 5).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator15, 6).
        THIS:toolStripItemList:SetValue(THIS:edit_SelectAll, 7).
        THIS:mM_Edit:DropDownItems:AddRange(THIS:toolStripItemList).
        THIS:mM_Edit:Name = "mM_Edit".
        THIS:mM_Edit:Size = NEW Size(39, 20).
        THIS:mM_Edit:Text = "&Edit".
        //
        // edit_Undo
        //
        THIS:edit_Undo:Name = "edit_Undo".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:Z).
        THIS:edit_Undo:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:edit_Undo:Size = NEW Size(144, 22).
        THIS:edit_Undo:Text = "&Undo".
        THIS:edit_Undo:Click:Subscribe(edit_Undo_Click).
        //
        // edit_Redo
        //
        THIS:edit_Redo:Name = "edit_Redo".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:Y).
        THIS:edit_Redo:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:edit_Redo:Size = NEW Size(144, 22).
        THIS:edit_Redo:Text = "&Redo".
        THIS:edit_Redo:Click:Subscribe(edit_Redo_Click).
        //
        // toolStripSeparator14
        //
        THIS:toolStripSeparator14:Name = "toolStripSeparator14".
        THIS:toolStripSeparator14:Size = NEW Size(141, 6).
        //
        // edit_Cut
        //
        THIS:edit_Cut:Image = CAST(THIS:resources:GetObject("edit_Cut:Image"), Image).
        THIS:edit_Cut:ImageTransparentColor = Color:Magenta.
        THIS:edit_Cut:Name = "edit_Cut".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:X).
        THIS:edit_Cut:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:edit_Cut:Size = NEW Size(144, 22).
        THIS:edit_Cut:Text = "Cu&t".
        THIS:edit_Cut:Click:Subscribe(edit_Cut_Click).
        //
        // edit_Copy
        //
        THIS:edit_Copy:Image = CAST(THIS:resources:GetObject("edit_Copy:Image"), Image).
        THIS:edit_Copy:ImageTransparentColor = Color:Magenta.
        THIS:edit_Copy:Name = "edit_Copy".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:C).
        THIS:edit_Copy:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:edit_Copy:Size = NEW Size(144, 22).
        THIS:edit_Copy:Text = "&Copy".
        THIS:edit_Copy:Click:Subscribe(edit_Copy_Click).
        //
        // edit_Paste
        //
        THIS:edit_Paste:Image = CAST(THIS:resources:GetObject("edit_Paste:Image"), Image).
        THIS:edit_Paste:ImageTransparentColor = Color:Magenta.
        THIS:edit_Paste:Name = "edit_Paste".
        THIS:tempEnum = EnumHelper:Or(Keys:Control, Keys:P).
        THIS:edit_Paste:ShortcutKeys = CAST(THIS:tempEnum, Keys).
        THIS:edit_Paste:Size = NEW Size(144, 22).
        THIS:edit_Paste:Text = "&Paste".
        THIS:edit_Paste:Click:Subscribe(edit_Paste_Click).
        //
        // toolStripSeparator15
        //
        THIS:toolStripSeparator15:Name = "toolStripSeparator15".
        THIS:toolStripSeparator15:Size = NEW Size(141, 6).
        //
        // edit_SelectAll
        //
        THIS:edit_SelectAll:Name = "edit_SelectAll".
        THIS:edit_SelectAll:Size = NEW Size(144, 22).
        THIS:edit_SelectAll:Text = "Select &All".
        THIS:edit_SelectAll:Click:Subscribe(edit_SelectAll_Click).
        //
        // mM_Tools
        //
        THIS:toolStripItemList = CAST(Array:CreateInstance(TypeHelper:GetType("ToolStripItem"), 1),
                                      "ToolStripItem[]").
        THIS:toolStripItemList:SetValue(THIS:tools_Customise, 0).
        THIS:mM_Tools:DropDownItems:AddRange(THIS:toolStripItemList).
        THIS:mM_Tools:Name = "mM_Tools".
        THIS:mM_Tools:Size = NEW Size(48, 20).
        THIS:mM_Tools:Text = "&Tools".
        //
        // tools_Customise
        //
        THIS:tools_Customise:Name = "tools_Customise".
        THIS:tools_Customise:Size = NEW Size(130, 22).
        THIS:tools_Customise:Text = "&Customize".
        THIS:tools_Customise:Click:Subscribe(tools_Customise_Click).
        //
        // Tools
        //
        THIS:Tools:GripStyle = ToolStripGripStyle:Hidden.
        THIS:toolStripItemList = CAST(Array:CreateInstance(TypeHelper:GetType("ToolStripItem"), 26),
                                      "ToolStripItem[]").
        THIS:toolStripItemList:SetValue(THIS:tb_New, 0).
        THIS:toolStripItemList:SetValue(THIS:tb_Open, 1).
        THIS:toolStripItemList:SetValue(THIS:tb_Save, 2).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator, 3).
        THIS:toolStripItemList:SetValue(THIS:tb_Cut, 4).
        THIS:toolStripItemList:SetValue(THIS:tb_Copy, 5).
        THIS:toolStripItemList:SetValue(THIS:tb_Paste, 6).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator1, 7).
        THIS:toolStripItemList:SetValue(THIS:tb_Bold, 8).
        THIS:toolStripItemList:SetValue(THIS:tb_Italic, 9).
        THIS:toolStripItemList:SetValue(THIS:tb_UnderLine, 10).
        THIS:toolStripItemList:SetValue(THIS:tb_Strike, 11).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator3, 12).
        THIS:toolStripItemList:SetValue(THIS:tb_AlignLeft, 13).
        THIS:toolStripItemList:SetValue(THIS:tb_AlignCenter, 14).
        THIS:toolStripItemList:SetValue(THIS:tb_AlignRight, 15).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator7, 16).
        THIS:toolStripItemList:SetValue(THIS:tb_UpperCase, 17).
        THIS:toolStripItemList:SetValue(THIS:tb_LowerCase, 18).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator8, 19).
        THIS:toolStripItemList:SetValue(THIS:tb_ZoomIn, 20).
        THIS:toolStripItemList:SetValue(THIS:tb_ZoomOut, 21).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator9, 22).
        THIS:toolStripItemList:SetValue(THIS:tb_Font, 23).
        THIS:toolStripItemList:SetValue(THIS:tb_FontSize, 24).
        THIS:toolStripItemList:SetValue(THIS:toolStripSeparator2, 25).
        THIS:Tools:Items:AddRange(THIS:toolStripItemList).
        THIS:Tools:Location = NEW Point(0, 24).
        THIS:Tools:Name = "Tools".
        THIS:Tools:Size = NEW Size(732, 25).
        THIS:Tools:TabIndex = 1.
        THIS:Tools:Text = "toolStrip1".
        //
        // tb_New
        //
        THIS:tb_New:DisplayStyle = ToolStripItemDisplayStyle:Image.
        THIS:tb_New:Image = CAST(THIS:resources:GetObject("tb_New:Image"), Image).
        THIS:tb_New:ImageTransparentColor = Color:Magenta.
        THIS:tb_New:Name = "tb_New".
        THIS:tb_New:Size = NEW Size(23, 22).
        THIS:tb_New:Text = "&New".
        THIS:tb_New:Click:Subscribe(tb_New_Click).
        //
        // tb_Open
        //
        THIS:tb_Open:DisplayStyle = ToolStripItemDisplayStyle:Image.
        THIS:tb_Open:Image = CAST(THIS:resources:GetObject("tb_Open:Image"), Image).
        THIS:tb_Open:ImageTransparentColor = Color:Magenta.
        THIS:tb_Open:Name = "tb_Open".
        THIS:tb_Open:Size = NEW Size(23, 22).
        THIS:tb_Open:Text = "&Open".
        THIS:tb_Open:Click:Subscribe(tb_Open_Click).
        //
        // tb_Save
        //
        THIS:tb_Save:DisplayStyle = ToolStripItemDisplayStyle:Image.
        THIS:tb_Save:Image = CAST(THIS:resources:GetObject("tb_Save:Image"), Image).
        THIS:tb_Save:ImageTransparentColor = Color:Magenta.
        THIS:tb_Save:Name = "tb_Save".
        THIS:tb_Save:Size = NEW Size(23, 22).
        THIS:tb_Save:Text = "&Save".
        THIS:tb_Save:Click:Subscribe(tb_Save_Click).
        //
        // toolStripSeparator
        //
        THIS:toolStripSeparator:Name = "toolStripSeparator".
        THIS:toolStripSeparator:Size = NEW Size(6, 25).
        //
        // tb_Cut
        //
        THIS:tb_Cut:DisplayStyle = ToolStripItemDisplayStyle:Image.
        THIS:tb_Cut:Image = CAST(THIS:resources:GetObject("tb_Cut:Image"), Image).
        THIS:tb_Cut:ImageTransparentColor = Color:Magenta.
        THIS:tb_Cut:Name = "tb_Cut".
        THIS:tb_Cut:Size = NEW Size(23, 22).
        THIS:tb_Cut:Text = "&Copy".
        THIS:tb_Cut:Click:Subscribe(tb_Cut_Click).
        //
        // tb_Copy
        //
        THIS:tb_Copy:DisplayStyle = ToolStripItemDisplayStyle:Image.
        THIS:tb_Copy:Image = CAST(THIS:resources:GetObject("tb_Copy:Image"), Image).
        THIS:tb_Copy:ImageTransparentColor = Color:Magenta.
        THIS:tb_Copy:Name = "tb_Copy".
        THIS:tb_Copy:Size = NEW Size(23, 22).
        THIS:tb_Copy:Text = "&Copy".
        THIS:tb_Copy:Click:Subscribe(tb_Copy_Click).

        THIS:ResumeLayout(FALSE).
        THIS:PerformLayout().
    END METHOD.

    METHOD PRIVATE VOID file_New_Click (INPUT sender AS Object,
                                        INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID file_Open_Click (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID file_Save_Click (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID edit_Undo_Click (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID edit_Redo_Click (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID edit_Cut_Click (INPUT sender AS Object,
                                        INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID edit_Copy_Click (INPUT sender AS Object,
                                         INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID edit_Paste_Click (INPUT sender AS Object,
                                          INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID edit_SelectAll_Click (INPUT sender AS Object,
                                              INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID tools_Customise_Click (INPUT sender AS Object,
                                               INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID tb_New_Click (INPUT sender AS Object,
                                      INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID tb_Open_Click (INPUT sender AS Object,
                                       INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID tb_Save_Click (INPUT sender AS Object,
                                       INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID tb_Cut_Click (INPUT sender AS Object,
                                      INPUT e      AS EventArgs):
    END METHOD.

    METHOD PRIVATE VOID tb_Copy_Click (INPUT sender AS Object,
                                       INPUT e      AS EventArgs):
    END METHOD.
END CLASS.