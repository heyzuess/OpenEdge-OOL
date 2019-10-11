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