USING System.*                FROM ASSEMBLY.
USING System.Drawing.*        FROM ASSEMBLY.
USING System.Windows.Forms.*  FROM ASSEMBLY.
USING System.ComponentModel.* FROM ASSEMBLY.
USING Progress.Windows.Form   FROM ASSEMBLY.

USING OOL.*.
USING OOL.NET.*.

CLASS OOL.NET.CALC_TEST INHERITS Form USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE Input      AS TextBox                         NO-UNDO.
    DEFINE PRIVATE VARIABLE buttons    AS "System.Windows.Forms.Button[]" NO-UNDO.
    DEFINE PRIVATE VARIABLE button     AS Button                          NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS IContainer                      NO-UNDO.

    DEFINE PRIVATE VARIABLE i_int      AS INTEGER                         NO-UNDO.
    DEFINE PRIVATE VARIABLE i_x        AS INTEGER                         NO-UNDO.
    DEFINE PRIVATE VARIABLE i_y        AS INTEGER                         NO-UNDO.

    DEFINE PRIVATE VARIABLE c_op       AS CHARACTER                       NO-UNDO.
    DEFINE PRIVATE VARIABLE dc_value   AS DECIMAL                         NO-UNDO.
    DEFINE PRIVATE VARIABLE i_gridW    AS INTEGER                         NO-UNDO INIT 4.
    DEFINE PRIVATE VARIABLE i_gridH    AS INTEGER                         NO-UNDO INIT 5.
    DEFINE PRIVATE VARIABLE l_reset    AS LOGICAL                         NO-UNDO INIT TRUE.

    DEFINE PRIVATE VARIABLE c_buttons  AS CHARACTER                       NO-UNDO
    INIT "CE,C,<<,~/,7,8,9,*,4,5,6,-,1,2,3,+,PN,0,.,=".

    DEFINE PRIVATE VARIABLE c_events   AS CHARACTER                       NO-UNDO
    INIT "CE,C,BS,Div,7,8,9,Mult,4,5,6,Sub,1,2,3,Add,PN,0,Dec,EQ".

    CONSTRUCTOR CALC_TEST ():
        SUPER().
        InitializeComponents().
        THIS-OBJECT:ComponentsCollection:Add(THIS-OBJECT:components).
        WAIT-FOR Application:Run(THIS-OBJECT).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    DESTRUCTOR CALC_TEST ():
    END DESTRUCTOR.

    METHOD PRIVATE VOID InitializeComponents ():
        THIS-OBJECT:SuspendLayout().

        THIS-OBJECT:buttons = CAST(System.Array:CreateInstance(
                                       Progress.Util.TypeHelper:GetType("System.Windows.Forms.Button"), 
                                       i_gridW * i_gridH),
                                   "System.Windows.Forms.Button[]").

        DO i_int = 1 TO i_gridW * i_gridH:
            ASSIGN i_x = IF i_x = i_gridW THEN 1       ELSE i_x + 1
                   i_y = IF i_x = 1       THEN i_y + 1 ELSE i_y.
            
            THIS-OBJECT:button        = NEW Button().
            THIS-OBJECT:button:Text   = ENTRY(i_int, c_buttons).
            THIS-OBJECT:button:Name   = THIS-OBJECT:button:Text.
            THIS-OBJECT:button:Width  = 50.
            THIS-OBJECT:button:Height = 50.
            THIS-OBJECT:button:Left   = ((i_x - 1) * THIS-OBJECT:button:Width)  + 30.
            THIS-OBJECT:button:Top    = ((i_y - 1) * THIS-OBJECT:button:Height) + 60.
            THIS-OBJECT:button:Font   = NEW Font(THIS-OBJECT:button:Font:FontFamily, 11).

            THIS-OBJECT:button:Click:Subscribe(Button_Click).
            
            THIS-OBJECT:Controls:Add(THIS-OBJECT:button).
            THIS-OBJECT:buttons:SetValue(THIS-OBJECT:button, i_int - 1).
        END.

        THIS-OBJECT:Input        = NEW TextBox().
        THIS-OBJECT:Input:Name   = "Calc_IO".
        THIS-OBJECT:Input:Text   = STRING(dc_value).
        THIS-OBJECT:Input:Width  = i_gridW * 50.
        THIS-OBJECT:Input:Height = 50.
        THIS-OBJECT:Input:Left   = 30.
        THIS-OBJECT:Input:Top    = 20.
        THIS-OBJECT:Input:TextAlign = HorizontalAlignment:Right.
        THIS-OBJECT:Input:Font = NEW Font(THIS-OBJECT:Input:Font:FontFamily, 16).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:Input).

        THIS-OBJECT:Name = "CALC_TEST".
        THIS-OBJECT:Text = "Calculator".
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(260, 340).

        THIS-OBJECT:Input:KeyPress:Subscribe(Input_KeyPress).

        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    METHOD PRIVATE VOID Button_Click (INPUT sender AS Object,
                                      INPUT e      AS EventArgs):
        THIS-OBJECT:Calc_Event(CAST(sender, Button):Text, "Button").
    END METHOD.

    METHOD PRIVATE VOID Input_KeyPress (INPUT sender AS Object,
                                        INPUT e      AS KeyPressEventArgs):
        
        THIS-OBJECT:Calc_Event(e:KeyChar, "key").
        ASSIGN e:Handled = TRUE.
    END METHOD.

    METHOD PRIVATE VOID Calc_Event (INPUT cip_key  AS CHARACTER,
                                    INPUT cip_type AS CHARACTER):
        IF cip_key = "+"  THEN
        DO:
            ASSIGN dc_value               = DECIMAL(THIS-OBJECT:Input:Text)
                   c_op                   = cip_key
                   THIS-OBJECT:Input:Text = c_op.
        END.
        ELSE
        IF cip_key = "-"  THEN
        DO:
            ASSIGN dc_value               = DECIMAL(THIS-OBJECT:Input:Text)
                   c_op                   = cip_key
                   THIS-OBJECT:Input:Text = c_op.
        END.
        ELSE
        IF cip_key = "*"  THEN
        DO:
            ASSIGN dc_value               = DECIMAL(THIS-OBJECT:Input:Text)
                   c_op                   = cip_key
                   THIS-OBJECT:Input:Text = c_op.
        END.        
        ELSE
        IF cip_key = "~/" THEN
        DO:
            ASSIGN dc_value               = DECIMAL(THIS-OBJECT:Input:Text)
                   c_op                   = cip_key
                   THIS-OBJECT:Input:Text = c_op.
        END.
        ELSE
        IF cip_key = "="  THEN
        DO:
            CASE c_op:
                WHEN "+" THEN
                    ASSIGN dc_value               = dc_value + DECIMAL(THIS-OBJECT:Input:Text)
                           c_op                   = ""
                           THIS-OBJECT:Input:Text = STRING(dc_value)
                           l_reset                = TRUE.
                WHEN "-" THEN
                    ASSIGN dc_value               = dc_value - DECIMAL(THIS-OBJECT:Input:Text)
                           c_op                   = ""
                           THIS-OBJECT:Input:Text = STRING(dc_value)
                           l_reset                = TRUE.
                WHEN "*" THEN
                    ASSIGN dc_value               = dc_value * DECIMAL(THIS-OBJECT:Input:Text)
                           c_op                   = ""
                           THIS-OBJECT:Input:Text = STRING(dc_value)
                           l_reset                = TRUE.
                WHEN "/" THEN
                    ASSIGN dc_value               = dc_value / DECIMAL(THIS-OBJECT:Input:Text)
                           c_op                   = ""
                           THIS-OBJECT:Input:Text = STRING(dc_value)
                           l_reset                = TRUE.
            END CASE.
        END.
        ELSE
        IF cip_key  = "C"      AND
           cip_type = "Button" THEN
        DO:
            ASSIGN dc_value               = 0
                   THIS-OBJECT:Input:Text = STRING(dc_value)
                   l_reset                = TRUE.
        END.
        ELSE
        IF LOOKUP(cip_key, "0,1,2,3,4,5,6,7,8,9") > 0 THEN
        DO:
            IF THIS-OBJECT:Input:Text = c_op THEN ASSIGN THIS-OBJECT:Input:Text = "".
            
            IF l_reset THEN ASSIGN THIS-OBJECT:Input:Text = ""
                                   l_reset                = FALSE.

            THIS-OBJECT:Input:AppendText(cip_key).
        END.
    END METHOD.
END CLASS.