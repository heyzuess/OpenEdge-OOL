&SCOPED-DEFINE SYS_STEP 8
USING Progress.Lang.*.
CLASS DIALOG:
    DEFINE PRIVATE VARIABLE h_currF  AS HANDLE    NO-UNDO.
    DEFINE PRIVATE VARIABLE w_window AS WINPROC   NO-UNDO.
    DEFINE PRIVATE VARIABLE c_temp   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_dCount AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_length AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_maxL   AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_int    AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_curr   AS INTEGER   NO-UNDO.

    DEFINE TEMP-TABLE dialog
        FIELD h_frame   AS HANDLE
        FIELD c_owner   AS CHARACTER
        FIELD c_message AS CHARACTER
        FIELD i_index   AS INTEGER.

    DEFINE TEMP-TABLE currDialog LIKE dialog.

    DEFINE TEMP-TABLE dLine
        FIELD h_handle AS HANDLE
        FIELD c_name   AS CHARACTER
        FIELD c_owner  AS CHARACTER
        FIELD c_line   AS CHARACTER
        FIELD c_form   AS CHARACTER
        FIELD i_x      AS INTEGER
        FIELD i_y      AS INTEGER
        FIELD i_line   AS INTEGER
        FIELD i_index  AS INTEGER.

    /* Regular Constructor */
    CONSTRUCTOR DIALOG():
        w_window = NEW WINPROC().
    END CONSTRUCTOR.

    /* Overloaded Constructor */
    CONSTRUCTOR DIALOG(INPUT wip_winproc AS CLASS OBJECT):
        w_window = CAST(wip_winproc,WINPROC).
    END CONSTRUCTOR.

    DESTRUCTOR DIALOG():
    END DESTRUCTOR.

    METHOD PUBLIC LOG addDialog(INPUT hip_handle  AS HANDLE,
                                INPUT cip_owner   AS CHARACTER,
                                INPUT cip_message AS CHARACTER):
        ASSIGN i_dCount = 0.

        IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.

        ASSIGN i_dCount = dCount(cip_owner).

        CREATE dialog.
        ASSIGN dialog.h_frame   = hip_handle
               dialog.c_owner   = cip_owner
               dialog.c_message = cip_message
               dialog.i_index   = i_dCount + 1.

        RETURN AVAILABLE dialog.
    END METHOD.

    METHOD PUBLIC LOG currDialog(INPUT cip_owner AS CHARACTER):
        ASSIGN h_currF = ?.

        FIND FIRST currDialog
             WHERE currDialog.c_owner = cip_owner
             NO-LOCK NO-ERROR.
        IF AVAILABLE currDialog THEN
        DO:
            FIND FIRST dialog
                 WHERE dialog.c_owner = currDialog.c_owner
                 AND   dialog.i_index = currDialog.i_index + 1
                 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dialog THEN
                DELETE currDialog.
            ELSE
                BUFFER-COPY dialog TO currDialog.
        END.
        IF NOT AVAILABLE currDialog THEN
        DO:
            FIND FIRST dialog
                 WHERE dialog.c_owner = cip_owner
                 AND   dialog.i_index = 1
                 NO-LOCK NO-ERROR.
            IF AVAILABLE dialog THEN
            DO:
                CREATE currDialog.
                BUFFER-COPY dialog TO currDialog.
            END.
        END.

        setDialog(currDialog.c_owner).

        ASSIGN h_currF = currDialog.h_frame.
        VIEW h_currF.

        RETURN AVAILABLE currDialog.
    END METHOD.

    METHOD PUBLIC LOG setDialog(INPUT cip_owner   AS CHARACTER):
        ASSIGN c_temp   = ""
               i_length = 0
               i_maxL   = 0
               i_int    = 0
               i_curr   = 0.

        FIND FIRST currDialog
             WHERE currDialog.c_owner = cip_owner
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE currDialog THEN RETURN FALSE.

        ASSIGN i_length = LENGTH(currDialog.c_message)
               i_maxL   = currDialog.h_frame:WIDTH-CHARS - 2.

        clearDialog().

        FIND FIRST dLine
             WHERE dLine.c_owner = currDialog.c_owner
             AND   dLine.i_index = currDialog.i_index
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dLine THEN
        DO:
            ASSIGN i_length = i_length / i_maxL
                   i_length = MAX(1,i_length)
                   i_curr   = 1.

            DO i_int = 1 TO i_length:
                ASSIGN c_temp = SUBSTRING(currDialog.c_message,i_curr,i_curr + i_maxL - 1).

                CREATE dLine.
                ASSIGN dLine.c_owner = currDialog.c_owner
                       dLine.c_line  = c_temp
                       dLine.c_form  = SUBSTITUTE("X~(&1~)",MIN(LENGTH(dLine.c_line),currDialog.h_frame:WIDTH-CHARS - 2))
                       dLine.i_line  = i_int
                       dLine.c_name  = SUBSTITUTE("&1_Line&2",dLine.c_owner,dLine.i_line)
                       dLine.i_index = currDialog.i_index
                       dLine.i_x     = 0
                       dLine.i_y     = (dLine.i_line - 1) * {&SYS_STEP}.

                dLine.h_handle = w_window:createText(currDialog.h_frame,
                                                     dLine.c_name,
                                                     dLine.c_line,
                                                     dLine.c_form,
                                                     dLine.i_x,
                                                     dLine.i_y).

                ASSIGN i_curr = i_curr + i_maxL.
            END.

            RETURN AVAILABLE dLine.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC INT dCount(INPUT cip_owner   AS CHARACTER):
        ASSIGN i_dCount = 0.

        FOR EACH  dialog
            WHERE dialog.c_owner = cip_owner:
            ASSIGN i_dCount = i_dCount + 1.
        END.

        RETURN i_dCount.
    END METHOD.

    METHOD PUBLIC LOG clearDialog():
        FOR EACH dLine:
            w_window:remWinObj(dLine.c_name,"TEXT").
        END.
        EMPTY TEMP-TABLE dLine.
        RETURN TEMP-TABLE dLine:HAS-RECORDS = FALSE.
    END METHOD.
END CLASS.
