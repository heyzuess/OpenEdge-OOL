/************************************************************************
  Author: Jesse Iberri
    Date: 6/27/2018
========================================================================
---------------------------- Method Library ----------------------------
========================================================================
     c_password : Password to be verified in program
     c_display  : Display settings for password frame
     c_lib      : If defined then program will be placed in an internal
                  procedure to be called by reference in included file
                  (i.e. RUN passwordPrompt[optional index])

     ------------- Child Settings - c_lib ----------------------
      =Below settings can be overwritten when c_lib is defined=
     -----------------------------------------------------------
     c_default  : Action to take on succesfull validation
     c_action   : Action to take on failed validation
     c_start    : Internal Procedure header when not defined
     c_end      : Internal Procedure close when not defined
     c_param    : Return Value data type is static variable
                  when defined and output parameter otherwise
     -----------------------------------------------------------

     c_help     : Help information displayed at bottom of frame
     c_init     : Any code to run before start of procedure
     l_strict   : Case sensitivity check on input
     l_noexit   : Prevents user from ctrl-c exit on the input screen
     i_index    : Unique identifier when multiple instances are made
     i_limit    : Limit before program force exits with default action
========================================================================
************************************************************************/
&IF DEFINED(c_password) = 0 &THEN
    MESSAGE "Password setting must be set"
    VIEW-AS ALERT-BOX.
    RETURN.
&ENDIF
&IF DEFINED(c_display) = 0 &THEN
    MESSAGE "Display setting must be set"
    VIEW-AS ALERT-BOX.
    RETURN.
&ENDIF
&IF DEFINED(c_lib) = 0 &THEN
    &IF DEFINED(c_default) = 0 &THEN
        &SCOPED-DEFINE c_default
    &ENDIF
    &IF DEFINED(c_action)  = 0 &THEN
        &SCOPED-DEFINE c_action RETURN "PASSWORD-FAIL".
    &ENDIF
    &SCOPED-DEFINE c_start
    &SCOPED-DEFINE c_end
    &SCOPED-DEFINE c_param VARIABLE
&ELSE
    &IF DEFINED(c_default) = 0 &THEN
        &SCOPED-DEFINE c_default
    &ENDIF
    &IF DEFINED(c_action) = 0 &THEN
        &SCOPED-DEFINE c_action
    &ENDIF
    &IF DEFINED(c_start) = 0 &THEN
        &SCOPED-DEFINE c_start PROCEDURE passwordPrompt{&i_index}:
    &ENDIF
    &IF DEFINED(c_end) = 0 &THEN
        &SCOPED-DEFINE c_end   END PROCEDURE.
    &ENDIF
    &IF DEFINED(c_param) = 0 &THEN
        &SCOPED-DEFINE c_param OUTPUT PARAMETER
    &ENDIF
&ENDIF
&IF DEFINED(c_help) = 0 &THEN
    &SCOPED-DEFINE c_help ""
&ENDIF
&IF DEFINED(c_init) = 0 &THEN
    &SCOPED-DEFINE c_init
&ENDIF
&IF DEFINED(i_index) = 0 &THEN
    &SCOPED-DEFINE i_index
&ENDIF
&IF DEFINED(i_limit) = 0 &THEN
    &SCOPED-DEFINE i_limit 3
&ENDIF
&IF DEFINED(l_strict) = 0 &THEN
    &SCOPED-DEFINE l_strict FALSE
&ELSE
    &SCOPED-DEFINE l_strict TRUE
&ENDIF
&IF DEFINED(l_noexit) > 0 &THEN
    &SCOPED-DEFINE UNDO_RETRY  ON ERROR  UNDO, RETRY ~
                               ON ENDKEY UNDO, RETRY ~
                               ON STOP   UNDO, RETRY
    &SCOPED-DEFINE RETRY_BLOCK IF RETRY THEN DO. ~
                                   MESSAGE "Are you sure you want to leave?" ~
                                   VIEW-AS ALERT-BOX BUTTONS YES-NO ~
                                   TITLE "Password Prompt" UPDATE llv_error{&i_index}. ~
                                   IF llv_error{&i_index} THEN LEAVE. ~
                                   ELSE NEXT. ~
                               END.
&ENDIF

FUNCTION i-caseSensitive{&i_index} RETURNS LOGICAL (INPUT cip_string{&i_index}  AS CHARACTER,
                                                    INPUT cip_compare{&i_index} AS CHARACTER):
    DEFINE VARIABLE fiv_start{&i_index} AS INTEGER NO-UNDO.
    DEFINE VARIABLE fiv_end{&i_index}   AS INTEGER NO-UNDO.
    DEFINE VARIABLE fiv_int{&i_index}   AS INTEGER NO-UNDO.
    DEFINE VARIABLE flv_log{&i_index}   AS LOGICAL NO-UNDO.

    ASSIGN flv_log{&i_index} = TRUE.
    IF LENGTH(cip_string{&i_index}) <> LENGTH(cip_compare{&i_index}) THEN RETURN FALSE.
    IF cip_string{&i_index} = "" OR cip_compare{&i_index} = ""       THEN RETURN FALSE.


    DO fiv_int{&i_index} = 1 TO LENGTH(cip_string{&i_index}):
        ASSIGN fiv_start{&i_index} = ASC(SUBSTRING(cip_string{&i_index},fiv_int{&i_index},1))
               fiv_end{&i_index}   = ASC(SUBSTRING(cip_compare{&i_index},fiv_int{&i_index},1)).
        IF fiv_start{&i_index} <> fiv_end{&i_index} THEN ASSIGN flv_log{&i_index} = FALSE.
    END.
    RETURN flv_log{&i_index}.
END FUNCTION.

{&c_start}
    DEFINE {&c_param} llv_error{&i_index} AS LOGICAL.

    DEFINE VARIABLE icv_password{&i_index} AS CHARACTER NO-UNDO.
    DEFINE VARIABLE icv_confirm{&i_index}  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iiv_length{&i_index}   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iiv_attempt{&i_index}  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE llv_leave{&i_index}    AS LOGICAL   NO-UNDO.

    DEFINE FRAME i-password{&i_index}
        icv_confirm{&i_index} NO-LABEL PASSWORD-FIELD
        HELP {&c_help}
    {&c_display}.

    ASSIGN icv_password{&i_index} = '{&c_password}'.

    {&c_init}

    ASSIGN icv_confirm{&i_index}:FORMAT = SUBSTITUTE("X(&1)",FRAME i-password{&i_index}:WIDTH-CHARS - 2).

    ON GO OF FRAME i-password{&i_index} DO:
        ASSIGN icv_confirm{&i_index}.
        IF icv_confirm{&i_index} = "" THEN
        DO:
            MESSAGE "Password can not be left blank."
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.

        IF icv_confirm{&i_index} <> icv_password{&i_index} THEN
        DO:
            ASSIGN iiv_attempt{&i_index} = iiv_attempt{&i_index} + 1.
            IF iiv_attempt{&i_index} >= {&i_limit} THEN
            DO:
                MESSAGE "Incorrect password, maximum attempts reached." SKIP(1)
                        "Program will now exit."
                VIEW-AS ALERT-BOX TITLE "ERROR".
                ASSIGN llv_leave{&i_index} = TRUE
                       llv_error{&i_index} = TRUE.
                RETURN.
            END.
            ELSE
            DO:
                MESSAGE "Incorrect password, please try again."
                VIEW-AS ALERT-BOX TITLE "ERROR".
                RETURN.
            END.
        END.

        IF {&l_strict} = TRUE AND
        NOT i-caseSensitive{&i_index}(icv_password{&i_index},icv_confirm{&i_index}) THEN
        DO:
            ASSIGN iiv_attempt{&i_index} = iiv_attempt{&i_index} + 1.
            IF iiv_attempt{&i_index} >= {&i_limit} THEN
            DO:
                MESSAGE "Incorrect password match, maximum attempts reached." SKIP(1)
                        "Program will now exit."
                VIEW-AS ALERT-BOX TITLE "ERROR".
                ASSIGN llv_leave{&i_index} = TRUE
                       llv_error{&i_index} = TRUE.
                RETURN.
            END.
            ELSE
            DO:
                MESSAGE "Incorrect password match, please try again."
                VIEW-AS ALERT-BOX TITLE "ERROR".
                RETURN.
            END.
        END.

        ASSIGN llv_leave{&i_index} = TRUE
               llv_error{&i_index} = FALSE.
    END.

    REPEAT {&UNDO_RETRY} :
        {&RETRY_BLOCK}
        ASSIGN icv_confirm{&i_index} = "".
        UPDATE icv_confirm{&i_index} WITH FRAME i-password{&i_index}.
        IF llv_leave{&i_index} THEN LEAVE.
        ELSE MESSAGE SUBSTITUTE("Attempt &1 of &2.",iiv_attempt{&i_index},{&i_limit}).
    END.
    HIDE FRAME i-password{&i_index}.
    IF NOT llv_leave{&i_index} THEN ASSIGN llv_error{&i_index} = TRUE.
    IF llv_error{&i_index} THEN DO: {&c_action} END.
    ELSE DO: {&c_default} END.
{&c_end}
