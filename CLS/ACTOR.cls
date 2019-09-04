CLASS ACTOR:
    DEFINE PUBLIC  PROPERTY h_handle AS HANDLE    NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_name   AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_class  AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_race   AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_colval AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_lastK  AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_health AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_x      AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_y      AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_step   AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_fps    AS DECIMAL   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY l_live   AS LOGICAL   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY l_tick   AS LOGICAL   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY l_col    AS LOGICAL   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_maxX   AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_maxY   AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PRIVATE PROPERTY i_dX     AS INTEGER   NO-UNDO GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY i_dY     AS INTEGER   NO-UNDO GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY d_initT  AS DATETIME  NO-UNDO GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY d_currT  AS DATETIME  NO-UNDO GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_tempX AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempY AS INTEGER NO-UNDO.

    DEFINE TEMP-TABLE control_button
        FIELD c_button  AS CHARACTER
        FIELD c_type    AS CHARACTER
        FIELD c_val     AS CHARACTER
        FIELD i_tOut    AS INTEGER
        FIELD l_tOut    AS LOGICAL
        FIELD l_enabled AS LOGICAL.

    DEFINE BUFFER b-control_button FOR control_button.

    CONSTRUCTOR PUBLIC ACTOR():
        ASSIGN c_name   = ""
               c_class  = ""
               c_race   = ""
               i_health = 10
               i_x      = 0
               i_y      = 0
               i_step   = 8
               i_dX     = 0
               i_dY     = 0
               l_live   = TRUE
               d_initT  = NOW.
    END CONSTRUCTOR.

    /* Overloaded */
    CONSTRUCTOR PUBLIC ACTOR(INPUT hip_handle AS HANDLE):
        ASSIGN c_name   = ""
               c_class  = ""
               c_race   = ""
               i_health = 10
               i_x      = 0
               i_y      = 0
               i_step   = 8
               i_dX     = 0
               i_dY     = 0
               l_live   = TRUE
               d_initT  = NOW
               h_handle = hip_handle.
    END CONSTRUCTOR.

    DESTRUCTOR PUBLIC ACTOR():
        EMPTY TEMP-TABLE control_button.
    END DESTRUCTOR.

    METHOD PUBLIC INT takeDMG(INPUT iip_damage AS INTEGER):
        DEFINE VARIABLE iv_num AS INTEGER NO-UNDO.

        ASSIGN iv_num = i_health - iip_damage NO-ERROR.
        RETURN iv_num.
    END METHOD.

    METHOD PUBLIC INT healDMG(INPUT iip_heal AS INTEGER):
        DEFINE VARIABLE iv_num AS INTEGER NO-UNDO.

        ASSIGN iv_num = i_health + iip_heal NO-ERROR.
        RETURN iv_num.
    END METHOD.

    METHOD PUBLIC LOG moveMe(INPUT iip_x AS INTEGER,
                             INPUT iip_y AS INTEGER):
        IF NOT VALID-HANDLE(h_handle) THEN RETURN FALSE.
        IF NOT tick()                 THEN RETURN FALSE.

        ASSIGN i_dX       = iip_x
               i_dY       = iip_y
               i_x        = h_handle:X
               i_y        = h_handle:Y.

        /*------------------------------
           Old code commented out here
        --------------------------------
        IF i_fps > 0 THEN
        DO:
            IF i_x + iip_x < 0                                           OR
               i_x + iip_x > h_handle:FRAME:WIDTH * i_step - i_step * 3  THEN
               	ASSIGN i_dX = i_dX * -1.
            IF i_y + iip_y < 0                                           OR
               i_y + iip_y > h_handle:FRAME:HEIGHT * i_step - i_step * 3 THEN
                ASSIGN i_dY = i_dY * -1.
        END.
        --------------------------------*/

        ASSIGN i_maxX     = h_handle:FRAME:WIDTH
               i_maxY     = h_handle:FRAME:HEIGHT
               i_x        = i_x + i_dX
               i_y        = i_y + i_dY
               h_handle:X = i_x
               h_handle:Y = i_y
        NO-ERROR.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG setupButton(INPUT cip_button AS CHARACTER,
                                  INPUT cip_type   AS CHARACTER,
                                  INPUT cip_val    AS CHARACTER):
        FIND FIRST control_button
             WHERE control_button.c_button = cip_button
             NO-LOCK NO-ERROR.
        IF AVAILABLE control_button THEN RETURN FALSE.

        CREATE control_button.
        ASSIGN control_button.c_button  = cip_button
               control_button.c_type    = cip_type
               control_button.c_val     = cip_val
               control_button.l_enabled = TRUE
        NO-ERROR.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG readKey(INPUT cip_button AS CHARACTER):
        FIND FIRST control_button
             WHERE control_button.c_button  = cip_button
             AND   control_button.l_enabled = TRUE
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE control_button THEN RETURN FALSE.

        IF control_button.l_tOut THEN
        DO:
            /* Checking if there are timeouts on button input */
            IF NOT tOut(control_button.c_button) THEN RETURN FALSE.
        END.

        CASE control_button.c_type:
            WHEN "DIR" THEN
            DO:
                ASSIGN i_tempX = INTEGER(ENTRY(1,control_button.c_val)) * i_step
            	       i_tempY = INTEGER(ENTRY(2,control_button.c_val)) * i_step
            	NO-ERROR.
                IF moveMe(i_tempX,i_tempY) THEN
                DO:
                    ASSIGN c_lastK = control_button.c_button.
                    RETURN TRUE.
                END.
                ELSE RETURN FALSE.
            END.
            WHEN "ACT" THEN
            DO:
            	  /* Placeholder for action buttons - c_val on button table will house action type
            	     which we will pass to internal method for evaluation
            	     - basic types as jump, shoot, fly should be supported
            	     - jump  should include logic to decide direction of jump
            	     - shoot should include logic to create and house actor
            	       that will move as an animation and have special properties
            	     - fly should allow set of button commands to be created to
            	       give actor sense of flying through the painted bitmap */
            	  RETURN TRUE.
            END.
            OTHERWISE RETURN FALSE.
        END CASE.
    END METHOD.

    METHOD PUBLIC CHAR setScreenVal(INPUT cip_val AS CHAR):
        ASSIGN h_handle:SCREEN-VALUE = cip_val.
        RETURN h_handle:SCREEN-VALUE.
    END METHOD.

    METHOD PUBLIC LOG tick():
        IF i_fps = 0 THEN DO:
            ASSIGN d_initT = NOW
                   l_tick  = FALSE.
            RETURN TRUE.
        END.

        ASSIGN d_currT = NOW
               l_tick  = FALSE.
        IF ABS(INTERVAL(d_initT,d_currT,"milliseconds")) > i_fps THEN
        DO:
            ASSIGN d_initT = NOW
                   l_tick  = FALSE.
            RETURN TRUE.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC LOG disableButton(INPUT cip_button AS CHARACTER,
                                    INPUT lip_tOut   AS LOGICAL,
                                    INPUT iip_tOut   AS INTEGER):
        FIND FIRST control_button
             WHERE control_button.c_button = cip_button
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE control_button THEN RETURN FALSE.

        IF lip_tOut AND NOT control_button.l_tOut THEN
            ASSIGN control_button.l_tOut    = TRUE
                   control_button.i_tOut    = iip_tOut
                   control_button.l_enabled = TRUE.
        ELSE
        IF NOT lip_tOut THEN
            ASSIGN control_button.l_enabled = FALSE.

        IF lip_tOut THEN
            RETURN control_button.l_enabled = FALSE.
        ELSE
            RETURN control_button.l_enabled.
    END METHOD.

    METHOD PUBLIC LOG enableButton(INPUT cip_button AS CHARACTER):
        FIND FIRST control_button
             WHERE control_button.c_button = cip_button
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE control_button THEN RETURN FALSE.

        ASSIGN control_button.l_enabled = TRUE
               control_button.l_tOut    = FALSE
               control_button.i_tOut    = 0.

        RETURN control_button.l_enabled.
    END METHOD.

    METHOD PUBLIC LOG tOut(INPUT cip_button AS CHARACTER):
        FIND FIRST b-control_button
             WHERE b-control_button.c_button = cip_button
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-control_button THEN RETURN FALSE.

        ASSIGN b-control_button.i_tOut = MAX(b-control_button.i_tOut - 1,0).

        IF b-control_button.i_tOut = 0 THEN
            ASSIGN b-control_button.l_enabled = FALSE.

        RETURN b-control_button.l_enabled.
    END METHOD.
END CLASS.
