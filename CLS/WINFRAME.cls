USING Progress.Lang.*.
&GLOBAL WIN_BLANK    0
&GLOBAL WIN_FILL     1
&GLOBAL WIN_LITE     3
&GLOBAL WIN_FRAME    "FRAME"
&GLOBAL WIN_TEXT     "TEXT"
&GLOBAL WIN_BUTTON   "BUTTON"
&GLOBAL WIN_FILLIN   "FILL-IN"
&GLOBAL WIN_MENUBAR  "MENUBAR"
&GLOBAL WIN_MENUSUB  "MENUSUB"
&GLOBAL WIN_SUBITEM  "SUBITEM"
&GLOBAL WIN_COMBOBOX "COMBO-BOX"
&GLOBAL WIN_RECT     "RECTANGLE"
&GLOBAL WIN_CLIENT   "WINDOW"
&GLOBAL WIN_NULL     HANDLE("0")
&GLOBAL SYS_STEP     8
&GLOBAL SYS_BUFF     2
&GLOBAL SYS_SPACE    "SPACE"
&GLOBAL PRJ_DEBUG    FALSE
&GLOBAL CURSOR-UP    "CURSOR-UP"
&GLOBAL CURSOR-DOWN  "CURSOR-DOWN"
&GLOBAL CURSOR-LEFT  "CURSOR-LEFT"
&GLOBAL CURSOR-RIGHT "CURSOR-RIGHT"
CLASS WINFRAME:
    DEFINE PUBLIC PROPERTY w_window AS WINPROC   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_main   AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_close  AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_min    AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_max    AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_norm   AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_box    AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_currW  AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_name   AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_w      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_h      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_mW     AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_mH     AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_x      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_y      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_exWin  AS LOGICAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_lastKey   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_ignoreObj AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempW     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempH     AS INTEGER   NO-UNDO.

    DEFINE TEMP-TABLE regOwner
        FIELD h_owner   AS HANDLE
        FIELD i_keys    AS INTEGER.

    DEFINE TEMP-TABLE regkey
        FIELD h_owner    AS HANDLE
        FIELD c_name     AS CHARACTER
        FIELD c_func     AS CHARACTER
        FIELD i_val      AS INTEGER
        FIELD i_idx      AS INTEGER
        FIELD l_pressed  AS LOGICAL.

    DEFINE TEMP-TABLE rootF
        FIELD h_handle   AS HANDLE
        FIELD i_branches AS INTEGER
        FIELD i_curr     AS INTEGER
        FIELD i_prev     AS INTEGER.

    DEFINE TEMP-TABLE branchF
        FIELD h_parent   AS HANDLE
        FIELD h_handle   AS HANDLE
        FIELD c_type     AS CHARACTER
        FIELD i_idx      AS INTEGER
        FIELD i_active   AS INTEGER
        FIELD l_active   AS LOGICAL.

    DEFINE TEMP-TABLE win_obj REFERENCE-ONLY
        FIELD c_name     AS CHARACTER
        FIELD c_type     AS CHARACTER
        FIELD h_handle   AS HANDLE.

    DEFINE BUFFER f-regkey FOR regkey.

    /* Normal */
    CONSTRUCTOR WINFRAME(INPUT cip_name   AS CHARACTER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER,
                         INPUT iip_mW     AS INTEGER,
                         INPUT iip_mH     AS INTEGER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):
        w_window = NEW WINPROC().
        c_name   = cip_name.
        i_w      = iip_w.
        i_h      = iip_h.
        i_mW     = iip_mW.
        i_mH     = iip_mH.
        i_x      = iip_x.
        i_y      = iip_y.
        INITIALIZE().
    END CONSTRUCTOR.
    /* Overloaded */
    CONSTRUCTOR WINFRAME(INPUT wip_window AS CLASS OBJECT,
                         INPUT cip_name   AS CHARACTER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER,
                         INPUT iip_mW     AS INTEGER,
                         INPUT iip_mH     AS INTEGER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):
        w_window = CAST(wip_window,WINPROC).
        l_exWin  = TRUE.
        c_name   = cip_name.
        i_w      = iip_w.
        i_h      = iip_h.
        i_mW     = iip_mW.
        i_mH     = iip_mH.
        i_x      = iip_x.
        i_y      = iip_y.
        INITIALIZE().
    END CONSTRUCTOR.

    DESTRUCTOR WINFRAME():
        EMPTY TEMP-TABLE regOwner.
        EMPTY TEMP-TABLE regkey.
        EMPTY TEMP-TABLE rootF.
        EMPTY TEMP-TABLE branchF.
        w_window:clearWinObjPool().
        IF NOT l_exWin THEN DELETE OBJECT w_window.
    END DESTRUCTOR.

    METHOD PRIVATE VOID INITIALIZE():
        h_main   = w_window:createFrame("Main","",i_w,i_h,i_x,i_y,{&WIN_BLANK},TRUE).
        h_min    = w_window:createButton(h_main,"Min","-",5,3,
                                         w_window:maxFrameWidth(h_main) - {&SYS_STEP} * 14,
                                         0).
        h_max    = w_window:createButton(h_main,"Max","+",5,3,
                                         w_window:maxFrameWidth(h_main) - {&SYS_STEP} * 9,
                                         0).
        h_close  = w_window:createButton(h_main,"Exit","x",5,3,
                                         w_window:maxFrameWidth(h_main) - {&SYS_STEP} * 4,
                                         0).
        h_norm   = w_window:createButton(h_main,"Norm","=",5,3,
                                         w_window:maxFrameWidth(h_main) - {&SYS_STEP} * 9,
                                         0).
        h_box    = w_window:createRect(h_main,"Content",
                                       INT(h_main:WIDTH-CHARS - 2),
                                       INT(h_main:HEIGHT-CHARS - 3),
                                       0,{&SYS_STEP},{&WIN_BLANK}).

        /* --DEBUG--
        DISPLAY "0" VALID-HANDLE(h_main)  SKIP(1)
                "1" VALID-HANDLE(h_min)   SKIP(1)
                "2" VALID-HANDLE(h_max)   SKIP(1)
                "3" VALID-HANDLE(h_close) SKIP(1)
                "4" VALID-HANDLE(h_norm)  SKIP(1)
                "5" VALID-HANDLE(h_box)   SKIP(1)
        WITH FRAME ASDFASDF.*/

        ASSIGN h_norm:VISIBLE = FALSE
               c_ignoreObj    = {&WIN_TEXT} + "," + {&WIN_RECT}.

        rootF(h_main).
        FOR EACH  branchF
            WHERE branchF.h_parent = h_main:
            IF LOOKUP(branchF.c_type,c_ignoreObj) > 0 THEN NEXT.
            w_window:enableObj(branchF.h_handle).
        END.
    END METHOD.

    METHOD PRIVATE LOG rootF(INPUT hip_handle AS HANDLE):
        IF NOT VALID-HANDLE(hip_handle)                        THEN RETURN FALSE.
        IF NOT w_window:winObjReg(OUTPUT TABLE win_obj BIND) THEN RETURN FALSE.

        FIND FIRST rootF
             WHERE rootF.h_handle = hip_handle
             NO-LOCK NO-ERROR.
        IF AVAILABLE rootF THEN RETURN FALSE.

        CREATE rootF.
        ASSIGN rootF.h_handle = hip_handle.

        FOR EACH  win_obj:
            IF NOT VALID-HANDLE(win_obj.h_handle)   THEN NEXT.
            IF win_obj.h_handle:FRAME <> hip_handle THEN NEXT.
            IF win_obj.h_handle = hip_handle        THEN NEXT.

            FIND FIRST branchF
                 WHERE branchF.h_parent = hip_handle
                 AND   branchF.h_handle = win_obj.h_handle
                 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE branchF THEN
            DO:
                CREATE branchF.
                ASSIGN rootF.i_branches = rootF.i_branches + 1
                       branchF.h_parent = hip_handle
                       branchF.h_handle = win_obj.h_handle
                       branchF.c_type   = win_obj.c_type
                       branchF.i_idx    = rootF.i_branches
                       branchF.i_active = win_obj.h_handle:DCOLOR.
            END.
        END.

        IF rootF.i_branches > 0 THEN
            ASSIGN rootF.i_curr = 1
                   rootF.i_prev = rootF.i_branches.

        RETURN AVAILABLE rootF.
    END METHOD.

    METHOD PUBLIC LOG addBranch(INPUT hip_obj AS HANDLE):
        FIND FIRST rootF
             WHERE rootF.h_handle = h_main
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE rootF THEN RETURN FALSE.

        FOR EACH win_obj:
            IF NOT VALID-HANDLE(win_obj.h_handle)  THEN NEXT.
            IF win_obj.h_handle:FRAME <> h_main THEN NEXT.
            IF win_obj.h_handle = h_main        THEN NEXT.

            IF win_obj.h_handle = hip_obj THEN LEAVE.
        END.
        IF NOT AVAILABLE win_obj THEN RETURN FALSE.

        FIND FIRST branchF
             WHERE branchF.h_parent = h_main
             AND   branchF.h_handle = hip_obj
             NO-LOCK NO-ERROR.
        IF AVAILABLE branchF THEN RETURN FALSE.

        CREATE branchF.
        ASSIGN rootF.i_branches = rootF.i_branches + 1
               branchF.h_parent = h_main
               branchF.h_handle = hip_obj
               branchF.c_type   = win_obj.c_type
               branchF.i_idx    = rootF.i_branches
               branchF.i_active = win_obj.h_handle:DCOLOR.

        RETURN AVAILABLE branchF.
    END METHOD.

    METHOD PUBLIC LOG releaseKey(INPUT hip_owner AS HANDLE,
                                 INPUT iip_key   AS INTEGER):
        FIND FIRST f-regkey
             WHERE f-regkey.h_owner = hip_owner
             AND   f-regkey.i_val   = iip_key
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE f-regkey THEN RETURN TRUE.

        DELETE f-regkey.
        RETURN AVAILABLE f-regkey = FALSE.
    END METHOD.

    METHOD PUBLIC LOG readKeyReg(INPUT hip_owner AS HANDLE,
                                 INPUT iip_key   AS INTEGER):
        IF iip_key <= 0 THEN RETURN FALSE.

        IF NOT VALID-HANDLE(hip_owner) THEN RETURN FALSE.

        FIND FIRST regOwner
             WHERE regOwner.h_owner = hip_owner
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE regOwner THEN
        DO:
            CREATE regOwner.
            ASSIGN regOwner.h_owner = hip_owner.
        END.

        CREATE f-regkey.
        ASSIGN regOwner.i_keys    = regOwner.i_keys + 1
               f-regkey.h_owner   = regOwner.h_owner
               f-regkey.c_name    = KEYLABEL(iip_key)
               f-regkey.c_func    = KEYFUNCTION(iip_key)
               f-regkey.i_val     = iip_key
               f-regkey.i_idx     = regOwner.i_keys
               f-regkey.l_pressed = TRUE.

        IF f-regkey.i_val = 32 THEN
            ASSIGN f-regkey.c_name = {&SYS_SPACE}
                   f-regkey.c_func = {&SYS_SPACE}.

        RETURN AVAILABLE f-regkey AND f-regkey.l_pressed.
    END METHOD.

    METHOD PUBLIC HANDLE rNextBranch(INPUT hip_handle AS HANDLE,
                                     INPUT cip_ignore AS CHARACTER):
        FIND FIRST rootF
             WHERE rootF.h_handle = hip_handle
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE rootF THEN RETURN {&WIN_NULL}.

        IF rootF.i_branches = 0 THEN RETURN {&WIN_NULL}.

        FIND FIRST branchF
             WHERE branchF.h_parent = rootF.h_handle
             AND   branchF.i_idx    = rootF.i_curr
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE branchF THEN RETURN {&WIN_NULL}.

        ASSIGN branchF.h_handle:DCOLOR = branchF.i_active
               branchF.l_active        = FALSE.
        rFocusBranch(branchF.h_parent,branchF.h_handle,FALSE).

        REPEAT:
            ASSIGN rootF.i_prev = rootF.i_curr
                   rootF.i_curr = IF rootF.i_curr + 1 > rootF.i_branches THEN
                                      1
                                  ELSE
                                      rootF.i_curr + 1.

            FIND FIRST branchF
                 WHERE branchF.h_parent = rootF.h_handle
                 AND   branchF.i_idx    = rootF.i_curr
                 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE branchF THEN RETURN {&WIN_NULL}.

            IF branchF.h_handle:VISIBLE = FALSE THEN NEXT.

            IF cip_ignore = "" THEN LEAVE.
            ELSE
            IF LOOKUP(branchF.c_type,cip_ignore) = 0 THEN LEAVE.
        END.

        ASSIGN branchF.h_handle:DCOLOR = IF branchF.h_parent:DCOLOR = {&WIN_BLANK} THEN
                                             {&WIN_FILL}
                                         ELSE
                                         IF branchF.h_parent:DCOLOR = {&WIN_FILL}  THEN
                                             {&WIN_LITE}
                                         ELSE
                                             {&WIN_BLANK}
               branchF.l_active        = TRUE.

        rFocusBranch(branchF.h_parent,branchF.h_handle,TRUE).

        RETURN branchF.h_handle.
    END METHOD.

    METHOD PUBLIC LOG rFocusBranch(INPUT hip_frame  AS HANDLE,
                                   INPUT hip_handle AS HANDLE,
                                   INPUT lip_log    AS LOGICAL):
        IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.
        IF NOT VALID-HANDLE(hip_frame)  THEN RETURN FALSE.

        FIND FIRST rootF
             WHERE rootF.h_handle = hip_frame
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE rootF  THEN RETURN FALSE.
        IF rootF.i_branches = 0 THEN RETURN FALSE.

        FIND FIRST branchF
             WHERE branchF.h_parent = rootF.h_handle
             AND   branchF.h_handle = hip_handle
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE branchF THEN RETURN FALSE.

        IF lip_log THEN
        DO:
            ASSIGN branchF.h_handle:DCOLOR = IF branchF.h_parent:DCOLOR = {&WIN_BLANK} THEN
                                                 {&WIN_FILL}
                                             ELSE
                                             IF branchF.h_parent:DCOLOR = {&WIN_FILL}  THEN
                                                 {&WIN_LITE}
                                             ELSE
                                                 {&WIN_BLANK}
                   branchF.l_active        = TRUE.
        END.
        ELSE DO:
            ASSIGN branchF.h_handle:DCOLOR = branchF.i_active
                   branchF.l_active        = FALSE.
        END.
        RETURN branchF.l_active = lip_log.
    END METHOD.

    METHOD PUBLIC LOG messageCUI(INPUT cip_message AS CHARACTER,
                                 INPUT cip_type    AS CHARACTER,
                                 INPUT lip_log     AS LOGICAL):
        CASE cip_type:
            WHEN "A" THEN ASSIGN cip_type = "ALERT".
            WHEN "E" THEN ASSIGN cip_type = "ERROR".
            WHEN "S" THEN ASSIGN cip_type = "SUCCESS".
            WHEN "W" THEN ASSIGN cip_type = "WARNING".
        END CASE.

        IF lip_log THEN
            MESSAGE cip_message VIEW-AS ALERT-BOX
            BUTTONS YES-NO TITLE cip_type UPDATE lip_log.
        ELSE
            MESSAGE cip_message VIEW-AS ALERT-BOX TITLE cip_type.

        RETURN lip_log.
    END METHOD.

    METHOD PUBLIC CHAR getRegKeyValue(INPUT hip_owner   AS HANDLE,
                                      INPUT lip_release AS LOGICAL):
        FOR FIRST regkey
            WHERE regkey.h_owner   = hip_owner
            AND   regkey.l_pressed = TRUE:
            c_lastKey = regkey.c_name.
            IF lip_release THEN releaseKey(regkey.h_owner,regkey.i_val).
            RETURN c_lastKey.
        END.
        RETURN "".
    END METHOD.

    METHOD PUBLIC LOG apply_event(INPUT hip_handle AS HANDLE,
                                  INPUT cip_key    AS CHARACTER):
        IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.
        readKeyReg(hip_handle,KEYCODE(cip_key)).
        getRegKeyValue(hip_handle,TRUE).
        RETURN event_handler().
    END METHOD.

    METHOD PUBLIC LOG event_handler():
        FOR FIRST regkey
            WHERE regkey.h_owner   = SELF
            AND   regkey.l_pressed = TRUE:
            c_lastKey = regkey.c_name.
            releaseKey(regkey.h_owner,regkey.i_val).
        END.

        CASE c_lastKey:
            WHEN "TAB"   THEN
            DO:
                w_window:disableObj(SELF).
                h_currW = rNextBranch(SELF:FRAME,c_ignoreObj).
                w_window:enableObj(h_currW).
            END.
            WHEN "ENTER" THEN
            DO:
                IF SELF:TYPE = {&WIN_BUTTON} THEN
                DO:
                    APPLY "CHOOSE" TO SELF.
                END.
            END.
            OTHERWISE
            DO:
            END.
        END CASE.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC VOID displayFrame():
        ON ANY-KEY OF h_min   OR
           ANY-KEY OF h_max   OR
           ANY-KEY OF h_norm  OR
           ANY-KEY OF h_close DO:
            readKeyReg(SELF,LASTKEY).

            IF event_handler() THEN RETURN.

            RETURN NO-APPLY.
        END.

        ON ENTRY OF h_min   OR
           ENTRY OF h_max   OR
           ENTRY OF h_norm  OR
           ENTRY OF h_close DO:
        END.

        ON LEAVE OF h_min   OR
           LEAVE OF h_max   OR
           LEAVE OF h_norm  OR
           LEAVE OF h_close DO:
        END.

        ON CHOOSE OF h_min   OR
           CHOOSE OF h_max   OR
           CHOOSE OF h_norm  OR
           CHOOSE OF h_close DO:
            IF SELF = h_close THEN
            DO:
                IF NOT messageCUI("Are you sure you want to leave?","",TRUE) THEN RETURN NO-APPLY.
                h_main:VISIBLE = FALSE.
                h_main:VISIBLE = FALSE.
                APPLY "WINDOW-CLOSE" TO CURRENT-WINDOW.
            END.
            ELSE
            IF SELF = h_max THEN
            DO:
                maxWinFrame().
            END.
        END.

        PUT CURSOR OFF.
        VIEW h_main.
        WAIT-FOR CLOSE OF CURRENT-WINDOW.
    END METHOD.

    METHOD PUBLIC LOG maxWinFrame():
        ASSIGN i_tempW = i_w
               i_tempH = i_h
               i_w     = i_mW
               i_h     = i_mH
               i_mW    = i_tempW
               i_mH    = i_tempH
               i_tempW = 0
               i_tempH = 0.

        w_window:remWinObj("Min",{&WIN_BUTTON}).
        w_window:remWinObj("Max",{&WIN_BUTTON}).
        w_window:remWinObj("Exit",{&WIN_BUTTON}).
        w_window:remWinObj("Norm",{&WIN_BUTTON}).
        w_window:remWinObj("Content",{&WIN_RECT}).
        w_window:remWinObj("Main",{&WIN_FRAME}).

        INITIALIZE().

        IF VALID-HANDLE(h_main) THEN displayFrame().

        RETURN VALID-HANDLE(h_main).
    END METHOD.
END CLASS.
