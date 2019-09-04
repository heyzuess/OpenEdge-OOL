&IF DEFINED(STANDARD_LIB) = 0 &THEN
    &GLOBAL-DEFINE STANDARD_LIB STANDARD_LIB
    &GLOBAL-DEFINE WIN_BUFF 2
    &GLOBAL-DEFINE WIN_STEP 8
    &GLOBAL-DEFINE NORMAL 0
    &GLOBAL-DEFINE ERROR  1

    &IF OPSYS = "WIN32" &THEN
        &GLOBAL-DEFINE DIR_DELIM "~\"
    &ELSE
        &GLOBAL-DEFINE DIR_DELIM "~/"
    &ENDIF

    &IF DEFINED(METHOD_LIBRARY) > 0 &THEN
        &GLOBAL-DEFINE CLASS_LIBRARY
        &GLOBAL-DEFINE METHOD_LIB "METHOD"
        &GLOBAL-DEFINE METHOD_END "END METHOD."

        &GLOBAL-DEFINE PREF_CHR  "PUBLIC CHARACTER"
        &GLOBAL-DEFINE PREF_LOG  "PUBLIC LOGICAL"
        &GLOBAL-DEFINE PREF_INT  "PUBLIC INTEGER"
        &GLOBAL-DEFINE PREF_HND  "PUBLIC HANDLE"
        &GLOBAL-DEFINE PREF_VOID "PUBLIC VOID"
        &GLOBAL-DEFINE PREF_DATE "PUBLIC DATE"
        &GLOBAL-DEFINE PREF_GRID

        &GLOBAL-DEFINE POST_CHR  " "
        &GLOBAL-DEFINE POST_LOG  " "
        &GLOBAL-DEFINE POST_INT  " "
        &GLOBAL-DEFINE POST_HND  " "
        &GLOBAL-DEFINE POST_VOID " "
        &GLOBAL-DEFINE POST_DATE " "
        &GLOBAL-DEFINE POST_GRID

        &GLOBAL-DEFINE VAR_PRIV PRIVATE VARIABLE
        &GLOBAL-DEFINE VAR_PUBL VARIABLE
        &GLOBAL-DEFINE PROP_PRIV_PREF PRIVATE PROPERTY
        &GLOBAL-DEFINE PROP_PRIV_POST GET. PRIVATE SET.
        &GLOBAL-DEFINE PROP_PUBL_PREF PUBLIC PROPERTY
        &GLOBAL-DEFINE PROP_PUBL_POST GET. PUBLIC SET.
    &ELSE
        &GLOBAL-DEFINE METHOD_LIB "FUNCTION"
        &GLOBAL-DEFINE METHOD_END "END FUNCTION."

        &GLOBAL-DEFINE PREF_CHR  " "
        &GLOBAL-DEFINE PREF_LOG  " "
        &GLOBAL-DEFINE PREF_INT  " "
        &GLOBAL-DEFINE PREF_HND  " "
        &GLOBAL-DEFINE PREF_VOID " "
        &GLOBAL-DEFINE PREF_DATE " "
        &IF DEFINED(CUST_GRID) = 0 &THEN
            &GLOBAL-DEFINE PREF_GRID NEW GLOBAL SHARED
        &ENDIF

        &GLOBAL-DEFINE POST_CHR  "RETURNS CHARACTER"
        &GLOBAL-DEFINE POST_LOG  "RETURNS LOGICAL"
        &GLOBAL-DEFINE POST_INT  "RETURNS INTEGER"
        &GLOBAL-DEFINE POST_HND  "RETURNS HANDLE"
        &GLOBAL-DEFINE POST_VOID "RETURNS LOGICAL"
        &GLOBAL-DEFINE POST_DATE "RETURNS DATE"
        &IF DEFINED(CUST_GRID) = 0 &THEN
            &GLOBAL-DEFINE POST_GRID
        &ENDIF

        &GLOBAL-DEFINE VAR_PRIV VARIABLE
        &GLOBAL-DEFINE VAR_PUBL VARIABLE
        &GLOBAL-DEFINE PROP_PRIV_PREF VARIABLE
        &GLOBAL-DEFINE PROP_PRIV_POST .
        &GLOBAL-DEFINE PROP_PUBL_PREF VARIABLE
        &GLOBAL-DEFINE PROP_PUBL_POST .

        DEFINE NEW GLOBAL SHARED VARIABLE gc_catch AS CHARACTER NO-UNDO.
        &GLOBAL-DEFINE TRY_BLK _TRY_BLK
        &GLOBAL-DEFINE TRY ~
            {&TRY_BLK}: ~
            REPEAT ON ERROR  UNDO {&TRY_BLK}, RETRY {&TRY_BLK} ~
                   ON ENDKEY UNDO {&TRY_BLK}, RETRY {&TRY_BLK} ~
                   ON STOP   UNDO {&TRY_BLK}, RETRY {&TRY_BLK}
        &GLOBAL-DEFINE CATCH ~
            IF RETRY THEN DO: ~
                IF messageCUI(SUBSTITUTE("Are you sure you want to leave &1~?",gc_catch), ~
                                         "A",TRUE) ~
                THEN ~
                    LEAVE {&TRY_BLK}. ~
                ELSE ~
                    NEXT {&TRY_BLK}. ~
            END.
    &ENDIF

    &IF DEFINED(COMMON_VARS) = 0 &THEN
        &GLOBAL-DEFINE COMMON_VARS COMMON_VARS
        DEFINE VARIABLE w_tempW  AS WINPROC.
        DEFINE VARIABLE h_tempH  AS HANDLE.
        DEFINE VARIABLE d_tempDt AS DATE.
        DEFINE VARIABLE i_tempX  AS INTEGER NO-UNDO.
        DEFINE VARIABLE i_tempY  AS INTEGER NO-UNDO.
        DEFINE VARIABLE i_xpos   AS INTEGER NO-UNDO EXTENT 2.
        DEFINE VARIABLE i_ypos   AS INTEGER NO-UNDO EXTENT 2.

        DEFINE VARIABLE h_temp%  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE c_temp%  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i_temp%  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE dc_temp% AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE dt_temp% AS DATE      NO-UNDO.

        DEFINE {&PROP_PUBL_PREF} DATATYPE AS DATATYPE {&PROP_PUBL_POST}
    &ENDIF

    &IF DEFINED(DEF_GRID)   > 0 &THEN
        &IF DEFINED(REF_GRID) > 0 &THEN
            &GLOBAL-DEFINE POST_GRID REFERENCE-ONLY
        &ENDIF
        &IF DEFINED(GRID_TABLE) = 0 &THEN
            &GLOBAL-DEFINE GRID_TABLE
            DEFINE {&PREF_GRID} TEMP-TABLE gridBlock {&POST_GRID}
                FIELD i_idx    AS INTEGER
                FIELD i_x      AS INTEGER
                FIELD i_y      AS INTEGER
                FIELD c_val    AS CHARACTER
                FIELD c_loc    AS CHARACTER
                FIELD c_data   AS CHARACTER
                FIELD l_fill   AS LOGICAL.
        &ENDIF
    &ENDIF

    &IF DEFINED(WIN_TABLE_) = 0 &THEN
        &GLOBAL-DEFINE WIN_TABLE_
        DEFINE TEMP-TABLE win_obj REFERENCE-ONLY
            FIELD c_name     AS CHARACTER
            FIELD c_type     AS CHARACTER
            FIELD c_data     AS CHARACTER
            FIELD h_handle   AS HANDLE.

        DEFINE TEMP-TABLE win_objDetail REFERENCE-ONLY
            FIELD c_name   AS CHARACTER
            FIELD c_type   AS CHARACTER
            FIELD c_val    AS CHARACTER
            FIELD i_set    AS INTEGER.
    &ENDIF

    &IF DEFINED(CIRCLE_TABLE) &THEN
        DEFINE TEMP-TABLE cObj REFERENCE-ONLY
            FIELD i_idx    AS INTEGER
            FIELD d_theta  AS DECIMAL
            FIELD d_x      AS DECIMAL
            FIELD d_y      AS DECIMAL
            FIELD i_x      AS INTEGER
            FIELD i_y      AS INTEGER
            FIELD h_handle AS HANDLE.
    &ENDIF

    &IF DEFINED(DIR_OBJ) &THEN
        &IF DEFINED(DIR_CUST) = 0 &THEN
            &GLOBAL-DEFINE DIR_REF REFERENCE-ONLY
        &ENDIF

        DEFINE TEMP-TABLE fileDir {&DIR_REF}
            FIELD i_idx  AS INTEGER
            FIELD c_name AS CHARACTER
            FIELD c_full AS CHARACTER
            FIELD c_type AS CHARACTER.
    &ENDIF

    &IF DEFINED(NO_STANDARD_LIB) = 0 &THEN
        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_LOG}
            &METHOD_NAME = "messageCUI"
            &METHOD_POST = {&POST_LOG}
            &METHOD_EXT  = "(INPUT cip_message AS CHARACTER,
                             INPUT cip_type    AS CHARACTER,
                             INPUT lip_log     AS LOGICAL):"}
            CASE cip_type:
                WHEN 'A' THEN ASSIGN cip_type = "ALERT".
                WHEN 'E' THEN ASSIGN cip_type = "ERROR".
                WHEN 'S' THEN ASSIGN cip_type = "SUCCESS".
                WHEN 'W' THEN ASSIGN cip_type = "WARNING".
            END CASE.

            _MSG_BLK_:
            REPEAT ON ERROR  UNDO _MSG_BLK_, RETRY _MSG_BLK_
                   ON ENDKEY UNDO _MSG_BLK_, RETRY _MSG_BLK_
                   ON STOP   UNDO _MSG_BLK_, RETRY _MSG_BLK_:
                IF RETRY THEN NEXT _MSG_BLK_.

                IF lip_log THEN
                    MESSAGE cip_message VIEW-AS ALERT-BOX
                    BUTTONS YES-NO TITLE cip_type UPDATE lip_log.
                ELSE
                    MESSAGE cip_message VIEW-AS ALERT-BOX TITLE cip_type.

                LEAVE _MSG_BLK_.
            END.

            RETURN lip_log.
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_LOG}
            &METHOD_NAME = "msgBox"
            &METHOD_POST = {&POST_LOG}
            &METHOD_EXT  = "(INPUT cip_message AS CHARACTER,
                             INPUT cip_type    AS CHARACTER,
                             INPUT lip_log     AS LOGICAL):"}
            CASE cip_type:
                WHEN 'A' THEN ASSIGN cip_type = "ALERT".
                WHEN 'E' THEN ASSIGN cip_type = "ERROR".
                WHEN 'S' THEN ASSIGN cip_type = "SUCCESS".
                WHEN 'W' THEN ASSIGN cip_type = "WARNING".
            END CASE.

            _MSG_BOX_:
            REPEAT ON ERROR  UNDO _MSG_BOX_, RETRY _MSG_BOX_
                   ON ENDKEY UNDO _MSG_BOX_, RETRY _MSG_BOX_
                   ON STOP   UNDO _MSG_BOX_, RETRY _MSG_BOX_:
                IF RETRY THEN NEXT _MSG_BOX_.

                IF SEARCH("msgBox.p") <> ? THEN
                    RUN msgBox.p(INPUT  cip_message,
                                 INPUT  cip_type,
                                 INPUT  lip_log,
                                 INPUT  0,
                                 INPUT  0,
                                 INPUT  TRUE,
                                 OUTPUT lip_log).
                ELSE
                IF lip_log THEN
                    MESSAGE cip_message VIEW-AS ALERT-BOX
                    BUTTONS YES-NO TITLE cip_type UPDATE lip_log.
                ELSE
                    MESSAGE cip_message VIEW-AS ALERT-BOX TITLE cip_type.

                LEAVE _MSG_BOX_.
            END.

            RETURN lip_log.
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_INT}
            &METHOD_NAME = "optBox"
            &METHOD_POST = {&POST_INT}
            &METHOD_EXT  = "(INPUT cip_ask AS CHARACTER,
                             INPUT cip_opt AS CHARACTER):"}

            ASSIGN i_tempX = 0.

            IF SEARCH("optBox.p") <> ? THEN
            RUN optBox.p(INPUT  cip_ask,
                         INPUT  cip_opt,
                         OUTPUT i_tempX).

            RETURN i_tempX.
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_INT}
            &METHOD_NAME = "maxFrameWidth"
            &METHOD_POST = {&POST_INT}
            &METHOD_EXT  = "(INPUT hip_frame AS HANDLE):"}

            ASSIGN i_tempX = 0.
            IF NOT VALID-HANDLE(hip_frame) THEN RETURN 0.

            ASSIGN i_tempX = (hip_frame:WIDTH-CHARS  - {&WIN_BUFF} - 1) * {&WIN_STEP}.
            RETURN i_tempX.
        {footer.i &METHOD_END  = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_INT}
            &METHOD_NAME = "maxFrameHeight"
            &METHOD_POST = {&POST_INT}
            &METHOD_EXT  = "(INPUT hip_frame AS HANDLE):"}

            ASSIGN i_tempX = 0.
            IF NOT VALID-HANDLE(hip_frame) THEN RETURN 0.

            ASSIGN i_tempX = (hip_frame:HEIGHT-CHARS - {&WIN_BUFF} - 1) * {&WIN_STEP}.
            RETURN i_tempX.
        {footer.i &METHOD_END  = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_LOG}
            &METHOD_NAME = "getFrameDim"
            &METHOD_POST = {&POST_LOG}
            &METHOD_EXT  = "(INPUT  hip_frame AS HANDLE,
                             OUTPUT iop_x     AS INTEGER,
                             OUTPUT iop_y     AS INTEGER):"}

            IF NOT VALID-HANDLE(hip_frame) THEN RETURN FALSE.

            ASSIGN iop_x = maxFrameWidth(hip_frame)
                   iop_y = maxFrameHeight(hip_frame).
            RETURN TRUE.
        {footer.i &METHOD_END  = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_CHR}
            &METHOD_NAME = "getProcName"
            &METHOD_POST = {&POST_CHR}
            &METHOD_EXT  = "(INPUT hip_proc AS HANDLE):"}

            IF NOT VALID-HANDLE(hip_proc) THEN RETURN "".
            RETURN ENTRY(NUM-ENTRIES(hip_proc:NAME,{&DIR_DELIM}),hip_proc:NAME,{&DIR_DELIM}).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_LOG}
            &METHOD_NAME = "getWinObjReg"
            &METHOD_POST = {&POST_LOG}
            &METHOD_EXT  = "(INPUT wip_winproc AS CLASS Progress.Lang.OBJECT):"}

            w_tempW = CAST(wip_winproc,WINPROC).
            IF NOT VALID-OBJECT(w_tempW) THEN RETURN FALSE.
            RETURN w_tempW:winObjReg(OUTPUT TABLE win_obj BIND).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_LOG}
            &METHOD_NAME = "getWinObjDetReg"
            &METHOD_POST = {&POST_LOG}
            &METHOD_EXT  = "(INPUT wip_winproc AS CLASS Progress.Lang.OBJECT):"}

            w_tempW = CAST(wip_winproc,WINPROC).
            IF NOT VALID-OBJECT(w_tempW) THEN RETURN FALSE.
            RETURN w_tempW:winObjDetReg(OUTPUT TABLE win_objDetail BIND).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_LOG}
            &METHOD_NAME = "isInObj"
            &METHOD_POST = {&POST_LOG}
            &METHOD_EXT  = "(INPUT hip_handle AS HANDLE,
                             INPUT iip_x      AS INTEGER,
                             INPUT iip_y      AS INTEGER):"}

            IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.

            ASSIGN i_xpos[1] = hip_handle:COL
                   i_xpos[2] = i_xpos[1] + hip_handle:WIDTH-CHARS
                   i_ypos[1] = hip_handle:ROW
                   i_ypos[2] = i_ypos[1] + hip_handle:HEIGHT-CHARS.

            RETURN iip_x >= i_xpos[1] AND
                   iip_x <  i_xpos[2] AND
                   iip_y >= i_ypos[1] AND
                   iip_y <  i_ypos[2].
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_DATE}
            &METHOD_NAME = "firstOfMonth"
            &METHOD_POST = {&POST_DATE}
            &METHOD_EXT  = "(INPUT dip_date AS DATE):"}

            IF dip_date = ? THEN RETURN ?.

            RETURN DATE(MONTH(dip_date),1,YEAR(dip_date)).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_DATE}
            &METHOD_NAME = "lastOfMonth"
            &METHOD_POST = {&POST_DATE}
            &METHOD_EXT  = "(INPUT dip_date AS DATE):"}

            IF dip_date = ? THEN RETURN ?.

            ASSIGN d_tempDt = firstOfMonth(dip_date) + 31.
            RETURN d_tempDt - DAY(d_tempDt).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_INT}
            &METHOD_NAME = "getCoord"
            &METHOD_POST = {&POST_INT}
            &METHOD_EXT  = "(INPUT iip_val AS INTEGER,
                             INPUT lip_xy  AS LOGICAL):"}

            IF lip_xy THEN
                ASSIGN iip_val = iip_val / 8 + 1.
            ELSE
                ASSIGN iip_val = (iip_val - 1) * 8.

            ASSIGN iip_val = MAX(iip_val,0).

            RETURN iip_val.
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_INT}
            &METHOD_NAME = "centerColOf"
            &METHOD_POST = {&POST_INT}
            &METHOD_EXT  = "(INPUT hip_this AS HANDLE,
                             INPUT hip_that AS HANDLE):"}
            IF hip_this = ? THEN
                RETURN INTEGER(hip_that:WIDTH-CHARS / 2).
            ELSE
                RETURN INTEGER(hip_that:WIDTH-CHARS / 2 - hip_this:WIDTH-CHARS / 2).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_INT}
            &METHOD_NAME = "centerRowOf"
            &METHOD_POST = {&POST_INT}
            &METHOD_EXT  = "(INPUT hip_this AS HANDLE,
                             INPUT hip_that AS HANDLE):"}
            IF hip_this = ? THEN
                RETURN INTEGER(hip_that:HEIGHT-CHARS / 2).
            ELSE
                RETURN INTEGER(hip_that:HEIGHT-CHARS / 2 - hip_this:HEIGHT-CHARS / 2).
        {footer.i &METHOD_END = {&METHOD_END}}

        {header.i
            &METHOD_LIB  = {&METHOD_LIB}
            &METHOD_PREF = {&PREF_CHR}
            &METHOD_NAME = "dataTypeLit"
            &METHOD_POST = {&POST_CHR}
            &METHOD_EXT  = "(INPUT cip_data AS CHARACTER):"}

            ASSIGN c_temp%  = ""
                   h_temp%  = ?
                   dt_temp% = ?
                   dc_temp% = 0
                   i_temp%  = 0.

            ASSIGN h_temp% = HANDLE(cip_data) NO-ERROR.
            IF VALID-HANDLE(h_temp%) THEN ASSIGN c_temp% = "HANDLE".

            IF c_temp% = ""           AND
               INDEX(cip_data,".") > 0 THEN
            DO:
                ASSIGN dc_temp% = DECIMAL(cip_data) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN ASSIGN c_temp% = "DECIMAL".
            END.

            IF c_temp% = "" THEN
            DO:
                ASSIGN dt_temp% = DATE(cip_data) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN ASSIGN c_temp% = "DATE".
            END.

            IF c_temp% = "" THEN
            DO:
                ASSIGN i_temp% = INTEGER(cip_data) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN
                    ASSIGN c_temp% = "INTEGER".
                ELSE
                    ASSIGN c_temp% = "CHARACTER".
            END.

            RETURN c_temp%.
        {footer.i &METHOD_END = {&METHOD_END}}

        &IF DEFINED(CIRCLE_TABLE) &THEN
            {header.i
                &METHOD_LIB  = {&METHOD_LIB}
                &METHOD_PREF = {&PREF_LOG}
                &METHOD_NAME = "getCircleObjReg"
                &METHOD_POST = {&POST_LOG}
                &METHOD_EXT  = "(INPUT wip_circle AS CLASS CIRCLE):"}

                IF NOT VALID-OBJECT(wip_circle) THEN RETURN FALSE.
                RETURN wip_circle:circleObjReg(OUTPUT TABLE cObj BIND).
            {footer.i &METHOD_END = {&METHOD_END}}
        &ENDIF
    &ENDIF
&ENDIF
