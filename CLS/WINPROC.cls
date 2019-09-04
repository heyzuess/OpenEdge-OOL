&SCOPED-DEFINE WIN_FRAME    "FRAME"
&SCOPED-DEFINE WIN_TEXT     "TEXT"
&SCOPED-DEFINE WIN_BUTTON   "BUTTON"
&SCOPED-DEFINE WIN_FILLIN   "FILL-IN"
&SCOPED-DEFINE WIN_MENUBAR  "MENUBAR"
&SCOPED-DEFINE WIN_MENUSUB  "MENUSUB"
&SCOPED-DEFINE WIN_SUBITEM  "SUBITEM"
&SCOPED-DEFINE WIN_CLIENT   "WINDOW"
&SCOPED-DEFINE WIN_COMBOBOX "COMBO-BOX"
&SCOPED-DEFINE WIN_RECT     "RECTANGLE"
&SCOPED-DEFINE WIN_IMAGE    "IMAGE"
&SCOPED-DEFINE WIN_NULL    HANDLE("0")
&SCOPED-DEFINE WIN_BUFF    2
&SCOPED-DEFINE WIN_STEP    8
&SCOPED-DEFINE WIN_BLANK   0
&SCOPED-DEFINE WIN_FILL    1
&SCOPED-DEFINE WIN_LITE    3
&SCOPED-DEFINE WIN_SPACE   "SPACE"
&SCOPED-DEFINE WIN_DELIM   "|"
CLASS WINPROC:
    DEFINE PUBLIC PROPERTY HANDLE       AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY w_currentWin AS HANDLE  GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY l_live       AS LOGICAL GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY i_numFrame   AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_numText    AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_numButton  AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_numFill    AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_cursorX    AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_cursorY    AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_numError   AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_error      AS LOGICAL GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_tempHandle AS HANDLE    NO-UNDO.
    DEFINE PRIVATE VARIABLE c_alphaNumer AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_before     AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_after      AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_temp       AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE l_clearPool  AS LOGICAL   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_maxX       AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_maxY       AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_temp       AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempX      AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempY      AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_dColor     AS INTEGER   NO-UNDO.

    DEFINE TEMP-TABLE win_obj
        FIELD c_name   AS CHARACTER
        FIELD c_type   AS CHARACTER
        FIELD c_data   AS CHARACTER
        FIELD h_handle AS HANDLE.

    DEFINE TEMP-TABLE win_objDetail
        FIELD c_name   AS CHARACTER
        FIELD c_type   AS CHARACTER
        FIELD c_val    AS CHARACTER
        FIELD i_set    AS INTEGER.

    DEFINE TEMP-TABLE win_err
        FIELD i_idx    AS INTEGER
        FIELD c_err    AS CHARACTER.

    DEFINE BUFFER x_obj FOR win_obj.

    CONSTRUCTOR WINPROC():
        ASSIGN l_live       = TRUE
               c_alphaNumer = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,1,2,3,4,5,6,7,8,9,0" +
                              "!,@,#,$,%,^,&,*,~(,~),-,=,_,+,<,>,~/,~\,?,:,;,~{,~},[,],`," + {&WIN_SPACE}
               w_currentWin = CURRENT-WINDOW
               HANDLE       = THIS-OBJECT:HANDLE.
    END CONSTRUCTOR.

    DESTRUCTOR WINPROC():
        clearWinObjPool().
    END DESTRUCTOR.

    METHOD PUBLIC HANDLE createWindow(INPUT cip_name  AS CHARACTER,
                                      INPUT cip_title AS CHARACTER,
                                      INPUT iip_w     AS INTEGER,
                                      INPUT iip_h     AS INTEGER,
                                      INPUT iip_mW    AS INTEGER,
                                      INPUT iip_mH    AS INTEGER,
                                      INPUT iip_x     AS INTEGER,
                                      INPUT iip_y     AS INTEGER,
                                      INPUT lip_mess  AS LOGICAL,
                                      INPUT lip_stat  AS LOGICAL):
        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_CLIENT}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_CLIENT}.

        CREATE WINDOW win_obj.h_handle
            ASSIGN NAME              = cip_name
                   TITLE             = cip_title
                   WIDTH-PIXELS      = iip_w
                   HEIGHT-PIXELS     = iip_h
                   MAX-WIDTH-PIXELS  = iip_mW
                   MAX-HEIGHT-PIXELS = iip_mH
                   X                 = iip_x
                   Y                 = iip_y
                   SENSITIVE         = TRUE
                   MESSAGE-AREA      = lip_mess
                   STATUS-AREA       = lip_stat.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createFrame(INPUT cip_name  AS CHARACTER,
                                     INPUT cip_title AS CHARACTER,
                                     INPUT iip_w     AS INTEGER,
                                     INPUT iip_h     AS INTEGER,
                                     INPUT iip_x     AS INTEGER,
                                     INPUT iip_y     AS INTEGER,
                                     INPUT iip_c     AS INTEGER,
                                     INPUT lip_box   AS LOGICAL):
        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_FRAME}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name  = cip_name
               win_obj.c_type  = {&WIN_FRAME}.

        IF cip_title = "" THEN
        CREATE FRAME win_obj.h_handle
            ASSIGN NAME         = cip_name
                   WIDTH-CHARS  = iip_w
                   HEIGHT-CHARS = iip_h
                   X            = iip_x
                   Y            = iip_y
                   HIDDEN       = TRUE
                   DCOLOR       = iip_c
                   TOP-ONLY     = FALSE
                   BOX          = lip_box
                   SENSITIVE    = TRUE
                   SCROLLABLE   = TRUE.

        ELSE
        CREATE FRAME win_obj.h_handle
            ASSIGN NAME         = cip_name
                   WIDTH-CHARS  = iip_w
                   HEIGHT-CHARS = iip_h
                   X            = iip_x
                   Y            = iip_y
                   HIDDEN       = TRUE
                   DCOLOR       = iip_c
                   TOP-ONLY     = FALSE
                   TITLE        = cip_title
                   SENSITIVE    = TRUE
                   SCROLLABLE   = TRUE.

        IF VALID-HANDLE(win_obj.h_handle) THEN ASSIGN i_numFrame = i_numFrame + 1.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createText(INPUT hip_frame  AS HANDLE,
                                    INPUT cip_name   AS CHARACTER,
                                    INPUT cip_value  AS CHARACTER,
                                    INPUT cip_format AS CHARACTER,
                                    INPUT iip_x      AS INTEGER,
                                    INPUT iip_y      AS INTEGER):
        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_TEXT}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_TEXT}.

        CREATE TEXT win_obj.h_handle
            ASSIGN FORMAT       = cip_format
                   SCREEN-VALUE = cip_value
                   X            = iip_x
                   Y            = iip_y
                   FRAME        = hip_frame.

        IF ERROR-STATUS:ERROR THEN
        DO:
            remWinObj(cip_name,{&WIN_TEXT}).
            RETURN {&WIN_NULL}.
        END.

        IF VALID-HANDLE(win_obj.h_handle) THEN ASSIGN i_numText = i_numText + 1.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createButton(INPUT hip_frame AS HANDLE,
                                      INPUT cip_name  AS CHARACTER,
                                      INPUT cip_label AS CHARACTER,
                                      INPUT iip_w     AS INTEGER,
                                      INPUT iip_h     AS INTEGER,
                                      INPUT iip_x     AS INTEGER,
                                      INPUT iip_y     AS INTEGER):
        ASSIGN i_dColor = 0.

        IF NOT VALID-HANDLE(hip_frame) THEN RETURN {&WIN_NULL}.

        i_dColor = hip_frame:DCOLOR.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_BUTTON}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_BUTTON}.

        CREATE BUTTON win_obj.h_handle
            ASSIGN FRAME        = hip_frame
                   LABEL        = cip_label
                   WIDTH-CHARS  = iip_w
                   HEIGHT-CHARS = iip_h
                   X            = iip_x
                   Y            = iip_y
                   SENSITIVE    = TRUE
                   DCOLOR       = i_dColor.

        IF ERROR-STATUS:ERROR THEN
        DO:
            remWinObj(cip_name,{&WIN_BUTTON}).
            RETURN {&WIN_NULL}.
        END.

        IF VALID-HANDLE(win_obj.h_handle) THEN ASSIGN i_numButton = i_numButton + 1.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createFill(INPUT hip_frame AS HANDLE,
                                    INPUT hip_label AS HANDLE,
                                    INPUT cip_name  AS CHARACTER,
                                    INPUT iip_w     AS INTEGER,
                                    INPUT iip_h     AS INTEGER,
                                    INPUT iip_x     AS INTEGER,
                                    INPUT iip_y     AS INTEGER):
        ASSIGN i_dColor = 0.
        IF NOT VALID-HANDLE(hip_frame) THEN
        DO:
            winError("Invalid parent frame, can not create frame").
            RETURN {&WIN_NULL}.
        END.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_FILLIN}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        ASSIGN i_dColor = IF hip_frame:DCOLOR = {&WIN_BLANK} THEN
                              {&WIN_LITE}
                          ELSE
                          IF hip_frame:DCOLOR = {&WIN_FILL} THEN
                              {&WIN_BLANK}
                          ELSE
                              {&WIN_FILL}.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_FILLIN}.

        CREATE FILL-IN win_obj.h_handle
            ASSIGN FRAME             = hip_frame
                   SIDE-LABEL-HANDLE = hip_label
                   NAME              = cip_name
                   WIDTH-CHARS       = iip_w
                   HEIGHT-CHARS      = iip_h
                   X                 = iip_x
                   Y                 = iip_y
                   DCOLOR            = i_dColor
                   FORMAT            = SUBSTITUTE("X~(&1~)",iip_w)
                   PRIVATE-DATA      = ""
                   SENSITIVE         = TRUE.

        IF VALID-HANDLE(win_obj.h_handle) THEN ASSIGN i_numFill = i_numFill + 1.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createFillLabel(INPUT hip_frame AS HANDLE,
                                         INPUT cip_name  AS CHARACTER,
                                         INPUT cip_label AS CHARACTER,
                                         INPUT cip_lpos  AS CHARACTER,
                                         INPUT iip_w     AS INTEGER,
                                         INPUT iip_h     AS INTEGER,
                                         INPUT iip_x     AS INTEGER,
                                         INPUT iip_y     AS INTEGER):
        ASSIGN i_dColor = 0.
        IF NOT VALID-HANDLE(hip_frame) THEN
        DO:
            winError("Frame handle is not valid, can not create Fill with Label").
            RETURN {&WIN_NULL}.
        END.

        ASSIGN h_tempHandle = {&WIN_NULL}.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_FILLIN}:
        END.
        IF AVAILABLE win_obj THEN
        DO:
            RETURN win_obj.h_handle.
        END.

        ASSIGN i_dColor = IF hip_frame:DCOLOR = {&WIN_BLANK} THEN
                              {&WIN_LITE}
                          ELSE
                          IF hip_frame:DCOLOR = {&WIN_FILL} THEN
                              {&WIN_BLANK}
                          ELSE
                              {&WIN_FILL}
               iip_x    = MAX(iip_x,1)
               iip_y    = MAX(iip_y,1).

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_FILLIN}
               win_obj.c_data = cip_lpos + {&WIN_DELIM} + SUBSTITUTE("&1_label",cip_name).

        ASSIGN h_tempHandle = createText(hip_frame,
                                         SUBSTITUTE("&1_label",cip_name),
                                         cip_label,
                                         SUBSTITUTE("X~(&1~)",LENGTH(cip_label)),
                                         0,0).

        CASE cip_lpos:
            WHEN "TOP"    THEN
            DO:
                ASSIGN h_tempHandle:COL = iip_x
                       h_tempHandle:ROW = iip_y
                       iip_x            = h_tempHandle:COL
                       iip_y            = h_tempHandle:ROW + 1.
            END.
            WHEN "BOTTOM" THEN
            DO:
                ASSIGN h_tempHandle:COL = iip_x
                       h_tempHandle:ROW = iip_y + 1.
            END.
            WHEN "LEFT"   THEN
            DO:
                ASSIGN h_tempHandle:COL = iip_x
                       h_tempHandle:ROW = iip_y
                       iip_x            = h_tempHandle:COL + h_tempHandle:WIDTH-CHARS + 1
                       iip_y            = iip_y.
            END.
            WHEN "RIGHT"  THEN
            DO:
                ASSIGN h_tempHandle:COL = iip_x + iip_w + 1
                       h_tempHandle:ROW = iip_y.
            END.
        END CASE.

        IF NOT VALID-HANDLE(h_tempHandle) THEN
        DO:
            remWinObj(cip_name,{&WIN_FILLIN}).
            winError("Error creating label object for fill").
            RETURN {&WIN_NULL}.
        END.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_FILLIN}:
        END.
        IF NOT AVAILABLE win_obj THEN
        DO:
            winError("Error finding win_obj table for fill object").
            RETURN {&WIN_NULL}.
        END.

        CREATE FILL-IN win_obj.h_handle
            ASSIGN FRAME             = hip_frame
                   SIDE-LABEL-HANDLE = h_tempHandle
                   NAME              = cip_name
                   WIDTH-CHARS       = iip_w
                   HEIGHT-CHARS      = iip_h
                   //X                 = iip_x
                   //Y                 = iip_y
                   COL               = iip_x
                   ROW               = iip_y
                   DCOLOR            = i_dColor
                   FORMAT            = SUBSTITUTE("X~(&1~)",iip_w)
                   PRIVATE-DATA      = cip_label
                   SENSITIVE         = TRUE.

        IF ERROR-STATUS:ERROR THEN
        DO:
            remWinObj(cip_name,{&WIN_FILLIN}).
            winError("Error creating fill: " + ERROR-STATUS:GET-MESSAGE(1)).
            RETURN {&WIN_NULL}.
        END.

        IF VALID-HANDLE(win_obj.h_handle) THEN ASSIGN i_numFill = i_numFill + 1.

        FOR FIRST x_obj
            WHERE x_obj.c_name = SUBSTITUTE("&1_label",cip_name)
            AND   x_obj.c_type = {&WIN_TEXT}:
        END.
        IF NOT AVAILABLE x_obj THEN
        DO:
            winError("Could not find label object for fill").
            RETURN {&WIN_NULL}.
        END.

        ASSIGN x_obj.c_data = STRING(win_obj.h_handle).

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createComboBox(INPUT hip_frame AS HANDLE,
                                        INPUT cip_name  AS CHARACTER,
                                        INPUT cip_list  AS CHARACTER,
                                        INPUT iip_w     AS INTEGER,
                                        INPUT iip_h     AS INTEGER,
                                        INPUT iip_x     AS INTEGER,
                                        INPUT iip_y     AS INTEGER):
        ASSIGN i_dColor = 0.
        IF NOT VALID-HANDLE(hip_frame) THEN RETURN {&WIN_NULL}.

        ASSIGN i_dColor = IF hip_frame:DCOLOR = {&WIN_BLANK} THEN
                              {&WIN_LITE}
                          ELSE
                          IF hip_frame:DCOLOR = {&WIN_FILL} THEN
                              {&WIN_BLANK}
                          ELSE
                              {&WIN_FILL}.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_COMBOBOX}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_COMBOBOX}.

        CREATE COMBO-BOX win_obj.h_handle
            ASSIGN FRAME        = hip_frame
                   NAME         = cip_name
                   LIST-ITEMS   = cip_list
                   WIDTH-CHARS  = iip_w
                   //HEIGHT-CHARS = iip_h
                   X            = iip_x
                   Y            = iip_y
                   FORMAT       = SUBSTITUTE("X~(&1~)",iip_w)
                   DCOLOR       = i_dColor
                   SENSITIVE    = TRUE.

        RETURN win_obj.h_handle.
    END METHOD.

    /* -- Frame handle here removed due to issues with assigning
          menubar to window handle in class method. Doing so
          will cause ctrl-@ to appear in labels for submenus. --
    METHOD PUBLIC HANDLE createMenuBar(INPUT hip_frame AS HANDLE,
                                       INPUT cip_name  AS CHARACTER):*/
    METHOD PUBLIC HANDLE createMenuBar(INPUT cip_name  AS CHARACTER):
        //IF NOT VALID-HANDLE(hip_frame) THEN RETURN {&WIN_NULL}.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_MENUBAR}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_MENUBAR}.

        CREATE MENU win_obj.h_handle.

        //ASSIGN hip_frame:MENUBAR = win_obj.h_handle.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createSubMenu(INPUT hip_parent AS HANDLE,
                                       INPUT cip_name   AS CHARACTER,
                                       INPUT cip_label  AS CHARACTER):
        IF NOT VALID-HANDLE(hip_parent) THEN RETURN {&WIN_NULL}.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_MENUSUB}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_MENUSUB}.

        CREATE SUB-MENU win_obj.h_handle
            ASSIGN PARENT = hip_parent
                   NAME   = cip_name
                   LABEL  = cip_label.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createSubItem(INPUT hip_parent AS HANDLE,
                                       INPUT cip_name   AS CHARACTER,
                                       INPUT cip_label  AS CHARACTER):
        IF NOT VALID-HANDLE(hip_parent) THEN RETURN {&WIN_NULL}.

        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = {&WIN_SUBITEM}:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_name = cip_name
               win_obj.c_type = {&WIN_SUBITEM}.

        CREATE MENU-ITEM win_obj.h_handle
            ASSIGN PARENT = hip_parent
                   NAME   = cip_name
                   LABEL  = cip_label.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createRect(INPUT hip_frame AS HANDLE,
                                    INPUT cip_name  AS CHARACTER,
                                    INPUT iip_w     AS INTEGER,
                                    INPUT iip_h     AS INTEGER,
                                    INPUT iip_x     AS INTEGER,
                                    INPUT iip_y     AS INTEGER,
                                    INPUT iip_c     AS INTEGER,
                                    INPUT lip_c     AS LOGICAL):
        IF NOT VALID-HANDLE(hip_frame) THEN RETURN {&WIN_NULL}.

        FOR FIRST win_obj
            WHERE win_obj.c_type = {&WIN_RECT}
            AND   win_obj.c_name = cip_name:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_type = {&WIN_RECT}
               win_obj.c_name = cip_name.

        CREATE RECTANGLE win_obj.h_handle
            ASSIGN NAME         = cip_name
                   WIDTH-CHARS  = iip_w
                   HEIGHT-CHARS = iip_h
                   X            = iip_x
                   Y            = iip_y
                   DCOLOR       = iip_c
                   EDGE-CHARS   = 0
                   VISIBLE      = TRUE
                   FRAME        = hip_frame
                   SENSITIVE    = TRUE
                   GRAPHIC-EDGE = lip_c.

        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE createImage(INPUT hip_frame AS HANDLE,
                                     INPUT cip_name  AS CHARACTER,
                                     INPUT iip_w     AS INTEGER,
                                     INPUT iip_h     AS INTEGER,
                                     INPUT iip_x     AS INTEGER,
                                     INPUT iip_y     AS INTEGER,
                                     INPUT lip_load  AS LOGICAL,
                                     INPUT cip_file  AS CHARACTER):
        IF NOT VALID-HANDLE(hip_frame) THEN RETURN {&WIN_NULL}.

        FOR FIRST win_obj
            WHERE win_obj.c_type = {&WIN_IMAGE}
            AND   win_obj.c_name = cip_name:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        CREATE win_obj.
        ASSIGN win_obj.c_type = {&WIN_IMAGE}
               win_obj.c_name = cip_name.

        CREATE IMAGE win_obj.h_handle
            ASSIGN NAME         = cip_name
                   WIDTH-CHARS  = iip_w
                   HEIGHT-CHARS = iip_h
                   X            = iip_x
                   Y            = iip_y
                   FRAME        = hip_frame.

        IF lip_load THEN win_obj.h_handle:LOAD-IMAGE(cip_file).
        RETURN win_obj.h_handle.
    END METHOD.

    METHOD PUBLIC HANDLE getWinObj(INPUT cip_name AS CHARACTER,
                                   INPUT cip_type AS CHARACTER):
        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = cip_type:
        END.
        IF AVAILABLE win_obj THEN RETURN win_obj.h_handle.

        RETURN {&WIN_NULL}.
    END METHOD.

    METHOD PUBLIC LOG remWinObj(INPUT hip_handle AS HANDLE):
        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN TRUE.

        IF win_obj.c_type = {&WIN_FILLIN} THEN
        DO:
            FOR FIRST x_obj
                WHERE x_obj.h_handle = hip_handle:SIDE-LABEL-HANDLE:
                IF AVAILABLE x_obj THEN
                DO:
                    IF NOT remWinObj(x_obj.h_handle) THEN RETURN FALSE.
                    IF AVAILABLE x_obj THEN DELETE x_obj.
                END.
            END.

            IF NOT AVAILABLE win_obj THEN
            FOR FIRST win_obj
                WHERE win_obj.h_handle = hip_handle:
            END.
        END.

        IF VALID-HANDLE(win_obj.h_handle) THEN DELETE OBJECT win_obj.h_handle.
        IF VALID-HANDLE(win_obj.h_handle) THEN RETURN FALSE.

        DELETE win_obj.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG remWinObj(INPUT cip_name AS CHARACTER,
                                INPUT cip_type AS CHARACTER):
        FOR FIRST win_obj
            WHERE win_obj.c_name = cip_name
            AND   win_obj.c_type = cip_type:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN TRUE.

        IF win_obj.c_type = {&WIN_FILLIN} THEN
        DO:
            FOR FIRST x_obj
                WHERE x_obj.h_handle = win_obj.h_handle:SIDE-LABEL-HANDLE:
                IF AVAILABLE x_obj THEN
                DO:
                    IF NOT remWinObj(x_obj.h_handle) THEN RETURN FALSE.
                    IF AVAILABLE x_obj THEN DELETE x_obj.
                END.
            END.
        END.

        IF VALID-HANDLE(win_obj.h_handle) THEN DELETE OBJECT win_obj.h_handle.
        IF VALID-HANDLE(win_obj.h_handle) THEN RETURN FALSE.

        DELETE win_obj.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG clearWinObjPool():
        ASSIGN l_clearPool = TRUE.
        FOR EACH x_obj:
            IF x_obj.c_type = {&WIN_CLIENT} THEN NEXT.
            IF NOT remWinObj(x_obj.h_handle) THEN
                ASSIGN l_clearPool = FALSE.
        END.
        RETURN l_clearPool.
    END METHOD.

    METHOD PUBLIC INT maxFrameWidth(INPUT hip_frame AS HANDLE):
        ASSIGN i_maxX = 0.

        IF NOT VALID-HANDLE(hip_frame) THEN RETURN 0.

        ASSIGN i_maxX = (hip_frame:WIDTH-CHARS  - {&WIN_BUFF} - 1) * {&WIN_STEP}.
        RETURN i_maxX.
    END METHOD.

    METHOD PUBLIC INT maxFrameHeight(INPUT hip_frame AS HANDLE):
        ASSIGN i_maxY = 0.

        IF NOT VALID-HANDLE(hip_frame) THEN RETURN 0.

        ASSIGN i_maxY = (hip_frame:HEIGHT-CHARS - {&WIN_BUFF} - 1) * {&WIN_STEP}.
        RETURN i_maxY.
    END METHOD.

    METHOD PUBLIC LOG winObjReg (OUTPUT TABLE win_obj BIND):
        RETURN TEMP-TABLE win_obj:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC LOG winObjDetReg (OUTPUT TABLE win_objDetail BIND):
        RETURN TEMP-TABLE win_objDetail:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC LOG winFillInput(INPUT hip_handle AS HANDLE,
                                   INPUT cip_val    AS CHARACTER):
        ASSIGN i_temp   = 0
               c_before = ""
               c_after  = "".

        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj           THEN RETURN FALSE.

        IF win_obj.c_type <> {&WIN_FILLIN} THEN RETURN FALSE.

        IF NOT alphaNumer(cip_val)         THEN RETURN FALSE.

        IF LENGTH(win_obj.h_handle:PRIVATE-DATA) >= win_obj.h_handle:WIDTH-CHARS
            THEN RETURN FALSE.

        IF cip_val = {&WIN_SPACE} THEN
        DO:
            IF i_cursorY > hip_handle:WIDTH-CHARS THEN RETURN FALSE.
            ASSIGN cip_val = "_".
        END.

        ASSIGN i_temp   = i_cursorY - win_obj.h_handle:COL
               c_before = SUBSTRING(win_obj.h_handle:PRIVATE-DATA,1,i_temp)
               c_after  = SUBSTRING(win_obj.h_handle:PRIVATE-DATA,
                                    i_temp + 1,
                                    LENGTH(win_obj.h_handle:PRIVATE-DATA) - LENGTH(c_before))
        NO-ERROR.

        ASSIGN win_obj.h_handle:PRIVATE-DATA = c_before + cip_val + c_after
               win_obj.h_handle:SCREEN-VALUE = REPLACE(win_obj.h_handle:PRIVATE-DATA,"_"," ").

        IF i_cursorY = win_obj.h_handle:COL THEN ASSIGN i_cursorY = i_cursorY + 1.
        ELSE moveCursor(i_cursorX,i_cursorY + 1).

        RETURN INDEX(win_obj.h_handle:PRIVATE-DATA,cip_val) > 0.
    END METHOD.

    METHOD PUBLIC LOG winFillRemove(INPUT hip_handle AS HANDLE):
        ASSIGN i_temp   = 0
               c_before = ""
               c_after  = "".

        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj              THEN RETURN FALSE.

        IF win_obj.c_type <> {&WIN_FILLIN}    THEN RETURN FALSE.

        IF win_obj.h_handle:PRIVATE-DATA = "" THEN RETURN FALSE.

        ASSIGN i_temp   = i_cursorY - win_obj.h_handle:COL
               c_before = SUBSTRING(win_obj.h_handle:PRIVATE-DATA,1,i_temp - 1)
               c_after  = SUBSTRING(win_obj.h_handle:PRIVATE-DATA,
                                    i_temp + 1,
                                    LENGTH(win_obj.h_handle:PRIVATE-DATA) - LENGTH(c_before))
        NO-ERROR.

        ASSIGN win_obj.h_handle:PRIVATE-DATA = c_before + c_after
               win_obj.h_handle:SCREEN-VALUE = REPLACE(win_obj.h_handle:PRIVATE-DATA,"_"," ").

        IF i_cursorY = win_obj.h_handle:COL THEN ASSIGN i_cursorY = i_cursorY - 1.
        moveCursor(i_cursorX,i_cursorY - 1).

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG moveCursor(INPUT iip_x AS INTEGER,
                                 INPUT iip_y AS INTEGER):
        PUT CURSOR ROW iip_x COL iip_y.
        ASSIGN i_cursorX = iip_x
               i_cursorY = iip_y.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG alphaNumer(INPUT cip_string AS CHARACTER):
        IF cip_string = "" THEN RETURN FALSE.
        RETURN LOOKUP(cip_string,c_alphaNumer) > 0.
    END METHOD.

    METHOD PUBLIC LOG enableObj(INPUT hip_handle AS HANDLE):
        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN FALSE.

        IF NOT VALID-HANDLE(win_obj.h_handle) THEN RETURN FALSE.

        IF win_obj.c_type = {&WIN_FILLIN} THEN
        DO:
            ASSIGN win_obj.h_handle:READ-ONLY = FALSE
                   win_obj.h_handle:SENSITIVE = TRUE.

            RETURN win_obj.h_handle:READ-ONLY = FALSE AND
                   win_obj.h_handle:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            ASSIGN win_obj.h_handle:SENSITIVE = TRUE.
            RETURN win_obj.h_handle:SENSITIVE = TRUE.
        END.
    END METHOD.

    METHOD PUBLIC LOG disableObj(INPUT hip_handle AS HANDLE):
        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN FALSE.

        IF NOT VALID-HANDLE(win_obj.h_handle) THEN RETURN FALSE.

        IF win_obj.c_type = {&WIN_FILLIN} THEN
        DO:
            ASSIGN win_obj.h_handle:READ-ONLY = TRUE
                   win_obj.h_handle:SENSITIVE = FALSE.

            RETURN win_obj.h_handle:READ-ONLY = TRUE AND
                   win_obj.h_handle:SENSITIVE = FALSE.
        END.
        ELSE
        DO:
            ASSIGN win_obj.h_handle:SENSITIVE = FALSE.
            RETURN win_obj.h_handle:SENSITIVE = FALSE.
        END.
    END METHOD.

    METHOD PUBLIC INT colorHighlight(INPUT iip_color AS INTEGER):
        ASSIGN iip_color = IF iip_color = {&WIN_BLANK} THEN
                               {&WIN_FILL}
                           ELSE
                           IF iip_color = {&WIN_FILL}  THEN
                               {&WIN_LITE}
                           ELSE
                               {&WIN_BLANK}.
        RETURN iip_color.
    END METHOD.

    METHOD PUBLIC LOG backup_winObj(INPUT hip_handle AS HANDLE,
                                    INPUT lip_over   AS LOGICAL):
        ASSIGN i_tempX = 0
               i_tempY = 0.

        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN FALSE.

        FOR FIRST win_objDetail
            WHERE win_objDetail.c_name = win_obj.c_name
            AND   win_objDetail.c_type = win_obj.c_type:
        END.
        IF AVAILABLE win_objDetail AND NOT lip_over THEN RETURN FALSE.

        IF NOT AVAILABLE win_objDetail THEN
        DO:
            CREATE win_objDetail.
            ASSIGN win_objDetail.c_name = win_obj.c_name
                   win_objDetail.c_type = win_obj.c_type.
        END.

        CASE win_obj.c_type:
            WHEN {&WIN_CLIENT} THEN
            DO:
                ASSIGN win_objDetail.c_val = win_obj.c_name                            + {&WIN_DELIM} +
                                             win_obj.h_handle:TITLE                    + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:WIDTH-CHARS)      + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:HEIGHT-CHARS)     + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:MAX-WIDTH-CHARS)  + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:MAX-HEIGHT-CHARS) + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:X)                + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:Y)                + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:MESSAGE-AREA)     + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:STATUS-AREA)
                      win_objDetail.i_set  = 0.
            END.
            WHEN {&WIN_FRAME} THEN
            DO:
                ASSIGN win_objDetail.c_val = win_obj.c_name                        + {&WIN_DELIM} +
                                             (IF win_obj.h_handle:TITLE = ? THEN
                                                 ""
                                             ELSE
                                                 win_obj.h_handle:TITLE)           + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:WIDTH-CHARS)  + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:HEIGHT-CHARS) + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:X)            + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:Y)            + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:DCOLOR)       + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:BOX)
                       win_objDetail.i_set = 1.
            END.
            WHEN {&WIN_TEXT} THEN
            DO:
                ASSIGN win_objDetail.c_val = win_obj.h_handle:FRAME:NAME    + {&WIN_DELIM} +
                                             win_obj.c_name                 + {&WIN_DELIM} +
                                             win_obj.h_handle:SCREEN-VALUE  + {&WIN_DELIM} +
                                             win_obj.h_handle:FORMAT        + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:X)     + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:Y)
                       win_objDetail.i_set = 3.
            END.
            WHEN {&WIN_BUTTON} THEN
            DO:
                ASSIGN win_objDetail.c_val = win_obj.h_handle:FRAME:NAME           + {&WIN_DELIM} +
                                             win_obj.c_name                        + {&WIN_DELIM} +
                                             win_obj.h_handle:LABEL                + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:WIDTH-CHARS)  + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:HEIGHT-CHARS) + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:X)            + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:Y)
                       win_objDetail.i_set = 2.
            END.
            WHEN {&WIN_FILLIN} THEN
            DO:
                CASE ENTRY(1,win_obj.c_data,{&WIN_DELIM}):
                    WHEN "TOP"    THEN ASSIGN i_tempX = 0
                                              i_tempy = -1 * {&WIN_STEP}.
                    WHEN "BOTTOM" THEN ASSIGN i_tempX = 0
                                              i_tempY = {&WIN_STEP}.
                    WHEN "LEFT"   THEN ASSIGN i_tempX = -1 * ((win_obj.h_handle:WIDTH-CHARS + 1) * {&WIN_STEP})
                                              i_tempY = 0.
                    WHEN "RIGHT"  THEN ASSIGN i_tempX = ((win_obj.h_handle:WIDTH-CHARS + 1) * {&WIN_STEP})
                                              i_tempY = 0.
                END CASE.

                ASSIGN win_objDetail.c_val = win_obj.h_handle:FRAME:NAME                             + {&WIN_DELIM} +
                                             win_obj.c_name                                          + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:SIDE-LABEL-HANDLE:SCREEN-VALUE) + {&WIN_DELIM} +
                                             ENTRY(1,win_obj.c_data,{&WIN_DELIM})                    + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:WIDTH-CHARS)                    + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:HEIGHT-CHARS)                   + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:X + i_tempX)                    + {&WIN_DELIM} +
                                             STRING(win_obj.h_handle:Y + i_tempY)
                       win_objDetail.i_set = 2.
            END.
            OTHERWISE DO:
                DELETE win_objDetail.
                RETURN FALSE.
            END.
        END CASE.
        RETURN AVAILABLE win_objDetail.
    END METHOD.

    METHOD PUBLIC LOGICAL respec_winObj (INPUT hip_handle AS HANDLE,
                                         INPUT iip_field  AS INTEGER,
                                         INPUT cip_value  AS CHARACTER):
        IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.

        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN FALSE.

        FOR FIRST win_objDetail
            WHERE win_objDetail.c_name = win_obj.c_name
            AND   win_objDetail.c_type = win_obj.c_type:
        END.
        IF NOT AVAILABLE win_objDetail THEN RETURN FALSE.

        IF iip_field = 0 OR
           iip_field = ? OR
           iip_field > NUM-ENTRIES(win_objDetail.c_val,{&WIN_DELIM})
        THEN RETURN FALSE.

        ASSIGN ENTRY(iip_field,win_objDetail.c_val,{&WIN_DELIM}) = cip_value.
        IF ERROR-STATUS:ERROR THEN RETURN FALSE.

        RETURN ENTRY(iip_field,win_objDetail.c_val,{&WIN_DELIM}) = cip_value.
    END METHOD.

    METHOD PUBLIC HANDLE remake_winObj (INPUT hip_handle AS HANDLE):
        ASSIGN c_temp       = ""
               h_tempHandle = ?.

        FOR FIRST win_obj
            WHERE win_obj.h_handle = hip_handle:
        END.
        IF NOT AVAILABLE win_obj THEN RETURN {&WIN_NULL}.

        FOR FIRST win_objDetail
            WHERE win_objDetail.c_name = win_obj.c_name
            AND   win_objDetail.c_type = win_obj.c_type:
        END.
        IF NOT AVAILABLE win_objDetail THEN RETURN {&WIN_NULL}.

        ASSIGN c_temp               = win_obj.c_name + "_DELETE" + {&WIN_DELIM} +
                                      win_obj.c_name             + {&WIN_DELIM} +
                                      win_obj.c_type
               win_obj.c_name       = ENTRY(1,c_temp,{&WIN_DELIM})
               win_objDetail.c_name = ENTRY(1,c_temp,{&WIN_DELIM}).

        IF win_obj.c_type = {&WIN_FILLIN} THEN
        DO:
            FOR FIRST x_obj
                WHERE x_obj.c_name = ENTRY(2,c_temp,{&WIN_DELIM}) + "_label"
                AND   x_obj.c_type = {&WIN_TEXT}:
            END.
            IF AVAILABLE x_obj THEN
                ASSIGN c_temp       = c_temp                   + {&WIN_DELIM} +
                                      x_obj.c_name + "_DELETE" + {&WIN_DELIM} +
                                      x_obj.c_name             + {&WIN_DELIM} +
                                      x_obj.c_type
                      x_obj.c_name  = ENTRY(4,c_temp,{&WIN_DELIM}).
        END.

        CASE win_obj.c_type:
            WHEN {&WIN_FRAME} THEN
            DO:
                h_tempHandle = createFrame(ENTRY(1,win_objDetail.c_val,{&WIN_DELIM}),
                                           ENTRY(2,win_objDetail.c_val,{&WIN_DELIM}),
                                           INTEGER(ENTRY(3,win_objDetail.c_val,{&WIN_DELIM})),
                                           INTEGER(ENTRY(4,win_objDetail.c_val,{&WIN_DELIM})),
                                           INTEGER(ENTRY(5,win_objDetail.c_val,{&WIN_DELIM})),
                                           INTEGER(ENTRY(6,win_objDetail.c_val,{&WIN_DELIM})),
                                           INTEGER(ENTRY(7,win_objDetail.c_val,{&WIN_DELIM})),
                                           LOGICAL(ENTRY(8,win_objDetail.c_val,{&WIN_DELIM}))
                                           ).
            END.
            WHEN {&WIN_TEXT} THEN
            DO:
                FOR FIRST x_obj
                    WHERE x_obj.c_name = ENTRY(1,win_objDetail.c_val,{&WIN_DELIM})
                    AND   x_obj.c_type = {&WIN_FRAME}:
                END.
                IF NOT AVAILABLE x_obj THEN RETURN {&WIN_NULL}.

                h_tempHandle = createText(x_obj.h_handle,
                                          ENTRY(2,win_objDetail.c_val,{&WIN_DELIM}),
                                          ENTRY(3,win_objDetail.c_val,{&WIN_DELIM}),
                                          ENTRY(4,win_objDetail.c_val,{&WIN_DELIM}),
                                          INTEGER(ENTRY(5,win_objDetail.c_val,{&WIN_DELIM})),
                                          INTEGER(ENTRY(6,win_objDetail.c_val,{&WIN_DELIM}))
                                          ).
            END.
            WHEN {&WIN_BUTTON} THEN
            DO:
                FOR FIRST x_obj
                    WHERE x_obj.c_name = ENTRY(1,win_objDetail.c_val,{&WIN_DELIM})
                    AND   x_obj.c_type = {&WIN_FRAME}:
                END.
                IF NOT AVAILABLE x_obj THEN RETURN {&WIN_NULL}.

                h_tempHandle = createButton(x_obj.h_handle,
                                            ENTRY(2,win_objDetail.c_val,{&WIN_DELIM}),
                                            ENTRY(3,win_objDetail.c_val,{&WIN_DELIM}),
                                            INTEGER(ENTRY(4,win_objDetail.c_val,{&WIN_DELIM})),
                                            INTEGER(ENTRY(5,win_objDetail.c_val,{&WIN_DELIM})),
                                            INTEGER(ENTRY(6,win_objDetail.c_val,{&WIN_DELIM})),
                                            INTEGER(ENTRY(7,win_objDetail.c_val,{&WIN_DELIM}))
                                           ).
            END.
            WHEN {&WIN_FILLIN} THEN
            DO:
                FOR FIRST x_obj
                    WHERE x_obj.c_name = ENTRY(1,win_objDetail.c_val,{&WIN_DELIM})
                    AND   x_obj.c_type = {&WIN_FRAME}:
                END.
                IF NOT AVAILABLE x_obj THEN RETURN {&WIN_NULL}.

                h_tempHandle = createFillLabel(x_obj.h_handle,
                                               ENTRY(2,win_objDetail.c_val,{&WIN_DELIM}),
                                               ENTRY(3,win_objDetail.c_val,{&WIN_DELIM}),
                                               ENTRY(4,win_objDetail.c_val,{&WIN_DELIM}),
                                               INTEGER(ENTRY(5,win_objDetail.c_val,{&WIN_DELIM})),
                                               INTEGER(ENTRY(6,win_objDetail.c_val,{&WIN_DELIM})),
                                               INTEGER(ENTRY(7,win_objDetail.c_val,{&WIN_DELIM})),
                                               INTEGER(ENTRY(8,win_objDetail.c_val,{&WIN_DELIM}))
                                              ).
            END.
            OTHERWISE RETURN {&WIN_NULL}.
        END CASE.

        IF NOT VALID-HANDLE(h_tempHandle) THEN RETURN {&WIN_NULL}.

        IF ENTRY(3,c_temp,{&WIN_DELIM}) = {&WIN_FILLIN} THEN
        DO:
            IF NOT remWinObj(ENTRY(4,c_temp,{&WIN_DELIM}),ENTRY(6,c_temp,{&WIN_DELIM})) THEN
            DO:
                remWinObj(ENTRY(5,c_temp,{&WIN_DELIM}),ENTRY(6,c_temp,{&WIN_DELIM})).
            END.
        END.

        IF NOT remWinObj(ENTRY(1,c_temp,{&WIN_DELIM}),ENTRY(3,c_temp,{&WIN_DELIM})) THEN
        DO:
            remWinObj(ENTRY(2,c_temp,{&WIN_DELIM}),ENTRY(3,c_temp,{&WIN_DELIM})).
            RETURN {&WIN_NULL}.
        END.

        DELETE win_objDetail.

        RETURN h_tempHandle.
    END METHOD.

    METHOD PUBLIC LOG messageCUI (INPUT cip_message AS CHARACTER,
                                  INPUT cip_type    AS CHARACTER,
                                  INPUT lip_log     AS LOGICAL):
        CASE cip_type:
            WHEN 'A' THEN ASSIGN cip_type = "ALERT".
            WHEN 'E' THEN ASSIGN cip_type = "ERROR".
            WHEN 'S' THEN ASSIGN cip_type = "SUCCESS".
            WHEN 'W' THEN ASSIGN cip_type = "WARNING".
        END CASE.
        IF lip_log THEN
            MESSAGE cip_message VIEW-AS ALERT-BOX
            BUTTONS YES-NO TITLE cip_type UPDATE lip_log.
        ELSE
            MESSAGE cip_message VIEW-AS ALERT-BOX TITLE cip_type.

        RETURN lip_log.
    END METHOD.

    METHOD PRIVATE VOID winError (INPUT cip_message AS CHARACTER):
        ASSIGN i_temp = 0.

        FOR LAST win_err:
            ASSIGN i_temp = win_err.i_idx.
        END.

        CREATE win_err.
        ASSIGN i_numError    = i_numError + 1
               i_temp        = i_temp + 1
               win_err.i_idx = i_temp
               win_err.c_err = cip_message
               l_error       = TRUE.
    END METHOD.

    METHOD PUBLIC CHAR getErrMsg (INPUT iip_error AS INTEGER):
        ASSIGN c_temp = "".

        FOR FIRST win_err
            WHERE win_err.i_idx = iip_error:
            ASSIGN c_temp = win_err.c_err.
            DELETE win_err.
            ASSIGN i_numError = i_numError - 1.
        END.

        RETURN c_temp.
    END METHOD.
END CLASS.
