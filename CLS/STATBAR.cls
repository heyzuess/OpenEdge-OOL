CLASS STATBAR:
    DEFINE PUBLIC PROPERTY w_wdc   AS WINPROC   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_frame AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_statH AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_statB AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_statH AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_statB AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_w     AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_h     AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_x     AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_y     AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_curr  AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_stat  AS LOGICAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_temp  AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_oid   AS INTEGER.
    DEFINE PRIVATE VARIABLE l_custH AS LOGICAL.
    DEFINE PRIVATE VARIABLE l_custW AS LOGICAL.
    DEFINE PRIVATE VARIABLE l_box   AS LOGICAL.

    CONSTRUCTOR STATBAR (INPUT wip_wdc   AS WINPROC,
                         INPUT hip_frame AS HANDLE,
                         INPUT cip_label AS CHARACTER,
                         INPUT cip_val   AS CHARACTER,
                         INPUT iip_l     AS INTEGER,
                         INPUT iip_c     AS INTEGER,
                         INPUT iip_x     AS INTEGER,
                         INPUT iip_y     AS INTEGER):

        ASSIGN w_wdc   = wip_wdc
               h_frame = hip_frame
               c_statH = cip_label
               c_statB = cip_val
               i_w     = iip_l
               i_curr  = iip_c
               i_x     = MAX(iip_x,1)
               i_y     = MAX(iip_y,1).

        makeObj().
    END CONSTRUCTOR.

    CONSTRUCTOR STATBAR (INPUT hip_frame AS HANDLE,
                         INPUT cip_label AS CHARACTER,
                         INPUT cip_val   AS CHARACTER,
                         INPUT iip_l     AS INTEGER,
                         INPUT iip_c     AS INTEGER,
                         INPUT iip_x     AS INTEGER,
                         INPUT iip_y     AS INTEGER):

        w_wdc   = NEW WINPROC().

        ASSIGN h_frame = hip_frame
               c_statH = cip_label
               c_statB = cip_val
               i_w     = iip_l
               i_curr  = iip_c
               i_x     = MAX(iip_x,1)
               i_y     = MAX(iip_y,1)
               l_custW = TRUE.

        makeObj().
    END CONSTRUCTOR.

    CONSTRUCTOR STATBAR (INPUT cip_label AS CHARACTER,
                         INPUT cip_val   AS CHARACTER,
                         INPUT iip_l     AS INTEGER,
                         INPUT iip_c     AS INTEGER,
                         INPUT iip_x     AS INTEGER,
                         INPUT iip_y     AS INTEGER,
                         INPUT lip_box   AS LOGICAL):

        w_wdc = NEW WINPROC().

        ASSIGN c_statH = cip_label
               c_statB = cip_val
               i_w     = iip_l
               i_curr  = iip_c
               i_x     = MAX(iip_x,1)
               i_y     = MAX(iip_y,1)
               l_custW = TRUE
               l_custH = TRUE
               l_box   = lip_box.

        makeObj().
    END CONSTRUCTOR.

    DESTRUCTOR STATBAR ():
        IF VALID-HANDLE(h_frame) THEN HIDE h_frame.

        IF l_custW THEN
        DO:
            w_wdc:clearWinObjPool().
            IF VALID-OBJECT(w_wdc) THEN DELETE OBJECT w_wdc.
        END.
        ELSE
        DO:
            w_wdc:remWinObj(h_statB).
        END.
    END DESTRUCTOR.

    METHOD PRIVATE VOID makeObj():
        ASSIGN i_h     = INTEGER(LOGICAL(c_statH > "")) + 1
               c_statH = IF c_statH = ?  THEN ""       ELSE c_statH
               c_statB = IF c_statB = ?  THEN ""       ELSE c_statB
               c_statB = IF c_statB = "" THEN CHR(219) ELSE c_statB.

        IF l_custH THEN
        DO:
            i_oid = TIME.
            h_frame = w_wdc:createFrame(SUBSTITUTE("Main&1",i_oid),"",
                                        i_w + IF l_box THEN 2 ELSE 0,
                                        i_h + IF l_box THEN 2 ELSE 0,
                                        0,0,1,
                                        l_box).
            h_frame:COL     = i_x.
            h_frame:ROW     = i_y.
            h_frame:OVERLAY = TRUE.
        END.

        IF c_statH > "" THEN
        DO:
            h_statB = w_wdc:createFillLabel(h_frame,SUBSTITUTE("Header&1",i_oid),"Top",c_statH,i_w,1,0,0).
            h_statH = h_statB:SIDE-LABEL-HANDLE.

            h_statH:FORMAT       = SUBSTITUTE("X(&1)",LENGTH(c_statH)).
            h_statH:COL          = IF l_custH THEN 1 ELSE i_x.
            h_statH:ROW          = IF l_custH THEN 1 ELSE i_y.
            h_statH:SCREEN-VALUE = c_statH.

            h_statB:COL          = h_statH:COL.
            h_statB:ROW          = h_statH:ROW + 1.
        END.
        ELSE
        DO:
            h_statB = w_wdc:createFill(h_frame,?,SUBSTITUTE("Header&1",i_oid),i_w,1,0,0).

            h_statB:COL = IF l_custH THEN 1 ELSE i_x.
            h_statB:ROW = IF l_custH THEN 1 ELSE i_y.
        END.

        h_statB:PFCOLOR      = IF l_custH AND l_box THEN 3 ELSE h_frame:DCOLOR.
        h_statB:SCREEN-VALUE = FILL(c_statB,i_curr).
    END METHOD.

    METHOD PUBLIC LOGICAL showStat ():
        IF c_statH = "" OR c_statH = ? THEN RETURN FALSE.

        ASSIGN c_temp = "".

        c_temp = SUBSTITUTE("&1~/&2",i_curr,i_w).

        IF LENGTH(c_statH) + LENGTH(c_temp) + 1 <= h_statB:WIDTH-CHARS THEN
        DO:
            h_statH:FORMAT = SUBSTITUTE("X(&1)",INTEGER(h_statB:WIDTH-CHARS)).
            h_statH:SCREEN-VALUE = c_statH +
                                   FILL(" ",INTEGER(h_statB:WIDTH-CHARS - (LENGTH(c_statH) + LENGTH(c_temp)))) +
                                   c_temp.
        END.
        ELSE
        DO:
            h_statH:FORMAT = SUBSTITUTE("X(&1)",LENGTH(c_statH) + LENGTH(c_temp) + 1).
            h_statH:SCREEN-VALUE = c_statH + " " + c_temp.
        END.

        l_stat = TRUE.

        RETURN l_stat.
    END METHOD.

    METHOD PUBLIC LOGICAL hideStat ():
        IF c_statH = "" OR c_statH = ? THEN RETURN FALSE.

        h_statH:FORMAT = SUBSTITUTE("X(&1)",LENGTH(c_statH)).
        h_statH:SCREEN-VALUE = c_statH.

        l_stat = FALSE.

        RETURN NOT l_stat.
    END METHOD.

    METHOD PUBLIC LOGICAL toggleStat ():
        IF l_stat THEN
            RETURN hideStat().
        ELSE
            RETURN showStat().
    END METHOD.

    METHOD PUBLIC LOGICAL addBar ():
        IF i_curr = i_w THEN RETURN FALSE.

        RETURN setBar(1).
    END METHOD.

    METHOD PUBLIC LOGICAL subBar ():
        IF i_curr = 0 THEN RETURN FALSE.

        RETURN setBar(-1).
    END METHOD.

    METHOD PUBLIC LOGICAL setBar (INPUT iip_val AS INTEGER):
        IF i_curr = ? THEN RETURN FALSE.

        ASSIGN i_curr = i_curr + iip_val
               i_curr = IF i_curr > i_w THEN
                            i_w
                        ELSE
                        IF i_curr < 0   THEN
                            0
                        ELSE
                           i_curr.

        h_statB:SCREEN-VALUE = FILL(c_statB,i_curr).

        IF l_stat THEN showStat().

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL viewBar ():
        h_frame:VISIBLE = TRUE.
        RETURN h_frame:VISIBLE.
    END METHOD.

    METHOD PUBLIC LOGICAL hideBar ():
        h_frame:VISIBLE = FALSE.
        RETURN NOT h_frame:VISIBLE.
    END METHOD.

    METHOD PUBLIC LOGICAL toggle():
        IF h_frame:VISIBLE THEN
            RETURN viewBar().
        ELSE
            RETURN hideBar().
    END METHOD.
END CLASS.