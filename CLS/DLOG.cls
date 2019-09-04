CLASS DLOG:
    DEFINE PUBLIC PROPERTY w_wdc    AS WINPROC   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY b_box    AS BOX       GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_box    AS HANDLE    GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY c_cursor AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_width  AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_height AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_col    AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_row    AS INTEGER   GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY i_l      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_w      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_c      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_r      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_x      AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_y      AS INTEGER   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_temp  AS HANDLE.

    DEFINE PRIVATE VARIABLE fiv_idx AS INTEGER.
    DEFINE PRIVATE VARIABLE fiv_x   AS INTEGER.
    DEFINE PRIVATE VARIABLE fiv_y   AS INTEGER.

    DEFINE PRIVATE VARIABLE fcv_key AS CHARACTER.
    DEFINE PRIVATE VARIABLE fiv_ret AS INTEGER.

    DEFINE TEMP-TABLE cursorObj
        FIELD i_x     AS INTEGER
        FIELD i_y     AS INTEGER
        FIELD i_curr  AS INTEGER
        FIELD c_hnd   AS CHARACTER
        FIELD c_value AS CHARACTER.

    DEFINE TEMP-TABLE dialogOp
        FIELD i_idx    AS INTEGER
        FIELD i_x      AS INTEGER
        FIELD i_y      AS INTEGER
        FIELD c_value  AS CHARACTER
        FIELD c_hnd    AS CHARACTER
        FIELD l_active AS LOGICAL.

    DEFINE BUFFER x_dialogOp FOR dialogOp.

    CONSTRUCTOR DLOG (INPUT iip_col    AS INTEGER,
                      INPUT iip_row    AS INTEGER,
                      INPUT iip_div    AS INTEGER,
                      INPUT cip_cursor AS CHARACTER):

        ASSIGN i_col    = MAX(iip_col,1)
               i_row    = MAX(iip_row,1)
               i_w      = iip_div
               i_c      = 3
               i_r      = 1
               c_cursor = cip_cursor.

    END CONSTRUCTOR.

    DESTRUCTOR DLOG ():
        IF VALID-OBJECT(w_wdc) THEN
        DO:
            HIDE h_box.

            FOR EACH dialogOp:
                ASSIGN h_temp = HANDLE(dialogOp.c_hnd).
                w_wdc:remWinObj(h_temp).
            END.

            FOR EACH cursorObj:
                ASSIGN h_temp = HANDLE(cursorObj.c_hnd).
                w_wdc:remWinObj(h_temp).
            END.

            EMPTY TEMP-TABLE dialogOp.
            EMPTY TEMP-TABLE cursorObj.

            IF VALID-OBJECT(b_box) THEN DELETE OBJECT b_box.
            IF VALID-OBJECT(w_wdc) THEN DELETE OBJECT w_wdc.
        END.
    END DESTRUCTOR.

    {errlib.i &CLASS_LIBRARY = *}

    METHOD PUBLIC LOGICAL addOp (INPUT cip_val AS CHARACTER):
        DEFINE VARIABLE fiv_idx AS INTEGER NO-UNDO.

        FOR LAST dialogOp:
            ASSIGN fiv_idx = dialogOp.i_idx.
        END.

        CREATE dialogOp.
        ASSIGN dialogOp.i_idx   = fiv_idx + 1
               dialogOp.c_value = cip_val
               i_height         = i_height + 1.

        RETURN AVAILABLE dialogOp AND dialogOp.i_idx <> fiv_idx.
    END METHOD.

    METHOD PUBLIC LOGICAL makeOp ():
        FOR FIRST dialogOp:
        END.
        IF NOT AVAILABLE dialogOp THEN
        DO:
            setErr("No dialog options have been created").
            RETURN FALSE.
        END.

        FOR EACH dialogOp
            BY   dialogOp.i_idx:

            IF i_idx MODULO i_w > 0 THEN
                ASSIGN i_l = IF LENGTH(dialogOp.c_value) > i_l THEN
                                 LENGTH(dialogOp.c_value)
                             ELSE
                                 i_l.
        END.
        ASSIGN i_l = i_l + 1.

        FOR EACH dialogOp
            BY   dialogOp.i_idx:

            ASSIGN i_x = IF i_x = i_w THEN 1         ELSE i_x + 1
                   i_y = IF i_x = 1   THEN i_y + 2   ELSE i_y
                   dialogOp.i_x = IF i_x > 1 THEN i_x + ((i_x - 1) * i_l) ELSE 1
                   dialogOp.i_x = dialogOp.i_x + i_c
                   dialogOp.i_y = i_y
                   dialogOp.i_y = dialogOp.i_y + i_r.
        END.

        FOR EACH dialogOp
            BREAK BY dialogOp.i_y
                  BY dialogOp.i_x:

            IF LAST-OF(dialogOp.i_y) THEN
                ASSIGN i_width = MAX(dialogOp.i_x + LENGTH(dialogOp.c_value) + 2,i_width).
        END.

        FOR EACH dialogOp
            BREAK BY dialogOp.i_x
                  BY dialogOp.i_y:

            IF LAST-OF(dialogOp.i_x) THEN
                ASSIGN i_height = MAX(dialogOp.i_y + 2,i_height).
        END.

        b_box = NEW BOX(i_width,i_height,i_col,i_row).
        IF NOT VALID-OBJECT(b_box) THEN
        DO:
            setErr("Failed to create container for object").
            RETURN FALSE.
        END.

        ASSIGN h_box = b_box:h_frame.
        h_box:OVERLAY = TRUE.

        FOR EACH dialogOp
            BY   dialogOp.i_idx:

            h_temp = b_box:w_wdc:createFill(h_box,?,SUBSTITUTE("Option&1",dialogOp.i_idx),LENGTH(dialogOp.c_value),1,0,0).
            IF NOT VALID-HANDLE(h_temp) THEN
            DO:
                setErr(SUBSTITUTE("Failed to create dialog &1",dialogOp.i_idx)).
                NEXT.
            END.

            ASSIGN dialogOp.c_hnd      = STRING(h_temp)
                   h_temp:COL          = dialogOp.i_x
                   h_temp:ROW          = dialogOp.i_y
                   h_temp:PFCOLOR      = h_box:DCOLOR
                   h_temp:SCREEN-VALUE = dialogOp.c_value.
        END.

        makeCurs(c_cursor).

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL makeCurs (INPUT cip_val AS CHARACTER):
        FOR FIRST cursorObj:
        END.
        IF AVAILABLE cursorObj THEN RETURN FALSE.

        FOR FIRST dialogOp:
        END.

        CREATE cursorObj.
        ASSIGN cursorObj.i_x   = dialogOp.i_x - 1
               cursorObj.i_y   = dialogOp.i_y
               cursorObj.c_val = cip_val.

        h_temp = b_box:w_wdc:createFill(h_box,?,SUBSTITUTE("Cursor&1",STRING(b_box:w_wdc)),1,1,0,0).
        IF NOT VALID-HANDLE(h_temp) THEN RETURN FALSE.

        ASSIGN cursorObj.c_hnd     = STRING(h_temp)
               h_temp:COL          = cursorObj.i_x
               h_temp:ROW          = cursorObj.i_y
               h_temp:SCREEN-VALUE = cursorObj.c_value.

        h_temp = HANDLE(dialogOp.c_hnd).

        ASSIGN dialogOp.l_active = TRUE
               h_temp:PFCOLOR    = 3
               cursorObj.i_curr  = dialogOp.i_idx.

        RETURN AVAILABLE cursorObj.
    END METHOD.

    METHOD PUBLIC LOGICAL moveCurs (INPUT iip_x AS INTEGER,
                                    INPUT iip_y AS INTEGER):
        ASSIGN fiv_x = 0
               fiv_y = 0.

        IF AVAILABLE x_dialogOp THEN RELEASE x_dialogOp.

        FOR FIRST cursorObj:
            ASSIGN fiv_x = cursorObj.i_x
                   fiv_y = cursorObj.i_y.

            IF fiv_y < 0 OR fiv_y > 0 THEN ASSIGN cursorObj.i_x = cursorObj.i_x + 1.

            SET-BLK:
            REPEAT:
                ASSIGN cursorObj.i_x = cursorObj.i_x + iip_x
                       cursorObj.i_y = cursorObj.i_y + iip_y.

                FOR EACH  dialogOp
                    BY    dialogOp.i_idx:

                    IF dialogOp.i_x   =  cursorObj.i_x    AND
                       dialogOp.i_y   =  cursorObj.i_y    AND
                       dialogOp.i_idx <> cursorObj.i_curr THEN LEAVE SET-BLK.
                END.

                IF cursorObj.i_x < i_c       THEN LEAVE SET-BLK.
                IF cursorObj.i_x > b_box:i_w THEN LEAVE SET-BLK.
                IF cursorObj.i_y < i_r       THEN LEAVE SET-BLK.
                IF cursorObj.i_y > b_box:i_h THEN LEAVE SET-BLK.
            END.

            IF NOT AVAILABLE dialogOp THEN
            DO:
                ASSIGN cursorObj.i_x = fiv_x
                       cursorObj.i_y = fiv_y.

                RETURN FALSE.
            END.

            FOR FIRST x_dialogOp
                WHERE x_dialogOp.i_idx = cursorObj.i_curr:
            END.
            ASSIGN h_temp           = HANDLE(x_dialogOp.c_hnd)
                   h_temp:PFCOLOR   = h_box:DCOLOR
                   cursorObj.i_curr = dialogOp.i_idx
                   h_temp           = HANDLE(dialogOp.c_hnd)
                   h_temp:PFCOLOR   = 3
                   cursorObj.i_x    = cursorObj.i_x - 1
                   h_temp           = HANDLE(cursorObj.c_hnd)
                   h_temp:COL       = cursorObj.i_x
                   h_temp:ROW       = cursorObj.i_y.

            RETURN TRUE.
        END.

        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC INTEGER getOp ():
        ASSIGN fiv_ret = -1.

        TRY_BLK:
        REPEAT ON ERROR  UNDO TRY_BLK, LEAVE TRY_BLK
               ON ENDKEY UNDO TRY_BLK, LEAVE TRY_BLK
               ON STOP   UNDO TRY_BLK, LEAVE TRY_BLK:

            IF RETRY THEN LEAVE TRY_BLK.

            ASSIGN fcv_key = "".
            PUT CURSOR OFF.
            VIEW h_box.
            READKEY PAUSE .1.
            IF LASTKEY > 0 THEN ASSIGN fcv_key = IF LASTKEY = 32 THEN "SPACEBAR" ELSE KEYLABEL(LASTKEY).

            IF fcv_key > "" THEN
            CASE fcv_key:
                WHEN "CURSOR-UP"    THEN
                DO:
                    moveCurs(0,-1).
                END.
                WHEN "CURSOR-DOWN"  THEN
                DO:
                    moveCurs(0,1).
                END.
                WHEN "CURSOR-LEFT"  THEN
                DO:
                    moveCurs(-1,0).
                END.
                WHEN "CURSOR-RIGHT" THEN
                DO:
                    moveCurs(1,0).
                END.
                WHEN "SPACEBAR" OR
                WHEN "ENTER"    OR
                WHEN "RETURN"   OR
                WHEN "PF1"      OR
                WHEN "F1"       THEN
                DO:
                    FOR FIRST cursorObj:
                        ASSIGN fiv_ret = cursorObj.i_curr.
                    END.

                    LEAVE TRY_BLK.
                END.
                WHEN "ESC" OR
                WHEN "PF4" OR
                WHEN "F4"  THEN
                DO:
                    LEAVE TRY_BLK.
                END.
            END CASE.
        END.

        HIDE h_box.

        RETURN fiv_ret.
    END METHOD.
END CLASS.