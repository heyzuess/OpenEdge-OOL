CLASS BOX:
    DEFINE PUBLIC PROPERTY a_obj   AS ARRAY   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY w_wdc   AS WINPROC GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_frame AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_w     AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_h     AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_c     AS INTEGER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_r     AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE PROPERTY l_wdc   AS LOGICAL GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY l_frame AS LOGICAL GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_temp AS HANDLE.

    DEFINE PRIVATE VARIABLE c_curr AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_bv   AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_bh   AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_tl   AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_tr   AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_bl   AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_br   AS CHARACTER.

    DEFINE PRIVATE VARIABLE i_x    AS INTEGER.
    DEFINE PRIVATE VARIABLE i_y    AS INTEGER.
    DEFINE PRIVATE VARIABLE i_int  AS INTEGER.
    DEFINE PRIVATE VARIABLE i_idx  AS INTEGER.
    DEFINE PRIVATE VARIABLE i_oid  AS INTEGER.

    DEFINE TEMP-TABLE menuBord
        FIELD i_idx AS INTEGER
        FIELD i_x   AS INTEGER
        FIELD i_y   AS INTEGER
        FIELD c_hnd AS CHARACTER
        FIELD c_val AS CHARACTER.

    {errlib.i &CLASS_LIBRARY = *}

    CONSTRUCTOR BOX (INPUT wip_wdc   AS WINPROC,
                     INPUT hip_frame AS HANDLE,
                     INPUT iip_w     AS INTEGER,
                     INPUT iip_h     AS INTEGER,
                     INPUT iip_c     AS INTEGER,
                     INPUT iip_r     AS INTEGER):

        ASSIGN w_wdc   = wip_wdc
               h_frame = hip_frame
               i_w     = iip_w
               i_h     = iip_h
               i_c     = iip_c
               i_r     = iip_r
               i_oid   = TIME
               c_bh    = CHR(205)
               c_tl    = CHR(201)
               c_bl    = CHR(200)
               c_bv    = CHR(186)
               c_tr    = CHR(187)
               c_br    = CHR(188).

        makeObj().
    END CONSTRUCTOR.

    CONSTRUCTOR BOX (INPUT hip_frame AS HANDLE,
                     INPUT iip_w     AS INTEGER,
                     INPUT iip_h     AS INTEGER,
                     INPUT iip_c     AS INTEGER,
                     INPUT iip_r     AS INTEGER):

        w_wdc = NEW WINPROC().
        l_wdc = TRUE.

        ASSIGN h_frame = hip_frame
               i_w     = iip_w
               i_h     = iip_h
               i_c     = iip_c
               i_r     = iip_r
               i_oid   = TIME
               c_bh    = CHR(205)
               c_tl    = CHR(201)
               c_bl    = CHR(200)
               c_bv    = CHR(186)
               c_tr    = CHR(187)
               c_br    = CHR(188).

        makeObj().
    END CONSTRUCTOR.

    CONSTRUCTOR BOX (INPUT iip_w AS INTEGER,
                     INPUT iip_h AS INTEGER,
                     INPUT iip_c AS INTEGER,
                     INPUT iip_r AS INTEGER):

        w_wdc = NEW WINPROC().
        l_wdc = TRUE.

        h_frame = w_wdc:createFrame(SUBSTITUTE("Main&1",STRING(w_wdc)),"",iip_w,iip_h,0,0,0,FALSE).
        l_frame = TRUE.

        ASSIGN i_w     = iip_w
               i_h     = iip_h
               i_c     = 0
               i_r     = 0
               i_oid   = TIME
               c_bh    = CHR(205)
               c_tl    = CHR(201)
               c_bl    = CHR(200)
               c_bv    = CHR(186)
               c_tr    = CHR(187)
               c_br    = CHR(188).

        makeObj().

        ASSIGN i_c = MAX(iip_c,1)
               i_r = MAX(iip_r,1)
               h_frame:COL = i_c
               h_frame:ROW = i_r
               h_frame:OVERLAY = TRUE.
    END CONSTRUCTOR.

    DESTRUCTOR BOX ():
        FOR EACH menuBord
            BY menuBord.i_idx:

            ASSIGN h_temp = HANDLE(menuBord.c_hnd).
            w_wdc:remWinObj(h_temp).
        END.

        IF VALID-OBJECT(a_obj) THEN
            DELETE OBJECT a_obj.

        IF l_frame THEN
        DO:
            HIDE h_frame.
            w_wdc:remWinObj(h_frame).
        END.

        IF l_wdc THEN
        DO:
            IF VALID-OBJECT(w_wdc) THEN
                DELETE OBJECT w_wdc.
        END.
    END DESTRUCTOR.

    METHOD PRIVATE VOID makeObj ():
        a_obj = NEW ARRAY().

        DO i_int = 1 TO (i_w * i_h):
            ASSIGN i_x    = IF i_x = i_w THEN 1       ELSE i_x + 1
                   i_y    = IF i_x = 1   THEN i_y + 1 ELSE i_y
                   c_curr = "".

            IF i_x = 1   AND i_y = 1   THEN ASSIGN c_curr = c_tl.
            IF i_x = i_w AND i_y = 1   THEN ASSIGN c_curr = c_tr.
            IF i_x = 1   AND i_y = i_h THEN ASSIGN c_curr = c_bl.
            IF i_x = i_w AND i_y = i_h THEN ASSIGN c_curr = c_br.

            IF i_x <> 1 AND i_x <> i_w AND (i_y = 1 OR i_y = i_h)  THEN ASSIGN c_curr = c_bh.
            IF (i_x = 1 OR i_x = i_w)  AND i_y <> 1 AND i_y <> i_h THEN ASSIGN c_curr = c_bv.

            IF c_curr > "" THEN
            DO:
                FOR LAST menuBord:
                    ASSIGN i_idx = menuBord.i_idx.
                END.

                CREATE menuBord.
                ASSIGN menuBord.i_idx = i_idx + 1
                       menuBord.i_x   = i_x + i_c
                       menuBord.i_y   = i_y + i_r
                       menuBord.c_val = c_curr.

                h_temp = w_wdc:createFill(h_frame,?,SUBSTITUTE("mb&2_&1",menuBord.i_idx,i_oid),1,1,0,0).
                IF NOT VALID-HANDLE(h_temp) THEN
                DO:
                    setErr(SUBSTITUTE("Failed to create object &1 &2 &3 &4",
                                      menuBord.i_idx,
                                      menuBord.i_x,
                                      menuBord.i_y,
                                      menuBord.c_val)).
                    NEXT.
                END.

                ASSIGN menuBord.c_hnd      = STRING(h_temp)
                       h_temp:COL          = menuBord.i_x
                       h_temp:ROW          = menuBord.i_y
                       h_temp:PFCOLOR      = h_frame:DCOLOR
                       h_temp:SCREEN-VALUE = menuBord.c_val.

                a_obj:pushBack(STRING(h_temp),"HANDLE").
            END.
        END.
    END METHOD.
END CLASS.