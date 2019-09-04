CLASS DMSG:
    DEFINE PUBLIC PROPERTY b_mess  AS BOX       GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_mess  AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY a_mcnt  AS ARRAY     GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY d_box   AS DLOG      GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_box   AS HANDLE    GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY h_frame AS HANDLE    GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY c_msg   AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_dopt  AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_curs  AS CHARACTER GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY i_value AS INTEGER   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_temp AS HANDLE.
    DEFINE PRIVATE VARIABLE c_curr AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_temp AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_int  AS INTEGER.
    DEFINE PRIVATE VARIABLE i_set  AS INTEGER.
    DEFINE PRIVATE VARIABLE i_dim  AS INTEGER.
    DEFINE PRIVATE VARIABLE l_set  AS LOGICAL.

    DEFINE TEMP-TABLE mSet
        FIELD i_idx AS INTEGER
        FIELD c_val AS CHARACTER
        FIELD c_hnd AS CHARACTER.

    CONSTRUCTOR DMSG (INPUT hip_frame AS HANDLE,
                      INPUT cip_msg   AS CHARACTER,
                      INPUT cip_dopt  AS CHARACTER,
                      INPUT cip_cur   AS CHARACTER):

        ASSIGN c_msg   = cip_msg
               c_dopt  = cip_dopt
               c_curs  = cip_cur
               h_frame = IF hip_frame = ? THEN
                             CURRENT-WINDOW
                         ELSE
                            hip_frame.

        makeDialog().
    END CONSTRUCTOR.

    CONSTRUCTOR DMSG (INPUT cip_msg   AS CHARACTER,
                      INPUT cip_dopt  AS CHARACTER,
                      INPUT cip_cur   AS CHARACTER):

        ASSIGN c_msg   = cip_msg
               c_dopt  = cip_dopt
               c_curs  = cip_cur
               h_frame = CURRENT-WINDOW.

        makeDialog().
    END CONSTRUCTOR.

    DESTRUCTOR DMSG ():
        HIDE h_mess.
        HIDE h_box.

        FOR EACH mSet
            BY   mSet.i_idx:

            ASSIGN h_temp = HANDLE(mSet.c_hnd).
            b_mess:w_wdc:remWinObj(h_temp).
        END.

        EMPTY TEMP-TABLE mSet.

        IF VALID-OBJECT(a_mcnt) THEN DELETE OBJECT a_mcnt.
        IF VALID-OBJECT(b_mess) THEN DELETE OBJECT b_mess.
        IF VALID-OBJECT(d_box)  THEN DELETE OBJECT d_box.
    END DESTRUCTOR.

    METHOD PRIVATE VOID makeDialog ():
        makeMessage().

        d_box = NEW DLOG(0,5,INTEGER(NUM-ENTRIES(c_dopt,"|") / 2),c_curs).

        DO i_int = 1 TO NUM-ENTRIES(c_dopt,"|"):
            d_box:addOp(ENTRY(i_int,c_dopt,"|")).
        END.

        d_box:makeOp().
        h_box = d_box:h_box.

        ASSIGN h_box:COL  = 1
               h_box:ROW  = h_frame:HEIGHT-CHARS - h_box:HEIGHT-CHARS + 1
               h_mess:COL = h_box:COL
               h_mess:ROW = h_box:ROW - h_mess:HEIGHT-CHARS
               i_value    = -1
               l_set      = TRUE.
    END METHOD.

    METHOD PRIVATE VOID makeMessage ():
        a_mcnt = NEW ARRAY().
        i_dim  = h_frame:WIDTH-CHARS - 2.

        IF NUM-ENTRIES(c_msg,"|") = 1 THEN
        DO:
            REPEAT:
                IF LENGTH(c_msg) > i_dim THEN
                DO:
                    ASSIGN c_temp = SUBSTRING(c_msg,1,i_dim)
                           c_msg  = SUBSTRING(c_msg,i_dim,LENGTH(c_msg)).

                    a_mcnt:pushBack(c_temp,"CHAR").
                END.
                ELSE LEAVE.
            END.

            a_mcnt:pushBack(c_msg,"CHAR").
        END.
        ELSE
        DO i_int = 1 TO NUM-ENTRIES(c_msg,"|"):
            ASSIGN c_curr = ENTRY(i_int,c_msg,"|").

            REPEAT:
                IF LENGTH(c_curr) > i_dim THEN
                DO:
                    ASSIGN c_temp = SUBSTRING(c_curr,1,i_dim)
                           c_curr = SUBSTRING(c_curr,i_dim,LENGTH(c_curr)).

                    a_mcnt:pushBack(c_temp,"CHAR").
                END.
                ELSE LEAVE.
            END.

            a_mcnt:pushBack(c_curr,"CHAR").
        END.

        DO i_int = 1 TO a_mcnt:LENGTH:
            ASSIGN c_curr = a_mcnt:getIndexChar(i_int)
                   i_set  = MAX(LENGTH(c_curr),i_set).

            CREATE mSet.
            ASSIGN mSet.i_idx = i_int
                   mSet.c_val = c_curr.
        END.

        b_mess = NEW BOX(i_set + 2,a_mcnt:LENGTH + 2,0,0).
        h_mess = b_mess:h_frame.

        IF TEMP-TABLE mSet:HAS-RECORDS AND VALID-OBJECT(a_mcnt) THEN
            DELETE OBJECT a_mcnt.

        FOR EACH mSet
            BY   mSet.i_idx:

            ASSIGN c_curr = mSet.c_val.

            h_temp = b_mess:w_wdc:createFill(h_mess,
                                             ?,
                                             SUBSTITUTE("msg&1_&2",STRING(h_mess),mSet.i_idx),
                                             MAX(LENGTH(c_curr),1),
                                             1,0,0).

            ASSIGN h_temp:SCREEN-VALUE = c_curr
                   h_temp:COL          = 2
                   h_temp:ROW          = 2 + mSet.i_idx - 1
                   h_temp:PFCOLOR      = h_mess:DCOLOR
                   mSet.c_hnd          = STRING(h_temp).
        END.
    END METHOD.

    METHOD PUBLIC INTEGER showDialog():
        IF NOT l_set THEN RETURN i_value.

        VIEW h_mess.
        ASSIGN i_value = d_box:getOp().

        HIDE h_mess.
        HIDE h_box.

        RETURN i_value.
    END METHOD.
END CLASS.