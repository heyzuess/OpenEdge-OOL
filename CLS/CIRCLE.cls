CLASS CIRCLE:
    DEFINE TEMP-TABLE cObj
        FIELD i_idx    AS INTEGER
        FIELD d_theta  AS DECIMAL
        FIELD d_x      AS DECIMAL
        FIELD d_y      AS DECIMAL
        FIELD i_x      AS INTEGER
        FIELD i_y      AS INTEGER
        FIELD h_handle AS HANDLE.

    DEFINE PUBLIC PROPERTY w_wdc     AS WINPROC   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_frame   AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_name    AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_charset AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_xcenter AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_ycenter AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_radius  AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY d_stepSze AS DECIMAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE l_ok AS LOGICAL NO-UNDO.

    CONSTRUCTOR CIRCLE (INPUT wip_wdc   AS CLASS WINPROC,
                        INPUT hip_frame AS HANDLE,
                        INPUT cip_name  AS CHARACTER,
                        INPUT cip_value AS CHARACTER,
                        INPUT iip_xc    AS INTEGER,
                        INPUT iip_yc    AS INTEGER,
                        INPUT iip_r     AS INTEGER):

        ASSIGN w_wdc     = wip_wdc
               h_frame   = hip_frame
               c_name    = cip_name
               c_charset = cip_value
               i_xcenter = iip_xc
               i_ycenter = iip_yc
               i_radius  = iip_r.

        IF NOT VALID-OBJECT(w_wdc)     THEN DELETE OBJECT THIS-OBJECT.
        IF NOT VALID-HANDLE(hip_frame) THEN DELETE OBJECT THIS-OBJECT.

        l_ok = initProc().
        IF NOT l_ok THEN DELETE OBJECT THIS-OBJECT.
    END CONSTRUCTOR.

    DESTRUCTOR CIRCLE ():
        FOR EACH cObj:
            IF VALID-HANDLE(cObj.h_handle) THEN
                w_wdc:remWinObj(cObj.h_handle).
        END.
    END DESTRUCTOR.

    {standard.i &METHOD_LIBRARY}

    METHOD PRIVATE LOGICAL initProc ():
        RUN makeCircle.p(INPUT  i_xcenter,
                         INPUT  i_ycenter,
                         INPUT  i_radius,
                         OUTPUT d_stepSze,
                         OUTPUT l_ok,
                         OUTPUT TABLE cObj).

        IF NOT TEMP-TABLE cObj:HAS-RECORDS THEN RETURN FALSE.

        FOR EACH cObj BY cObj.i_idx:
            ASSIGN cObj.h_handle =
                w_wdc:createFill(h_frame,
                                 ?,
                                 SUBSTITUTE("&2_C_&1",cObj.i_idx,c_name),
                                 1,
                                 1,
                                 0,
                                 0).

            IF NOT VALID-HANDLE(cObj.h_handle) THEN
            DO:
                messageCUI(SUBSTITUTE("Failed to create pos &1",cObj.i_idx),"E",FALSE).
                RETURN FALSE.
            END.

            ASSIGN cObj.h_handle:SCREEN-VALUE = c_charset
                   cObj.h_handle:ROW          = cObj.i_x
                   cObj.h_handle:COL          = cObj.i_y.
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL circleObjReg (OUTPUT TABLE cObj BIND):
        RETURN TEMP-TABLE cObj:HAS-RECORDS.
    END METHOD.
END CLASS.