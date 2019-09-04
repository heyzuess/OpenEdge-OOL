CLASS CLOCK:
    DEFINE PUBLIC PROPERTY w_wdc     AS WINPROC GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY a_objList AS ARRAY   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY t_tick    AS TICK    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_frame   AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_handle  AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_customW AS LOGICAL GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_customH AS LOGICAL GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_err     AS LOGICAL GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_err     AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_temp AS HANDLE  NO-UNDO.
    DEFINE PRIVATE VARIABLE i_int  AS INTEGER NO-UNDO.

    DEFINE TEMP-TABLE cErr
        FIELD i_idx AS INTEGER
        FIELD c_err AS CHARACTER.

    CONSTRUCTOR CLOCK (INPUT wip_wdc   AS WINPROC,
                       INPUT hip_frame AS HANDLE):

        w_wdc     = wip_wdc.
        a_objList = NEW ARRAY().
        h_frame   = h_frame.
        l_customW = FALSE.
        l_customH = FALSE.

        makeTime().
    END CONSTRUCTOR.

    CONSTRUCTOR CLOCK (INPUT hip_frame AS HANDLE):

        w_wdc     = NEW WINPROC().
        a_objList = NEW ARRAY().
        h_frame   = h_frame.
        l_customW = TRUE.
        l_customH = FALSE.

        makeTime().
    END CONSTRUCTOR.

    CONSTRUCTOR CLOCK ():

        w_wdc     = NEW WINPROC().
        a_objList = NEW ARRAY().
        h_frame   = w_wdc:createFrame("ClockMain","",10,3,0,0,0,FALSE).
        l_customW = TRUE.
        l_customH = TRUE.

        makeTime().
    END CONSTRUCTOR.

    DESTRUCTOR CLOCK ():
        DO i_int = 1 TO a_objList:LENGTH:
            h_temp = a_objList:getIndexHnd(i_int).
            w_wdc:remWinObj(h_temp).
        END.

        IF l_customW THEN
        DO:
            IF VALID-OBJECT(w_wdc) THEN DELETE OBJECT w_wdc.
        END.

        IF VALID-OBJECT(t_tick)    THEN DELETE OBJECT t_tick.
        IF VALID-OBJECT(a_objList) THEN DELETE OBJECT a_objList.

        EMPTY TEMP-TABLE cErr.
    END DESTRUCTOR.

    METHOD PRIVATE LOGICAL makeTime ():
       IF NOT VALID-OBJECT(w_wdc)   THEN
        DO:
            setErr("Invalid WINPROC object - Constructor 3").
            RETURN FALSE.
            //DELETE OBJECT THIS-OBJECT.
        END.

        IF NOT VALID-HANDLE(h_frame) THEN
        DO:
            setErr("Invalid Parent Frame - Constructor 3").
            RETURN FALSE.
            //DELETE OBJECT THIS-OBJECT.
        END.

        t_tick   = NEW TICK(60).
        h_handle = w_wdc:createFill(h_frame,?,"Clock",8,1,0,0).

        IF NOT VALID-HANDLE(h_handle) THEN
        DO:
            setErr("Failed to create fill for clock - Constructor 3").
            RETURN FALSE.
            //DELETE OBJECT THIS-OBJECT.
        END.

        a_objList:pushBack(STRING(h_handle),"HANDLE").
        IF l_customH THEN a_objList:pushBack(STRING(h_frame),"HANDLE").

        h_handle:SENSITIVE    = FALSE.
        h_handle:SCREEN-VALUE = "00:00:00".
        h_handle:SCREEN-VALUE = STRING(TIME,"HH:MM:SS").

        getTime().
    END METHOD.

    METHOD PUBLIC VOID getTime ():
        IF t_tick:mTick() THEN ASSIGN h_handle:SCREEN-VALUE = STRING(TIME,"HH:MM:SS").
    END METHOD.

    METHOD PUBLIC VOID setTime (INPUT dip_time AS DATETIME):
        ASSIGN h_handle:SCREEN-VALUE = STRING(dip_time,"HH:MM:SS").
    END METHOD.

    METHOD PRIVATE LOGICAL setErr (INPUT cip_message AS CHARACTER):
        i_int = 0.
        FOR LAST cErr:
            ASSIGN i_int = cErr.i_idx.
        END.

        CREATE cErr.
        ASSIGN i_err      = i_err + 1
               l_err      = i_err > 0
               cErr.i_idx = i_int + 1
               cErr.c_err = cip_message.

        RETURN l_err.
    END METHOD.

    METHOD PUBLIC CHARACTER getErr (INPUT iip_idx AS INTEGER):
        FOR FIRST cErr
            WHERE cErr.i_idx = iip_idx:
            RETURN cErr.c_err.
        END.
        RETURN "".
    END METHOD.
END CLASS.