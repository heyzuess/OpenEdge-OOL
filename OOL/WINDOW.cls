USING OOL.*.
CLASS OOL.WINDOW INHERITS OOL.WINOBJ:
    DEFINE PUBLIC PROPERTY TITLE        AS CHARACTER GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY MAX-WIDTH    AS INTEGER   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY MAX-HEIGHT   AS INTEGER   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY MESSAGE-AREA AS LOGICAL   GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY STATUS-AREA  AS LOGICAL   GET. PUBLIC SET.

    CONSTRUCTOR WINDOW ():
        SUPER().

    END CONSTRUCTOR.

    CONSTRUCTOR WINDOW (INPUT cip_name  AS CHARACTER,
                        INPUT cip_title AS CHARACTER,
                        INPUT iip_w     AS INTEGER,
                        INPUT iip_h     AS INTEGER,
                        INPUT iip_mW    AS INTEGER,
                        INPUT iip_mH    AS INTEGER,
                        INPUT iip_x     AS INTEGER,
                        INPUT iip_y     AS INTEGER,
                        INPUT lip_mess  AS LOGICAL,
                        INPUT lip_stat  AS LOGICAL):
        SUPER().

        ASSIGN THIS-OBJECT:NAME         = cip_name
               THIS-OBJECT:TITLE        = cip_title
               THIS-OBJECT:WIDTH        = iip_w
               THIS-OBJECT:HEIGHT       = iip_h
               THIS-OBJECT:MAX-WIDTH    = iip_mW
               THIS-OBJECT:MAX-HEIGHT   = iip_mH
               THIS-OBJECT:X            = iip_x
               THIS-OBJECT:Y            = iip_y
               THIS-OBJECT:MESSAGE-AREA = lip_mess
               THIS-OBJECT:STATUS-AREA  = lip_stat.
    END CONSTRUCTOR.

    DESTRUCTOR WINDOW ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID FOCUS ():
        IF THIS-OBJECT:LIVE THEN
            ASSIGN CURRENT-WINDOW = THIS-OBJECT:HANDLE.
    END METHOD.

    METHOD PUBLIC VOID FOCUS (INPUT lip_waitfor AS LOGICAL):
        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN CURRENT-WINDOW = THIS-OBJECT:HANDLE.
            IF lip_waitfor THEN
                WAIT-FOR "WINDOW-CLOSE" OF CURRENT-WINDOW.
        END.
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:

        END.

        CREATE WINDOW THIS-OBJECT:HANDLE ASSIGN
            NAME              = THIS-OBJECT:NAME
            TITLE             = THIS-OBJECT:TITLE
            WIDTH-PIXELS      = THIS-OBJECT:WIDTH
            HEIGHT-PIXELS     = THIS-OBJECT:HEIGHT
            MAX-WIDTH-PIXELS  = THIS-OBJECT:MAX-WIDTH
            MAX-HEIGHT-PIXELS = THIS-OBJECT:MAX-HEIGHT
            X                 = THIS-OBJECT:X
            Y                 = THIS-OBJECT:Y
            MESSAGE-AREA      = THIS-OBJECT:MESSAGE-AREA
            STATUS-AREA       = THIS-OBJECT:STATUS-AREA.

        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).

        IF THIS-OBJECT:LIVE THEN
        DO:
            IF VALID-HANDLE(THIS-OBJECT:PARENT:HANDLE) THEN
            DO:
                THIS-OBJECT:PARENT:ADD-CHILD(THIS-OBJECT:HANDLE).
                THIS-OBJECT:HANDLE:PARENT = THIS-OBJECT:PARENT:HANDLE.
            END.
            
            THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.