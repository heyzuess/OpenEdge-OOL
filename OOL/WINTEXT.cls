USING OOL.*.
CLASS OOL.WINTEXT INHERITS OOL.WINOBJ:
    DEFINE PUBLIC PROPERTY VALUE        AS CHARACTER GET.
    PUBLIC  SET (cip_value AS CHARACTER):
        ASSIGN THIS-OBJECT:VALUE = cip_value.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:SCREEN-VALUE = THIS-OBJECT:VALUE.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY WIDTH  AS INTEGER   GET. 
    PUBLIC SET (iip_tempWidth AS INTEGER):
        ASSIGN THIS-OBJECT:WIDTH = iip_tempWidth.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:WIDTH-CHARS = THIS-OBJECT:WIDTH
                   THIS-OBJECT:HANDLE:FORMAT      = SUBSTITUTE("X(&1)", THIS-OBJECT:WIDTH).
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE EVENT FOCUS-OFF SIGNATURE VOID (aip_data AS ARRAY).

    CONSTRUCTOR WINTEXT ():
        SUPER().
        THIS-OBJECT:REF-ONLY = TRUE.
    END CONSTRUCTOR.

    CONSTRUCTOR WINTEXT (INPUT oip_parent AS WINOBJ,
                         INPUT cip_name   AS CHARACTER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):
        SUPER().

        ASSIGN THIS-OBJECT:PARENT        = oip_parent
               THIS-OBJECT:NAME          = cip_name
               THIS-OBJECT:WIDTH         = MAX(iip_w, 1)
               THIS-OBJECT:HEIGHT        = MAX(iip_h, 1)
               THIS-OBJECT:X             = MAX(iip_x, 1)
               THIS-OBJECT:Y             = MAX(iip_y, 1).

        THIS-OBJECT:REF-ONLY = TRUE.
        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    DESTRUCTOR WINTEXT ():
    END DESTRUCTOR.

    METHOD PUBLIC OVERRIDE VOID ENABLE ():
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID WAIT ():
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:
        END.

        CREATE TEXT THIS-OBJECT:HANDLE ASSIGN
            NAME         = THIS-OBJECT:NAME
            FRAME        = THIS-OBJECT:PARENT:HANDLE
            WIDTH-CHARS  = THIS-OBJECT:WIDTH
            HEIGHT-CHARS = THIS-OBJECT:HEIGHT
            FORMAT       = SUBSTITUTE("X(&1)", THIS-OBJECT:WIDTH)
            COL          = THIS-OBJECT:X
            ROW          = THIS-OBJECT:Y
            SCREEN-VALUE = THIS-OBJECT:VALUE
            DCOLOR       = THIS-OBJECT:COLOR.

        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).
        
        IF THIS-OBJECT:LIVE                        AND
           VALID-HANDLE(THIS-OBJECT:PARENT:HANDLE) THEN
        DO:
            THIS-OBJECT:PARENT:ADD-CHILD(THIS-OBJECT).
            THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.