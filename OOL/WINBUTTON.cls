USING OOL.*.
CLASS OOL.WINBUTTON INHERITS OOL.WINOBJ:

    DEFINE PUBLIC PROPERTY LABEL AS CHARACTER GET.
    PUBLIC SET (cip_label AS CHARACTER):
        ASSIGN THIS-OBJECT:LABEL = cip_label.
    
        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:LABEL = THIS-OBJECT:LABEL.
        END.
    END SET.
    
    DEFINE PUBLIC OVERRIDE PROPERTY WIDTH AS INTEGER GET.
    PUBLIC SET (iip_width AS INTEGER):
        ASSIGN THIS-OBJECT:WIDTH = iip_width.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:WIDTH-CHARS = THIS-OBJECT:WIDTH.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY HEIGHT AS INTEGER GET.
    PUBLIC SET (iip_height AS INTEGER):
        ASSIGN THIS-OBJECT:HEIGHT = iip_height.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:HEIGHT-CHARS = THIS-OBJECT:HEIGHT.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE EVENT FOCUS-OFF SIGNATURE VOID (aip_data AS ARRAY).
    
    DEFINE PRIVATE VARIABLE a_tempData AS ARRAY.

    CONSTRUCTOR WINBUTTON ():
        SUPER().

    END CONSTRUCTOR.

    CONSTRUCTOR WINBUTTON (INPUT oip_parent AS WINOBJ, 
                           INPUT cip_label  AS CHARACTER,
                           INPUT iip_x      AS INTEGER,
                           INPUT iip_y      AS INTEGER):
        SUPER().

        ASSIGN THIS-OBJECT:PARENT = oip_parent
               THIS-OBJECT:LABEL  = cip_label
               THIS-OBJECT:WIDTH  = LENGTH(cip_label) + 2
               THIS-OBJECT:HEIGHT = 1
               THIS-OBJECT:X      = MAX(iip_x, 1)
               THIS-OBJECT:Y      = MAX(iip_y, 1).

        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    CONSTRUCTOR WINBUTTON (INPUT oip_parent AS WINOBJ,
                           INPUT cip_label  AS CHARACTER,
                           INPUT iip_w      AS INTEGER,
                           INPUT iip_h      AS INTEGER,
                           INPUT iip_x      AS INTEGER,
                           INPUT iip_y      AS INTEGER):
        SUPER().

        ASSIGN THIS-OBJECT:PARENT = oip_parent
               THIS-OBJECT:LABEL  = cip_label
               THIS-OBJECT:WIDTH  = iip_w
               THIS-OBJECT:HEIGHT = iip_h
               THIS-OBJECT:X      = MAX(iip_x, 1)
               THIS-OBJECT:Y      = MAX(iip_y, 1).

        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    DESTRUCTOR WINBUTTON ():
    END DESTRUCTOR.

    METHOD PUBLIC OVERRIDE VOID ENABLE ():
        IF NOT THIS-OBJECT:LIVE THEN RETURN.

        IF NOT THIS-OBJECT:VISIBLE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:VISIBLE = TRUE.
        END.

        IF NOT THIS-OBJECT:SENSITIVE THEN
            ASSIGN THIS-OBJECT:HANDLE:SENSITIVE = TRUE.

        PUT CURSOR OFF.
        THIS-OBJECT:WAIT().

        a_tempData = NEW ARRAY().
        a_tempData:PUSH-BACK(THIS-OBJECT:NAME, THIS-OBJECT:LAST-KEY).
        THIS-OBJECT:FOCUS-OFF:PUBLISH(a_tempData).

        IF NOT THIS-OBJECT:SENSITIVE THEN
            ASSIGN THIS-OBJECT:HANDLE:SENSITIVE = FALSE.

        IF NOT THIS-OBJECT:VISIBLE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:VISIBLE = FALSE.
        END.
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID WAIT ():
        /* THIS-OBJECT:HANDLE causes an issue here for some reason */
        WAIT-FOR "ANY-KEY" OF HANDLE FOCUS HANDLE.
        ASSIGN THIS-OBJECT:LAST-KEY = IF LASTKEY = 32 THEN "SPACEBAR" ELSE KEYLABEL(LASTKEY).
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:

        END.

        CREATE BUTTON THIS-OBJECT:HANDLE ASSIGN
            FRAME        = THIS-OBJECT:PARENT:HANDLE
            LABEL        = THIS-OBJECT:LABEL
            WIDTH-CHARS  = THIS-OBJECT:WIDTH
            HEIGHT-CHARS = THIS-OBJECT:HEIGHT
            COL          = THIS-OBJECT:X
            ROW          = THIS-OBJECT:Y
            SENSITIVE    = THIS-OBJECT:SENSITIVE
            DCOLOR       = THIS-OBJECT:COLOR
            VISIBLE      = THIS-OBJECT:VISIBLE
            NAME         = THIS-OBJECT:NAME.
            
        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).

        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:PARENT:ADD-CHILD(THIS-OBJECT).
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.