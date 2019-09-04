USING OOL.*.
CLASS OOL.WINFRAME INHERITS OOL.WINOBJ:
    DEFINE PUBLIC PROPERTY BUFFER-HANDLE AS HANDLE GET. PROTECTED SET.

    DEFINE PUBLIC PROPERTY TITLE  AS CHARACTER GET.
    PUBLIC SET (cip_tempTitle AS CHARACTER):
        ASSIGN THIS-OBJECT:TITLE = cip_tempTitle.

        IF THIS-OBJECT:TITLE > "" AND NOT THIS-OBJECT:BOX THEN ASSIGN THIS-OBJECT:BOX = TRUE.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:TITLE = THIS-OBJECT:TITLE.
        END.
    END SET.
    
    DEFINE PUBLIC PROPERTY BOX    AS LOGICAL   GET. 
    PUBLIC SET (lip_tempBox AS LOGICAL):
        ASSIGN THIS-OBJECT:BOX = lip_tempBox.

        IF THIS-OBJECT:TITLE > "" AND NOT THIS-OBJECT:BOX THEN ASSIGN THIS-OBJECT:TITLE = "".
        
        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:REGISTER().
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE PROPERTY WIDTH AS INTEGER GET.
    PUBLIC SET (iip_tempWidth AS INTEGER):
        ASSIGN THIS-OBJECT:WIDTH = iip_tempWidth.

        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:HANDLE:WIDTH-CHARS = THIS-OBJECT:WIDTH.
        END.
    END SET.

    DEFINE PUBLIC OVERRIDE EVENT FOCUS-OFF SIGNATURE VOID (aip_data AS ARRAY).

    DEFINE PRIVATE VARIABLE o_current AS WINOBJ.
    DEFINE PRIVATE VARIABLE i_current AS INTEGER.

    CONSTRUCTOR WINFRAME ():
        SUPER().
        
    END CONSTRUCTOR.

    CONSTRUCTOR WINFRAME (INPUT cip_name  AS CHARACTER,
                          INPUT cip_title AS CHARACTER,
                          INPUT iip_w     AS INTEGER,
                          INPUT iip_h     AS INTEGER,
                          INPUT iip_x     AS INTEGER,
                          INPUT iip_y     AS INTEGER,
                          INPUT iip_c     AS INTEGER,
                          INPUT lip_box   AS LOGICAL):
        SUPER().

        ASSIGN THIS-OBJECT:NAME   = cip_name
               THIS-OBJECT:WIDTH  = MAX(iip_w, 1)
               THIS-OBJECT:HEIGHT = MAX(iip_h, 1)
               THIS-OBJECT:X      = MAX(iip_x, 1)
               THIS-OBJECT:Y      = MAX(iip_y, 1)
               THIS-OBJECT:COLOR  = iip_c
               THIS-OBJECT:TITLE  = cip_title
               THIS-OBJECT:BOX    = lip_box.

        THIS-OBJECT:REGISTER().
    END CONSTRUCTOR.

    DESTRUCTOR WINFRAME ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID ENABLE-ALL ():
        DO i_current = 1 TO THIS-OBJECT:NUM-CHILDREN:
            o_current = THIS-OBJECT:GET-CHILD(i_current).
            
            IF o_current:REF-ONLY THEN NEXT.

            o_current:ENABLE().
        END.
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID ENABLE ():
    END METHOD.

    METHOD PUBLIC OVERRIDE VOID WAIT ():
    END METHOD.

    METHOD PUBLIC OVERRIDE LOGICAL REGISTER ():
        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:HANDLE:VISIBLE = FALSE.
            ASSIGN THIS-OBJECT:BUFFER-HANDLE = THIS-OBJECT:HANDLE.
        END.

        CREATE FRAME THIS-OBJECT:HANDLE ASSIGN
            NAME         = THIS-OBJECT:NAME
            WIDTH-CHARS  = THIS-OBJECT:WIDTH
            HEIGHT-CHARS = THIS-OBJECT:HEIGHT
            COL          = THIS-OBJECT:X
            ROW          = THIS-OBJECT:Y
            DCOLOR       = THIS-OBJECT:COLOR
            BOX          = THIS-OBJECT:BOX.

        ASSIGN THIS-OBJECT:LIVE = VALID-HANDLE(THIS-OBJECT:HANDLE).

        IF THIS-OBJECT:LIVE THEN
        DO:
            DO i_current = 1 TO THIS-OBJECT:NUM-CHILDREN:
                o_current = THIS-OBJECT:GET-CHILD(i_current).

                DELETE OBJECT o_current:HANDLE.
                ASSIGN o_current:PARENT = THIS-OBJECT.
                o_current:REGISTER().
            END.

            IF VALID-HANDLE(THIS-OBJECT:BUFFER-HANDLE) THEN
            DO:
                DELETE OBJECT THIS-OBJECT:BUFFER-HANDLE.
                ASSIGN THIS-OBJECT:BUFFER-HANDLE = ?.
            END.

            IF THIS-OBJECT:BOX AND THIS-OBJECT:TITLE > "" THEN
                ASSIGN THIS-OBJECT:HANDLE:TITLE = THIS-OBJECT:TITLE.
        
            THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
        END.

        RETURN THIS-OBJECT:LIVE.
    END METHOD.
END CLASS.