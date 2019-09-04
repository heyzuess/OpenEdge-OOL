CLASS TICK:
    DEFINE PUBLIC  PROPERTY i_fps    AS DECIMAL   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY l_tick   AS LOGICAL   NO-UNDO GET. PUBLIC  SET.
    DEFINE PRIVATE PROPERTY d_initT  AS DATETIME  NO-UNDO GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY d_currT  AS DATETIME  NO-UNDO GET. PRIVATE SET.

    CONSTRUCTOR TICK():
        SUPER().
        ASSIGN i_fps   = 0
               l_tick  = FALSE
               d_initT = NOW.
    END CONSTRUCTOR.

    CONSTRUCTOR TICK(INPUT iip_fps AS DECIMAL):
        SUPER().
        ASSIGN i_fps   = iip_fps
               l_tick  = FALSE
               d_initT = NOW.
    END CONSTRUCTOR.

    DESTRUCTOR TICK():
    END DESTRUCTOR.

    METHOD PUBLIC LOG mTick():
        IF i_fps = 0 THEN DO:
            ASSIGN d_initT = NOW
                   l_tick  = FALSE.
            RETURN TRUE.
        END.

        ASSIGN d_currT = NOW
               l_tick  = FALSE.
        IF ABS(INTERVAL(d_initT,d_currT,"milliseconds")) > i_fps THEN
        DO:
            ASSIGN d_initT = NOW
                   l_tick  = FALSE.
            RETURN TRUE.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC VOID mWait():
        TICK-BLK: REPEAT: IF mTick() THEN LEAVE TICK-BLK. END.
    END METHOD.
END CLASS.
