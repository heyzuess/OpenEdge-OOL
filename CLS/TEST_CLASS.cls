CLASS TIMER:
    DEFINE PRIVATE VARIABLE t_tick AS TICK.
    DEFINE PRIVATE VARIABLE h_prog AS HANDLE.
    DEFINE PRIVATE VARIABLE c_prog AS CHARACTER.
    DEFINE PRIVATE VARIABLE l_exit AS LOGICAL.

    CONSTRUCTOR TIMER(INPUT hip_prog AS HANDLE,
                           INPUT cip_prog AS CHARACTER,
                           INPUT iip_fps  AS INTEGER):
        SUPER().
        c_prog = cip_prog.
        t_tick = NEW TICK(iip_fps).
        REPEAT WHILE NOT l_exit:
            IF t_tick:mTick() THEN RUN VALUE(c_prog) IN hip_prog.
        END.
    END CONSTRUCTOR.

    DESTRUCTOR TIMER():
    END DESTRUCTOR.

    METHOD PUBLIC VOID closeTick():
        l_exit = TRUE.
    END METHOD.
END CLASS.
