CLASS TIMER:
    DEFINE PUBLIC PROPERTY t_tick  AS TICK      GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_super AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_prog  AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_prog  AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_fps   AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_exit  AS LOGICAL   GET. PRIVATE SET.

    CONSTRUCTOR TIMER(INPUT hip_prog AS HANDLE,
                      INPUT cip_prog AS CHARACTER,
                      INPUT iip_fps  AS INTEGER):
        SUPER().
        c_prog = cip_prog.
        h_prog = hip_prog.
        i_fps  = iip_fps.
        t_tick = NEW TICK(iip_fps).
    END CONSTRUCTOR.

    DESTRUCTOR TIMER():
    END DESTRUCTOR.

    METHOD PUBLIC VOID startTick():
        RUN TIMER.P PERSISTENT SET h_super (INPUT THIS-OBJECT).
    END METHOD.

    METHOD PUBLIC VOID closeTick():
        l_exit = TRUE.
    END METHOD.

    METHOD PUBLIC VOID timerTrigger():
        RUN VALUE(c_prog) IN h_prog.
    END METHOD.
END CLASS.
