CLASS LOGOBJ:
    DEFINE PUBLIC PROPERTY c_logfile AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_active  AS LOGICAL   GET. PRIVATE SET.

    CONSTRUCTOR LOGOBJ (INPUT cip_logfile AS CHARACTER):
        ASSIGN c_logfile = cip_logfile
               l_active  = SEARCH(c_logfile) = ?.
    END CONSTRUCTOR.

    DESTRUCTOR LOGOBJ():
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL toggleActive (INPUT lip_toggle AS LOGICAL):
        IF SEARCH(c_logfile) <> ? THEN RETURN FALSE.

        l_active = lip_toggle.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL writeLog (INPUT lip_error AS LOGICAL,
                                    INPUT cip_msg   AS CHARACTER):
        IF NOT l_active THEN RETURN FALSE.

        OUTPUT TO VALUE(c_logfile) APPEND.
            IF OPSYS = "UNIX" THEN OS-COMMAND SILENT VALUE("chmod 666 " + c_logfile).

            IF lip_error THEN PUT UNFORMATTED "*** ERROR *** ".

            PUT UNFORMATTED STRING(TODAY,"99/99/9999") + " "     +
                            STRING(TIME,"HH:MM:SS")    + " --- " +
                            cip_msg SKIP.
        OUTPUT CLOSE.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL remLog ():
        IF SEARCH(c_logfile) = ? THEN RETURN TRUE.

        OS-DELETE VALUE(c_logfile).

        l_active = OS-ERROR = 0.

        RETURN NOT l_active.
    END METHOD.
END CLASS.