&IF DEFINED(OS-PROCS) = 0 &THEN
    &IF OPSYS = "WIN32" &THEN
        &GLOBAL-DEFINE OS-DELIM "~\"
        &GLOBAL-DEFINE OS-PWD   cd
    &ELSE
        &GLOBAL-DEFINE OS-DELIM "~/"
        &GLOBAL-DEFINE OS-PWD   pwd
    &ENDIF

    FUNCTION OS-SCRIPT RETURNS CHARACTER (INPUT A AS CHARACTER) FORWARD.

    FUNCTION OS-SCRIPT RETURNS CHARACTER (INPUT cip_script AS CHARACTER):
        DEFINE VARIABLE fcv_errorFile AS CHARACTER.
        DEFINE VARIABLE fcv_errorMsg  AS CHARACTER.
        DEFINE VARIABLE fcv_temp      AS CHARACTER.

        ASSIGN fcv_errorFile = "." + {&OS-DELIM} + STRING(TIME) + "_error.txt"
               cip_script    = cip_script + " 2> " + fcv_errorFile.

        OS-COMMAND SILENT VALUE(cip_script).

        IF SEARCH(fcv_errorFile) = ? THEN RETURN fcv_errorMsg.

        INPUT FROM VALUE(fcv_errorFile).
            REPEAT WHILE TRUE:
                IMPORT UNFORMATTED fcv_temp.
                ASSIGN fcv_errorMsg = fcv_errorMsg + " " + fcv_temp.
            END.
        INPUT CLOSE.

        OS-DELETE VALUE(fcv_errorFile).

        RETURN fcv_errorMsg.
    END FUNCTION.
&ENDIF