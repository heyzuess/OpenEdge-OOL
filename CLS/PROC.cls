CLASS PROC:
    DEFINE PUBLIC PROPERTY PROC-NAME AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY PROC-PATH AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY PROC-LOAD AS LOGICAL   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY PROC-INIT AS LOGICAL   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY PROC-EXTD AS LOGICAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_idx AS INTEGER.

    DEFINE TEMP-TABLE fileData
        FIELD i_idx  AS INTEGER
        FIELD c_data AS CHARACTER.

    CONSTRUCTOR PROC (INPUT cip_program AS CHARACTER):
        SUPER().
        ASSIGN THIS-OBJECT:PROC-NAME = cip_program.

        readProc().
    END CONSTRUCTOR.

    CONSTRUCTOR PROC ():
        SUPER().
        ASSIGN THIS-OBJECT:PROC-NAME = STRING(TIME * RANDOM(1,3)) + "_" + STRING(ETIME) + ".p".
    END CONSTRUCTOR.

    DESTRUCTOR PROC ():
        EMPTY TEMP-TABLE fileData.
        IF SEARCH(PROC-PATH + PROC-NAME) <> ? AND
           NOT THIS-OBJECT:PROC-LOAD          THEN
            OS-DELETE VALUE(PROC-PATH + PROC-NAME).
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL defProc (INPUT cip_string AS CHARACTER):
        IF THIS-OBJECT:PROC-LOAD THEN RETURN FALSE.

        EMPTY TEMP-TABLE fileData.

        CREATE fileData.
        ASSIGN fileData.i_idx  = 1
               fileData.c_data = cip_string.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL addProc (INPUT cip_string AS CHARACTER):
        ASSIGN i_idx = 0.

        FOR LAST fileData:
            ASSIGN i_idx = fileData.i_idx.
        END.

        CREATE fileData.
        ASSIGN fileData.i_idx  = i_idx + 1
               fileData.c_data = cip_string.
        
        IF THIS-OBJECT:PROC-LOAD AND NOT THIS-OBJECT:PROC-EXTD THEN
        DO:
            ASSIGN THIS-OBJECT:PROC-NAME = STRING(TIME * RANDOM(1,3)) + "_" + STRING(ETIME) + ".p"
                   THIS-OBJECT:PROC-EXTD = TRUE.
        END.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC VOID setPath (INPUT cip_string AS CHARACTER):
        ASSIGN THIS-OBJECT:PROC-PATH = cip_string.
    END METHOD.

    METHOD PUBLIC LOGICAL writeProc ( ):
        IF THIS-OBJECT:PROC-INIT THEN RETURN FALSE.

        IF THIS-OBJECT:PROC-PATH = "" THEN ASSIGN THIS-OBJECT:PROC-PATH = "./".

        OUTPUT TO VALUE(THIS-OBJECT:PROC-PATH + THIS-OBJECT:PROC-NAME).
            FOR EACH fileData
                BY   fileData.i_idx:
            
                PUT UNFORMATTED fileData.c_data SKIP.
            END.
        OUTPUT CLOSE.

        ASSIGN THIS-OBJECT:PROC-INIT = SEARCH(THIS-OBJECT:PROC-PATH + THIS-OBJECT:PROC-NAME) <> ?.
    END METHOD.

    METHOD PUBLIC LOGICAL readProc ( ):
        IF THIS-OBJECT:PROC-LOAD THEN RETURN FALSE.

        IF SEARCH(THIS-OBJECT:PROC-NAME) = ? THEN RETURN FALSE.

        FOR LAST fileData:
            ASSIGN i_idx = fileData.i_idx.
        END.

        INPUT FROM VALUE(THIS-OBJECT:PROC-NAME).
            REPEAT WHILE TRUE:
                CREATE fileData.
                ASSIGN i_idx          = i_idx + 1
                       fileData.i_idx = i_idx.

                IMPORT UNFORMATTED fileData.c_data.
            END.
        INPUT CLOSE.

        ASSIGN THIS-OBJECT:PROC-LOAD = TEMP-TABLE fileData:HAS-RECORDS.

        RETURN THIS-OBJECT:PROC-LOAD.
    END METHOD.

    METHOD PUBLIC LOGICAL execProc ( ):
        RUN VALUE(THIS-OBJECT:PROC-PATH + THIS-OBJECT:PROC-NAME) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            DO i_idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
                MESSAGE "Error running" THIS-OBJECT:PROC-PATH + THIS-OBJECT:PROC-NAME SKIP
                        "Error #" i_idx  SKIP
                        "Error message:" SKIP
                        ERROR-STATUS:GET-MESSAGE(i_idx)
                VIEW-AS ALERT-BOX ERROR.
            END.
            RETURN FALSE.
        END.
        RETURN TRUE.
    END METHOD.
END CLASS.