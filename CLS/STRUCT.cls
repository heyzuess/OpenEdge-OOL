CLASS STRUCT:
    DEFINE PUBLIC  PROPERTY PARENT-PROC  AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC  PROPERTY CLASS-NAME   AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC  PROPERTY CLASS-STRUCT AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC  PROPERTY CLASS-PATH   AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC  PROPERTY WRITE-FILE   AS CHARACTER GET. PRIVATE SET.
    DEFINE PRIVATE PROPERTY CALL-FILE    AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC  PROPERTY VALID-CLASS  AS LOGICAL   GET. PRIVATE SET.

    &SCOPED-DEFINE ERR "*** ERROR ***"

    CONSTRUCTOR STRUCT (INPUT cip_name AS CHARACTER,
                        INPUT cip_data AS CHARACTER):
        SUPER().
        ASSIGN THIS-OBJECT:CLASS-NAME   = cip_name
               THIS-OBJECT:CLASS-STRUCT = cip_data
               THIS-OBJECT:PARENT-PROC  = PROGRAM-NAME(2).
    END CONSTRUCTOR.

    CONSTRUCTOR STRUCT (INPUT cip_name AS CHARACTER):
        SUPER().
        ASSIGN THIS-OBJECT:CLASS-NAME  = cip_name
               THIS-OBJECT:PARENT-PROC = PROGRAM-NAME(2).
    END CONSTRUCTOR.

    CONSTRUCTOR STRUCT ():
        SUPER().
        ASSIGN THIS-OBJECT:PARENT-PROC = PROGRAM-NAME(2).
    END CONSTRUCTOR.

    DESTRUCTOR STRUCT ():
        IF VALID-CLASS THEN
        DO:
            OS-DELETE VALUE(CALL-FILE).
            OS-DELETE VALUE(WRITE-FILE).
        END.
    END DESTRUCTOR.

    METHOD PUBLIC VOID className (INPUT cip_name AS CHARACTER):
        ASSIGN CLASS-NAME = cip_name.
    END METHOD.

    METHOD PUBLIC VOID defStruct (INPUT cip_data AS CHARACTER):
        ASSIGN CLASS-STRUCT = cip_data + " ".
    END METHOD.

    METHOD PUBLIC VOID addStruct (INPUT cip_data AS CHARACTER):
        ASSIGN CLASS-STRUCT = CLASS-STRUCT + cip_data + " ".
    END METHOD.

    METHOD PUBLIC LOGICAL writeStruct (INPUT cip_path AS CHARACTER):
        structOutput(cip_path).
    END METHOD.

    METHOD PUBLIC LOGICAL writeStruct ():
        structOutput("./").
    END METHOD.

    METHOD PRIVATE LOGICAL structOutput (INPUT cip_path AS CHARACTER):
        IF CLASS-NAME = "" OR CLASS-NAME = ? THEN
        DO:
            MESSAGE {&ERR} "CLASS-NAME is blank".
            RETURN FALSE.
        END.

        IF cip_path = "" OR cip_path = ?     THEN
        DO:
            MESSAGE {&ERR} "Path is blank".
            RETURN FALSE.
        END.

        IF SEARCH(cip_path + CLASS-NAME + ".cls") <> ? THEN
        DO:
            MESSAGE {&ERR} "Class already exists." SEARCH(cip_path + CLASS-NAME + ".cls").
            RETURN FALSE.
        END.

        ASSIGN CLASS-STRUCT = "CLASS " + CLASS-NAME + ":" + CHR(10) + CLASS-STRUCT + CHR(10) + "END CLASS."
               CLASS-PATH   = cip_path
               WRITE-FILE   = CLASS-PATH + CLASS-NAME + ".cls"
               CALL-FILE    = CLASS-PATH + STRING(TIME * RANDOM(1,3)) + ".p".

        OUTPUT TO VALUE(CALL-FILE).
            PUT UNFORMATTED
                    "OUTPUT TO VALUE~('" + WRITE-FILE + "'~). " +
                    "PUT UNFORMATTED ~""  +  CLASS-STRUCT + "~" SKIP. " +
                    "OUTPUT CLOSE." SKIP.
        OUTPUT CLOSE.

        IF SEARCH(CALL-FILE) = ? THEN
        DO:
            MESSAGE {&ERR} "Failed to call CALL-FILE".
            RETURN FALSE.
        END.

        RUN VALUE(CALL-FILE).
        OS-DELETE VALUE(CALL-FILE).

        ASSIGN VALID-CLASS = SEARCH(WRITE-FILE) <> ?.

        RETURN VALID-CLASS.
    END METHOD.
END CLASS.