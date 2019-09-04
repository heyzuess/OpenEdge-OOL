CLASS FILEOBJ:
    DEFINE PUBLIC PROPERTY c_dir AS CHARACTER GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY i_err AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_err AS LOGICAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_temp AS INTEGER NO-UNDO.

    {FILEOBJ.i}
    /* -- Reference from include --
    DEFINE TEMP-TABLE tempFile
        FIELD i_idx  AS INTEGER
        FIELD c_file AS CHARACTER
        FIELD l_read AS LOGICAL.

    DEFINE TEMP-TABLE fileRec
        FIELD i_parent AS INTEGER
        FIELD i_idx    AS INTEGER
        FIELD c_value  AS CHARACTER.*/

    DEFINE TEMP-TABLE errMsg
        FIELD i_idx AS INTEGER
        FIELD c_msg AS CHARACTER.

    CONSTRUCTOR FILEOBJ ():
        c_dir = "./".
        readDir().
    END CONSTRUCTOR.

    CONSTRUCTOR FILEOBJ (INPUT cip_dir AS CHARACTER):
        c_dir = cip_dir.
        readDir().
    END CONSTRUCTOR.

    DESTRUCTOR FILEOBJ ():
        EMPTY TEMP-TABLE tempFile.
        EMPTY TEMP-TABLE fileRec.
        EMPTY TEMP-TABLE errMsg.
    END DESTRUCTOR.

    METHOD PRIVATE VOID errMsg (INPUT cip_msg AS CHARACTER):
        FOR LAST errMsg:
            ASSIGN i_temp = errMsg.i_idx.
        END.

        CREATE errMsg.
        ASSIGN i_temp       = i_temp + 1
               errMsg.i_idx = i_temp
               errMsg.c_msg = cip_msg
               i_err        = i_temp
               l_err        = TRUE.
    END METHOD.

    METHOD PUBLIC VOID clrErr ():
        EMPTY TEMP-TABLE errMsg.
        ASSIGN i_err = 0
               l_err = FALSE.
    END METHOD.

    METHOD PUBLIC CHARACTER getErr (INPUT iip_idx AS INTEGER):
        FOR FIRST errMsg
            WHERE errMsg.i_idx = iip_idx:
            RETURN errMsg.c_msg.
        END.
        RETURN "".
    END METHOD.

    METHOD PUBLIC LOGICAL readDir ():
        EMPTY TEMP-TABLE tempFile.
        EMPTY TEMP-TABLE fileRec.
        i_temp = 0.

        IF c_dir = "" THEN
        DO:
            errMsg("Read directory path can not be left blank").
            RETURN FALSE.
        END.

        INPUT FROM OS-DIR(c_dir).
            REPEAT WHILE TRUE:
                CREATE tempFile.
                IMPORT tempFile.c_file.
            END.
        INPUT CLOSE.

        IF NOT TEMP-TABLE tempFile:HAS-RECORDS THEN
        DO:
            errMsg(SUBSTITUTE("Failed to find files in read directory: &1",c_dir)).
            RETURN FALSE.
        END.

        FOR EACH tempFile:
            ASSIGN tempFile.c_file = TRIM(tempFile.c_file).
            IF tempFile.c_file = "" THEN
            DO:
                DELETE tempFile.
                NEXT.
            END.

            ASSIGN i_temp         = i_temp + 1
                   tempFile.i_idx = i_temp.
        END.

        IF NOT TEMP-TABLE tempFile:HAS-RECORDS THEN
        DO:
            errMsg(SUBSTITUTE("No csv files found in read directory: &1",c_dir)).
            RETURN FALSE.
        END.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL readFile (INPUT cip_file AS CHARACTER):
        i_temp = 0.
        IF cip_file = "" THEN
        DO:
            errMsg("Read file is blank").
            RETURN FALSE.
        END.

        FOR FIRST tempFile
            WHERE tempFile.c_file = cip_file:
        END.
        IF NOT AVAILABLE tempFile THEN
        DO:
            errMsg(SUBSTITUTE("File &1 is not in current object list",cip_file)).
            RETURN FALSE.
        END.

        ASSIGN cip_file = c_dir + cip_file.

        IF SEARCH(cip_file) = ? THEN
        DO:
            errMsg(SUBSTITUTE("&1 could not be found",cip_file)).
            RETURN FALSE.
        END.

        INPUT FROM VALUE(cip_file).
            REPEAT WHILE TRUE:
                CREATE fileRec.
                IMPORT UNFORMATTED fileRec.c_value.

                ASSIGN fileRec.i_parent = tempFile.i_idx.
            END.
        INPUT CLOSE.

        FOR FIRST fileRec
            WHERE fileRec.i_parent = tempFile.i_idx:
        END.
        IF NOT AVAILABLE fileRec THEN
        DO:
            errMsg(SUBSTITUTE("No records gathered from file &1",cip_file)).
            RETURN FALSE.
        END.

        FOR EACH  fileRec
            WHERE fileRec.i_parent = tempFile.i_idx:

            IF fileRec.c_val = "" THEN
            DO:
                DELETE fileRec.
                NEXT.
            END.

            ASSIGN i_temp         = i_temp + 1
                   fileRec.i_idx = i_temp.
        END.

        FOR FIRST fileRec
            WHERE fileRec.i_parent = tempFile.i_idx:
        END.
        IF NOT AVAILABLE fileRec THEN
        DO:
            errMsg(SUBSTITUTE("No data gathered from file &1",cip_file)).
            RETURN FALSE.
        END.
    END METHOD.

    METHOD PUBLIC VOID filterList (INPUT cip_type AS CHARACTER):
        IF cip_type = "" THEN RETURN.

        i_temp = 0.
        FOR EACH tempFile BY tempFile.i_idx:
            ASSIGN tempFile.c_file = TRIM(tempFile.c_file).
            IF tempFile.c_file = "" THEN
            DO:
                FOR EACH  fileRec
                    WHERE fileRec.i_parent = tempFile.i_idx:
                    DELETE fileRec.
                END.
                DELETE tempFile.
                NEXT.
            END.

            IF INDEX(tempFile.c_file,cip_type) = 0 THEN
            DO:
                FOR EACH  fileRec
                    WHERE fileRec.i_parent = tempFile.i_idx:
                    DELETE fileRec.
                END.
                DELETE tempFile.
                NEXT.
            END.

            IF SUBSTRING(tempFile.c_file,LENGTH(tempFile.c_file) - (LENGTH(cip_type) - 1), LENGTH(cip_type)) <> cip_type THEN
            DO:
                FOR EACH  fileRec
                    WHERE fileRec.i_parent = tempFile.i_idx:
                    DELETE fileRec.
                END.
                DELETE tempFile.
                NEXT.
            END.

            ASSIGN i_temp         = i_temp + 1.

            FOR EACH  fileRec
                WHERE fileRec.i_parent  = tempFile.i_idx:
                ASSIGN fileRec.i_parent = i_temp.
            END.

            tempFile.i_idx = i_temp.
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL clrTempFile ():
        EMPTY TEMP-TABLE tempFile.
        EMPTY TEMP-TABLE fileRec.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL remTempFile (INPUT iip_idx AS INTEGER):
        FOR FIRST tempFile
            WHERE tempFile.i_idx = iip_idx:
            FOR EACH  fileRec
                WHERE fileRec.i_parent = tempFile.i_idx:
                DELETE fileRec.
            END.

            DELETE tempFile.
        END.
        RETURN AVAILABLE tempFile = FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL fileObjReg (OUTPUT TABLE tempFile BIND,
                                      OUTPUT TABLE fileRec  BIND):
        RETURN TEMP-TABLE tempFile:HAS-RECORDS AND
               TEMP-TABLE fileRec:HAS-RECORDS.
    END METHOD.
END CLASS.
