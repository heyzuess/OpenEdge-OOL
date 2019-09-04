CLASS DIROBJ:
    DEFINE PUBLIC PROPERTY c_dir   AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_type  AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_files AS INTEGER   GET. PRIVATE SET.

    CONSTRUCTOR DIROBJ (INPUT cip_dir  AS CHARACTER,
                        INPUT cip_type AS CHARACTER):
        ASSIGN c_dir  = cip_dir
               c_type = cip_type.

        makeObj().
    END CONSTRUCTOR.

    CONSTRUCTOR DIROBJ (INPUT cip_dir  AS CHARACTER):
        ASSIGN c_dir  = cip_dir
               c_type = "".

        makeObj().
    END CONSTRUCTOR.

    CONSTRUCTOR DIROBJ ():
        ASSIGN c_dir  = "./"
               c_type = "".

        makeObj().
    END CONSTRUCTOR.

    {standard.i &METHOD_LIBRARY = *
                &DIR_OBJ        = *
                &DIR_CUST       = *}

    {errlib.i   &CLASS_LIBRARY  = *}

    DESTRUCTOR DIROBJ ():
        EMPTY TEMP-TABLE fileDir.
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL makeObj ():
        INPUT FROM OS-DIR(c_dir).
            REPEAT WHILE TRUE:
                CREATE fileDir.
                IMPORT fileDir.c_name fileDir.c_full fileDir.c_type.

                ASSIGN i_files       = i_files + 1
                       fileDir.i_idx = i_files.
            END.
        INPUT CLOSE.

        IF NOT TEMP-TABLE fileDir:HAS-RECORDS THEN
        DO:
            setErr("No records found for directory: " + c_dir).
            RETURN FALSE.
        END.

        ASSIGN i_files = 0.
        FOR EACH fileDir
            BY   fileDir.i_idx:

            ASSIGN FILE-INFO:FILE-NAME = fileDir.c_name.

            IF SUBSTRING(FILE-INFO:FILE-TYPE,1,1) = "D" OR
               fileDir.c_type                     = "D" OR
               fileDir.c_name                     = ""  THEN
            DO:
                DELETE fileDir.
                NEXT.
            END.

            IF c_type > "" THEN
            DO:
                IF INDEX(fileDir.c_name,c_type) = 0 THEN
                DO:
                    DELETE fileDir.
                    NEXT.
                END.

                IF SUBSTRING(fileDir.c_name,
                             LENGTH(fileDir.c_name) - (LENGTH(c_type) - 1),
                             LENGTH(c_type)) <> c_type THEN
                DO:
                    DELETE fileDir.
                    NEXT.
                END.
            END.

            ASSIGN i_files       = i_files + 1
                   fileDir.i_idx = i_files.
        END.

        IF NOT TEMP-TABLE fileDir:HAS-RECORDS THEN
        DO:
            setErr("No records found after cleanup in directory: " + c_dir).
            RETURN FALSE.
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL dirObjReg (OUTPUT TABLE fileDir BIND):
        RETURN TEMP-TABLE fileDir:HAS-RECORDS.
    END METHOD.
END CLASS.