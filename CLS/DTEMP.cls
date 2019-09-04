USING Progress.Lang.*.
CLASS DTEMP:
    DEFINE PUBLIC PROPERTY HANDLE     AS HANDLE  GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY BUFFER     AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY NUM-FIELDS AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_header AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_types  AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_name   AS CHARACTER.
    DEFINE PRIVATE VARIABLE i        AS INTEGER.

    DEFINE TEMP-TABLE struct
        FIELD c_name   AS CHARACTER
        FIELD c_header AS CHARACTER
        FIELD c_types  AS CHARACTER.

    CONSTRUCTOR DTEMP(INPUT cip_name   AS CHARACTER,
                      INPUT cip_header AS CHARACTER,
                      INPUT cip_types  AS CHARACTER):
        IF NUM-ENTRIES(cip_header) <> NUM-ENTRIES(cip_types) OR
           NUM-ENTRIES(cip_header) = 0                       OR
           NUM-ENTRIES(cip_types)  = 0 THEN RETURN.

        CREATE TEMP-TABLE HANDLE.
        DO NUM-FIELDS = 1 TO NUM-ENTRIES(cip_header):
            HANDLE:ADD-NEW-FIELD(ENTRY(NUM-FIELDS,cip_header),
                                 ENTRY(NUM-FIELDS,cip_types)).
        END.
        ASSIGN NUM-FIELDS = NUM-FIELDS - 1.
        HANDLE:TEMP-TABLE-PREPARE(cip_name).
        BUFFER = HANDLE:DEFAULT-BUFFER-HANDLE.
        ASSIGN c_name   = cip_name
               c_header = cip_header
               c_types  = cip_types.
    END CONSTRUCTOR.

    CONSTRUCTOR DTEMP(INPUT hip_table AS HANDLE):
        IF NOT VALID-HANDLE(hip_table) THEN RETURN.


        ASSIGN c_name = hip_table:name.
        DO i = 1 TO hip_table:NUM-FIELDS:
            ASSIGN c_header = c_header + hip_table:buffer-field(i):name      + ","
                   c_types  = c_types  + hip_table:buffer-field(i):data-type + ",".
        END.
        ASSIGN c_header = IF c_header > "" THEN SUBSTRING(c_header,1,LENGTH(c_header) - 1) ELSE c_header
               c_types  = IF c_types  > "" THEN SUBSTRING(c_types,1,LENGTH(c_types) - 1)   ELSE c_types.

        CREATE TEMP-TABLE HANDLE.
        DO NUM-FIELDS = 1 TO NUM-ENTRIES(c_header):
            HANDLE:ADD-NEW-FIELD(ENTRY(NUM-FIELDS,c_header),
                                 ENTRY(NUM-FIELDS,c_types)).
        END.
        ASSIGN NUM-FIELDS = NUM-FIELDS - 1.
        HANDLE:TEMP-TABLE-PREPARE(c_name).
        BUFFER = HANDLE:DEFAULT-BUFFER-HANDLE.
    END CONSTRUCTOR.

    CONSTRUCTOR DTEMP(INPUT cip_file AS CHARACTER):
        IF SEARCH(cip_file) = ?     THEN RETURN.

        IF NOT LOAD-TABLE(cip_file) THEN RETURN.

        CREATE TEMP-TABLE HANDLE.
        DO NUM-FIELDS = 1 TO NUM-ENTRIES(c_header):
            HANDLE:ADD-NEW-FIELD(ENTRY(NUM-FIELDS,c_header),
                                 ENTRY(NUM-FIELDS,c_types)).
        END.
        ASSIGN NUM-FIELDS = NUM-FIELDS - 1.
        HANDLE:TEMP-TABLE-PREPARE(c_name).
        BUFFER = HANDLE:DEFAULT-BUFFER-HANDLE.
    END CONSTRUCTOR.

    DESTRUCTOR DTEMP():
        HANDLE:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
    END DESTRUCTOR.

    METHOD PUBLIC LOG NEW-REC ():
        RETURN BUFFER:BUFFER-CREATE().
    END METHOD.

    METHOD PUBLIC LOG INPUT-VAL (INPUT cip_field AS CHARACTER,
                                 INPUT cip_type  AS CHARACTER,
                                 INPUT cip_value AS CHARACTER):
        IF cip_field = "" THEN RETURN FALSE.
        IF cip_value = "" THEN RETURN FALSE.
        IF cip_type  = "" THEN RETURN FALSE.

        CASE cip_type:
            WHEN "Character" OR WHEN "char" THEN
            DO:
                BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = cip_value.
                RETURN BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = cip_value.
            END.
            WHEN "Integer" OR WHEN "int" THEN
            DO:
                BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = INT(cip_value).
                RETURN BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = INT(cip_value).
            END.
            WHEN "Decimal" OR WHEN "Dec" THEN
            DO:
                BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = DEC(cip_value).
                RETURN BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = DEC(cip_value).
            END.
            WHEN "DateTime" OR WHEN "Date" THEN
            DO:
                BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = DATE(cip_value).
                RETURN BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = DATE(cip_value).
            END.
            WHEN "Logical" OR WHEN "Log" THEN
            DO:
                BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = LOGICAL(cip_value).
                RETURN BUFFER:BUFFER-FIELD(cip_field):BUFFER-VALUE = LOGICAL(cip_value).
            END.
            OTHERWISE RETURN FALSE.
        END CASE.
    END METHOD.

    METHOD PUBLIC LOG BACKUP-TABLE (INPUT cip_file AS CHARACTER):
        EMPTY TEMP-TABLE struct.
        CREATE struct.

        ASSIGN struct.c_name   = c_name
               struct.c_header = c_header
               struct.c_types  = c_types.

        RETURN TEMP-TABLE struct:WRITE-JSON("FILE",cip_file,TRUE,?).
    END METHOD.

    METHOD PRIVATE LOG LOAD-TABLE (INPUT cip_file AS CHARACTER):
        IF SEARCH(cip_file) = ? THEN RETURN FALSE.

        EMPTY TEMP-TABLE struct.
        TEMP-TABLE struct:READ-JSON("file",cip_file,"EMPTY").
        FOR FIRST struct:
            ASSIGN c_name   = struct.c_name
                   c_header = struct.c_header
                   c_types  = struct.c_types.
        END.
        RETURN AVAILABLE struct.
    END METHOD.
END CLASS.
