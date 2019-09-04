Using Progress.Lang.*.
CLASS TEMPDATA:
    DEFINE PUBLIC PROPERTY NAME      AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY SET-TABLE AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY NUM-ROWS  AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY CUR-ROW   AS INTEGER   GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY ISERROR   AS LOGICAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_currT   AS DTEMP.
    DEFINE PRIVATE VARIABLE h_query   AS HANDLE    NO-UNDO.
    DEFINE PRIVATE VARIABLE c_headerL AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_typesL  AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_valsL   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE c_file    AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_results AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_idx     AS INTEGER   NO-UNDO.

    DEFINE TEMP-TABLE MainTable
        FIELD c_name   AS CHARACTER
        FIELD i_numF   AS INTEGER.

    DEFINE TEMP-TABLE TableField
        FIELD c_table  AS CHARACTER
        FIELD c_name   AS CHARACTER
        FIELD c_type   AS CHARACTER
        FIELD c_value  AS CHARACTER.

    DEFINE TEMP-TABLE ResultSet LIKE TableField
        FIELD c_search AS CHARACTER
        FIELD i_idx    AS INTEGER.

    DEFINE TEMP-TABLE errorLog
        FIELD i_idx AS INTEGER
        FIELD c_val AS CHARACTER.

    CONSTRUCTOR TEMPDATA(INPUT cip_table AS CHARACTER):
        IF cip_table = "" THEN
        DO:
            LOG-ERROR("Table name can not be left blank").
            RETURN.
        END.

        FIND FIRST MainTable
             WHERE MainTable.c_name = cip_table
             NO-LOCK NO-ERROR.
        IF AVAILABLE MainTable THEN
        DO:
            LOG-ERROR(SUBSTITUTE("&1 has already been loaded into memory.",cip_table)).
            RETURN.
        END.

        ASSIGN c_file = SUBSTITUTE("./users/&1_struct.json",REPLACE(cip_table," ","_")).
        IF SEARCH(c_file) = ? THEN
        DO:
            LOG-ERROR(SUBSTITUTE("&1 Table structure is missing.",cip_table)).
            RETURN.
        END.

        TEMP-TABLE TableField:READ-JSON("FILE",c_file,"APPEND").

        IF NOT TEMP-TABLE TableField:HAS-RECORDS THEN
        DO:
            LOG-ERROR(SUBSTITUTE("Failed to grab data set for table &1",cip_table)).
            RETURN.
        END.

        CREATE MainTable.
        ASSIGN MainTable.c_name = cip_table.

        FOR EACH  TableField
            WHERE TableField.c_table = MainTable.c_name:
            ASSIGN MainTable.i_numF = MainTable.i_numF + 1
                   c_headerL        = c_headerL + TableField.c_name  + ","
                   c_typesL         = c_typesL  + TableField.c_type  + ","
                   c_valsL          = c_valsL   + TableField.c_value + ",".
        END.
        ASSIGN c_headerL = SUBSTRING(c_headerL,1,LENGTH(c_headerL) - 1)
               c_typesL  = SUBSTRING(c_typesL,1,LENGTH(c_typesL) - 1)
               c_valsL   = SUBSTRING(c_valsL,1,LENGTH(c_valsL) - 1).

        h_currT = NEW DTEMP(cip_table,c_headerL,c_typesL).
        IF NOT VALID-OBJECT(h_currT) THEN
        DO:
            LOG-ERROR(SUBSTITUTE("Failed to load &1 into local memory.",cip_table)).
            RETURN.
        END.

        NAME      = cip_table.
        SET-TABLE = h_currT:BUFFER.
    END CONSTRUCTOR.

    DESTRUCTOR  TEMPDATA():
        EMPTY TEMP-TABLE MainTable.
        EMPTY TEMP-TABLE TableField.
        EMPTY TEMP-TABLE ResultSet.
    END DESTRUCTOR.

    METHOD PRIVATE VOID LOG-ERROR (INPUT cip_message AS CHARACTER):
        ISERROR = TRUE.

        FOR LAST errorLog:
            ASSIGN i_idx = errorLog.i_idx.
        END.

        CREATE errorLog.
        ASSIGN errorLog.i_idx = i_idx + 1
               errorLog.c_val = cip_message.
    END METHOD.

    METHOD PUBLIC CHAR GET-MESSAGE (INPUT iip_num AS INTEGER):
        FOR FIRST errorLog
            WHERE errorLog.i_idx = iip_num:
            RETURN errorLog.c_val.
        END.
        RETURN "".
    END METHOD.

    METHOD PUBLIC LOG SET-QUERY (INPUT cip_search AS CHARACTER):
        IF TEMP-TABLE ResultSet:HAS-RECORDS THEN
        DO:
            LOG-ERROR("Current query has not been cleared.").
            RETURN FALSE.
        END.

        CREATE QUERY h_query.
        h_query:SET-BUFFERS(h_currT:BUFFER).
        h_query:QUERY-PREPARE(cip_search).
        h_query:QUERY-OPEN().
        REPEAT:
            h_query:GET-NEXT().
            IF h_query:QUERY-OFF-END THEN LEAVE.

            ASSIGN i_results = i_results + 1.
            FOR EACH TableField:
                CREATE ResultSet.
                BUFFER-COPY TableField TO ResultSet.
                ASSIGN ResultSet.i_idx    = i_results
                       ResultSet.c_search = cip_search
                       ResultSet.c_table  = TableField.c_table
                       ResultSet.c_name   = TableField.c_name
                       ResultSet.c_type   = TableField.c_type
                       ResultSet.c_value  = STRING(h_currT:BUFFER:BUFFER-FIELD(TableField.c_name):BUFFER-VALUE()).
            END.
        END.
        h_query:QUERY-CLOSE().

        NUM-ROWS = i_results.
        RETURN TEMP-TABLE ResultSet:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC LOG CLEAR-QUERY ():
        IF NOT VALID-HANDLE(h_query) THEN RETURN FALSE.
        h_query = ?.
        RETURN VALID-HANDLE(h_query).
    END METHOD.

    METHOD PUBLIC CHAR GET-VAL (INPUT cip_field AS CHARACTER,
                                INPUT cip_value AS CHARACTER):
        FIND FIRST ResultSet
             WHERE ResultSet.c_name = cip_field
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ResultSet THEN RETURN "".

        CASE cip_value:
            WHEN "DATATYPE" THEN
            DO:
                RETURN ResultSet.c_type.
            END.
            WHEN "VALUE"    THEN
            DO:
                RETURN ResultSet.c_value.
            END.
            OTHERWISE RETURN "".
        END CASE.
    END METHOD.
END CLASS.
