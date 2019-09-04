{tables.i
  &TABLE_DETAIL = *
}

FUNCTION getDetail  RETURNS LOGICAL   (INPUT A AS HANDLE)    FORWARD.

FUNCTION compDetail RETURNS CHARACTER (INPUT A AS HANDLE,
                                       INPUT B AS HANDLE)    FORWARD.

FUNCTION getDate    RETURNS CHARACTER (INPUT A AS CHARACTER) FORWARD.

FUNCTION getTime    RETURNS CHARACTER (INPUT A AS CHARACTER) FORWARD.

FUNCTION getDetail RETURNS LOGICAL (INPUT hip_table AS HANDLE):
    DEFINE VARIABLE fiv_int AS INTEGER.
    DEFINE VARIABLE fiv_idx AS INTEGER.
    DEFINE VARIABLE flv_ret AS LOGICAL.

    FOR LAST tableDetail:
        ASSIGN fiv_idx = tableDetail.i_oid.
    END.

    ASSIGN fiv_idx = fiv_idx + 1.

    FOR FIRST tableDetail
        WHERE tableDetail.c_table = STRING(hip_table):
    END.
    IF AVAILABLE tableDetail THEN RETURN FALSE.

    DO fiv_int = 1 TO hip_table:NUM-FIELDS:
        FOR FIRST tableDetail
            WHERE tableDetail.c_table = STRING(hip_table)
            AND   tableDetail.c_name  = hip_table:BUFFER-FIELD(fiv_int):NAME:
        END.
        IF AVAILABLE tableDetail THEN NEXT.

        CREATE tableDetail.
        ASSIGN tableDetail.c_table = STRING(hip_table)
               tableDetail.i_oid   = fiv_idx
               tableDetail.i_idx   = fiv_int
               tableDetail.c_name  = hip_table:NAME
               tableDetail.c_field = hip_table:BUFFER-FIELD(fiv_int):NAME
               tableDetail.c_type  = hip_table:BUFFER-FIELD(fiv_int):DATA-TYPE
               tableDetail.c_value = STRING(hip_table:BUFFER-FIELD(fiv_int):BUFFER-VALUE())
               flv_ret             = TRUE.
    END.

    RETURN flv_ret.
END FUNCTION.

FUNCTION compDetail RETURNS CHARACTER (INPUT hip_tableA AS HANDLE,
                                       INPUT hip_tableB AS HANDLE):
    DEFINE VARIABLE fcv_ret       AS CHARACTER.
    DEFINE VARIABLE fcv_timestamp AS CHARACTER.
    DEFINE VARIABLE fiv_diff      AS INTEGER.
    DEFINE VARIABLE fiv_int       AS INTEGER.

    DEFINE BUFFER x_tableDetail FOR tableDetail.
    DEFINE BUFFER y_tableDetail FOR tableDetail.

    ASSIGN fiv_diff = TIME.

    FOR FIRST x_tableDetail
        WHERE x_tableDetail.c_name = hip_tableA:NAME:
    END.
    IF NOT AVAILABLE x_tableDetail THEN
    DO:
        ASSIGN fcv_ret = SUBSTITUTE("No details have been gathered for table &1",hip_tableA:NAME).
        RETURN fcv_ret.
    END.

    FOR FIRST y_tableDetail
        WHERE y_tableDetail.c_name   = hip_tableB:NAME
        AND   y_tableDetail.c_table <> x_tableDetail.c_table:
    END.
    IF NOT AVAILABLE y_tableDetail THEN
    DO:
        ASSIGN fcv_ret = SUBSTITUTE("No details have been gathered for table &1",hip_tableB:NAME).
        RETURN fcv_ret.
    END.

    FOR EACH  x_tableDetail
        WHERE x_tableDetail.c_name = hip_tableA:NAME:

        FOR FIRST y_tableDetail
            WHERE y_tableDetail.c_name   = hip_tableB:NAME
            AND   y_tableDetail.c_table <> x_tableDetail.c_table
            AND   y_tableDetail.c_field  = x_tableDetail.c_field:
        END.
        IF NOT AVAILABLE y_tableDetail THEN
        DO:
            ASSIGN fcv_ret = SUBSTITUTE("Table &1 does not have a field similiar to &2 in table &3",
                                        y_tableDetail.c_name,x_tableDetail.c_field,x_tableDetail.c_name).

            RETURN fcv_ret.
        END.

        IF y_tableDetail.c_value <> x_tableDetail.c_value THEN
        DO:
            FOR LAST  tableDiff
                WHERE tableDiff.i_diff = fiv_diff
                BY    tableDiff.i_idx:

                ASSIGN fiv_int = tableDiff.i_idx.
            END.

            ASSIGN fcv_timestamp = {timstamp.i TODAY TIME}.

            CREATE tableDiff.
            ASSIGN tableDiff.i_diff   = fiv_diff
                   tableDiff.i_idx    = fiv_int + 1
                   tableDiff.c_date   = getDate(fcv_timestamp)
                   tableDiff.c_time   = getTime(fcv_timestamp)
                   tableDiff.c_tableA = x_tableDetail.c_name
                   tableDiff.c_tableB = y_tableDetail.c_name
                   tableDiff.c_field  = x_tableDetail.c_field
                   tableDiff.c_diff   = SUBSTITUTE("Table A value '&1' does not match Table B value '&2'",
                                                   x_tableDetail.c_value,y_tableDetail.c_value).
        END.
    END.

    RETURN fcv_ret.
END FUNCTION.

FUNCTION getDate    RETURNS CHARACTER (INPUT cip_string AS CHARACTER):
    IF cip_string = "" OR cip_string = ? THEN RETURN cip_string.

    RETURN SUBSTRING(cip_string,5,2) + "/" +
           SUBSTRING(cip_string,7,2) + "/" +
           SUBSTRING(cip_string,1,4).
END FUNCTION.

FUNCTION getTime    RETURNS CHARACTER (INPUT cip_string AS CHARACTER):
    IF cip_string = "" OR cip_string = ? THEN RETURN cip_string.

    RETURN SUBSTRING(cip_string,9,2)  + ":" +
           SUBSTRING(cip_string,11,2) + ":" +
           SUBSTRING(cip_string,13,4).
END FUNCTION.