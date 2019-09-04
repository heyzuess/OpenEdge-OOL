CLASS ARRAY:
    DEFINE PUBLIC PROPERTY LENGTH AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_temp    AS HANDLE.
    DEFINE PRIVATE VARIABLE c_valType AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_out     AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_case    AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_set     AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_temp    AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_int     AS INTEGER.
    DEFINE PRIVATE VARIABLE dc_temp   AS DECIMAL.
    DEFINE PRIVATE VARIABLE i_temp    AS INTEGER.
    DEFINE PRIVATE VARIABLE l_temp    AS LOGICAL.
    DEFINE PRIVATE VARIABLE dt_temp   AS DATE.

    DEFINE PRIVATE VARIABLE i_idx     AS INTEGER.
    DEFINE PRIVATE VARIABLE c_val     AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_type    AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_key     AS CHARACTER.

    DEFINE PRIVATE VARIABLE i_err     AS INTEGER.

    METHOD PRIVATE CHARACTER nxtError ():
        ASSIGN i_err = i_err + 1.
        RETURN STRING(SUBSTITUTE("*** ERROR (&1) *** - ",i_err)).
    END METHOD.

    METHOD PRIVATE VOID clrError ():
        ASSIGN i_err = 0.
    END METHOD.

    &SCOPED-DEFINE ERRMSG nxtError()
    &SCOPED-DEFINE CLRERR clrError()

    DEFINE TEMP-TABLE arraySet
        FIELD i_idx  AS INTEGER
        FIELD c_val  AS CHARACTER
        FIELD c_key  AS CHARACTER
        FIELD c_type AS CHARACTER.

    DEFINE TEMP-TABLE arrayParam
        FIELD i_idx       AS INTEGER
        FIELD c_dataType  AS CHARACTER
        FIELD c_argType   AS CHARACTER
        FIELD c_fieldName AS CHARACTER
        FIELD c_data      AS CHARACTER.

    CONSTRUCTOR ARRAY ():
        SUPER().
        ASSIGN c_valType = "CHARACTER,CHAR,INTEGER,INT,DECIMAL,DEC," +
                           "LOGICAL,LOG,WIDGET-HANDLE,HANDLE,DATETIME,DATE".
    END CONSTRUCTOR.

    CONSTRUCTOR ARRAY(INPUT cip_data AS CHARACTER):
        SUPER().
        ASSIGN c_valType = "CHARACTER,CHAR,INTEGER,INT,DECIMAL,DEC," +
                           "LOGICAL,LOG,WIDGET-HANDLE,HANDLE,DATETIME,DATE".

        parseParam(cip_data).
    END CONSTRUCTOR.

    DESTRUCTOR ARRAY ():
        EMPTY TEMP-TABLE arraySet.
    END DESTRUCTOR.

    {standard.i &METHOD_LIBRARY = *}

    METHOD PUBLIC LOG pushBack (INPUT cip_val  AS CHARACTER):

        ASSIGN c_type = dataTypeLit(cip_val).

        IF LOOKUP(c_type,c_valType) = 0 THEN RETURN FALSE.

        CREATE arraySet.
        ASSIGN LENGTH   = LENGTH + 1
               arraySet.i_idx  = LENGTH
               arraySet.c_type = c_type
               arraySet.c_val  = cip_val.

        RETURN AVAILABLE arraySet.
    END METHOD.

    METHOD PUBLIC LOG pushBack (INPUT cip_key  AS CHARACTER,
                                INPUT cip_val  AS CHARACTER):

        ASSIGN c_type = dataTypeLit(cip_val).

        IF LOOKUP(c_type,c_valType) = 0 THEN RETURN FALSE.

        CREATE arraySet.
        ASSIGN LENGTH   = LENGTH + 1
               arraySet.i_idx  = LENGTH
               arraySet.c_key  = cip_key
               arraySet.c_type = c_type
               arraySet.c_val  = cip_val.

        RETURN AVAILABLE arraySet.
    END METHOD.

    METHOD PUBLIC LOG pushBack (INPUT cip_key  AS CHARACTER,
                                INPUT cip_val  AS CHARACTER,
                                INPUT cip_type AS CHARACTER):

        IF LOOKUP(cip_type,c_valType) = 0 THEN RETURN FALSE.

        CREATE arraySet.
        ASSIGN LENGTH   = LENGTH + 1
               arraySet.i_idx  = LENGTH
               arraySet.c_key  = cip_key
               arraySet.c_type = cip_type
               arraySet.c_val  = cip_val.

        RETURN AVAILABLE arraySet.
    END METHOD.

    METHOD PUBLIC LOG popIndex (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN FALSE.

        DELETE arraySet.
        DO i_temp = iip_idx + 1 TO LENGTH:
            FOR FIRST arraySet
                WHERE arraySet.i_idx = i_temp:
                ASSIGN arraySet.i_idx = arraySet.i_idx - 1.
            END.
        END.
        LENGTH = LENGTH - 1.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG popIndex (INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN FALSE.

        ASSIGN i_idx = arraySet.i_idx.

        DELETE arraySet.
        DO i_temp = i_idx + 1 TO LENGTH:
            FOR FIRST arraySet
                WHERE arraySet.i_idx = i_temp:
                ASSIGN arraySet.i_idx = arraySet.i_idx - 1.
            END.
        END.
        LENGTH = LENGTH - 1.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC CHARACTER getDataType (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?.

        RETURN arraySet.c_type.
    END METHOD.

    METHOD PUBLIC CHARACTER getDataType (INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?.

        RETURN arraySet.c_type.
    END METHOD.

    METHOD PUBLIC LOGICAL emptySet ():
        EMPTY  TEMP-TABLE arraySet.
        ASSIGN LENGTH = 0.
        RETURN TEMP-TABLE arraySet:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC LOGICAL forEach (INPUT cip_callback AS CHARACTER):
        /* Callback structure should be similar to forEach function of javascript
           ex:
           DEFINE VARIABLE array AS ARRAY.
           array = NEW ARRAY().
           array.pushBack("Some data","CHAR").
           array.ForEach("(listOfParameters) => ~{ body of callback function ~}").

           Where listOfParameters = Fields from the array table to access
           and
           body of callback function = logic to execute for each array.
         */

        ASSIGN c_temp = ""
               c_out  = ""
               i_int  = 0.

        IF VALID-OBJECT(h_temp) THEN DELETE OBJECT h_temp.

        EMPTY TEMP-TABLE arrayParam.

        ASSIGN c_temp = "./temp_" + STRING(TIME + RANDOM(1,10)) + ".p".

        IF INDEX(cip_callback,"=>") = 0 THEN
        DO:
            MESSAGE {&ERRMSG} "Improper call back structure".
            RETURN FALSE.
        END.

        ASSIGN c_out        = SUBSTRING(cip_callback,1,INDEX(cip_callback,"=>") - 1)
               cip_callback = REPLACE(cip_callback,c_out,"")
               cip_callback = TRIM(REPLACE(cip_callback,"=>",""))
               cip_callback = SUBSTRING(cip_callback,2,LENGTH(cip_callback) - 2)
               c_out        = REPLACE(c_out,"~)","")
               c_out        = TRIM(REPLACE(c_out,"~(",""))
        NO-ERROR.

        IF c_out = "" OR c_out = ? THEN
        DO:
            MESSAGE {&ERRMSG} "no input parameters listed".
            RETURN FALSE.
        END.

        DO i_int = 1 TO NUM-ENTRIES(c_out):
            ASSIGN c_case = TRIM(ENTRY(i_int,c_out)).

            CASE c_case:
                WHEN "i_idx" THEN
                DO:
                    CREATE arrayParam.
                    ASSIGN arrayParam.i_idx       = i_int
                           arrayParam.c_dataType  = "INTEGER"
                           arrayParam.c_argType   = "INPUT-OUTPUT"
                           arrayParam.c_fieldName = c_case.

                    ASSIGN c_set = c_set                            +
                                   "DEFINE INPUT-OUTPUT PARAMETER " +
                                   c_case                           +
                                   " AS INTEGER."                   +
                                   CHR(10).
                END.
                WHEN "c_type" OR
                WHEN "c_val"  OR
                WHEN "c_key"  THEN
                DO:
                    CREATE arrayParam.
                    ASSIGN arrayParam.i_idx       = i_int
                           arrayParam.c_dataType  = "CHARACTER"
                           arrayParam.c_argType   = "INPUT-OUTPUT"
                           arrayParam.c_fieldName = c_case.

                    ASSIGN c_set = c_set                            +
                                   "DEFINE INPUT-OUTPUT PARAMETER " +
                                   c_case                           +
                                   " AS CHARACTER."                 +
                                   CHR(10).
                END.
                OTHERWISE
                    ASSIGN c_set = c_set.
            END CASE.
        END.

        ASSIGN i_int = NUM-ENTRIES(c_out).

        IF c_set = "" THEN
        DO:
            MESSAGE {&ERRMSG} "no input parameters gathered".
            RETURN FALSE.
        END.

        IF NOT TEMP-TABLE arrayParam:HAS-RECORDS THEN
        DO:
            MESSAGE {&ERRMSG} "no parameters found".
            RETURN FALSE.
        END.

        IF i_int = 0 THEN
        DO:
            MESSAGE {&ERRMSG} "No parameters set".
            RETURN FALSE.
        END.
        IF i_int > 4 THEN
        DO:
            MESSAGE {&ERRMSG} "Too many parameters set, expecting 4".
            RETURN FALSE.
        END.

        OUTPUT TO VALUE(c_temp).
            PUT UNFORMATTED c_set        SKIP.
            PUT UNFORMATTED cip_callback SKIP.
        OUTPUT CLOSE.

        IF SEARCH(c_temp) = ? THEN
        DO:
            MESSAGE {&ERRMSG} "Callback not found".
            RETURN FALSE.
        END.

        CREATE CALL h_temp.

        FOR EACH arraySet
            BY   arraySet.i_idx:

            h_temp:CLEAR().

            ASSIGN i_idx  = 0
                   c_type = ""
                   c_val  = "".

            ASSIGN h_temp:CALL-NAME      = c_temp
                   h_temp:CALL-TYPE      = PROCEDURE-CALL-TYPE
                   h_temp:NUM-PARAMETERS = i_int.

            FOR EACH arrayParam
                BY   arrayParam.i_idx:

                CASE arrayParam.c_fieldName:
                    WHEN "i_idx"  THEN
                    DO:
                        ASSIGN i_idx  = arraySet.i_idx  NO-ERROR.

                        h_temp:SET-PARAMETER(arrayParam.i_idx,
                                             arrayParam.c_dataType,
                                             arrayParam.c_argType,
                                             i_idx).
                    END.
                    WHEN "c_type" THEN
                    DO:
                        ASSIGN c_type = arraySet.c_type NO-ERROR.

                        h_temp:SET-PARAMETER(arrayParam.i_idx,
                                             arrayParam.c_dataType,
                                             arrayParam.c_argType,
                                             c_type).
                    END.
                    WHEN "c_val"  THEN
                    DO:
                        ASSIGN c_val  = arraySet.c_val  NO-ERROR.

                        h_temp:SET-PARAMETER(arrayParam.i_idx,
                                             arrayParam.c_dataType,
                                             arrayParam.c_argType,
                                             c_val).
                    END.
                    WHEN "c_key"  THEN
                    DO:
                        ASSIGN c_key = arraySet.c_key   NO-ERROR.

                        h_temp:SET-PARAMETER(arrayParam.i_idx,
                                             arrayParam.c_dataType,
                                             arrayParam.c_argType,
                                             c_key).
                    END.
                END CASE.
            END.

            h_temp:INVOKE NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                IF VALID-OBJECT(h_temp) THEN DELETE OBJECT h_temp.
                OS-DELETE VALUE(c_temp).

                MESSAGE {&ERRMSG} "Callback error encountered in array index" arraySet.i_idx.
                PAUSE.
                HIDE MESSAGE.

                DO i_temp = 1 TO ERROR-STATUS:NUM-MESSAGES:
                    MESSAGE TRIM({&ERRMSG}) + "--->" i_temp " - " ERROR-STATUS:GET-MESSAGE(i_temp).
                    PAUSE.
                    HIDE MESSAGE.
                END.

                RETURN FALSE.
            END.

            FOR EACH arrayParam
                BY   arrayParam.i_idx:

                CASE arrayParam.c_fieldName:
                    WHEN "i_idx"  THEN
                        ASSIGN arraySet.i_idx  = i_idx  NO-ERROR.
                    WHEN "c_type" THEN
                        ASSIGN arraySet.c_type = c_type NO-ERROR.
                    WHEN "c_val"  THEN
                        ASSIGN arraySet.c_val  = c_val  NO-ERROR.
                    WHEN "c_key"  THEN
                        ASSIGN arraySet.c_key  = c_key  NO-ERROR.
                END CASE.
            END.
        END.

        IF VALID-OBJECT(h_temp) THEN DELETE OBJECT h_temp.
        OS-DELETE VALUE(c_temp).
        RETURN TRUE.
    END METHOD.

    METHOD PRIVATE LOGICAL parseParam (INPUT cip_data AS CHARACTER):
        ASSIGN i_int  = 0
               i_temp = 0
               c_temp = ""
               c_type = ""
               c_case = "".

        P-BLK:
        DO i_int = 1 TO NUM-ENTRIES(cip_data):
            ASSIGN c_temp = ENTRY(i_int,cip_data)
            NO-ERROR.

            IF c_temp = "" OR c_temp = ? THEN NEXT P-BLK.

            ASSIGN c_type = "".

            IF NUM-ENTRIES(c_temp,":") > 1 THEN
            DO:
                ASSIGN c_type = ENTRY(1,c_temp,":")
                       c_temp = REPLACE(c_temp,c_type,"")
                       c_temp = SUBSTRING(c_temp,2,LENGTH(c_temp))
                       c_type = TRIM(c_type)
                       c_temp = TRIM(c_temp)
                NO-ERROR.
            END.

            ASSIGN c_case  = ""
                   h_temp  = ?
                   dt_temp = ?
                   dc_temp = 0
                   i_idx   = 0.

            ASSIGN h_temp = HANDLE(c_temp) NO-ERROR.
            IF VALID-HANDLE(h_temp) THEN ASSIGN c_case = "HANDLE".

            IF c_case = ""           AND
               INDEX(c_temp,".") > 0 THEN
            DO:
                ASSIGN dc_temp = DECIMAL(c_temp) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN ASSIGN c_case = "DECIMAL".
            END.

            IF c_case = "" THEN
            DO:
                ASSIGN dt_temp = DATE(c_temp) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN ASSIGN c_case = "DATE".
            END.

            IF c_case = "" THEN
            DO:
                ASSIGN i_idx = INTEGER(c_temp) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN
                    ASSIGN c_case = "INTEGER".
                ELSE
                    ASSIGN c_case = "CHARACTER".
            END.

            CREATE arraySet.
            ASSIGN arraySet.i_idx  = i_int
                   arraySet.c_val  = c_temp
                   arraySet.c_key  = c_type
                   arraySet.c_type = c_case.
        END.

        RETURN TEMP-TABLE arraySet:HAS-RECORDS.
    END METHOD.

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "CHARACTER"
        &METHOD_NAME = "getIndexCHAR"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN arraySet.c_val.
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "CHARACTER"
        &METHOD_NAME = "getIndexCHAR"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN arraySet.c_val.
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "INTEGER"
        &METHOD_NAME = "getIndexINT"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN INT(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "INTEGER"
        &METHOD_NAME = "getIndexINT"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN INT(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "DECIMAL"
        &METHOD_NAME = "getIndexDEC"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN DEC(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "DECIMAL"
        &METHOD_NAME = "getIndexDEC"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN DEC(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "LOGICAL"
        &METHOD_NAME = "getIndexLOG"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN LOGICAL(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "LOGICAL"
        &METHOD_NAME = "getIndexLOG"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN LOGICAL(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "HANDLE"
        &METHOD_NAME = "getIndexHND"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN HANDLE(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "HANDLE"
        &METHOD_NAME = "getIndexHND"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN HANDLE(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "DATE"
        &METHOD_NAME = "getIndexDATE"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN DATE(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}

    {header.i
        &METHOD_LIB  = "METHOD PUBLIC"
        &METHOD_PREF = "DATE"
        &METHOD_NAME = "getIndexDATE"
        &METHOD_POST = " "
        &METHOD_EXT  = "(INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?."}
        RETURN DATE(arraySet.c_val).
    {footer.i &METHOD_END = "END METHOD."}
END CLASS.
