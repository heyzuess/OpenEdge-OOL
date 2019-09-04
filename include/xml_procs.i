DEFINE TEMP-TABLE xmlFile
    FIELD i_idx  AS INTEGER
    FIELD c_line AS CHARACTER
    FIELD c_file AS CHARACTER.

DEFINE TEMP-TABLE xmlElement
    FIELD i_idx    AS INTEGER
    FIELD i_line   AS INTEGER
    FIELD i_parent AS INTEGER
    FIELD i_level  AS INTEGER
    FIELD c_tag    AS CHARACTER
    FIELD l_end    AS LOGICAL.

DEFINE TEMP-TABLE xmlAttr
    FIELD i_idx   AS INTEGER
    FIELD i_attr  AS INTEGER
    FIELD c_name  AS CHARACTER
    FIELD c_value AS CHARACTER.

DEFINE TEMP-TABLE xmlValue
    FIELD i_idx   AS INTEGER
    FIELD c_value AS CHARACTER.

DEFINE DATASET xmlDataSet FOR xmlFile, xmlElement, xmlAttr, xmlValue
    DATA-RELATION dr_1 FOR xmlFile, xmlElement
        RELATION-FIELDS (i_idx, i_line) NESTED
    DATA-RELATION dr_2 FOR xmlElement, xmlAttr
        RELATION-FIELDS (i_idx, i_idx)  NESTED
    DATA-RELATION dr_3 FOR xmlElement, xmlValue
        RELATION-FIELDS (i_idx, i_idx)  NESTED.

FUNCTION readXML   RETURNS LOGICAL (INPUT A AS CHARACTER) FORWARD.

FUNCTION xmlToJson RETURNS LOGICAL (INPUT A AS CHARACTER) FORWARD.

FUNCTION readXML RETURNS LOGICAL (INPUT cip_filename AS CHARACTER):
    DEFINE VARIABLE flv_return AS LOGICAL.

    RUN ipReadXML(INPUT cip_filename,OUTPUT flv_return) NO-ERROR.

    RETURN flv_return.
END FUNCTION.

FUNCTION xmlToJson RETURNS LOGICAL (INPUT cip_filename AS CHARACTER):
    RETURN DATASET xmlDataSet:WRITE-JSON("FILE",cip_filename,TRUE).
END FUNCTION.

PROCEDURE ipReadXML:
    DEFINE INPUT  PARAMETER cip_filename AS CHARACTER.
    DEFINE OUTPUT PARAMETER lop_ok       AS LOGICAL.

    DEFINE VARIABLE c_file  AS CHARACTER.
    DEFINE VARIABLE c_curr  AS CHARACTER.
    DEFINE VARIABLE c_temp  AS CHARACTER.
    DEFINE VARIABLE c_val   AS CHARACTER.
    DEFINE VARIABLE i_idx   AS INTEGER.
    DEFINE VARIABLE i_start AS INTEGER.
    DEFINE VARIABLE i_end   AS INTEGER.
    DEFINE VARIABLE i_level AS INTEGER.

    DEFINE BUFFER x_xmlElement FOR xmlElement.
    DEFINE BUFFER y_xmlElement FOR xmlElement.

    ASSIGN c_file = cip_filename.

    EMPTY TEMP-TABLE xmlFile.
    EMPTY TEMP-TABLE xmlElement.
    EMPTY TEMP-TABLE xmlAttr.
    EMPTY TEMP-TABLE xmlValue.

    INPUT FROM VALUE(c_file).
        REPEAT WHILE TRUE:
            CREATE xmlFile.
            ASSIGN i_idx          = i_idx + 1
                   xmlFile.i_idx  = i_idx
                   xmlFile.c_file = c_file.

            IMPORT UNFORMATTED xmlFile.c_line.
        END.
    INPUT CLOSE.

    IF NOT TEMP-TABLE xmlFile:HAS-RECORDS THEN RETURN.

    FOR EACH  xmlFile
        WHERE xmlFile.c_file = c_file:
        ASSIGN xmlFile.c_line = TRIM(xmlFile.c_line).
    END.

    ASSIGN c_val   = ""
           c_temp  = ""
           i_start = 0
           i_end   = 0
           i_level = 0
           i_idx   = 0.

    FOR EACH  xmlFile
        WHERE xmlFile.c_file = c_file:
        ASSIGN c_temp = TRIM(xmlFile.c_line).

        REPEAT:
            ASSIGN i_start = INDEX(c_temp,"<")
                   i_end   = INDEX(c_temp,">").

            IF i_start = 0 OR i_end = 0 THEN LEAVE.

            ASSIGN c_val  = SUBSTRING(c_temp,i_start,i_end)
                   c_temp = SUBSTRING(c_temp,i_end + 1,LENGTH(c_temp)).

            FOR LAST xmlElement:
                ASSIGN i_idx = xmlElement.i_idx.
            END.

            CREATE xmlElement.
            ASSIGN xmlElement.i_idx  = i_idx + 1
                   xmlElement.i_line = xmlFile.i_idx
                   xmlElement.c_tag  = c_val
                   xmlElement.l_end  = INDEX(c_val,"/") > 0.
        END.
    END.

    ASSIGN c_val   = ""
           c_temp  = ""
           i_start = 0
           i_end   = 0
           i_level = 0
           i_idx   = 0.

    FOR EACH xmlElement:
        IF xmlElement.i_idx = 1 THEN NEXT.

        IF NUM-ENTRIES(xmlElement.c_tag," ") > 1 AND
           NUM-ENTRIES(xmlElement.c_tag,"=") > 1 THEN
        DO:
            DO i_level = 2 TO NUM-ENTRIES(xmlElement.c_tag," "):
                ASSIGN c_curr = REPLACE(ENTRY(i_level,xmlElement.c_tag," "),">","")
                       c_temp = ENTRY(1,c_curr,"=")
                       c_val  = REPLACE(ENTRY(2,c_curr,"="),'"',"").

                FOR LAST  xmlAttr
                    WHERE xmlAttr.i_idx = xmlElement.i_idx:

                    ASSIGN i_idx = xmlAttr.i_attr.
                END.

                CREATE xmlAttr.
                ASSIGN xmlAttr.i_idx   = xmlElement.i_idx
                       xmlAttr.i_attr  = i_idx + 1
                       xmlAttr.c_name  = c_temp
                       xmlAttr.c_value = c_val.
            END.

            ASSIGN c_temp = ENTRY(1,xmlElement.c_tag," ") + ">".

            FOR FIRST xmlFile
                WHERE xmlFile.c_file = c_file
                AND   xmlFile.i_idx  = xmlElement.i_line:

                ASSIGN xmlFile.c_line   = REPLACE(xmlFile.c_line,xmlElement.c_tag,c_temp)
                       xmlElement.c_tag = c_temp.
            END.
        END.
    END.

    ASSIGN c_val   = ""
           c_temp  = ""
           i_start = 0
           i_end   = 0
           i_level = 0
           i_idx   = 0.

    FOR EACH xmlElement:
        IF xmlElement.i_idx = 1 THEN NEXT.

        FOR FIRST x_xmlElement
            WHERE REPLACE(x_xmlElement.c_tag,"/","") = xmlElement.c_tag
            AND   x_xmlElement.c_tag                <> xmlElement.c_tag:
        END.
        IF AVAILABLE x_xmlElement THEN
        DO:
            FOR FIRST xmlFile
                WHERE xmlFile.c_file = c_file
                AND   xmlFile.i_idx  = xmlElement.i_line:
            END.

            ASSIGN i_start = INDEX(xmlFile.c_line,xmlElement.c_tag)   + LENGTH(xmlElement.c_tag)
                   i_end   = INDEX(xmlFile.c_line,x_xmlElement.c_tag) - LENGTH(x_xmlElement.c_tag)
                   c_temp  = SUBSTRING(xmlFile.c_line,i_start,i_end).

            IF c_temp > "" THEN
            DO:
                CREATE xmlValue.
                ASSIGN xmlValue.i_idx   = xmlElement.i_idx
                       xmlValue.c_value = c_temp.
            END.

            ASSIGN i_level              = i_level + 1
                   xmlElement.i_level   = i_level
                   x_xmlElement.i_level = i_level.
        END.
        ELSE
            ASSIGN i_level = i_level - 1.
    END.

    ASSIGN c_val   = ""
           c_temp  = ""
           i_start = 0
           i_end   = 0
           i_level = 0
           i_idx   = 0.

    FOR EACH xmlElement
        BY   xmlElement.i_level
        BY   xmlElement.i_idx:

        FOR FIRST y_xmlElement
            WHERE REPLACE(y_xmlElement.c_tag,"/","") =  xmlElement.c_tag
            AND   y_xmlElement.c_tag                 <> xmlElement.c_tag:
        END.
        IF NOT AVAILABLE y_xmlElement THEN NEXT.

        FOR EACH  x_xmlElement
            WHERE x_xmlElement.i_level = xmlElement.i_level + 1
            AND   x_xmlElement.i_idx   > xmlElement.i_idx
            AND   x_xmlElement.i_idx   < y_xmlElement.i_idx:

            ASSIGN x_xmlElement.i_parent = xmlElement.i_idx.
        END.
    END.

    ASSIGN lop_ok = TRUE.
END PROCEDURE.