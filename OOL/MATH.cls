USING OOL.*.
CLASS OOL.MATH:
    DEFINE PRIVATE VARIABLE a_temp AS ARRAY.
    DEFINE PRIVATE VARIABLE c_char AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_int  AS INTEGER.
    DEFINE PRIVATE VARIABLE d_sum  AS DECIMAL.

    CONSTRUCTOR MATH ():
    END CONSTRUCTOR.

    DESTRUCTOR MATH ():
    END DESTRUCTOR.

    METHOD PUBLIC INTEGER FLOOR (INPUT dip_decimal AS DECIMAL):
        RETURN INTEGER(ENTRY(1, STRING(dip_decimal), ".")).
    END METHOD.

    METHOD PUBLIC INTEGER FLOOR (INPUT iip_int1 AS INTEGER,
                                 INPUT iip_int2 AS INTEGER):
        RETURN THIS-OBJECT:FLOOR(DECIMAL(iip_int1 / iip_int2)).
    END METHOD.

    METHOD PUBLIC DECIMAL AVERAGE (INPUT dip_int1 AS DECIMAL,
                                   INPUT dip_int2 AS DECIMAL):
        RETURN DECIMAL((dip_int1 + dip_int2) / 2).
    END METHOD.

    METHOD PUBLIC DECIMAL AVERAGE (INPUT aip_array AS ARRAY):
        ASSIGN d_sum = 0.
        IF NOT VALID-OBJECT(aip_array) THEN RETURN 0.

        a_temp = aip_array.
        a_temp:FOR-EACH:SUBSCRIBE(AVERAGE_ARRAY).
        a_temp:FOR-EACH().
        a_temp:FOR-EACH:UNSUBSCRIBE(AVERAGE_ARRAY).

        ASSIGN d_sum = d_sum / a_temp:LENGTH.

        RETURN d_sum.
    END METHOD.

    METHOD PUBLIC DECIMAL AVERAGE (INPUT cip_list AS CHARACTER):
        ASSIGN d_sum = 0.
        DO i_int = 1 TO NUM-ENTRIES(cip_list):
            ASSIGN c_char = TRIM(ENTRY(i_int, cip_list)).
            IF c_char          = "" THEN ASSIGN c_char = "0".
            IF DECIMAL(c_char) = ?  THEN ASSIGN c_char = "0".

            ASSIGN d_sum = d_sum + DECIMAL(c_char) NO-ERROR.
        END.

        ASSIGN d_sum = d_sum / NUM-ENTRIES(cip_list).

        RETURN d_sum.
    END METHOD.

    METHOD PRIVATE VOID AVERAGE_ARRAY ():
        ASSIGN d_sum = d_sum + a_temp:INDEX-INT(a_temp:THIS).
    END METHOD.
END CLASS.