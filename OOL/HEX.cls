USING OOL.*.
CLASS OOL.HEX:
    DEFINE PRIVATE VARIABLE math        AS MATH.

    DEFINE PRIVATE VARIABLE c_hexVals   AS CHARACTER INIT
    "0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F".

    DEFINE PRIVATE VARIABLE c_temp      AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_temp      AS INTEGER.
    DEFINE PRIVATE VARIABLE i_remainder AS INTEGER.
    DEFINE PRIVATE VARIABLE i_return    AS INTEGER.

    CONSTRUCTOR HEX ():
        SUPER().
        math = NEW MATH().
    END CONSTRUCTOR.

    DESTRUCTOR HEX () :
    END DESTRUCTOR.

    METHOD PUBLIC CHARACTER HexToString (INPUT iip_integer AS INTEGER):
        ASSIGN c_temp      = ""
               i_temp      = 0
               i_remainder = 0.
        
        REPEAT:
            ASSIGN i_temp      = math:Floor(iip_integer / 16)
                   i_remainder = iip_integer MODULO 16
                   c_temp      = IF i_remainder >= 10 THEN
                                    CHR(ASC('A') + i_remainder - 10) + c_temp
                                 ELSE
                                    STRING(i_remainder) + c_temp.

            IF i_temp = 0 THEN LEAVE.

            ASSIGN iip_integer = i_temp.
        END.

        ASSIGN c_temp = "0x" + c_temp.

        RETURN c_temp.
    END METHOD.

    METHOD PUBLIC INTEGER StringToHex (INPUT cip_string AS CHARACTER):
        ASSIGN c_temp      = ""
               i_temp      = 0
               i_remainder = 0
               i_return    = 0.

        IF cip_string BEGINS "0x" THEN
            ASSIGN cip_string = SUBSTRING(cip_string, 3, LENGTH(cip_string)).
        ELSE
        IF cip_string BEGINS "#"  THEN
            ASSIGN cip_string = SUBSTRING(cip_string, 2, LENGTH(cip_string)).
        ELSE
            RETURN ?.

        DO i_temp = 1 TO LENGTH(cip_string):
            ASSIGN c_temp      = SUBSTRING(cip_string, i_temp, 1)
                   i_remainder = IF c_temp >= "A" THEN
                                     LOOKUP(c_temp, c_hexVals) - 1
                                 ELSE
                                     INTEGER(c_temp)
                   i_remainder = IF i_temp = LENGTH(cip_string) THEN
                                     i_remainder
                                 ELSE
                                     EXP(16, LENGTH(cip_string) - i_temp) * i_remainder
                   i_return    = i_return + i_remainder.
        END.

        RETURN i_return.
    END METHOD.
END CLASS.