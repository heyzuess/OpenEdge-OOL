USING OOL.*.
USING OOL.WINFORM.*.

DEFINE VARIABLE o_login AS WINLOGIN.

o_login = NEW WINLOGIN().
ASSIGN o_login:WIDTH   = 30
       o_login:HEIGHT  = 10
       o_login:VISIBLE = TRUE
       o_login:BOX     = TRUE.
o_login:REGISTER().
o_login:FOCUS-OFF:SUBSCRIBE("Validate_Form").
o_login:ENABLE-ALL().

PROCEDURE Validate_Form:
    DEFINE INPUT PARAMETER aip_data AS ARRAY.
    DEFINE VARIABLE iv_int AS INTEGER.

    IF VALID-OBJECT(aip_data) THEN
    DO iv_int = 1 TO aip_data:LENGTH:
        DISPLAY iv_int 
                aip_data:INDEX-KEY(iv_int)
                aip_data:INDEX-CHAR(iv_int)
        WITH FRAME A DOWN.
        DOWN WITH FRAME A.
    END.
END PROCEDURE.