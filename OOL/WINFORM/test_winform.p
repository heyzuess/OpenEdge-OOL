USING OOL.*.
USING OOL.WINFORM.*.

DEFINE VARIABLE x AS WINLOGIN.

x = NEW WINLOGIN().
ASSIGN x:WIDTH   = 30
       x:HEIGHT  = 10
       x:X       = 1
       x:Y       = 1
       x:VISIBLE = TRUE
       x:BOX     = TRUE
       x:COLOR   = 0.
x:REGISTER().
x:FOCUS-OFF:SUBSCRIBE("validate").
x:ENABLE-ALL().

PROCEDURE validate:
    DEFINE INPUT PARAMETER aip_data AS ARRAY.
    DEFINE VARIABLE i AS INT.
    
    IF NOT VALID-OBJECT(aip_data) THEN RETURN.
    
    DO i = 1 TO aip_data:LENGTH:
        DISPLAY aip_data:INDEX-KEY(i) aip_data:INDEX-CHAR(i) WITH FRAME A DOWN.
        DOWN WITH FRAME A.
    END.
END PROCEDURE.
