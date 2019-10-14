USING OOL.*.
CLASS OOL.testOOL.TEST:
    DEFINE PUBLIC PROPERTY VALUE  AS CHARACTER EXTENT
    GET (INPUT iip_idx AS INTEGER):
        RETURN THIS-OBJECT:VALUE[iip_idx].
    END GET.
    PUBLIC SET (INPUT cip_value AS CHARACTER,
                INPUT iip_idx   AS INTEGER):
        ASSIGN THIS-OBJECT:VALUE[iip_idx] = cip_value.
    END SET.
    DEFINE PUBLIC PROPERTY LENGTH AS INTEGER   GET. PROTECTED SET.

    DEFINE PRIVATE VARIABLE c_temp AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_int  AS INTEGER.
    
    CONSTRUCTOR TEST ():
        SUPER().
    END CONSTRUCTOR.

    CONSTRUCTOR TEST (INPUT cip_value AS CHARACTER EXTENT):
        SUPER().
        THIS-OBJECT:VALUE  = cip_value.
        THIS-OBJECT:LENGTH = EXTENT(cip_value).
    END CONSTRUCTOR.

    DESTRUCTOR TEST ():
    END DESTRUCTOR.

    METHOD PUBLIC OVERRIDE CHARACTER ToString ():
        ASSIGN c_temp = "".
        DO i_int = 1 TO THIS-OBJECT:LENGTH:
            ASSIGN c_temp = c_temp + "[" + THIS-OBJECT:VALUE[i_int] + "]".
        END.

        RETURN c_temp.
    END METHOD.

    METHOD PUBLIC CHARACTER ToString (INPUT iip_idx AS INTEGER):
        RETURN THIS-OBJECT:VALUE[iip_idx].
    END METHOD.
END CLASS.