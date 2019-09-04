USING Progress.Lang.OBJECT.
USING OOL.*.
CLASS OOL.ARRAY:
    DEFINE PUBLIC PROPERTY LENGTH AS INTEGER GET. 
    PRIVATE SET (iip_length AS INTEGER):
        ASSIGN THIS-OBJECT:LENGTH = iip_length
               THIS-OBJECT:THIS   = THIS-OBJECT:LENGTH.
    END SET.
    
    DEFINE PUBLIC PROPERTY THIS   AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE lc_long AS LONGCHAR.

    DEFINE TEMP-TABLE arraySet NO-UNDO
        FIELD i_idx  AS INTEGER
        FIELD c_key  AS CHARACTER
        FIELD c_val  AS CHARACTER
        FIELD c_type AS CHARACTER.

    DEFINE TEMP-TABLE arrayComplex NO-UNDO
        FIELD i_idx    AS INTEGER
        FIELD c_type   AS CHARACTER
        FIELD c_clob   AS CLOB
        FIELD o_object AS OBJECT.

    DEFINE PUBLIC EVENT FOR-EACH SIGNATURE VOID ().

    CONSTRUCTOR ARRAY ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR ARRAY ():
    END DESTRUCTOR.

    METHOD PUBLIC VOID FOR-EACH ():
        FOR EACH arraySet
            BY   arraySet.i_idx:
            
            THIS-OBJECT:THIS = arraySet.i_idx.
            THIS-OBJECT:FOR-EACH:PUBLISH().
        END.
    END METHOD.

    METHOD PRIVATE VOID ADD-SET(INPUT cip_val  AS CHARACTER,
                                INPUT cip_type AS CHARACTER):

        THIS-OBJECT:LENGTH = THIS-OBJECT:LENGTH + 1.

        CREATE arraySet.
        ASSIGN arraySet.i_idx  = THIS-OBJECT:LENGTH
               arraySet.c_val  = cip_val
               arraySet.c_type = cip_type.
    END METHOD.

    METHOD PRIVATE VOID ADD-SET(INPUT cip_key  AS CHARACTER,
                                INPUT cip_val  AS CHARACTER,
                                INPUT cip_type AS CHARACTER):
        THIS-OBJECT:LENGTH = THIS-OBJECT:LENGTH + 1.

        CREATE arraySet.
        ASSIGN arraySet.i_idx  = THIS-OBJECT:LENGTH
               arraySet.c_val  = cip_val
               arraySet.c_key  = cip_key
               arraySet.c_type = cip_type.
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_val  AS CHARACTER):
        THIS-OBJECT:ADD-SET(cip_val, "CHARACTER").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key  AS CHARACTER,
                                  INPUT cip_val  AS CHARACTER):
        THIS-OBJECT:ADD-SET(cip_key, cip_val, "CHARACTER").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT iip_val  AS INTEGER):
        THIS-OBJECT:ADD-SET(STRING(iip_val), "INTEGER").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key AS CHARACTER,
                                  INPUT iip_val AS INTEGER):
        THIS-OBJECT:ADD-SET(cip_key, STRING(iip_val), "INTEGER").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT dip_val  AS DECIMAL):
        THIS-OBJECT:ADD-SET(STRING(dip_val), "DECIMAL").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key AS CHARACTER,
                                  INPUT dip_val AS DECIMAL):
        THIS-OBJECT:ADD-SET(cip_key, STRING(dip_val), "DECIMAL").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT dip_val AS DATE):
        THIS-OBJECT:ADD-SET(STRING(dip_val), "DATE").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key AS CHARACTER,
                                  INPUT dip_val AS DATE):
        THIS-OBJECT:ADD-SET(cip_key, STRING(dip_val), "DATE").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT lip_val  AS LOGICAL):
        THIS-OBJECT:ADD-SET(STRING(lip_val), "LOGICAL").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key AS CHARACTER,
                                  INPUT lip_val AS LOGICAL):
        THIS-OBJECT:ADD-SET(cip_key, STRING(lip_val), "LOGICAL").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT hip_val  AS HANDLE):
        THIS-OBJECT:ADD-SET(STRING(hip_val), "HANDLE").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key AS CHARACTER,
                                  INPUT hip_val AS HANDLE):
        THIS-OBJECT:ADD-SET(cip_key, STRING(hip_val), "HANDLE").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT iip_val  AS INT64):
        THIS-OBJECT:ADD-SET(STRING(iip_val), "INT64").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT cip_key AS CHARACTER,
                                  INPUT iip_val AS INT64):
        THIS-OBJECT:ADD-SET(cip_key, STRING(iip_val), "INT64").
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT oip_val  AS OBJECT):
        THIS-OBJECT:ADD-SET("", "COMPLEX").

        CREATE arrayComplex.
        ASSIGN arrayComplex.i_idx    = THIS-OBJECT:LENGTH
               arrayComplex.o_object = oip_val
               arrayComplex.c_type   = "OBJECT".
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT oip_winobj AS WINOBJ):
        THIS-OBJECT:ADD-SET("", "COMPLEX").

        CREATE arrayComplex.
        ASSIGN arrayComplex.i_idx    = THIS-OBJECT:LENGTH
               arrayComplex.c_type   = "WINOBJ"
               arrayComplex.o_object = CAST(oip_winobj, OBJECT).
    END METHOD.

    METHOD PUBLIC VOID PUSH-BACK (INPUT lcip_val AS LONGCHAR):
        THIS-OBJECT:ADD-SET("", "COMPLEX").

        CREATE arrayComplex.
        ASSIGN arrayComplex.i_idx  = THIS-OBJECT:LENGTH
               arrayComplex.c_type = "LONGCHAR".

        COPY-LOB lcip_val TO arrayComplex.c_clob.
    END METHOD.

    METHOD PRIVATE VOID DEL-SET (BUFFER x FOR arraySet):
        IF NOT AVAILABLE x THEN RETURN.

        IF x.c_type = "COMPLEX" THEN
        FOR FIRST arrayComplex
            WHERE arrayComplex.i_idx = x.i_idx:
        
            DELETE arrayComplex.
        END.

        DELETE x.

        THIS-OBJECT:LENGTH = 0.
        FOR EACH  arraySet
            BY    arraySet.i_idx:
            
            ASSIGN THIS-OBJECT:LENGTH = THIS-OBJECT:LENGTH + 1
                   arraySet.i_idx     = THIS-OBJECT:LENGTH.
        END.
    END METHOD.

    METHOD PUBLIC VOID POP-INDEX (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN.

        THIS-OBJECT:DEL-SET(BUFFER arraySet).
    END METHOD.

    METHOD PUBLIC VOID POP-INDEX (INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN.

        THIS-OBJECT:DEL-SET(BUFFER arraySet).
    END METHOD.

    METHOD PUBLIC CHARACTER DATA-TYPE (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        
            IF arraySet.c_type = "COMPLEX" THEN
            FOR FIRST arrayComplex
                WHERE arrayComplex.i_idx = arraySet.i_idx:
            
                RETURN arrayComplex.c_type.
            END.
            ELSE
                RETURN arraySet.c_type.
        END.
        RETURN "".
    END METHOD.

    METHOD PUBLIC CHARACTER DATA-TYPE (INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
        
            IF arraySet.c_type = "COMPLEX" THEN
            FOR FIRST arrayComplex
                WHERE arrayComplex.i_idx = arraySet.i_idx:
            
                RETURN arrayComplex.c_type.
            END.
            ELSE
                RETURN arraySet.c_type.
        END.
        RETURN "".
    END METHOD.
    
    METHOD PUBLIC VOID EMPTY ():
        EMPTY TEMP-TABLE arraySet.
        EMPTY TEMP-TABLE arrayComplex.
        ASSIGN THIS-OBJECT:LENGTH = 0.
    END METHOD.

    METHOD PRIVATE CHARACTER INDEX-VAL (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
            
            RETURN arraySet.c_val.
        END.
        RETURN "".
    END METHOD.

    METHOD PRIVATE CHARACTER INDEX-VAL (INPUT cip_key AS CHARACTER):
        FOR FIRST arraySet
            WHERE arraySet.c_key = cip_key:
            RETURN arraySet.c_val.
        END.
        RETURN "".
    END METHOD.

    METHOD PUBLIC CHARACTER INDEX-CHAR (INPUT iip_idx AS INTEGER):
        RETURN THIS-OBJECT:INDEX-VAL(iip_idx).
    END METHOD.

    METHOD PUBLIC CHARACTER INDEX-CHAR (INPUT cip_key AS CHARACTER):
        RETURN THIS-OBJECT:INDEX-VAL(cip_key).
    END METHOD.

    METHOD PUBLIC INTEGER INDEX-INT (INPUT iip_idx AS INTEGER):
        RETURN INTEGER(THIS-OBJECT:INDEX-VAL(iip_idx)).
    END METHOD.

    METHOD PUBLIC INTEGER INDEX-INT (INPUT cip_key AS CHARACTER):
        RETURN INTEGER(THIS-OBJECT:INDEX-VAL(cip_key)).
    END METHOD.

    METHOD PUBLIC DECIMAL INDEX-DEC (INPUT iip_idx AS INTEGER):
        RETURN DECIMAL(THIS-OBJECT:INDEX-VAL(iip_idx)).
    END METHOD.

    METHOD PUBLIC DECIMAL INDEX-DEC (INPUT cip_key AS CHARACTER):
        RETURN DECIMAL(THIS-OBJECT:INDEX-VAL(cip_key)).
    END METHOD.

    METHOD PUBLIC DATE    INDEX-DATE (INPUT iip_idx AS INTEGER):
        RETURN DATE(THIS-OBJECT:INDEX-VAL(iip_idx)).
    END METHOD.

    METHOD PUBLIC DATE    INDEX-DATE (INPUT cip_key AS CHARACTER):
        RETURN DATE(THIS-OBJECT:INDEX-VAL(cip_key)).
    END METHOD.

    METHOD PUBLIC LOGICAL INDEX-LOG (INPUT iip_idx AS INTEGER):
        RETURN LOGICAL(THIS-OBJECT:INDEX-VAL(iip_idx)).
    END METHOD.

    METHOD PUBLIC LOGICAL INDEX-LOG (INPUT cip_key AS CHARACTER):
        RETURN LOGICAL(THIS-OBJECT:INDEX-VAL(cip_key)).
    END METHOD.

    METHOD PUBLIC HANDLE INDEX-HANDLE (INPUT iip_idx AS INTEGER):
        RETURN HANDLE(THIS-OBJECT:INDEX-VAL(iip_idx)).
    END METHOD.

    METHOD PUBLIC HANDLE INDEX-HANDLE (INPUT cip_key AS CHARACTER):
        RETURN HANDLE(THIS-OBJECT:INDEX-VAL(cip_key)).
    END METHOD.

    METHOD PUBLIC INT64 INDEX-INT64 (INPUT iip_idx AS INTEGER):
        RETURN INT64(THIS-OBJECT:INDEX-VAL(iip_idx)).
    END METHOD.

    METHOD PUBLIC INT64 INDEX-INT64 (INPUT cip_key AS CHARACTER):
        RETURN INT64(THIS-OBJECT:INDEX-VAL(cip_key)).
    END METHOD.

    METHOD PUBLIC OBJECT INDEX-OBJ (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?.

        FOR FIRST arrayComplex
            WHERE arrayComplex.i_idx = arraySet.i_idx:
        
            RETURN arrayComplex.o_object.
        END.
        RETURN ?.
    END METHOD.

    METHOD PUBLIC WINOBJ INDEX-WINOBJ (INPUT iip_idx AS INTEGER):
        FOR FIRST arraySet
            WHERE arraySet.i_idx  = iip_idx
            AND   arraySet.c_type = "COMPLEX":
        END.
        IF NOT AVAILABLE arraySet THEN RETURN ?.

        FOR FIRST arrayComplex
            WHERE arrayComplex.i_idx = arraySet.i_idx:
        
            RETURN CAST(arrayComplex.o_object, WINOBJ).
        END.
        RETURN ?.
    END METHOD.

    METHOD PUBLIC LONGCHAR INDEX-LONGCHAR (INPUT iip_idx AS INTEGER):
        lc_long = ?.

        FOR FIRST arraySet
            WHERE arraySet.i_idx  = iip_idx
            AND   arraySet.c_type = "COMPLEX":
        END.
        IF NOT AVAILABLE arraySet THEN RETURN lc_long.

        FOR FIRST arrayComplex
            WHERE arrayComplex.i_idx = arraySet.i_idx:
            
            COPY-LOB FROM arrayComplex.c_clob TO lc_long.
        END.
        RETURN lc_long.
    END METHOD.
END CLASS.