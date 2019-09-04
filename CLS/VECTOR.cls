CLASS VECTOR:
    DEFINE PUBLIC PROPERTY LENGTH AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_temp AS INTEGER.

    DEFINE TEMP-TABLE vectorSet
        FIELD i_idx AS INTEGER
        FIELD c_val AS CHARACTER
        FIELD o_obj AS Progress.Lang.Object.

    CONSTRUCTOR VECTOR():

    END CONSTRUCTOR.

    DESTRUCTOR VECTOR():
        FOR EACH vectorSet BY vectorSet.i_idx:
            IF VALID-OBJECT(vectorSet.o_obj) THEN DELETE OBJECT vectorSet.o_obj.
        END.
        EMPTY TEMP-TABLE vectorSet.
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL pushBack  (INPUT oip_temp AS Progress.Lang.Object):
        FOR LAST vectorSet:
            ASSIGN i_temp = vectorSet.i_idx.
        END.

        CREATE vectorSet.
        ASSIGN vectorSet.i_idx = i_temp + 1
               vectorSet.c_val = STRING(oip_temp)
               vectorSet.o_obj = oip_temp
               LENGTH          = LENGTH + 1.

        RETURN vectorSet.i_idx <> i_temp.
    END METHOD.

    METHOD PUBLIC LOG popIndex (INPUT iip_idx AS INTEGER):
        FOR FIRST vectorSet
            WHERE vectorSet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE vectorSet THEN RETURN FALSE.

        DELETE vectorSet.
        DO i_temp = iip_idx + 1 TO LENGTH:
            FOR FIRST vectorSet
                WHERE vectorSet.i_idx = i_temp:
                ASSIGN vectorSet.i_idx = vectorSet.i_idx - 1.
            END.
        END.
        LENGTH = LENGTH - 1.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL emptySet ():
        EMPTY  TEMP-TABLE vectorSet.
        ASSIGN LENGTH = 0.
        RETURN TEMP-TABLE vectorSet:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC CHARACTER getClassType (INPUT iip_idx AS INTEGER):
        FOR FIRST vectorSet
            WHERE vectorSet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE vectorSet THEN RETURN ?.

        IF NUM-ENTRIES(vectorSet.c_val,"_") >= 1 THEN
            RETURN ENTRY(1,vectorSet.c_val,"_").
        ELSE
            RETURN vectorSet.c_val.
    END METHOD.

    METHOD PUBLIC CHARACTER getClassName (INPUT iip_idx AS INTEGER):
        FOR FIRST vectorSet
            WHERE vectorSet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE vectorSet THEN RETURN ?.

        RETURN vectorSet.c_val.
    END METHOD.

    METHOD PUBLIC Progress.Lang.Object getClassByName (INPUT cip_name AS CHARACTER):
        FOR FIRST vectorSet
            WHERE vectorSet.c_val = cip_name:
        END.
        IF NOT AVAILABLE vectorSet THEN RETURN ?.

        RETURN vectorSet.o_obj.
    END METHOD.

    METHOD PUBLIC Progress.Lang.Object getClassByIndex (INPUT iip_idx AS INTEGER):
        FOR FIRST vectorSet
            WHERE vectorSet.i_idx = iip_idx:
        END.
        IF NOT AVAILABLE vectorSet THEN RETURN ?.

        RETURN vectorSet.o_obj.
    END METHOD.
END CLASS.