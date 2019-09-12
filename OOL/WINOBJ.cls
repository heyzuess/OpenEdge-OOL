USING OOL.*.
CLASS OOL.WINOBJ ABSTRACT:
    DEFINE PUBLIC    PROPERTY PARENT        AS WINOBJ    GET. PUBLIC    SET.
    DEFINE PUBLIC    PROPERTY HANDLE        AS HANDLE    GET. PROTECTED SET.
    DEFINE PUBLIC    PROPERTY NUM-CHILDREN  AS INTEGER   GET. PROTECTED SET.
    DEFINE PUBLIC    PROPERTY CURRENT-CHILD AS INTEGER   GET. PROTECTED SET.
    DEFINE PUBLIC    PROPERTY LIVE          AS LOGICAL   GET. PROTECTED SET.
    DEFINE PUBLIC    PROPERTY REF-ONLY      AS LOGICAL   GET. PROTECTED SET.
    DEFINE PUBLIC    PROPERTY THIS          AS INTEGER   GET. PRIVATE   SET.
    DEFINE PUBLIC    PROPERTY BREAK         AS LOGICAL   GET. PRIVATE   SET.
    DEFINE PROTECTED PROPERTY SELF-REF      AS LOGICAL   GET. PROTECTED SET.
    DEFINE PROTECTED PROPERTY LAST-KEY      AS CHARACTER GET. PROTECTED SET.

    DEFINE PUBLIC PROPERTY NAME   AS CHARACTER GET.
    PUBLIC SET (cip_tempName AS CHARACTER):
        ASSIGN THIS-OBJECT:NAME = cip_tempName.
        
        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:NAME = THIS-OBJECT:NAME.
        END.
    END SET.

    DEFINE PUBLIC ABSTRACT PROPERTY WIDTH  AS INTEGER   GET.
    PUBLIC SET (iip_tempWidth AS INTEGER):
        /*
        ASSIGN THIS-OBJECT:WIDTH = iip_tempWidth.

        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:HANDLE:WIDTH-CHARS = THIS-OBJECT:WIDTH.
        END.
        */
    END SET.

    DEFINE PUBLIC ABSTRACT PROPERTY HEIGHT AS INTEGER   GET.
    PUBLIC SET (iip_tempHeight AS INTEGER):
        /*
        ASSIGN THIS-OBJECT:HEIGHT = iip_tempHeight.

        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:HANDLE:HEIGHT-CHARS = THIS-OBJECT:HEIGHT.
        END.*/
    END SET.
    
    DEFINE PUBLIC PROPERTY X      AS INTEGER   GET. 
    PUBLIC SET (iip_tempX AS INTEGER):
        ASSIGN THIS-OBJECT:X = MAX(iip_tempX, 1).

        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:HANDLE:COL = THIS-OBJECT:X.
        END.
    END SET.
    
    DEFINE PUBLIC PROPERTY Y      AS INTEGER   GET. 
    PUBLIC SET (iip_tempY AS INTEGER):
        ASSIGN THIS-OBJECT:Y = MAX(iip_tempY, 1).

        IF THIS-OBJECT:LIVE THEN
        DO:
            THIS-OBJECT:HANDLE:ROW = THIS-OBJECT:Y.
        END.
    END SET.
    
    DEFINE PUBLIC PROPERTY COLOR  AS INTEGER   GET.
    PUBLIC SET (iip_tempColor AS INTEGER):
        ASSIGN THIS-OBJECT:COLOR = iip_tempColor.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:DCOLOR  = THIS-OBJECT:COLOR.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY VISIBLE AS LOGICAL  GET.
    PUBLIC SET (lip_tempVis AS LOGICAL):
        ASSIGN THIS-OBJECT:VISIBLE = lip_tempVis.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:VISIBLE = THIS-OBJECT:VISIBLE.
        END.
    END SET.

    DEFINE PUBLIC PROPERTY SENSITIVE AS LOGICAL GET.
    PUBLIC SET (lip_sense AS LOGICAL):
        ASSIGN THIS-OBJECT:SENSITIVE = lip_sense.

        IF THIS-OBJECT:LIVE THEN
        DO:
            ASSIGN THIS-OBJECT:HANDLE:SENSITIVE = THIS-OBJECT:SENSITIVE.
        END.
    END SET.

    DEFINE PUBLIC EVENT FOR-EACH SIGNATURE VOID (wip_obj AS WINOBJ).

    DEFINE PUBLIC ABSTRACT EVENT FOCUS-OFF SIGNATURE VOID (aip_data AS ARRAY).

    DEFINE PROTECTED TEMP-TABLE children
        FIELD i_idx    AS INTEGER
        FIELD h_handle AS HANDLE
        FIELD o_object AS Progress.Lang.OBJECT
        FIELD c_type   AS CHARACTER.

    DEFINE PRIVATE VARIABLE o_current AS WINOBJ.
    DEFINE PRIVATE VARIABLE i_current AS INTEGER.

    CONSTRUCTOR WINOBJ ():
        SUPER().
        ASSIGN THIS-OBJECT:X = 1
               THIS-OBJECT:Y = 1.
    END CONSTRUCTOR.

    DESTRUCTOR WINOBJ ():
        IF VALID-OBJECT(THIS-OBJECT:PARENT) THEN THIS-OBJECT:PARENT:DEL-CHILD(THIS-OBJECT).
        
        IF THIS-OBJECT:HAS-CHILDREN() THEN
        DO i_current = 1 TO THIS-OBJECT:NUM-CHILDREN:
            ASSIGN o_current = THIS-OBJECT:GET-CHILD(i_current).
            
            IF VALID-OBJECT(o_current) THEN
            DO:
                //ASSIGN o_current:VISIBLE = FALSE.
                DELETE OBJECT o_current.
            END.
        END.
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL HAS-CHILDREN ():
        RETURN TEMP-TABLE children:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC WINOBJ GET-CHILD (INPUT iip_idx AS INTEGER):
        FOR FIRST children
            WHERE children.i_idx = iip_idx:
            ASSIGN THIS-OBJECT:CURRENT-CHILD = children.i_idx.
            RETURN CAST(children.o_object, WINOBJ).
        END.
        RETURN ?.
    END METHOD.

    METHOD PUBLIC HANDLE GET-CHILD-HANDLE (INPUT iip_idx AS INTEGER):
        FOR FIRST children
            WHERE children.i_idx = iip_idx:
            ASSIGN THIS-OBJECT:CURRENT-CHILD = children.i_idx.
            RETURN children.h_handle.
        END.
        RETURN ?.
    END METHOD.

    METHOD PUBLIC WINOBJ GET-FIRST-CHILD-BY (INPUT cip_prop  AS CHARACTER,
                                             INPUT cip_value AS CHARACTER):
        o_current = ?.
        FOR EACH children
            BY   children.i_idx:
        
            ASSIGN o_current = CAST(children.o_object, WINOBJ).
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF o_current:NAME = cip_value       THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF o_current:X = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF o_current:Y = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                OTHERWISE ASSIGN o_current = ?.
            END CASE.
        END.

        RETURN o_current.
    END METHOD.

    METHOD PUBLIC HANDLE GET-FIRST-CHILD-HANDLE-BY (INPUT cip_prop  AS CHARACTER,
                                                    INPUT cip_value AS CHARACTER):
        FOR EACH children
            BY   children.i_idx:
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF children.h_handle:NAME = cip_value         THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF children.h_handle:X = INTEGER(cip_value)   THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF children.h_handle:Y = INTEGER(cip_value)   THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "COL" THEN
                DO:
                    IF children.h_handle:COL = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "ROW" THEN
                DO:
                    IF children.h_handle:ROW = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
            END CASE.
        END.

        RETURN ?.
    END METHOD.

    METHOD PUBLIC WINOBJ GET-LAST-CHILD-BY (INPUT cip_prop  AS CHARACTER,
                                            INPUT cip_value AS CHARACTER):
        o_current = ?.
        FOR EACH children
            BY   children.i_idx
            DESC:
        
            ASSIGN o_current = CAST(children.o_object, WINOBJ).
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF o_current:NAME = cip_value       THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF o_current:X = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF o_current:Y = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                OTHERWISE ASSIGN o_current = ?.
            END CASE.
        END.

        RETURN o_current.
    END METHOD.

    METHOD PUBLIC HANDLE GET-LAST-CHILD-HANDLE-BY (INPUT cip_prop  AS CHARACTER,
                                                   INPUT cip_value AS CHARACTER):
        FOR EACH children
            BY   children.i_idx
            DESC:
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF children.h_handle:NAME = cip_value         THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF children.h_handle:X = INTEGER(cip_value)   THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF children.h_handle:Y = INTEGER(cip_value)   THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "COL" THEN
                DO:
                    IF children.h_handle:COL = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "ROW" THEN
                DO:
                    IF children.h_handle:ROW = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
            END CASE.
        END.

        RETURN ?.
    END METHOD.

    METHOD PUBLIC WINOBJ GET-NEXT-CHILD-BY (INPUT cip_prop  AS CHARACTER,
                                            INPUT cip_value AS CHARACTER):
        o_current = ?.
        FOR EACH  children
            WHERE children.i_idx > THIS-OBJECT:CURRENT-CHILD
            BY    children.i_idx:
        
            ASSIGN o_current = CAST(children.o_object, WINOBJ).
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF o_current:NAME = cip_value       THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF o_current:X = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF o_current:Y = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                OTHERWISE ASSIGN o_current = ?.
            END CASE.
        END.

        IF o_current = ? THEN ASSIGN o_current = THIS-OBJECT:GET-FIRST-CHILD-BY(cip_prop, cip_value).

        RETURN o_current.
    END METHOD.

    METHOD PUBLIC HANDLE GET-NEXT-CHILD-HANDLE-BY (INPUT cip_prop  AS CHARACTER,
                                                   INPUT cip_value AS CHARACTER):
        FOR EACH  children
            WHERE children.i_idx > THIS-OBJECT:CURRENT-CHILD
            BY    children.i_idx:
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF children.h_handle:NAME = cip_value       THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF children.h_handle:X = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF children.h_handle:Y = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
            END CASE.
        END.

        RETURN THIS-OBJECT:GET-FIRST-CHILD-HANDLE-BY(cip_prop, cip_value).
    END METHOD.

    METHOD PUBLIC WINOBJ GET-PREV-CHILD-BY (INPUT cip_prop  AS CHARACTER,
                                            INPUT cip_value AS CHARACTER):
        o_current = ?.
        FOR EACH  children
            WHERE children.i_idx < THIS-OBJECT:CURRENT-CHILD
            BY    children.i_idx
            DESC:
        
            ASSIGN o_current = CAST(children.o_object, WINOBJ).
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF o_current:NAME = cip_value       THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF o_current:X = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF o_current:Y = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD(children.i_idx).
                END.
                OTHERWISE ASSIGN o_current = ?.
            END CASE.
        END.

        IF o_current = ? THEN ASSIGN o_current = THIS-OBJECT:GET-LAST-CHILD-BY(cip_prop, cip_value).

        RETURN o_current.
    END METHOD.

    METHOD PUBLIC HANDLE GET-PREV-CHILD-HANDLE-BY (INPUT cip_prop  AS CHARACTER,
                                                   INPUT cip_value AS CHARACTER):
        FOR EACH  children
            WHERE children.i_idx < THIS-OBJECT:CURRENT-CHILD
            BY    children.i_idx
            DESC:
            
            CASE cip_prop:
                WHEN "NAME" THEN
                DO:
                    IF children.h_handle:NAME = cip_value       THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "X" THEN
                DO:
                    IF children.h_handle:X = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
                WHEN "Y" THEN
                DO:
                    IF children.h_handle:Y = INTEGER(cip_value) THEN RETURN THIS-OBJECT:GET-CHILD-HANDLE(children.i_idx).
                END.
            END CASE.
        END.

        RETURN THIS-OBJECT:GET-LAST-CHILD-HANDLE-BY(cip_prop, cip_value).
    END METHOD.

    METHOD PUBLIC WINOBJ GET-FIRST-CHILD ():
        RETURN THIS-OBJECT:GET-CHILD(1).
    END METHOD.

    METHOD PUBLIC HANDLE GET-FIRST-CHILD-HANDLE ():
        RETURN THIS-OBJECT:GET-CHILD-HANDLE(1).
    END METHOD.

    METHOD PUBLIC WINOBJ GET-LAST-CHILD ():
        RETURN THIS-OBJECT:GET-CHILD(THIS-OBJECT:NUM-CHILDREN).
    END METHOD.

    METHOD PUBLIC HANDLE GET-LAST-CHILD-HANDLE ():
        RETURN THIS-OBJECT:GET-CHILD-HANDLE(THIS-OBJECT:NUM-CHILDREN).
    END METHOD.

    METHOD PUBLIC WINOBJ GET-NEXT-CHILD ():
        ASSIGN THIS-OBJECT:CURRENT-CHILD = THIS-OBJECT:CURRENT-CHILD + 1.
        IF THIS-OBJECT:CURRENT-CHILD > THIS-OBJECT:NUM-CHILDREN THEN
            ASSIGN THIS-OBJECT:CURRENT-CHILD = 1.

        RETURN THIS-OBJECT:GET-CHILD(THIS-OBJECT:CURRENT-CHILD).
    END METHOD.

    METHOD PUBLIC HANDLE GET-NEXT-CHILD-HANDLE ():
        ASSIGN THIS-OBJECT:CURRENT-CHILD = THIS-OBJECT:CURRENT-CHILD + 1.
        IF THIS-OBJECT:CURRENT-CHILD > THIS-OBJECT:NUM-CHILDREN THEN
            ASSIGN THIS-OBJECT:CURRENT-CHILD = 1.

        RETURN THIS-OBJECT:GET-CHILD-HANDLE(THIS-OBJECT:CURRENT-CHILD).
    END METHOD.

    METHOD PUBLIC WINOBJ GET-PREV-CHILD ():
        ASSIGN THIS-OBJECT:CURRENT-CHILD = THIS-OBJECT:CURRENT-CHILD - 1.
        IF THIS-OBJECT:CURRENT-CHILD < 1 THEN
            ASSIGN THIS-OBJECT:CURRENT-CHILD = THIS-OBJECT:NUM-CHILDREN.

        RETURN THIS-OBJECT:GET-CHILD(THIS-OBJECT:CURRENT-CHILD).
    END METHOD.

    METHOD PUBLIC HANDLE GET-PREV-CHILD-HANDLE ():
        ASSIGN THIS-OBJECT:CURRENT-CHILD = THIS-OBJECT:CURRENT-CHILD - 1.
        IF THIS-OBJECT:CURRENT-CHILD < 1 THEN
            ASSIGN THIS-OBJECT:CURRENT-CHILD = THIS-OBJECT:NUM-CHILDREN.

        RETURN THIS-OBJECT:GET-CHILD-HANDLE(THIS-OBJECT:CURRENT-CHILD).
    END METHOD.

    METHOD PUBLIC LOGICAL ADD-CHILD (INPUT hip_handle AS HANDLE):
        FOR FIRST children
            WHERE children.h_handle = hip_handle:
        END.
        IF AVAILABLE children THEN RETURN FALSE.

        CREATE children.
        ASSIGN THIS-OBJECT:NUM-CHILDREN = THIS-OBJECT:NUM-CHILDREN + 1
               children.i_idx           = THIS-OBJECT:NUM-CHILDREN
               children.h_handle        = hip_handle
               children.c_type          = hip_handle:TYPE
        NO-ERROR.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL ADD-CHILD (INPUT oip_handle AS WINOBJ):
        FOR FIRST children
            WHERE children.o_object = oip_handle:
        END.
        IF AVAILABLE children THEN RETURN FALSE.

        CREATE children.
        ASSIGN THIS-OBJECT:NUM-CHILDREN = THIS-OBJECT:NUM-CHILDREN + 1
               children.i_idx           = THIS-OBJECT:NUM-CHILDREN
               children.o_object        = oip_handle
               children.h_handle        = oip_handle:HANDLE
               children.c_type          = oip_handle:HANDLE:TYPE
        NO-ERROR.

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC VOID DEL-CHILD (INPUT hip_handle AS HANDLE):
        FOR FIRST children
            WHERE children.h_handle = hip_handle:
        END.
        IF AVAILABLE children THEN DELETE children.
    END METHOD.

    METHOD PUBLIC VOID DEL-CHILD (INPUT oip_handle AS WINOBJ):
        FOR FIRST children
            WHERE children.o_object = oip_handle:
        END.
        
        IF AVAILABLE children THEN DELETE children.
    END METHOD.

    METHOD PUBLIC LOGICAL IS-CHILD (INPUT oip_handle AS WINOBJ):
        FOR FIRST children
            WHERE children.o_object = oip_handle:
        END.
        RETURN AVAILABLE children.
    END METHOD.

    METHOD PUBLIC LOGICAL IS-CHILD (INPUT hip_handle AS HANDLE):
        FOR FIRST children
            WHERE children.h_handle = hip_handle:
        END.
        RETURN AVAILABLE children.
    END METHOD.

    METHOD PUBLIC VOID FOR-EACH ():
        FOR-EACH:
        FOR EACH children
            BY   children.i_idx:
            
            THIS-OBJECT:THIS = children.i_idx.
            THIS-OBJECT:FOR-EACH:PUBLISH(CAST(children.o_object, WINOBJ)).

            IF THIS-OBJECT:BREAK THEN
            DO:
                ASSIGN THIS-OBJECT:BREAK = FALSE.
                LEAVE FOR-EACH.
            END.
        END.
        THIS-OBJECT:THIS = ?.
    END METHOD.

    METHOD PUBLIC VOID BREAK ():
        ASSIGN THIS-OBJECT:BREAK = TRUE.
    END METHOD.

    METHOD PUBLIC ABSTRACT LOGICAL REGISTER ().

    METHOD PUBLIC ABSTRACT VOID ENABLE ().

    METHOD PUBLIC ABSTRACT VOID WAIT ().
END CLASS.