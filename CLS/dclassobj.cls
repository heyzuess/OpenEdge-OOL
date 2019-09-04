CLASS DCALLOBJ:
    DEFINE PUBLIC PROPERTY h_handle   AS HANDLE    GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY i_numParam AS INTEGER   GET. PRIVATE SET.
    
    DEFINE PRIVATE VARIABLE cv_currParam AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE iv_numParam  AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE iv_currP     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE iv_idxVal    AS INTEGER   NO-UNDO EXTENT.
    
    DEFINE TEMP-TABLE objparam
        FIELD i_index AS INTEGER
        FIELD c_prog  AS CHARACTER
        FIELD c_type  AS CHARACTER
        FIELD c_ptype AS CHARACTER
        FIELD c_val   AS CHARACTER.

    /* Constructor */
    CONSTRUCTOR DCALLOBJ():
    END CONSTRUCTOR.
    
    /* Overloaded */
    CONSTRUCTOR DCALLOBJ(INPUT cip_program AS CHARACTER,
                         INPUT cip_paramL  AS CHARACTER):
        h_handle = setProgram(cip_program,cip_paramL).
    END CONSTRUCTOR.
    
    /* Destructor */
    DESTRUCTOR  DCALLOBJ():
    END DESTRUCTOR.
    
    METHOD PUBLIC HANDLE setProgram(INPUT cip_program AS CHARACTER,
                                    INPUT cip_paramL  AS CHARACTER):
        
        IF NUM-ENTRIES(cip_paramL) = 0 THEN RETURN h_handle.                            
        DO iv_currP = 1 TO NUM-ENTRIES(cip_paramL,"|")
            ASSIGN cv_currParam = ENTRY(iv_currP,cip_paramL,"|").
            
            FIND FIRST objparam
                 WHERE objparam.c_prog = cip_program
                 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE objparam THEN
            DO:
                CREATE objparam.
                ASSIGN objparam.i_index = iv_currP
                       objparam.c_prog  = cip_program
                       objparam.c_type  = ENTRY(1,cv_currParam)
                       objparam.c_ptype = ENTRY(2,cv_currParam).
            END.
        END.
        
        iv_currP = 0.
        FOR EACH objparam:
            ASSIGN iv_currP = iv_currP + 1.
        END.
        
        IF iv_currP = 0 THEN RETURN h_handle.
        	
        ASSIGN EXTENT(iv_idxVal) = iv_currP.
        
        CREATE CALL h_handle.
        h_handle:CALL-NAME      = cip_program.
        h_handle:NUM-PARAMETERS = iv_currP.
        
        FOR EACH objparam:
            h_handle:SET-PARAMETER(objparam.i_index,
                                   objparam.c_type,
                                   objparam.c_ptype,
                                   iv_idxVal[objparam.i_index]).
        END.
        
        RETURN h_handle.
    END METHOD.
END CLASS.