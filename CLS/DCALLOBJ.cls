&SCOPED-DEFINE INP_TYPE "INPUT"
&SCOPED-DEFINE OTP_TYPE "OUTPUT"
&SCOPED-DEFINE CHR_TYPE "CHARACTER"
&SCOPED-DEFINE INT_TYPE "INTEGER"
&SCOPED-DEFINE LOG_TYPE "LOGICAL"
CLASS DCALLOBJ:
    DEFINE PUBLIC PROPERTY h_handle   AS HANDLE    GET. PUBLIC  SET.
    DEFINE PUBLIC PROPERTY i_numParam AS INTEGER   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_progset  AS LOGICAL   GET. PRIVATE SET.
    
    DEFINE PRIVATE VARIABLE cv_currParam AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE iv_numParam  AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE iv_currP     AS INTEGER   NO-UNDO.
    
    DEFINE PRIVATE VARIABLE cv_idxVal    AS CHARACTER NO-UNDO EXTENT.
    DEFINE PRIVATE VARIABLE iv_idxVal    AS INTEGER   NO-UNDO EXTENT.
    DEFINE PRIVATE VARIABLE lv_idxVal    AS LOGICAL   NO-UNDO EXTENT.
    
    DEFINE TEMP-TABLE objprog
        FIELD c_prog  AS CHARACTER
        FIELD i_numP  AS INTEGER
        FIELD i_char  AS INTEGER
        FIELD i_int   AS INTEGER
        FIELD i_log   AS INTEGER
        FIELD l_set   AS LOGICAL.
    
    DEFINE TEMP-TABLE objparam
        FIELD i_index AS INTEGER
        FIELD i_idx   AS INTEGER
        FIELD c_prog  AS CHARACTER
        FIELD c_type  AS CHARACTER
        FIELD c_ptype AS CHARACTER
        FIELD c_val   AS CHARACTER.

    /* Constructor */
    CONSTRUCTOR DCALLOBJ(INPUT cip_program AS CHARACTER,
                         INPUT cip_paramL  AS CHARACTER):                 
        h_handle = setProgram(cip_program,cip_paramL).
    END CONSTRUCTOR.
    
    /* Destructor */
    DESTRUCTOR  DCALLOBJ():
        EMPTY TEMP-TABLE objparam.
        DELETE OBJECT h_handle.
    END DESTRUCTOR.
    
    METHOD PUBLIC HANDLE setProgram(INPUT cip_program AS CHARACTER,
                                    INPUT cip_paramL  AS CHARACTER):
        IF l_progset THEN RETURN h_handle.
        
        FIND FIRST objprog
             WHERE objprog.c_prog = cip_program
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE objprog THEN
        DO:
            CREATE objprog.
            ASSIGN objprog.c_prog = cip_program.
        END.
        
        IF NUM-ENTRIES(cip_paramL,"|") > 0 THEN                          
        DO:
            DO iv_currP = 1 TO NUM-ENTRIES(cip_paramL,"|"):
                ASSIGN cv_currParam = ENTRY(iv_currP,cip_paramL,"|").

                FIND FIRST objparam
                     WHERE objparam.c_prog  = cip_program
                     AND   objparam.i_index = iv_currP
                     NO-LOCK NO-ERROR.
                IF NOT AVAILABLE objparam THEN
                DO:
                    CREATE objparam.
                    ASSIGN objparam.i_index = iv_currP
                           objparam.c_prog  = cip_program
                           objparam.c_type  = ENTRY(1,cv_currParam)
                           objparam.c_ptype = ENTRY(2,cv_currParam)
                           objprog.i_numP   = objprog.i_numP + 1.
                    
                    IF objparam.c_ptype = {&INP_TYPE}  THEN
                        ASSIGN objparam.c_val = ENTRY(3,cv_currParam).
                    ELSE
                    IF objparam.c_ptype = {&OTP_TYPE} THEN
                        CASE objparam.c_type:
                            WHEN {&CHR_TYPE} THEN
                                ASSIGN objprog.i_char = objprog.i_char + 1
                                       objparam.i_idx = objprog.i_char.
                            WHEN {&INT_TYPE}   THEN
                            	  ASSIGN objprog.i_int  = objprog.i_int  + 1
                            	         objparam.i_idx = objprog.i_int.
                            WHEN {&LOG_TYPE} THEN
                                ASSIGN objprog.i_log  = objprog.i_log  + 1
                                       objparam.i_idx = objprog.i_log.
                        END CASE.
                    ELSE
                    DO:
                        ASSIGN objparam.c_val = ENTRY(3,cv_currParam).
                        
                        CASE objparam.c_type:
                            WHEN {&CHR_TYPE} THEN
                                ASSIGN objprog.i_char = objprog.i_char + 1
                                       objparam.i_idx = objprog.i_char.
                            WHEN {&INT_TYPE}   THEN
                            	  ASSIGN objprog.i_int  = objprog.i_int  + 1
                            	         objparam.i_idx = objprog.i_int.
                            WHEN {&LOG_TYPE} THEN
                                ASSIGN objprog.i_log  = objprog.i_log  + 1
                                       objparam.i_idx = objprog.i_log.
                        END CASE.
                    END.
                END.
            END.
        
            ASSIGN EXTENT(cv_idxVal) = MAX(objprog.i_char,1)
                   EXTENT(iv_idxVal) = MAX(objprog.i_int, 1)
                   EXTENT(lv_idxVal) = MAX(objprog.i_log, 1).
            
            CREATE CALL h_handle.
            h_handle:CALL-NAME      = cip_program.
            h_handle:NUM-PARAMETERS = objprog.i_numP.
            i_numParam              = objprog.i_numP.
            
            FOR EACH objparam:
                IF objparam.c_ptype = {&INP_TYPE}  THEN
                DO:
                    h_handle:SET-PARAMETER(objparam.i_index,
                                           objparam.c_type,
                                           objparam.c_ptype,
                                           objparam.c_val).
                
                    CASE objparam.c_type:
                        WHEN {&CHR_TYPE} THEN
                        DO:
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   objparam.c_val).
                        END.
                        WHEN {&INT_TYPE}   THEN
                        DO:
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   INTEGER(objparam.c_val)).
                        END.
                        WHEN {&LOG_TYPE} THEN
                        DO:
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   LOGICAL(objparam.c_val)).
                        END.
                    END CASE.
                END.
                ELSE
                IF objparam.c_ptype = {&OTP_TYPE} THEN
                    CASE objparam.c_type:
                        WHEN {&CHR_TYPE} THEN
                        DO:
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   cv_idxVal[objparam.i_idx]).
                        END.
                        WHEN {&INT_TYPE}   THEN
                        DO:
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   iv_idxVal[objparam.i_idx]).
                        END.
                        WHEN {&LOG_TYPE}  THEN
                        DO:
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   lv_idxVal[objparam.i_idx]).
                        END.
                    END CASE.
                ELSE
                DO:
                    CASE objparam.c_type:
                        WHEN {&CHR_TYPE} THEN
                        DO:
                            ASSIGN cv_idxVal[objparam.i_idx] = objparam.c_val.
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   cv_idxVal[objparam.i_idx]).
                        END.
                        WHEN {&INT_TYPE}   THEN
                        DO:
                            ASSIGN iv_idxVal[objparam.i_idx] = INTEGER(objparam.c_val).
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   iv_idxVal[objparam.i_idx]).
                        END.
                        WHEN {&LOG_TYPE} THEN
                        DO:
                            ASSIGN lv_idxVal[objparam.i_idx] = LOGICAL(objparam.c_val).
                            h_handle:SET-PARAMETER(objparam.i_index,
                                                   objparam.c_type,
                                                   objparam.c_ptype,
                                                   lv_idxVal[objparam.i_idx]).
                        END.
                    END CASE.
                END.
            END.
        END.
        ELSE DO:
            CREATE CALL h_handle.
            h_handle:CALL-NAME = cip_program.
        END.

        IF VALID-HANDLE(h_handle) THEN
            ASSIGN objprog.l_set = TRUE
                   l_progset     = objprog.l_set.
        
        RETURN h_handle.
    END METHOD.
    
    METHOD PUBLIC CHAR getParamVal(INPUT iip_param AS INTEGER):
        FIND FIRST objprog NO-LOCK NO-ERROR.
        IF NOT AVAILABLE objprog  THEN RETURN "".
        	
        FIND FIRST objparam
             WHERE objparam.c_prog  = objprog.c_prog
             AND   objparam.i_index = iip_param
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE objparam THEN RETURN "".
        
        CASE objparam.c_type:
            WHEN {&CHR_TYPE} THEN
            DO:
                RETURN cv_idxVal[objparam.i_idx].
            END.
            WHEN {&INT_TYPE}   THEN
            DO:
                RETURN STRING(iv_idxVal[objparam.i_idx]).
            END.
            WHEN {&LOG_TYPE} THEN
            DO:
                RETURN STRING(lv_idxVal[objparam.i_idx]).
            END.
        END CASE.
            
        RETURN "".
    END METHOD.
    
    METHOD PUBLIC LOG runProgram():
        h_handle:INVOKE.
        
        FOR EACH  objparam
            WHERE objparam.c_ptype <> {&INP_TYPE}:
            CASE objparam.c_type:
                WHEN {&CHR_TYPE} THEN
                    ASSIGN objparam.c_val = cv_idxVal[objparam.i_idx].
                WHEN {&INT_TYPE}   THEN
                    ASSIGN objparam.c_val = STRING(iv_idxVal[objparam.i_idx]).
                WHEN {&LOG_TYPE} THEN
                    ASSIGN objparam.c_val = STRING(lv_idxVal[objparam.i_idx]).
            END CASE.
        END.
        
        RETURN TRUE.
    END METHOD.
END CLASS.