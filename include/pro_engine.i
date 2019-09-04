/************ CONSTANTS ************/
&IF DEFINED(i_level) = 0 &THEN
    &SCOPED-DEFINE i_level 
&ENDIF
&IF DEFINED(i_stepcount{&i_level}) = 0 &THEN
    &SCOPED-DEFINE i_stepcount{&i_level} 8
&ENDIF
&IF DEFINED(i_mapH{&i_level}) = 0 &THEN
    &SCOPED-DEFINE i_mapH{&i_level} 9
&ENDIF
&IF DEFINED(i_mapW{&i_level}) = 0 &THEN
    &SCOPED-DEFINE i_mapW{&i_level} 20
&ENDIF
&IF DEFINED(i_frameH{&i_level}) = 0 &THEN
    &SCOPED-DEFINE i_frameH{&i_level} 11
&ENDIF
&IF DEFINED(i_frameW{&i_level}) = 0 &THEN
    &SCOPED-DEFINE i_frameW{&i_level} 21
&ENDIF
&IF DEFINED(c_mapList{&i_level}) = 0 &THEN
    &SCOPED-DEFINE c_mapList{&i_level} ""
&ENDIF
&IF DEFINED(c_mapDisp{&i_level}) = 0 &THEN
    &SCOPED-DEFINE c_mapDisp{&i_level}
&ENDIF

/************ VARIABLES ************/
//DEFINE VARIABLE av_main  AS ACTOR     NO-UNDO.
DEFINE VARIABLE cv_alpha{&i_level}    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_list{&i_level}     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_char{&i_level}     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_mini{&i_level}     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_key{&i_level}      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_disp{&i_level}     AS CHARACTER NO-UNDO EXTENT {&i_mapH{&i_level}} FORMAT 'X({&i_mapW{&i_level}})' .
DEFINE VARIABLE cv_error{&i_level}    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_tlist{&i_level}    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_tvals{&i_level}    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv_ttype{&i_level}    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iv_row{&i_level}      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iv_col{&i_level}      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iv_int{&i_level}      AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iv_x{&i_level}        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iv_y{&i_level}        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iv_rx{&i_level}       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iv_ry{&i_level}       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv_complete{&i_level} AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dv_timecomp{&i_level} AS DECIMAL   NO-UNDO.

/************ TEMP-TABLES **********/
DEFINE TEMP-TABLE tt-grid{&i_level} NO-UNDO
    FIELD i_row   AS INTEGER
    FIELD i_col   AS INTEGER
    FIELD i_x     AS INTEGER
    FIELD i_y     AS INTEGER
    FIELD c_val   AS CHARACTER
    FIELD c_type  AS CHARACTER.
    
DEFINE TEMP-TABLE tt-obj{&i_level} NO-UNDO
    FIELD c_type  AS CHARACTER
    FIELD c_val   AS CHARACTER
    FIELD i_x     AS INTEGER
    FIELD i_y     AS INTEGER
    FIELD i_col   AS INTEGER
    FIELD i_row   AS INTEGER.
    
DEFINE TEMP-TABLE tt-inv{&i_level} NO-UNDO
    FIELD h_owner AS HANDLE
    FIELD c_type  AS CHARACTER
    FIELD c_val   AS CHARACTER
    FIELD c_key   AS CHARACTER
    FIELD i_qty   AS INTEGER.
    
/************ FRAMES ***************/
DEFINE FRAME f-map{&i_level}
    {&c_mapDisp{&i_level}}
    cv_mini{&i_level}    FORMAT 'X(1)'
WITH DOWN SIZE {&i_frameW{&i_level}} BY {&i_frameH{&i_level}}
OVERLAY WITH NO-LABELS.

/*cv_disp{&i_level}[1] AT ROW 1 COL 1
    cv_disp{&i_level}[2] AT ROW 2 COL 1
    cv_disp{&i_level}[3] AT ROW 3 COL 1
    cv_disp{&i_level}[4] AT ROW 4 COL 1
    cv_disp{&i_level}[5] AT ROW 5 COL 1
    cv_disp{&i_level}[6] AT ROW 6 COL 1
    cv_disp{&i_level}[7] AT ROW 7 COL 1
    cv_disp{&i_level}[8] AT ROW 8 COL 1
    cv_disp{&i_level}[9] AT ROW 9 COL 1*/

/************ FORWARDS *************/
FUNCTION f-setmap{&i_level}    RETURNS LOGICAL (INPUT cip_maplist AS CHARACTER) FORWARD.

FUNCTION f-seterror{&i_level}  RETURNS LOGICAL (INPUT cip_message AS CHARACTER) FORWARD.

FUNCTION f-message{&i_level}   RETURNS LOGICAL (INPUT cip_message AS CHARACTER,
                                                INPUT cip_type    AS CHARACTER,
                                                INPUT lip_log     AS LOGICAL)   FORWARD.

FUNCTION f-checkstep{&i_level} RETURNS LOGICAL (INPUT  hip_handle AS HANDLE,
                                                INPUT  iip_x      AS INTEGER,
                                                INPUT  iip_y      AS INTEGER,
                                                OUTPUT cop_error  AS CHARACTER) FORWARD.
                                      
FUNCTION f-paint{&i_level}     RETURNS LOGICAL (INPUT iip_row     AS INTEGER,
                                                INPUT iip_col     AS INTEGER,
                                                INPUT cip_val     AS CHARACTER) FORWARD.
                                      
FUNCTION f-roundnum{&i_level}  RETURNS INTEGER (INPUT iip_num     AS INTEGER)   FORWARD.

FUNCTION f-border{&i_level}    RETURNS LOGICAL (INPUT iip_extent  AS INTEGER)   FORWARD.

FUNCTION f-complete{&i_level}  RETURNS LOGICAL ()                               FORWARD.

FUNCTION f-clearData{&i_level} RETURNS LOGICAL ()                               FORWARD.

FUNCTION f-init{&i_level}      RETURNS LOGICAL ()                               FORWARD.

/************ SYS-SETTINGS *********/
//ASSIGN av_main = NEW actor('Nub','Lad','Commoner').
f-init{&i_level}().

/************ TRIGGERS *************/
ON ANY-KEY OF cv_mini{&i_level} IN FRAME f-map{&i_level} DO:
    ASSIGN cv_key{&i_level} = LAST-EVENT:LABEL.
    
    IF LOOKUP(cv_key{&i_level},cv_alpha{&i_level}) > 0 THEN
        ASSIGN cv_key{&i_level} = 'ALPHA'.
    
    CASE cv_key{&i_level}:
        WHEN 'CURSOR-UP' THEN
        DO:
            IF f-checkstep{&i_level}(cv_mini{&i_level}:HANDLE,cv_mini{&i_level}:X,cv_mini{&i_level}:Y - {&i_stepcount{&i_level}},OUTPUT cv_error{&i_level}) THEN
                ASSIGN cv_mini{&i_level}:Y = cv_mini{&i_level}:Y - {&i_stepcount{&i_level}}.
            ELSE DO:
                MESSAGE cv_error{&i_level}.
                /*PAUSE.*/
            END.
        END.
        WHEN 'CURSOR-DOWN' THEN
        DO:
            IF f-checkstep{&i_level}(cv_mini{&i_level}:HANDLE,cv_mini{&i_level}:X,cv_mini{&i_level}:Y + {&i_stepcount{&i_level}},OUTPUT cv_error{&i_level}) THEN
                ASSIGN cv_mini{&i_level}:Y = cv_mini{&i_level}:Y + {&i_stepcount{&i_level}}.
            ELSE DO:
                MESSAGE cv_error{&i_level}.
                /*PAUSE.*/
            END.
        END.
        WHEN 'CURSOR-LEFT' THEN
        DO:
            IF f-checkstep{&i_level}(cv_mini{&i_level}:HANDLE,cv_mini{&i_level}:X - {&i_stepcount{&i_level}},cv_mini{&i_level}:Y,OUTPUT cv_error{&i_level}) THEN
                ASSIGN cv_mini{&i_level}:X = cv_mini{&i_level}:X - {&i_stepcount{&i_level}}.
            ELSE DO:
                MESSAGE cv_error{&i_level}.
                /*PAUSE.*/
            END.
        END.
        WHEN 'CURSOR-RIGHT' THEN
        DO:
            IF f-checkstep{&i_level}(cv_mini{&i_level}:HANDLE,cv_mini{&i_level}:X + {&i_stepcount{&i_level}},cv_mini{&i_level}:Y,OUTPUT cv_error{&i_level}) THEN
                ASSIGN cv_mini{&i_level}:X = cv_mini{&i_level}:X + {&i_stepcount{&i_level}}.
            ELSE DO:
                MESSAGE cv_error{&i_level}.
                /*PAUSE.*/
            END.
        END.
        WHEN 'F4' OR WHEN 'ESC' THEN
        DO:
            RETURN NO-APPLY.
        END.
        WHEN 'CTRL-C' THEN
        DO:
            APPLY 'ERROR' TO SELF.
            RETURN.
        END.
        WHEN 'ALPHA' THEN
            RETURN.
        OTHERWISE RETURN NO-APPLY.
    END CASE.
    
    ASSIGN iv_x{&i_level} = cv_mini{&i_level}:X
           iv_y{&i_level} = cv_mini{&i_level}:Y.
    
    IF lv_complete{&i_level} THEN
    DO:
        APPLY 'GO' TO cv_mini{&i_level} IN FRAME f-map{&i_level}.
        HIDE FRAME f-map{&i_level}.
    END.
    
    MESSAGE SUBSTITUTE('POS: &1, &2',iv_x{&i_level},iv_y{&i_level}).
    RETURN.
END.

PROCEDURE new_game{&i_level}:
/************ MAIN-BLK *************/
f-init{&i_level}().
MESSAGE SUBSTITUTE('POS: &1, &2',iv_x{&i_level},iv_y{&i_level}).
MAIN-BLK:
REPEAT:
    DISPLAY {&c_mapDisp{&i_level}}
    WITH FRAME f-map{&i_level}.
    
    UPDATE cv_mini{&i_level} WITH FRAME f-map{&i_level}.
    IF lv_complete{&i_level} THEN
    DO:
        //f-clearData{&i_level}().
        LEAVE MAIN-BLK.
    END.
END.
ASSIGN dv_timecomp{&i_level} = ETIME / 1000.
END PROCEDURE.

/************ FUNCTIONS ************/
FUNCTION f-checkstep{&i_level} RETURNS LOGICAL (INPUT  hip_handle AS HANDLE,
                                                INPUT  iip_x      AS INTEGER,
                                                INPUT  iip_y      AS INTEGER,
                                                OUTPUT cop_error  AS CHARACTER):
    DEFINE VARIABLE fiv_kx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE fiv_ky     AS INTEGER NO-UNDO.
    DEFINE VARIABLE flv_valkey AS LOGICAL NO-UNDO.
    DEFINE VARIABLE flv_haskey AS LOGICAL NO-UNDO.
                                      
    FIND FIRST tt-grid{&i_level}
         WHERE tt-grid{&i_level}.i_x = iip_x
         AND   tt-grid{&i_level}.i_y = iip_y
         NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-grid{&i_level} THEN
    DO:
        ASSIGN cop_error = SUBSTITUTE('No grid record could be found for coordinate &1,&2',
                                      iip_x,iip_y).
        RETURN FALSE.
    END.
    
    CASE tt-grid{&i_level}.c_val:
        WHEN '#' THEN
        DO:
            ASSIGN cop_error = SUBSTITUTE("Wall encountered at pos &1,&2",iip_x,iip_y).
            RETURN FALSE.
        END.
        WHEN ' ' THEN
        DO:
            RETURN TRUE.
        END.
        WHEN '|' OR WHEN '_' THEN
        DO:
            FIND FIRST tt-obj{&i_level}
                 WHERE tt-obj{&i_level}.c_type = "DOOR"
                 AND   tt-obj{&i_level}.i_x    = iip_x
                 AND   tt-obj{&i_level}.i_y    = iip_y
                 NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE tt-obj{&i_level} THEN
            DO:
                ASSIGN cop_error = SUBSTITUTE('No obj record could be found for door at pos &1,&2',
                                              iip_x,iip_y).
                RETURN FALSE.
            END.
            
            IF tt-obj{&i_level}.c_val = "UNLOCKED" THEN
                RETURN TRUE.
            
            LOCK-BLK:
            FOR EACH tt-inv{&i_level}
                WHERE tt-inv{&i_level}.h_owner = hip_handle
                AND   tt-inv{&i_level}.c_type  = "KEY"
                NO-LOCK:
                
                ASSIGN flv_haskey = TRUE
                       fiv_kx     = INTEGER(ENTRY(1,tt-inv{&i_level}.c_key))
                       fiv_ky     = INTEGER(ENTRY(2,tt-inv{&i_level}.c_key)).
                
                IF tt-obj{&i_level}.i_col = fiv_kx AND
                   tt-obj{&i_level}.i_row = fiv_ky THEN
                DO:
                    ASSIGN flv_valkey = TRUE.
                    LEAVE LOCK-BLK.
                END.
            END.
            
            IF NOT AVAILABLE tt-inv{&i_level}         AND
                         NOT flv_haskey     OR
               (AVAILABLE tt-inv{&i_level}            AND
                          tt-inv{&i_level}.i_qty = 0) THEN
            DO:
                ASSIGN cop_error = SUBSTITUTE("Door is locked at pos &1,&2",iip_x,iip_y).
                RETURN FALSE.
            END.
            
            IF NOT flv_valkey THEN
            DO:
                ASSIGN cop_error = SUBSTITUTE("No keys in inventory open door at pos &1,&2",
                                              iip_x,iip_y).
                RETURN FALSE.
            END.
            
            ASSIGN tt-inv{&i_level}.i_qty = tt-inv{&i_level}.i_qty - 1
                   tt-obj{&i_level}.c_val = "UNLOCKED".
                   
            IF tt-inv{&i_level}.i_qty = 0 THEN DELETE tt-inv{&i_level}.
            RETURN TRUE.
        END.
        WHEN 'k' THEN
        DO:
            FIND FIRST tt-obj{&i_level}
                 WHERE tt-obj{&i_level}.c_type = "KEY"
                 AND   tt-obj{&i_level}.i_x    = iip_x
                 AND   tt-obj{&i_level}.i_y    = iip_y
                 NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE tt-obj{&i_level} THEN
            DO:
                ASSIGN cop_error = SUBSTITUTE('No obj record could be found for key at pos &1,&2',
                                              iip_x,iip_y).
                RETURN FALSE.
            END.
            
            FIND FIRST tt-inv{&i_level}
                 WHERE tt-inv{&i_level}.h_owner = hip_handle
                 AND   tt-inv{&i_level}.c_type  = tt-obj{&i_level}.c_type
                 AND   tt-inv{&i_level}.c_val   = tt-obj{&i_level}.c_val
                 AND   tt-inv{&i_level}.c_key   = STRING(tt-obj{&i_level}.i_col) + ',' + STRING(tt-obj{&i_level}.i_row)
                 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-inv{&i_level} THEN
            DO:
                CREATE tt-inv{&i_level}.
                ASSIGN tt-inv{&i_level}.h_owner = hip_handle
                       tt-inv{&i_level}.c_type  = tt-obj{&i_level}.c_type
                       tt-inv{&i_level}.c_val   = tt-obj{&i_level}.c_val
                       tt-inv{&i_level}.c_key   = STRING(tt-obj{&i_level}.i_col) + ',' + STRING(tt-obj{&i_level}.i_row).
            END.
            
            ASSIGN tt-inv{&i_level}.i_qty = tt-inv{&i_level}.i_qty + 1.
            
            MESSAGE SUBSTITUTE('Found key at pos &1,&2 ~!',iip_x,iip_y).
            RETURN TRUE.
        END.
        WHEN '@' THEN
        DO:
            f-message{&i_level}('LEVEL {&i_level} COMPLETE~!','A',FALSE).
            f-complete{&i_level}().
            RETURN TRUE.
            //APPLY 'CTRL-C' TO SELF.
        END.
        OTHERWISE DO:
            ASSIGN cop_error = SUBSTITUTE("Unkown object encountered at pos &1,&2",iip_x,iip_y).
            RETURN FALSE.
        END.
    END CASE.
END FUNCTION.

FUNCTION f-paint{&i_level}     RETURNS LOGICAL (INPUT iip_row     AS INTEGER,
                                                INPUT iip_col     AS INTEGER,
                                                INPUT cip_val     AS CHARACTER):
    IF LENGTH(cip_val) <> 1 THEN
    DO:
        f-seterror{&i_level}(SUBSTITUTE('Length of input &1 is too long: &2 chars',
                              cip_val,LENGTH(cip_val))).
        RETURN FALSE.
    END.
    
    ASSIGN SUBSTRING(cv_disp{&i_level}[iip_row],iip_col,1) = cip_val NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        f-seterror{&i_level}(ERROR-STATUS:GET-MESSAGE[1]).
        RETURN FALSE.
    END.
    
    FOR FIRST tt-grid{&i_level}
        WHERE tt-grid{&i_level}.i_row = iip_row
        AND   tt-grid{&i_level}.i_col = iip_col
        NO-LOCK:
        ASSIGN tt-grid{&i_level}.c_val = cip_val.
    END.
    
    RETURN TRUE.
END FUNCTION.

FUNCTION f-message{&i_level}   RETURNS LOGICAL (INPUT cip_message AS CHARACTER,
                                                INPUT cip_type    AS CHARACTER,
                                                INPUT lip_log     AS LOGICAL):
    CASE cip_type:
        WHEN 'A' THEN ASSIGN cip_type = 'ALERT'.
        WHEN 'E' THEN ASSIGN cip_type = 'ERROR'.
        WHEN 'W' THEN ASSIGN cip_type = 'WARNING'.
    END CASE.
    
    RUN message.p(INPUT  cip_message,
                  INPUT  cip_type,
                  INPUT  lip_log,
                  INPUT  9,
                  INPUT  2,
                  OUTPUT lip_log).
    RETURN lip_log.
END FUNCTION.

FUNCTION f-seterror{&i_level}  RETURNS LOGICAL (INPUT cip_message AS CHARACTER):
    ASSIGN cv_error{&i_level} = cip_message.
    RETURN TRUE.
END FUNCTION.

FUNCTION f-roundnum{&i_level}  RETURNS INTEGER (INPUT iip_num     AS INTEGER):
    DEFINE VARIABLE fcv_mod AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE fiv_mod AS INTEGER   NO-UNDO.
    DEFINE VARIABLE fiv_mdl AS INTEGER   NO-UNDO.
    DEFINE VARIABLE fdv_mod AS DECIMAL   NO-UNDO.
    
    ASSIGN fdv_mod = iip_num / {&i_stepcount{&i_level}}
           fiv_mdl = iip_num MODULO {&i_stepcount{&i_level}}.
    
    IF fdv_mod = 0 OR
       fiv_mdl = 0 THEN RETURN iip_num.
    
    ASSIGN fcv_mod[1] = ENTRY(1,STRING(fdv_mod,'>>9.999'),'.')
           fcv_mod[2] = ENTRY(2,STRING(fdv_mod,'>>9.999'),'.')
           fiv_mod    = INTEGER(fcv_mod[1]).
           
    IF INTEGER(SUBSTRING(fcv_mod[2],1,1)) >= 5 THEN
        ASSIGN fiv_mod = fiv_mod + 1.
    ELSE
        ASSIGN fiv_mod = fiv_mod - 1.
        
    RETURN fiv_mod * {&i_stepcount{&i_level}}.
END FUNCTION.

FUNCTION f-border{&i_level}    RETURNS LOGICAL (INPUT iip_extent  AS INTEGER):
    DEFINE VARIABLE fcv_tl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fcv_tr  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fcv_bl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fcv_br  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fcv_ms  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fcv_fs  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fcv_fl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fiv_int{&i_level} AS INTEGER   NO-UNDO.
    
    /* Dont have special chars for unix display yet */
    IF OPSYS = 'UNIX' THEN RETURN TRUE.
    
    /* Win32 Special Chars */
    ASSIGN fcv_tl = CHR(201)
           fcv_tr = CHR(187)
           fcv_bl = CHR(200)
           fcv_br = CHR(188)
           fcv_ms = CHR(186)
           fcv_fs = CHR(205)
           fcv_fl = CHR(176).

    
    DO fiv_int{&i_level} = 1 TO iip_extent:
        IF fiv_int{&i_level} = 1 OR fiv_int{&i_level} = iip_extent THEN
        DO:
            ASSIGN cv_disp{&i_level}[fiv_int{&i_level}] = REPLACE(cv_disp{&i_level}[fiv_int{&i_level}],'#',fcv_fs) NO-ERROR.
            IF fiv_int{&i_level} = 1 THEN
                ASSIGN SUBSTRING(cv_disp{&i_level}[fiv_int{&i_level}],1,1)                        = fcv_tl
                       SUBSTRING(cv_disp{&i_level}[fiv_int{&i_level}],LENGTH(cv_disp{&i_level}[fiv_int{&i_level}]),1) = fcv_tr.
            IF fiv_int{&i_level} = iip_extent THEN
                ASSIGN SUBSTRING(cv_disp{&i_level}[fiv_int{&i_level}],1,1)                        = fcv_bl
                       SUBSTRING(cv_disp{&i_level}[fiv_int{&i_level}],LENGTH(cv_disp{&i_level}[fiv_int{&i_level}]),1) = fcv_br.
        END.
        ELSE
        DO:
            ASSIGN cv_disp{&i_level}[fiv_int{&i_level}]                                       = REPLACE(cv_disp{&i_level}[fiv_int{&i_level}],'#',fcv_fl)
                   SUBSTRING(cv_disp{&i_level}[fiv_int{&i_level}],1,1)                        = fcv_ms
                   SUBSTRING(cv_disp{&i_level}[fiv_int{&i_level}],LENGTH(cv_disp{&i_level}[fiv_int{&i_level}]),1) = fcv_ms.
        END.
    END.
    
    IF ERROR-STATUS:ERROR THEN
    DO:
        f-seterror{&i_level}(ERROR-STATUS:GET-MESSAGE[1]).
        RETURN FALSE.
    END.
    RETURN TRUE.
END FUNCTION.

FUNCTION f-setmap{&i_level}    RETURNS LOGICAL (INPUT cip_maplist AS CHARACTER):
    /*Rest of initial setup meant to be placed in functions for library cleanup*/
END FUNCTION.

FUNCTION f-complete{&i_level}  RETURNS LOGICAL ():
    ASSIGN lv_complete{&i_level} = TRUE.
    RETURN lv_complete{&i_level}.
END FUNCTION.

FUNCTION f-clearData{&i_level} RETURNS LOGICAL ():
    EMPTY TEMP-TABLE tt-grid{&i_level}.
    EMPTY TEMP-TABLE tt-obj{&i_level}.
    EMPTY TEMP-TABLE tt-inv{&i_level}.
END FUNCTION.

FUNCTION f-init{&i_level}      RETURNS LOGICAL ():
    //IF NOT lv_complete{&i_level} THEN RETURN lv_complete{&i_level}.
    
    f-clearData{&i_level}().
    
    ASSIGN cv_alpha{&i_level} = 
        'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z' + ',' +
        'a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z' + ',' +
        'BACKSPACE'.
    
    /* Create Map Below - Size 9 by 14 chars 
    ========================================
    - To make larger, increase array to
    number of lines added by character
    width (i.e. 9 lines by 14 chars) */
    ASSIGN cv_list{&i_level} = {&c_mapList{&i_level}}
           iv_int{&i_level}[2] = num-entries(cv_list{&i_level})
           cv_tlist{&i_level}  = "#, ,|,_,k,@".
           cv_tvals{&i_level}  = "BLOCK,SPACE,DOOR,DOOR,KEY,ENDLEVEL".

    /* Assigning array of character lines to display map */
    DO iv_int{&i_level}[1] = 1 TO iv_int{&i_level}[2]
    WHILE iv_int{&i_level}[1] <= iv_int{&i_level}[2]:
        ASSIGN cv_disp{&i_level}[iv_int{&i_level}[1]] = ENTRY(iv_int{&i_level}[1],cv_list{&i_level}).
    END.
    
    /* Creating temp table of characters in map grid */
    ASSIGN iv_int{&i_level}[2] = LENGTH(cv_list{&i_level})
           iv_row{&i_level}    = 1.
    
    DO iv_int{&i_level}[1] = 1 TO iv_int{&i_level}[2]
    WHILE iv_int{&i_level}[1] <= iv_int{&i_level}[2]:
        ASSIGN iv_col{&i_level}  = iv_col{&i_level} + 1
               cv_char{&i_level} = SUBSTRING(cv_list{&i_level},iv_int{&i_level}[1],1).
        IF cv_char{&i_level} = "," THEN
            ASSIGN iv_col{&i_level} = 0
                   iv_row{&i_level} = iv_row{&i_level} + 1.
        ELSE DO:
            IF LOOKUP(cv_char{&i_level},cv_tlist{&i_level}) > 0 THEN
                ASSIGN cv_ttype{&i_level} = ENTRY(LOOKUP(cv_char{&i_level},cv_tlist{&i_level}),cv_tvals{&i_level}).
            ELSE
                ASSIGN cv_ttype{&i_level} = "UNDEF".
            
            CREATE tt-grid{&i_level}.
            ASSIGN tt-grid{&i_level}.i_col  = iv_col{&i_level}
                   tt-grid{&i_level}.i_row  = iv_row{&i_level}
                   tt-grid{&i_level}.c_val  = cv_char{&i_level}
                   tt-grid{&i_level}.c_type = cv_ttype{&i_level}
                   tt-grid{&i_level}.i_x    = (iv_col{&i_level} - 1) * {&i_stepcount{&i_level}}
                   tt-grid{&i_level}.i_y    = (iv_row{&i_level} - 1) * {&i_stepcount{&i_level}}.
                   
            IF cv_ttype{&i_level} = "DOOR" THEN
            DO:
                CREATE tt-obj{&i_level}.
                ASSIGN tt-obj{&i_level}.c_type = cv_ttype{&i_level}
                       tt-obj{&i_level}.c_val  = "LOCKED"
                       tt-obj{&i_level}.i_x    = tt-grid{&i_level}.i_x
                       tt-obj{&i_level}.i_y    = tt-grid{&i_level}.i_y
                       tt-obj{&i_level}.i_col  = tt-grid{&i_level}.i_col
                       tt-obj{&i_level}.i_row  = tt-grid{&i_level}.i_row.
                       
                CREATE tt-obj{&i_level}.
                ASSIGN tt-obj{&i_level}.c_type  = "KEY"
                       tt-obj{&i_level}.c_val   = "DOOR"
                       tt-obj{&i_level}.i_col   = tt-grid{&i_level}.i_col
                       tt-obj{&i_level}.i_row   = tt-grid{&i_level}.i_row.
            END.
        END.
    END.
    
    /* Placing actor in initial blank space in map grid */
    FIND FIRST tt-grid{&i_level}
         WHERE tt-grid{&i_level}.c_val = ' '
         NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-grid{&i_level} THEN
    DO:
        f-message{&i_level}('Error finding first empty cell','E',FALSE).
        RETURN FALSE.
    END.
    
    ASSIGN cv_mini{&i_level}:X IN FRAME f-map{&i_level} = tt-grid{&i_level}.i_x
           cv_mini{&i_level}:Y IN FRAME f-map{&i_level} = tt-grid{&i_level}.i_y.
           
    /*ETIME(YES).*/
    FOR EACH  tt-obj{&i_level}
        WHERE tt-obj{&i_level}.c_type = "KEY"
        AND   tt-obj{&i_level}.c_val  = "DOOR"
        NO-LOCK:
        
        KEY-BLK:
        REPEAT:
            /* Take random value from i_stepcount to door col range - 1
            ------------------------------------------------------------------------
               (-1 is used due to grid characteristics; i.e. row 1 col 1 is position 
                x:0,y:0; so in theory: (col - 1) * i_stepcount will give you correct
                coordinate conversion).
            -----------------------------------------------------------------------*/
            ASSIGN iv_rx{&i_level} = RANDOM({&i_stepcount{&i_level}} - 1,(tt-obj{&i_level}.i_col - 1) * {&i_stepcount{&i_level}})
                   iv_ry{&i_level} = RANDOM({&i_stepcount{&i_level}} - 1,(tt-obj{&i_level}.i_row - 1) * {&i_stepcount{&i_level}})
                   iv_rx{&i_level} = f-roundnum{&i_level}(iv_rx{&i_level})
                   iv_ry{&i_level} = f-roundnum{&i_level}(iv_ry{&i_level}).
        
            FIND FIRST tt-grid{&i_level}
                 WHERE tt-grid{&i_level}.c_val = ' '
                 AND   tt-grid{&i_level}.i_x   = iv_rx{&i_level}
                 AND   tt-grid{&i_level}.i_y   = iv_ry{&i_level}
                 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-grid{&i_level} THEN
                NEXT KEY-BLK.
            
            IF  tt-grid{&i_level}.i_x = cv_mini{&i_level}:X
            AND tt-grid{&i_level}.i_y = cv_mini{&i_level}:Y THEN
                NEXT KEY-BLK.
            
            IF  tt-grid{&i_level}.i_row >= tt-obj{&i_level}.i_row
            AND tt-grid{&i_level}.i_col >= tt-obj{&i_level}.i_col THEN
                NEXT KEY-BLK.
            
            ASSIGN tt-obj{&i_level}.i_x = tt-grid{&i_level}.i_x
                   tt-obj{&i_level}.i_y = tt-grid{&i_level}.i_y.
                   
            f-paint{&i_level}(tt-grid{&i_level}.i_row,tt-grid{&i_level}.i_col,"k").
                   
            LEAVE KEY-BLK.
        END.
    END.
    
    /*=================== - Test Random Spawn of Key(s) - =========================
    f-message{&i_level}(substitute('etime:&1 - etime secs: &2',etime,etime / 1000),'a',false).
    ===============================================================================*/
    
    /* Set End Level  - can be regulated to randomization */
    /*f-paint{&i_level}(5,16,"@").*/
    
    f-border{&i_level}(EXTENT(cv_disp{&i_level})).
    
    ASSIGN lv_complete{&i_level} = FALSE.
    ETIME(YES).
    RETURN TRUE.
END FUNCTION.