USING Progress.Lang.*.
CLASS ACTORDISPLAY:   
    DEFINE PUBLIC  PROPERTY h_handle  AS HANDLE    NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_name    AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY c_title   AS CHARACTER NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_w       AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_h       AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_x       AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    DEFINE PUBLIC  PROPERTY i_y       AS INTEGER   NO-UNDO GET. PUBLIC  SET.
    
    DEFINE PRIVATE VARIABLE c_lastKey   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_numRef    AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempX     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE i_tempY     AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE a_currActor AS ACTOR     NO-UNDO.
    DEFINE PRIVATE VARIABLE a_buffActor AS ACTOR     NO-UNDO.
    
    DEFINE TEMP-TABLE control_actor
        FIELD h_handle AS HANDLE
        FIELD a_actor  AS CLASS OBJECT
        FIELD c_name   AS CHARACTER
        FIELD c_val    AS CHARACTER
        FIELD c_ref    AS CHARACTER
        FIELD c_form   AS CHARACTER
        FIELD i_ref    AS INTEGER.
        
    DEFINE TEMP-TABLE control_button
        FIELD c_owner  AS CHARACTER
        FIELD c_button AS CHARACTER
        FIELD c_type   AS CHARACTER
        FIELD c_val    AS CHARACTER.
        
    DEFINE BUFFER b-control_actor FOR control_actor.
    
    CONSTRUCTOR PUBLIC ACTORDISPLAY(INPUT cip_name  AS CHARACTER,
                                    INPUT cip_title AS CHARACTER,
                                    INPUT iip_w     AS INTEGER,
                                    INPUT iip_h     AS INTEGER,
                                    INPUT iip_x     AS INTEGER,
                                    INPUT iip_y     AS INTEGER):
        ASSIGN c_name  = cip_name
               c_title = cip_title
               i_w     = iip_w
               i_h     = iip_h
               i_x     = iip_x
               i_y     = iip_y.
        
        CREATE FRAME h_handle
            ASSIGN WIDTH-CHARS  = i_w
                   HEIGHT-CHARS = i_h
                   X            = i_x
                   Y            = i_y
                   HIDDEN       = TRUE
                   TITLE        = c_title.
    END CONSTRUCTOR.
    
    DESTRUCTOR  PUBLIC ACTORDISPLAY():
        EMPTY TEMP-TABLE control_actor.
        EMPTY TEMP-TABLE control_button.
    END DESTRUCTOR.
    
    METHOD PUBLIC LOG addActor(INPUT cip_name   AS CHARACTER,
                               INPUT cip_val    AS CHARACTER,
                               INPUT cip_format AS CHARACTER,
                               INPUT iip_x      AS INTEGER,
                               INPUT iip_y      AS INTEGER):
        FIND FIRST control_actor
             WHERE control_actor.c_name = cip_name
             NO-LOCK NO-ERROR.
        IF AVAILABLE control_actor THEN RETURN FALSE.
        	
        CREATE control_actor.
        ASSIGN i_numRef               = i_numRef + 1
               control_actor.c_name   = cip_name
               control_actor.c_val    = cip_val
               control_actor.c_form   = cip_format
               control_actor.c_ref    = SUBSTITUTE("Actor&1",i_numRef)
               control_actor.i_ref    = i_numRef.
        
        CREATE TEXT control_actor.h_handle
            ASSIGN SCREEN-VALUE = control_actor.c_val
                   FORMAT       = control_actor.c_form
                   FRAME        = h_handle
                   X            = iip_x
                   Y            = iip_y.
                   
        control_actor.a_actor = NEW ACTOR().
        a_currActor           = CAST(control_actor.a_actor,ACTOR).
        
        ASSIGN a_currActor:c_name   = control_actor.c_name
               a_currActor:h_handle = control_actor.h_handle.

        a_currActor:setScreenVal(control_actor.c_val).

        RETURN VALID-HANDLE(control_actor.h_handle).
    END METHOD.
    
    METHOD PUBLIC LOG addActorButton(INPUT cip_aname  AS CHARACTER,
                                     INPUT cip_button AS CHARACTER,
                                     INPUT cip_dir    AS CHARACTER,
                                     INPUT cip_val    AS CHARACTER):
        FIND FIRST control_actor
             WHERE control_actor.c_name = cip_aname
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE control_actor THEN RETURN FALSE.
        
        ASSIGN a_currActor = CAST(control_actor.a_actor,ACTOR).
        IF a_currActor:setupButton(cip_button,cip_dir,cip_val) THEN
        DO:
            CREATE control_button.
            ASSIGN control_button.c_owner  = cip_aname
                   control_button.c_button = cip_button
                   control_button.c_type   = cip_dir
                   control_button.c_val    = cip_val.
            
            RETURN TRUE.
        END.
        ELSE RETURN FALSE.
    END METHOD.
    
    METHOD PUBLIC LOG Display():
        MAIN-BLK:
        REPEAT:
          VIEW h_handle.
          READKEY PAUSE 0.
          ASSIGN c_lastKey = KEY-FUNC(LAST-KEY).
          IF c_lastKey = "END-ERROR" THEN LEAVE MAIN-BLK.
          FOR EACH control_actor:
              a_currActor = CAST(control_actor.a_actor,ACTOR).
              a_currActor:readKey(c_lastKey).
          END.
        END.
        
        RETURN TRUE.
    END METHOD.
    
    METHOD PRIVATE LOG detectCollision(INPUT cip_lastkey AS CHARACTER):
        FOR EACH  control_button
            WHERE control_button.c_button = cip_lastkey
            NO-LOCK,
            EACH  control_actor
            WHERE control_actor.c_name = control_button.c_owner
            NO-LOCK:
            a_currActor = CAST(control_actor.a_actor,ACTOR).
            ASSIGN i_tempX = (INTEGER(ENTRY(1,control_button.c_val)) * a_currActor.i_step) + a_currActor.i_x
                   i_tempY = (INTEGER(ENTRY(2,control_button.c_val)) * a_currActor.i_step) + a_currActor.i_y.
                   
            FOR EACH  b-control_actor
                WHERE b-control_actor.c_name <> control_actor.c_name
                NO-LOCK:
                
                
            END.
        END.
        
        RETURN FALSE.
    END METHOD.
END CLASS.