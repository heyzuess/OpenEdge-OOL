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
    DEFINE PRIVATE VARIABLE l_construct AS LOGICAL   NO-UNDO.
    DEFINE PRIVATE VARIABLE a_actorList AS ACTOR     NO-UNDO EXTENT.
    
    DEFINE TEMP-TABLE control_actor
        FIELD h_handle AS HANDLE
        FIELD c_name   AS CHARACTER
        FIELD c_val    AS CHARACTER
        FIELD c_ref    AS CHARACTER
        FIELD c_form   AS CHARACTER
        FIELD i_ref    AS INTEGER.
    
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
                     
        RETURN VALID-HANDLE(control_actor.h_handle).
    END METHOD.
    
    METHOD PUBLIC LOG addActorButton(INPUT cip_aname  AS CHARACTER,
                                     INPUT cip_button AS CHARACTER,
                                     INPUT cip_dir    AS CHARACTER,
                                     INPUT cip_val    AS CHARACTER):
        IF NOT l_construct THEN RETURN l_construct.
        
        FIND FIRST control_actor
             WHERE control_actor.c_name = cip_aname
             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE control_actor THEN RETURN FALSE.
        
        RETURN a_actorList[control_actor.i_ref]:setupButton(cip_button,cip_dir,cip_val).
    END METHOD.
    
    METHOD PUBLIC LOG Display():
        IF NOT l_construct THEN RETURN l_construct.
        
        MAIN-BLK:
        REPEAT:
          VIEW h_handle.
          READKEY PAUSE 0.
          ASSIGN c_lastKey = KEY-FUNC(LAST-KEY).
          IF c_lastKey = "END-ERROR" THEN LEAVE MAIN-BLK.
          FOR EACH control_actor:
              a_actorList[control_actor.i_ref]:readKey(c_lastKey).
          END.
        END.
        
        RETURN l_construct.
    END METHOD.
    
    METHOD PUBLIC LOG regActorSet():
        RETURN setActorList().
    END METHOD.
    
    METHOD PRIVATE LOG setActorList():
        IF l_construct THEN RETURN l_construct.
        	
        ASSIGN EXTENT(a_actorList) = i_numRef
               l_construct         = TRUE.
        
        FOR EACH control_actor:
            a_actorList[control_actor.i_ref] = NEW ACTOR().
            ASSIGN a_actorList[control_actor.i_ref]:c_name   = control_actor.c_name
                   a_actorList[control_actor.i_ref]:h_handle = control_actor.h_handle.
                   
            a_actorList[control_actor.i_ref]:setScreenVal(control_actor.c_val).
            IF l_construct THEN ASSIGN l_construct = VALID-HANDLE(a_actorList[control_actor.i_ref]:h_handle).
        END.
        
        RETURN l_construct.
    END METHOD.
END CLASS.