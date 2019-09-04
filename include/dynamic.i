&IF DEFINED(DYNAMIC_LIB) = 0 &THEN
    &GLOBAL-DEFINE DYNAMIC_LIB DYNAMIC_LIB

    &IF DEFINED(CONTROL_LIB) = 0 &THEN
        &GLOBAL-DEFINE P1-UP    "W"
        &GLOBAL-DEFINE P1-DOWN  "S"
        &GLOBAL-DEFINE P1-LEFT  "A"
        &GLOBAL-DEFINE P1-RIGHT "D"
        &GLOBAL-DEFINE P2-UP    "CURSOR-UP"
        &GLOBAL-DEFINE P2-DOWN  "CURSOR-DOWN"
        &GLOBAL-DEFINE P2-LEFT  "CURSOR-LEFT"
        &GLOBAL-DEFINE P2-RIGHT "CURSOR-RIGHT"
    &ELSE
        &IF DEFINED(P1-UP) = 0 &THEN
            &GLOBAL-DEFINE P1-UP    'TRUE'
        &ENDIF

        &IF DEFINED(P1-DOWN) = 0 &THEN
            &GLOBAL-DEFINE P1-DOWN  'TRUE'
        &ENDIF

        &IF DEFINED(P1-LEFT) = 0 &THEN
            &GLOBAL-DEFINE P1-LEFT  'TRUE'
        &ENDIF

        &IF DEFINED(P1-RIGHT) = 0 &THEN
            &GLOBAL-DEFINE P1-RIGHT 'TRUE'
        &ENDIF

        &IF DEFINED(P2-UP) = 0 &THEN
            &GLOBAL-DEFINE P2-UP    'TRUE'
        &ENDIF

        &IF DEFINED(P2-DOWN) = 0 &THEN
            &GLOBAL-DEFINE P2-DOWN  'TRUE'
        &ENDIF

        &IF DEFINED(P2-LEFT) = 0 &THEN
            &GLOBAL-DEFINE P2-LEFT  'TRUE'
        &ENDIF

        &IF DEFINED(P2-RIGHT) = 0 &THEN
            &GLOBAL-DEFINE P2-RIGHT 'TRUE'
        &ENDIF
    &ENDIF

    &IF DEFINED(P1-ADD) &THEN
        &GLOBAL-DEFINE P1-SEP ,
    &ELSE
        &GLOBAL-DEFINE P1-SEP
    &ENDIF

    &GLOBAL-DEFINE P1-CTRLS  ~
        {&P1-UP}    + "," +  ~
        {&P1-DOWN}  + "," +  ~
        {&P1-LEFT}  + "," +  ~
        {&P1-RIGHT} +        ~
        "{&P1-SEP}{&P1-ADD}" ~

    &IF DEFINED(P2-ADD) &THEN
        &GLOBAL-DEFINE P2-SEP ,
    &ELSE
        &GLOBAL-DEFINE P2-SEP
    &ENDIF

    &GLOBAL-DEFINE P2-CTRLS  ~
        {&P2-UP}    + "," +  ~
        {&P2-DOWN}  + "," +  ~
        {&P2-LEFT}  + "," +  ~
        {&P2-RIGHT} +        ~
        "{&P2-SEP}{&P2-ADD}" ~

    &IF DEFINED(ADD-UP-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE ADD-UP-CTRLS ""
        &GLOBAL-DEFINE ADD-UP-SEP   ""
    &ELSE
        &GLOBAL-DEFINE ADD-UP-SEP ","
    &ENDIF

    &IF DEFINED(UP-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE UP-CTRLS        ~
            {&P1-UP} + "," +           ~
            {&P2-UP} + {&ADD-UP-SEP} + ~
            {&ADD-UP-CTRLS}            ~

    &ENDIF
    &IF DEFINED(ADD-DOWN-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE ADD-DOWN-CTRLS ""
        &GLOBAL-DEFINE ADD-DOWN-SEP   ""
    &ELSE
        &GLOBAL-DEFINE ADD-DOWN-SEP ","
    &ENDIF

    &IF DEFINED(DOWN-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE DOWN-CTRLS          ~
            {&P1-DOWN} + "," +             ~
            {&P2-DOWN} + {&ADD-DOWN-SEP} + ~
            {&ADD-DOWN-CTRLS}              ~

    &ENDIF

    &IF DEFINED(ADD-LEFT-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE ADD-LEFT-CTRLS ""
        &GLOBAL-DEFINE ADD-LEFT-SEP   ""
    &ELSE
        &GLOBAL-DEFINE ADD-LEFT-SEP ","
    &ENDIF

    &IF DEFINED(LEFT-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE LEFT-CTRLS          ~
            {&P1-LEFT} + "," +             ~
            {&P2-LEFT} + {&ADD-LEFT-SEP} + ~
            {&ADD-LEFT-CTRLS}              ~

    &ENDIF

    &IF DEFINED(ADD-RIGHT-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE ADD-RIGHT-CTRLS ""
        &GLOBAL-DEFINE ADD-RIGHT-SEP   ""
    &ELSE
        &GLOBAL-DEFINE ADD-RIGHT-SEP ","
    &ENDIF

    &IF DEFINED(RIGHT-CTRLS) = 0 &THEN
        &GLOBAL-DEFINE RIGHT-CTRLS           ~
            {&P1-RIGHT} + "," +              ~
            {&P2-RIGHT} + {&ADD-RIGHT-SEP} + ~
            {&ADD-RIGHT-CTRLS}               ~

    &ENDIF

    {standard.i}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "moveMe"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT hip_handle AS HANDLE,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER):"}

        IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.
        ASSIGN hip_handle:COL = iip_x
               hip_handle:ROW = iip_y
        NO-ERROR.
        RETURN ERROR-STATUS:ERROR = FALSE.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "moveMeDir"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT hip_handle AS HANDLE,
                         INPUT cip_dir    AS CHARACTER):"}

        IF NOT VALID-HANDLE(hip_handle) THEN RETURN FALSE.

        IF LOOKUP(cip_dir,{&UP-CTRLS})    > 0 THEN
            ASSIGN cip_dir = "UP".
        ELSE
        IF LOOKUP(cip_dir,{&DOWN-CTRLS})  > 0 THEN
            ASSIGN cip_dir = "DOWN".
        ELSE
        IF LOOKUP(cip_dir,{&LEFT-CTRLS})  > 0 THEN
            ASSIGN cip_dir = "LEFT".
        ELSE
        IF LOOKUP(cip_dir,{&RIGHT-CTRLS}) > 0 THEN
            ASSIGN cip_dir = "RIGHT".
        ELSE
            ASSIGN cip_dir = "UNKOWN".

        CASE cip_dir:
            WHEN "UP"    THEN
            DO:
                ASSIGN hip_handle:ROW = hip_handle:ROW - 1
                NO-ERROR.
            END.
            WHEN "DOWN"  THEN
            DO:
                ASSIGN hip_handle:ROW = hip_handle:ROW + 1
                NO-ERROR.
            END.
            WHEN "LEFT"  THEN
            DO:
                ASSIGN hip_handle:COL = hip_handle:COL - 1
                NO-ERROR.
            END.
            WHEN "RIGHT" THEN
            DO:
                ASSIGN hip_handle:COL = hip_handle:COL + 1
                NO-ERROR.
            END.
            OTHERWISE RETURN FALSE.
        END CASE.
        RETURN ERROR-STATUS:ERROR = FALSE.
    {footer.i &METHOD_END = {&METHOD_END}}
&ENDIF
