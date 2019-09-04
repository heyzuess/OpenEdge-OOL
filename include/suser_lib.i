&IF DEFINED(SUSER_LIB) = 0 &THEN
    &GLOBAL-DEFINE SUSER_LIB

    {standard.i}

    &IF DEFINED(METHOD_LIBRARY) = 0 &THEN
        &SCOPED-DEFINE STYPE NEW GLOBAL SHARED
    &ELSE
        &SCOPED-DEFINE STYPE
    &ENDIF

    DEFINE {&STYPE} TEMP-TABLE sUser
        FIELD c_login   AS CHARACTER
        FIELD c_name    AS CHARACTER
        FIELD c_sname   AS CHARACTER
        FIELD c_pw      AS CHARACTER
        FIELD d_lastIn  AS DATETIME
        FIELD d_lastOut AS DATETIME
        FIELD i_lastIn  AS INTEGER
        FIELD i_lastOut AS INTEGER
        FIELD i_uid     AS INTEGER
        FIELD l_log     AS LOGICAL.

    DEFINE {&STYPE} VARIABLE i_uid AS INTEGER INIT ?.

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "sMakeUser"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT cip_login AS CHARACTER,
                         INPUT cip_name  AS CHARACTER,
                         INPUT cip_sname AS CHARACTER,
                         INPUT cip_pw    AS CHARACTER,
                         INPUT iip_uid   AS INTEGER):"}
        FOR FIRST sUser
            WHERE sUser.i_uid = iip_uid:
        END.
        IF AVAILABLE sUser THEN RETURN FALSE.

        CREATE sUser.
        ASSIGN sUser.i_uid   = iip_uid
               sUser.c_login = cip_login
               sUser.c_name  = cip_name
               sUser.c_sname = cip_sname
               sUser.c_pw    = cip_pw.

        RETURN AVAILABLE sUser.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "sLogUser"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT iip_uid   AS INTEGER,
                         INPUT lip_log   AS LOGICAL):"}
        FOR FIRST sUser
            WHERE sUser.i_uid = iip_uid:
        END.
        IF sUser.l_log = lip_log THEN RETURN FALSE.

        ASSIGN sUser.d_lastIn  = IF lip_log THEN TODAY ELSE ?
               sUser.i_lastIn  = IF lip_log THEN TIME  ELSE ?
               sUser.d_lastOut = IF lip_log THEN ? ELSE TODAY
               sUser.i_lastOut = IF lip_log THEN ? ELSE TIME
               sUser.l_log     = lip_log.

        RETURN sUser.l_log = lip_log.
    {footer.i &METHOD_END = {&METHOD_END}}
&ENDIF