&IF DEFINED(ERR_LIB) = 0 &THEN
    &GLOBAL-DEFINE ERR_LIB

    &IF DEFINED(CLASS_LIBRARY) > 0 &THEN
        &GLOBAL-DEFINE PROP_PREF DEFINE PUBLIC PROPERTY
        &GLOBAL-DEFINE PROP_POST GET. PRIVATE SET.
        &GLOBAL-DEFINE PRIV_PREF DEFINE PRIVATE VARIABLE
        &GLOBAL-DEFINE PRIV_POST NO-UNDO.

        {standard.i &METHOD_LIBRARY = *}
    &ELSE
        &GLOBAL-DEFINE PROP_PREF DEFINE VARIABLE
        &GLOBAL-DEFINE PROP_POST NO-UNDO.
        &GLOBAL-DEFINE PRIV_PREF DEFINE VARIABLE
        &GLOBAL-DEFINE PRIV_POST NO-UNDO.

        {standard.i}
    &ENDIF

    &IF DEFINED(NO_ERR_TABLE) = 0 &THEN
        DEFINE TEMP-TABLE cErr
            FIELD i_idx AS INTEGER
            FIELD c_err AS CHARACTER.
    &ENDIF

    {&PROP_PREF} i_err    AS INTEGER {&PROP_POST}
    {&PROP_PREF} l_err    AS LOGICAL {&PROP_POST}

    {&PRIV_PREF} i_errIdx AS INTEGER {&PRIV_POST}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_VOID}
        &METHOD_NAME = "setErr"
        &METHOD_POST = {&POST_VOID}
        &METHOD_EXT  = "(INPUT cip_err AS CHARACTER):"}

        FOR LAST cErr:
            ASSIGN i_errIdx = cErr.i_idx.
        END.

        CREATE cErr.
        ASSIGN i_err      = i_err + 1
               l_err      = TRUE
               cErr.i_idx = i_errIdx + 1
               cErr.c_err = cip_err.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_CHR}
        &METHOD_NAME = "getErr"
        &METHOD_POST = {&POST_CHR}
        &METHOD_EXT  = "(INPUT iip_idx AS INTEGER):"}

        FOR FIRST cErr
            WHERE cErr.i_idx = iip_idx:
            RETURN cErr.c_err.
        END.
        RETURN "".
    {footer.i &METHOD_END = {&METHOD_END}}
&ENDIF