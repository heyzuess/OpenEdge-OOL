&IF DEFINED(GTK_PROCS) = 0 &THEN
    &GLOBAL-DEFINE GTK_PROCS
    &GLOBAL-DEFINE GTK_PATH C:\GTK\
    &GLOBAL-DEFINE GTK_TEMP "{&GTK_PATH}new.c"

    &SCOPED-DEFINE GTK_WINDOW_TITLE  XXX_WINDOW_TITLE_XXX
    &SCOPED-DEFINE GTK_WINDOW_WIDTH  XXX_WINDOW_WIDTH_XXX
    &SCOPED-DEFINE GTK_WINDOW_HEIGHT XXX_WINDOW_HEIGHT_XXX

    {standard.i}

    IF INDEX(PROPATH,"{&GTK_PATH}") = 0 THEN
        ASSIGN PROPATH = PROPATH + "," + "{&GTK_PATH}".

    DEFINE VARIABLE gtk_c_temp AS CHARACTER NO-UNDO.
    DEFINE VARIABLE gtk_i_temp AS INTEGER   NO-UNDO.

    DEFINE TEMP-TABLE gtkTempFile
        FIELD c_name AS CHARACTER
        FIELD c_exe  AS CHARACTER
        FIELD l_comp AS LOGICAL
        FIELD i_idx  AS INTEGER.

    DEFINE TEMP-TABLE gtkTempData
        FIELD i_idx  AS INTEGER
        FIELD i_line AS INTEGER
        FIELD c_line AS CHARACTER.
    /*
    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "testGTK"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "():"}
        MESSAGE 'A' VIEW-AS ALERT-BOX.
    {footer.i &METHOD_END = {&METHOD_END}}*/

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_newFile"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT cip_filename AS CHARACTER):"}
        EMPTY TEMP-TABLE gtkTempFile.
        EMPTY TEMP-TABLE gtkTempData.
        ASSIGN gtk_i_temp = 0.

        IF cip_filename = ""            THEN RETURN FALSE.
        IF INDEX(cip_filename,".c") = 0 THEN RETURN FALSE.

        FOR FIRST gtkTempFile
            WHERE gtkTempFile.c_name = cip_filename:
        END.
        IF AVAILABLE gtkTempFile THEN RETURN FALSE.

        FOR LAST gtkTempFile:
            ASSIGN gtk_i_temp = gtkTempFile.i_idx.
        END.

        CREATE gtkTempFile.
        ASSIGN gtkTempFile.c_name = cip_filename
               gtkTempFile.i_idx  = gtk_i_temp + 1
               gtk_i_temp         = 0.

        INPUT FROM VALUE({&GTK_TEMP}).
            REPEAT WHILE TRUE:
                CREATE gtkTempData.
                IMPORT UNFORMATTED gtkTempData.c_line.

                ASSIGN gtk_i_temp         = gtk_i_temp + 1
                       gtkTempData.i_idx  = gtkTempFile.i_idx
                       gtkTempData.i_line = gtk_i_temp.
            END.
        INPUT CLOSE.

        FOR FIRST gtkTempData
            WHERE gtkTempData.i_idx = gtkTempFile.i_idx:
        END.
        RETURN AVAILABLE gtkTempData.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_setTitle"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT cip_title AS CHARACTER):"}
        FOR FIRST gtkTempFile,
            EACH  gtkTempData
            WHERE gtkTempData.i_idx = gtkTempFile.i_idx
            BY    gtkTempData.i_line:

            IF INDEX(gtkTempData.c_line,"{&GTK_WINDOW_TITLE}") > 0 THEN
            DO:
                ASSIGN gtkTempData.c_line = REPLACE(gtkTempData.c_line,"{&GTK_WINDOW_TITLE}",cip_title).
                RETURN INDEX(gtkTempData.c_line,cip_title) > 0.
            END.
        END.
        RETURN FALSE.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_setWidth"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT iip_w AS INTEGER):"}
        FOR FIRST gtkTempFile,
            EACH  gtkTempData
            WHERE gtkTempData.i_idx = gtkTempFile.i_idx
            BY    gtkTempData.i_line:

            IF INDEX(gtkTempData.c_line,"{&GTK_WINDOW_WIDTH}") > 0 THEN
            DO:
                ASSIGN gtkTempData.c_line = REPLACE(gtkTempData.c_line,"{&GTK_WINDOW_WIDTH}",STRING(iip_w)).
                RETURN INDEX(gtkTempData.c_line,STRING(iip_w)) > 0.
            END.
        END.
        RETURN FALSE.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_setHeight"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT iip_h AS INTEGER):"}
        FOR FIRST gtkTempFile,
            EACH  gtkTempData
            WHERE gtkTempData.i_idx = gtkTempFile.i_idx
            BY    gtkTempData.i_line:

            IF INDEX(gtkTempData.c_line,"{&GTK_WINDOW_HEIGHT}") > 0 THEN
            DO:
                ASSIGN gtkTempData.c_line = REPLACE(gtkTempData.c_line,"{&GTK_WINDOW_HEIGHT}",STRING(iip_h)).
                RETURN INDEX(gtkTempData.c_line,STRING(iip_h)) > 0.
            END.
        END.
        RETURN FALSE.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_writeFile"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "():"}
        FOR FIRST gtkTempFile:
        END.
        IF NOT AVAILABLE gtkTempFile THEN RETURN FALSE.

        IF SEARCH(gtkTempFile.c_name) <> ? THEN OS-COMMAND SILENT VALUE("del " + gtkTempFile.c_name).

        OUTPUT TO VALUE(gtkTempFile.c_name).
            FOR EACH gtkTempData BY gtkTempData.i_line:
                PUT UNFORMATTED gtkTempData.c_line CHR(13).
            END.
        OUTPUT CLOSE.

        RETURN SEARCH(gtkTempFile.c_name) <> ?.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_compileFile"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "(INPUT cip_exe AS CHARACTER):"}
        ASSIGN gtk_c_temp = "".

        FOR FIRST gtkTempFile:
        END.
        IF NOT AVAILABLE gtkTempFile THEN RETURN FALSE.

        IF SEARCH(gtkTempFile.c_name) = ? THEN RETURN FALSE.

        IF cip_exe = "" THEN ASSIGN cip_exe = REPLACE(gtkTempFile.c_name,".c",".exe").

        ASSIGN gtk_c_temp = 'gcc &1 -o &2 `pkg-config --cflags --libs gtk+-3.0`'
               gtk_c_temp = SUBSTITUTE(gtk_c_temp,gtkTempFile.c_name,cip_exe).

        OS-COMMAND SILENT VALUE(gtk_c_temp).

        IF SEARCH(cip_exe) <> ? THEN
            ASSIGN gtkTempFile.c_exe  = cip_exe
                   gtkTempFile.l_comp = TRUE.

        RETURN gtkTempFile.l_comp.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_runFile"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "():"}
        FOR FIRST gtkTempFile:
        END.
        IF NOT AVAILABLE gtkTempFile THEN RETURN FALSE.

        IF SEARCH(gtkTempFile.c_name) = ? THEN RETURN FALSE.

        IF SEARCH(gtkTempFile.c_exe)  = ? THEN RETURN FALSE.

        IF NOT gtkTempFile.l_comp THEN RETURN FALSE.

        OS-COMMAND SILENT VALUE(gtkTempFile.c_exe).

        RETURN gtkTempFile.l_comp.
    {footer.i &METHOD_END = {&METHOD_END}}

    {header.i
        &METHOD_LIB  = {&METHOD_LIB}
        &METHOD_PREF = {&PREF_LOG}
        &METHOD_NAME = "GTK_clearFile"
        &METHOD_POST = {&POST_LOG}
        &METHOD_EXT  = "():"}
        FOR FIRST gtkTempFile:
        END.
        IF NOT AVAILABLE gtkTempFile THEN RETURN FALSE.

        EMPTY TEMP-TABLE gtkTempFile.
        EMPTY TEMP-TABLE gtkTempData.

        RETURN TRUE.
    {footer.i &METHOD_END = {&METHOD_END}}
&ENDIF