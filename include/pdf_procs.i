&IF DEFINED(PDF_LIB) = 0 &THEN
    &SCOPED-DEFINE PDF_LIB
    &IF DEFINED(PDF_SCOPE) = 0 &THEN
        &SCOPED-DEFINE PDF_SCOPE
    &ENDIF

    DEFINE VARIABLE iPageno_ AS INTEGER NO-UNDO.
    DEFINE VARIABLE iError_  AS INTEGER NO-UNDO.
    DEFINE VARIABLE lError_  AS LOGICAL NO-UNDO.

    DEFINE {&PDF_SCOPE} TEMP-TABLE pdfTEMP
        FIELD i_pageno AS INTEGER
        FIELD c_file   AS CHARACTER
        FIELD c_pdf    AS CHARACTER.

    DEFINE {&PDF_SCOPE} TEMP-TABLE pdfMerge
        FIELD c_file   AS CHARACTER
        FIELD c_list   AS CHARACTER.

    DEFINE {&PDF_SCOPE} TEMP-TABLE pdfError
        FIELD i_idx    AS INTEGER
        FIELD c_error  AS CHARACTER.

    FUNCTION PDF_ISERROR   RETURNS LOGICAL ()                            FORWARD.

    FUNCTION PDF_NUM_ERR   RETURNS INTEGER ()                            FORWARD.

    FUNCTION PDF_GET_ERR   RETURNS CHAR    (INPUT X AS INTEGER)          FORWARD.

    FUNCTION PDF_CLR_ERR   RETURNS LOGICAL ()                            FORWARD.

    FUNCTION PDF_ERROR     RETURNS LOGICAL (INPUT X AS CHARACTER)        FORWARD.

    FUNCTION PS2PDF_SCRIPT RETURNS LOGICAL (INPUT        X AS CHARACTER,
                                            INPUT-OUTPUT Y AS CHARACTER) FORWARD.

    FUNCTION PS2PDF_MERGE  RETURNS LOGICAL (INPUT X AS CHARACTER,
                                            INPUT Y AS CHARACTER,
                                            INPUT Z AS LOGICAL)          FORWARD.

    PROCEDURE PS_TO_PDF:
        DEFINE INPUT  PARAMETER cip_filename AS CHARACTER.
        DEFINE OUTPUT PARAMETER cop_pdfFile  AS CHARACTER.

        IF cip_filename = "" THEN
        DO:
            PDF_ERROR("Filename is blank.").
            RETURN.
        END.

        IF SUBSTRING(cip_filename,LENGTH(cip_filename) - 2, 3) <> ".ps" THEN
        DO:
            PDF_ERROR("File must be a postscript file.").
            RETURN.
        END.

        IF SEARCH(cip_filename) = ? THEN
        DO:
            PDF_ERROR("Failed to find file on propath.").
            RETURN.
        END.

        ASSIGN cop_pdfFile = REPLACE(cip_filename,".ps",".pdf").

        IF NOT PS2PDF_SCRIPT(cip_filename,cop_pdfFile) THEN
        DO:
            PDF_ERROR(SUBSTITUTE("Error converting &1 to pdf.",cip_filename)).
            RETURN.
        END.
    END PROCEDURE.

    PROCEDURE PDF_ADD_FILE:
        DEFINE INPUT  PARAMETER cip_filename AS CHARACTER NO-UNDO.

        IF cip_filename = "" THEN
        DO:
            PDF_ERROR("Filename is blank.").
            RETURN.
        END.

        IF SUBSTRING(cip_filename,LENGTH(cip_filename) - 2, 3) <> ".ps" THEN
        DO:
            PDF_ERROR("File must be a postscript file.").
            RETURN.
        END.

        CREATE pdfTEMP.
        ASSIGN iPageno_         = iPageno_ + 1
               pdfTEMP.i_pageNo = iPageno_
               pdfTEMP.c_file   = cip_filename.

        RUN PS_TO_PDF(pdfTEMP.c_file, OUTPUT pdfTEMP.c_pdf).
    END PROCEDURE.

    PROCEDURE PDF_MERGE_ALL_FILES:
        DEFINE INPUT PARAMETER cip_mergefile AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcv_lastPage AS LOGICAL NO-UNDO.

        IF cip_mergefile = "" THEN
        DO:
            PDF_ERROR("Merge file can not be left blank.").
            RETURN.
        END.

        IF NOT TEMP-TABLE pdfTEMP:HAS-RECORDS THEN
        DO:
            PDF_ERROR("No files have been added to memory.").
            RETURN.
        END.

        FOR EACH pdfTEMP BREAK BY pdfTEMP.i_pageNo:
            IF LAST(pdfTEMP.i_pageNo) THEN ASSIGN pcv_lastPage = TRUE.

            IF NOT PS2PDF_MERGE(pdfTEMP.c_pdf,cip_mergefile,pcv_lastPage) THEN
            DO:
                PDF_ERROR(SUBSTITUTE("Error merging pdf file &1.",pdfTEMP.c_pdf)).
                RETURN.
            END.
        END.
    END PROCEDURE.

    PROCEDURE PDF_CLEAR_ALL:
        DEFINE VARIABLE pcv_int AS INTEGER NO-UNDO.

        FOR EACH pdfTEMP:
            IF SEARCH(pdfTEMP.c_pdf) <> ? THEN
                OS-COMMAND SILENT VALUE("rm " + pdfTEMP.c_pdf).

            /*IF SEARCH(pdfTEMP.c_file) <> ? THEN
                OS-COMMAND SILENT VALUE("rm " + pdfTEMP.c_file).*/

            DELETE pdfTEMP.
        END.
        EMPTY TEMP-TABLE pdfTEMP.

        FOR EACH pdfMerge:
            IF SEARCH(pdfMerge.c_file) <> ? THEN
                OS-COMMAND SILENT VALUE("rm " + pdfMerge.c_file).

            DO pcv_int = 1 TO NUM-ENTRIES(pdfMerge.c_list):
                IF SEARCH(ENTRY(pcv_int,pdfMerge.c_list)) <> ? THEN
                    OS-COMMAND SILENT VALUE("rm " + ENTRY(pcv_int,pdfMerge.c_list)).
            END.
        END.
        EMPTY TEMP-TABLE pdfMerge.

        PDF_CLR_ERR().
    END PROCEDURE.

    FUNCTION PDF_ERROR RETURNS LOGICAL (INPUT cip_error AS CHARACTER):
        CREATE pdfError.
        ASSIGN iError_          = iError_ + 1
               lError_          = TRUE
               pdfError.i_idx   = iError_
               pdfError.c_error = cip_error.

        RETURN TRUE.
    END FUNCTION.

    FUNCTION PS2PDF_SCRIPT RETURNS LOGICAL (INPUT        cip_file AS CHARACTER,
                                            INPUT-OUTPUT cip_pdf  AS CHARACTER):
        DEFINE VARIABLE fcv_script AS CHARACTER NO-UNDO.
        DEFINE VARIABLE fcv_ps2pdf AS CHARACTER NO-UNDO INIT
        "gs -q -dSAFER -dNOPAUSE -dBATCH -sOutputFile=&2 -sDEVICE=pdfwrite -c .setpdfwrite -f &1 &3".

        ASSIGN fcv_script = SUBSTITUTE(fcv_ps2pdf,cip_file,cip_pdf,"> /dev/null 2>&1").
        OS-COMMAND SILENT VALUE(fcv_script).

        IF SEARCH(cip_pdf) = ? THEN
        DO:
            ASSIGN cip_pdf = "".
            RETURN FALSE.
        END.

        message "a" search(cip_pdf) cip_pdf view-as alert-box.

        RETURN TRUE.
    END FUNCTION.

    FUNCTION PS2PDF_MERGE  RETURNS LOGICAL (INPUT cip_file  AS CHARACTER,
                                            INPUT cip_merge AS CHARACTER,
                                            INPUT lip_last  AS LOGICAL):
        DEFINE VARIABLE fcv_script   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE fcv_pdfmerge AS CHARACTER NO-UNDO INIT
        "gs -q -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -dAutoRotatePages=/None -sOutputFile=&1 &2".

        FOR FIRST pdfMerge
            WHERE pdfMerge.c_file = cip_merge:
        END.
        IF NOT AVAILABLE pdfMerge THEN
        DO:
            CREATE pdfMerge.
            ASSIGN pdfMerge.c_file = cip_merge.
        END.

        ASSIGN pdfMerge.c_list = pdfMerge.c_list + cip_file + ",".

        IF NOT lip_last THEN
            RETURN TRUE.
        ELSE
        DO:
            ASSIGN pdfMerge.c_list = SUBSTRING(pdfMerge.c_list,1,LENGTH(pdfMerge.c_list) - 1)
                   fcv_script      = SUBSTITUTE(fcv_pdfmerge,cip_merge,pdfMerge.c_list).

            OS-COMMAND SILENT VALUE(fcv_script).

            RETURN SEARCH(cip_merge) <> ?.
        END.
    END FUNCTION.

    FUNCTION PDF_ISERROR   RETURNS LOGICAL ():
        RETURN lError_.
    END FUNCTION.

    FUNCTION PDF_NUM_ERR   RETURNS INTEGER ():
        RETURN iError_.
    END FUNCTION.

    FUNCTION PDF_GET_ERR   RETURNS CHAR    (INPUT iip_error AS INTEGER):
        FOR FIRST pdfError
            WHERE pdfError.i_idx = iip_error:
            RETURN pdfError.c_error.
        END.
        RETURN "".
    END FUNCTION.

    FUNCTION PDF_CLR_ERR   RETURNS LOGICAL ():
        ASSIGN iPageno_ = 0
               lError_  = FALSE.
        EMPTY TEMP-TABLE pdfError.

        RETURN NOT TEMP-TABLE pdfError:HAS-RECORDS.
    END FUNCTION.
&ENDIF
