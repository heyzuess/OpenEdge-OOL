CLASS IMGLIB:
    DEFINE PUBLIC PROPERTY w_win      AS WINPROC GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_image    AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY a_iList    AS ARRAY   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_currentI AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_dir      AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_filename AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_ext      AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_curr     AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_startP   AS INTEGER.
    DEFINE PRIVATE VARIABLE i_currP    AS INTEGER.
    DEFINE PRIVATE VARIABLE i_endP     AS INTEGER.

    CONSTRUCTOR IMGLIB(INPUT wip_win  AS Progress.Lang.OBJECT,
                       INPUT aip_list AS Progress.Lang.OBJECT):
        w_win   = CAST(wip_win,WINPROC).
        IF NOT VALID-OBJECT(w_win)   THEN DELETE OBJECT THIS-OBJECT.

        a_iList = CAST(aip_list,ARRAY).
        IF NOT VALID-OBJECT(a_iList) THEN DELETE OBJECT THIS-OBJECT.
    END CONSTRUCTOR.

    CONSTRUCTOR IMGLIB(INPUT wip_win  AS Progress.Lang.OBJECT):
        w_win   = CAST(wip_win,WINPROC).
        IF NOT VALID-OBJECT(w_win)   THEN DELETE OBJECT THIS-OBJECT.

        a_iList = NEW ARRAY().
        IF NOT VALID-OBJECT(a_iList) THEN DELETE OBJECT THIS-OBJECT.
    END CONSTRUCTOR.

    CONSTRUCTOR IMGLIB():
        w_win   = NEW WINPROC().
        IF NOT VALID-OBJECT(w_win)   THEN DELETE OBJECT THIS-OBJECT.

        a_iList = NEW ARRAY().
        IF NOT VALID-OBJECT(a_iList) THEN DELETE OBJECT THIS-OBJECT.
    END CONSTRUCTOR.

    DESTRUCTOR IMGLIB():
        IF VALID-OBJECT(w_win)   THEN DELETE OBJECT w_win.
        IF VALID-OBJECT(a_iList) THEN DELETE OBJECT a_iList.
    END DESTRUCTOR.

    METHOD PUBLIC LOG setImageCont (INPUT hip_frame AS HANDLE,
                                    INPUT iip_w     AS INTEGER,
                                    INPUT iip_h     AS INTEGER,
                                    INPUT iip_x     AS INTEGER,
                                    INPUT iip_y     AS INTEGER):
        IF NOT VALID-HANDLE(hip_frame) THEN RETURN FALSE.

        IF VALID-HANDLE(h_image) THEN DELETE OBJECT h_image.

        h_image = w_win:createImage(hip_frame,
                                    SUBSTITUTE("imageOBJ_&1",hip_frame),
                                    iip_w,iip_h,iip_x,iip_y,FALSE,"").

        RETURN VALID-HANDLE(h_image).
    END METHOD.

    METHOD PUBLIC LOG loadImageLib (INPUT cip_dir  AS CHARACTER,
                                    INPUT cip_file AS CHARACTER,
                                    INPUT cip_ext  AS CHARACTER,
                                    INPUT iip_x    AS INTEGER,
                                    INPUT iip_y    AS INTEGER):
        ASSIGN c_dir      = cip_dir
               c_filename = cip_file
               c_ext      = cip_ext
               i_endP     = iip_y
               i_startP   = iip_x.

        DO i_currP = i_startP TO i_endP:
            ASSIGN c_curr = c_dir + c_filename + STRING(i_currP) + "." + cip_ext.
            a_iList:pushBack(c_curr,"CHAR").
        END.
        i_currP = i_startP.
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG loadImageFile (INPUT cip_dir  AS CHARACTER,
                                     INPUT cip_file AS CHARACTER):
        RETURN a_iList:pushBack(c_dir + c_filename,"CHAR").
    END METHOD.

    METHOD PUBLIC LOG getNextImg ():
        IF a_iList:LENGTH = 0       THEN RETURN FALSE.

        ASSIGN i_currP = i_currP + 1.
        IF i_currP > a_iList:LENGTH THEN
        DO:
            ASSIGN i_currP = a_iList:LENGTH.
            RETURN FALSE.
        END.

        ASSIGN c_curr = a_iList:getIndexCHAR(i_currP).
        IF SEARCH(c_curr) = ? THEN RETURN FALSE.

        h_image:LOAD-IMAGE(c_curr).

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG getPrevImg ():
        IF a_iList:LENGTH = 0       THEN RETURN FALSE.

        ASSIGN i_currP = i_currP - 1.
        IF i_currP < 1              THEN
        DO:
            ASSIGN i_currP = 1.
            RETURN FALSE.
        END.

        ASSIGN c_curr = a_iList:getIndexCHAR(i_currP).
        IF SEARCH(c_curr) = ? THEN RETURN FALSE.

        h_image:LOAD-IMAGE(c_curr).

        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG getImg (INPUT iip_idx AS INTEGER):
        IF a_iList:LENGTH = 0 THEN RETURN FALSE.
        IF iip_idx < 1 OR iip_idx > a_iList:LENGTH THEN RETURN FALSE.

        ASSIGN c_curr = a_iList:getIndexCHAR(iip_idx).
        IF SEARCH(c_curr) = ? THEN RETURN FALSE.

        h_image:LOAD-IMAGE(c_curr).
        ASSIGN i_currP = iip_idx.

        RETURN TRUE.
    END METHOD.
END CLASS.
