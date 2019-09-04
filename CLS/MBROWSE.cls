CLASS MBROWSE:
    DEFINE PUBLIC PROPERTY h_table   AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_query   AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_buffer  AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_field   AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_browse  AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_col     AS HANDLE  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_records AS INTEGER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE i_int AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE i_set AS INTEGER NO-UNDO.

    CONSTRUCTOR MBROWSE (INPUT hip_frame  AS HANDLE,
                         INPUT hip_table  AS HANDLE,
                         INPUT cip_search AS CHARACTER,
                         INPUT cip_name   AS CHARACTER,
                         INPUT iip_x      AS INTEGER,
                         INPUT iip_y      AS INTEGER,
                         INPUT iip_w      AS INTEGER,
                         INPUT iip_h      AS INTEGER):

        IF NOT VALID-HANDLE(hip_frame) THEN RETURN.
        IF NOT VALID-HANDLE(hip_table) THEN RETURN.
        IF cip_search = ""             THEN RETURN.

        CREATE QUERY h_query.
        CREATE BUFFER h_buffer FOR TABLE hip_table:HANDLE:DEFAULT-BUFFER-HANDLE.

        h_query:SET-BUFFERS(h_buffer).
        h_query:QUERY-PREPARE(cip_search).

        CREATE BROWSE h_browse
            ASSIGN QUERY     = h_query
                   FRAME     = hip_frame
                   VISIBLE   = NO
                   SENSITIVE = YES
                   X         = iip_x
                   Y         = iip_y
                   WIDTH     = iip_w
                   DOWN      = iip_h.

        DO i_int = 1 TO h_buffer:NUM-FIELDS:
            h_field = h_buffer:BUFFER-FIELD(i_int).
            h_col   = h_browse:ADD-LIKE-COLUMN(h_field).
        END.

        /*
        DO i_set = 1 TO i_records:
            h_buffer:BUFFER-CREATE().

            DO i_int = 1 TO h_buffer:NUM-FIELDS:
                h_field = h_buffer:BUFFER-FIELD(i_int).
                h_field:BUFFER-VALUE = h_field:NAME + ' ' + STRING(i_set).
            END.
        END.
        */

        h_browse:EXPANDABLE = TRUE.
        h_browse:VISIBLE    = TRUE.
        h_query:QUERY-OPEN.
    END CONSTRUCTOR.

    DESTRUCTOR MBROWSE ():
        IF VALID-HANDLE(h_query)  THEN DELETE OBJECT h_query.
        IF VALID-HANDLE(h_buffer) THEN DELETE OBJECT h_buffer.
        IF VALID-HANDLE(h_browse) THEN DELETE OBJECT h_browse.
    END DESTRUCTOR.

    METHOD PUBLIC VOID toggleBrowse (INPUT lip_log AS LOGICAL):
        ASSIGN h_browse:VISIBLE = lip_log.
    END METHOD.
END CLASS.
