CLASS SGRID:
    DEFINE PUBLIC PROPERTY w_win   AS WINPROC    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY a_grid  AS ARRAY      GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_main  AS HANDLE     GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_temp  AS HANDLE     GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY i_curr  AS INTEGER    GET. PUBLIC  SET.

    DEFINE PUBLIC PROPERTY c_name  AS CHARACTER  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_token AS CHARACTER  GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_sizeX AS INTEGER    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_sizeY AS INTEGER    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_posX  AS INTEGER    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY i_posY  AS INTEGER    GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY l_custW AS LOGICAL    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_custH AS LOGICAL    GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_col   AS CHARACTER.
    DEFINE PRIVATE VARIABLE i_int   AS INTEGER.
    DEFINE PRIVATE VARIABLE i_x     AS INTEGER.
    DEFINE PRIVATE VARIABLE i_y     AS INTEGER.

    DEFINE PRIVATE VARIABLE c_head  AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_type  AS CHARACTER.
    DEFINE PRIVATE VARIABLE l_stamp AS LOGICAL.
    DEFINE PRIVATE VARIABLE l_head  AS LOGICAL.
    DEFINE PRIVATE VARIABLE h_x     AS HANDLE.

    {standard.i
      &METHOD_LIBRARY  = *
      &DEF_GRID        = *
      &NO_STANDARD_LIB = *
    }

    {errlib.i}

    CONSTRUCTOR SGRID (INPUT wip_win  AS WINPROC,
                       INPUT hip_main AS HANDLE,
                       INPUT cip_name AS CHARACTER,
                       INPUT iip_w    AS INTEGER,
                       INPUT iip_h    AS INTEGER,
                       INPUT iip_x    AS INTEGER,
                       INPUT iip_y    AS INTEGER):
        w_win   = wip_win.
        a_grid  = NEW ARRAY().
        h_main  = hip_main.
        c_name  = cip_name.
        i_sizeX = iip_w.
        i_sizeY = iip_h.
        i_posX  = iip_x.
        i_posY  = iip_y.
        l_custW = FALSE.
        l_custH = FALSE.
        buildGrid().
    END CONSTRUCTOR.

    CONSTRUCTOR SGRID (INPUT hip_main AS HANDLE,
                       INPUT cip_name AS CHARACTER,
                       INPUT iip_w    AS INTEGER,
                       INPUT iip_h    AS INTEGER,
                       INPUT iip_x    AS INTEGER,
                       INPUT iip_y    AS INTEGER):
        w_win   = NEW WINPROC().
        a_grid  = NEW ARRAY().
        h_main  = hip_main.
        c_name  = cip_name.
        i_sizeX = iip_w.
        i_sizeY = iip_h.
        i_posX  = iip_x.
        i_posY  = iip_y.
        l_custW = TRUE.
        l_custH = FALSE.
        buildGrid().
    END CONSTRUCTOR.

    CONSTRUCTOR SGRID (INPUT cip_name AS CHARACTER,
                       INPUT iip_a    AS INTEGER,
                       INPUT iip_b    AS INTEGER,
                       INPUT iip_c    AS INTEGER,
                       INPUT iip_d    AS INTEGER,
                       INPUT iip_w    AS INTEGER,
                       INPUT iip_h    AS INTEGER,
                       INPUT iip_x    AS INTEGER,
                       INPUT iip_y    AS INTEGER):
        w_win   = NEW WINPROC().
        a_grid  = NEW ARRAY().
        h_main  = w_win:createFrame(SUBSTITUTE("MainGrid_&1",STRING(THIS-OBJECT)),"",
                                    iip_a,iip_b,iip_c,iip_d,0,TRUE).
        c_name  = cip_name.
        i_sizeX = iip_w.
        i_sizeY = iip_h.
        i_posX  = iip_x.
        i_posY  = iip_y.
        l_custW = TRUE.
        l_custH = TRUE.
        buildGrid().
    END CONSTRUCTOR.

    DESTRUCTOR SGRID ():
        DO i_int = 1 TO a_grid:LENGTH:
            h_temp = a_grid:getIndexHND(i_int).
            w_win:remWinObj(h_temp).
        END.
        IF l_custH THEN
        DO:
            w_win:remWinObj(h_main).
        END.
        IF l_custW THEN
        DO:
            IF VALID-OBJECT(w_win) THEN DELETE OBJECT w_win.
        END.

        IF VALID-OBJECT(a_grid) THEN DELETE OBJECT a_grid.

        EMPTY TEMP-TABLE cErr.
        EMPTY TEMP-TABLE gridBlock.
    END DESTRUCTOR.

    METHOD PRIVATE LOGICAL buildGrid ():
        c_type  = " , ".
        c_head  = " , ".
        c_token = CHR(219).
        c_col   = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvqxyz".
        i_curr  = 1.

        /*
        h_temp = w_win:createFill(h_main,?,"col",i_sizeX,1,16,0).
        h_temp:SCREEN-VALUE = c_col.
        h_temp:DCOLOR       = 0.
        h_temp:SENSITIVE    = FALSE.*/

        DO i_int = 1 TO (i_sizeX * i_sizeY):
            ASSIGN i_x     = IF i_x / i_sizeX >= 1 THEN 1       ELSE i_x + 1
                   i_y     = IF i_x = 1            THEN i_y + 1 ELSE i_y
                   l_stamp = IF l_stamp            THEN FALSE   ELSE TRUE.

            /*
            h_temp = w_win:createFill(h_main,?,SUBSTITUTE("row&1",i_y),2,1,0,i_y * {&WIN_STEP}).
            h_temp:SCREEN-VALUE = STRING(i_y,">9").
            h_temp:DCOLOR       = 0.
            h_temp:SENSITIVE    = FALSE.*/

            a_grid:pushBack(STRING(w_win:createFill(h_main,?,
                                                    SUBSTITUTE("&2grid&1",i_int,c_name),
                                                    1,1,
                                                    ((i_x - 1) + i_posX) * {&WIN_STEP},
                                                    ((i_y - 1) + i_posY) * {&WIN_STEP})),"HANDLE").

            h_temp = a_grid:getIndexHND(i_int).
            IF NOT VALID-HANDLE(h_temp) THEN setErr("Failed to make grid " + STRING(i_int)).

            ASSIGN h_temp:SCREEN-VALUE = IF i_y MODULO 2 > 0 THEN
                                             ENTRY(INT(l_stamp) + 1,c_type)
                                         ELSE
                                             ENTRY(INT(l_head) + 1,c_head)

                   h_temp:DCOLOR       = IF i_y MODULO 2 > 0 THEN
                                             INT(ENTRY(INT(l_stamp) + 1,"1,3"))
                                         ELSE
                                             INT(ENTRY(INT(l_stamp) + 1,"3,1"))
                   h_temp:SENSITIVE    = FALSE.

            CREATE gridBlock.
            ASSIGN gridBlock.i_idx  = i_int
                   gridBlock.i_x    = i_x
                   gridBlock.i_y    = i_y
                   gridBlock.c_val  = h_temp:SCREEN-VALUE
                   gridBlock.c_loc  = SUBSTRING(c_col,i_x,1) + STRING(i_y)
                   gridBlock.l_fill = gridBlock.c_val <> " ".

            IF i_y MODULO 2 > 0 AND i_x = i_sizeX THEN
                ASSIGN l_head = IF l_head THEN FALSE ELSE TRUE.
        END.
    END METHOD.

    METHOD PUBLIC LOG gridObjReg (OUTPUT TABLE gridBlock BIND):
        RETURN TEMP-TABLE gridBlock:HAS-RECORDS.
    END METHOD.

    METHOD PUBLIC LOG getObjReg (BUFFER x FOR gridBlock):
        h_x = ?.
        FOR EACH x:
            FOR FIRST gridBlock
                WHERE gridBlock.i_idx = x.i_idx:
                IF gridBlock.c_data = "" AND gridBlock.i_idx <> i_curr THEN NEXT.

                ASSIGN h_temp              = a_grid:getIndexHND(gridBlock.i_idx)
                       h_temp:SCREEN-VALUE = gridBlock.c_data.

                IF gridBlock.i_idx = i_curr THEN
                    ASSIGN gridBlock.c_val = c_token.
            END.
        END.
        h_temp:SCREEN-VALUE = c_token.
        RETURN TEMP-TABLE x:HAS-RECORDS.
    END METHOD.
END CLASS.
