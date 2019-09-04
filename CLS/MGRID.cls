CLASS MGRID:
    DEFINE PUBLIC PROPERTY w_wdc   AS WINPROC   GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY h_frame AS HANDLE    GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY a_grid  AS ARRAY     GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY c_file  AS CHARACTER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE h_temp  AS HANDLE.
    DEFINE PRIVATE VARIABLE i_start AS INTEGER.
    DEFINE PRIVATE VARIABLE i_int   AS INTEGER.
    DEFINE PRIVATE VARIABLE i_x     AS INTEGER.
    DEFINE PRIVATE VARIABLE i_y     AS INTEGER.

    {standard.i &METHOD_LIBRARY = ""
                &DEF_GRID       = ""}

    DEFINE TEMP-TABLE mapGrid LIKE gridBlock.

    CONSTRUCTOR MGRID (INPUT wip_wdc   AS WINPROC,
                       INPUT hip_frame AS HANDLE,
                       INPUT cip_file  AS CHARACTER):
        ASSIGN  w_wdc   = wip_wdc
                h_frame = hip_frame
                c_file  = cip_file.

        a_grid = NEW ARRAY().

        IF NOT TEMP-TABLE gridBlock:READ-JSON("FILE",c_file,"EMPTY") THEN
            DELETE OBJECT THIS-OBJECT.

        /*
        FOR EACH gridBlock BREAK BY gridBlock.i_idx:
            IF gridBlock.c_data > "" THEN LEAVE.
        END.
        ASSIGN i_start = gridBlock.i_y.*/
        i_start = 1.

        FOR EACH  gridBlock
            WHERE gridBlock.i_y >= i_start
            BREAK BY gridBlock.i_idx:

            ASSIGN i_int = i_int + 1.
            CREATE mapGrid.
            BUFFER-COPY gridBlock EXCEPT gridBlock.i_idx TO mapGrid.
            ASSIGN mapGrid.i_idx = i_int
                   mapGrid.i_x   = mapGrid.i_x - 1
                   mapGrid.i_y   = mapgrid.i_y - 1.
        END.

        /*
        FOR EACH mapGrid BREAK BY mapGrid.i_x BY mapGrid.i_y :
            IF LAST-OF(mapGrid.i_x) THEN
            DO:
                ASSIGN i_x   = mapGrid.i_x       * {&WIN_STEP}
                       i_y   = (mapGrid.i_y - 1) * {&WIN_STEP}.
                LEAVE.
            END.
        END.*/

        FOR EACH mapGrid BREAK BY mapGrid.i_idx:
            /*MESSAGE mapgrid.i_idx SKIP
                    mapGrid.i_x mapgrid.i_y SKIP
                    mapgrid.i_x * {&WIN_STEP}
                    (mapgrid.i_y) * {&WIN_STEP}
            VIEW-AS ALERT-BOX.
            PAUSE.*/

            a_grid:pushBack(STRING(w_wdc:createFill(h_frame,
                                                    ?,
                                                    SUBSTITUTE("grid&1",mapGrid.i_idx),
                                                    1,
                                                    1,
                                                    mapGrid.i_x  * {&WIN_STEP},
                                                    mapGrid.i_y  * {&WIN_STEP})),
                            "HANDLE").
            h_temp = a_grid:getIndexHND(mapGrid.i_idx).
            h_temp:SCREEN-VALUE = mapGrid.c_data.
        END.

        EMPTY TEMP-TABLE gridBlock.
    END CONSTRUCTOR.

    DESTRUCTOR MGRID ():
        EMPTY TEMP-TABLE gridBlock.
        EMPTY TEMP-TABLE mapGrid.
        DO i_int = 0 TO a_grid:LENGTH:
            ASSIGN h_temp = a_grid:getIndexHND(i_int).
            IF VALID-HANDLE(h_temp) THEN
            DO:
                ASSIGN h_temp:SCREEN-VALUE = "".
                w_wdc:remWinObj(h_temp).
            END.
        END.
        IF VALID-OBJECT(a_grid) THEN DELETE OBJECT a_grid.
    END DESTRUCTOR.

    METHOD PUBLIC VOID repaintAll ():
        FOR EACH mapGrid BY mapGrid.i_idx:
            ASSIGN h_temp              = a_grid:getIndexHND(mapGrid.i_idx)
                   h_temp:SCREEN-VALUE = mapGrid.c_data.
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL paintGrid (INPUT iip_x AS INTEGER,
                                     INPUT iip_y AS INTEGER):
        FOR FIRST mapGrid
            WHERE mapGrid.i_x = iip_x
            AND   mapGrid.i_y = iip_y:
            ASSIGN h_temp              = a_grid:getIndexHND(mapGrid.i_idx)
                   h_temp:SCREEN-VALUE = mapGrid.c_data.

            RETURN h_temp:SCREEN-VALUE = mapGrid.c_data.
        END.
        RETURN FALSE.
    END METHOD.

    METHOD PUBLIC CHARACTER getGridVal (INPUT iip_x AS INTEGER,
                                        INPUT iip_y AS INTEGER):
        FOR FIRST mapgrid
            WHERE mapgrid.i_x = iip_x
            AND   mapgrid.i_y = iip_y:

            RETURN mapgrid.c_data.
        END.
        RETURN "".
    END METHOD.

    METHOD PUBLIC LOGICAL getGridObjReg (OUTPUT TABLE mapgrid BIND):
        RETURN TEMP-TABLE mapgrid:HAS-RECORDS.
    END METHOD.
END CLASS.