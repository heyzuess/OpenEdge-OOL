&IF DEFINED(TABLE_DETAIL) > 0 &THEN
    &SCOPED-DEFINE TABLE_DETAIL {&TABLE_DETAIL_SCOPE}

    DEFINE {&TABLE_DETAIL} TEMP-TABLE tableDetail {&TABLE_DETAIL_REF}
        FIELD i_oid   AS INTEGER
        FIELD i_idx   AS INTEGER
        FIELD c_table AS CHARACTER FORMAT 'X(15)'
        FIELD c_name  AS CHARACTER FORMAT 'X(15)'
        FIELD c_field AS CHARACTER FORMAT 'X(15)'
        FIELD c_type  AS CHARACTER FORMAT 'X(15)'
        FIELD c_value AS CHARACTER FORMAT 'X(15)'.

    DEFINE {&TABLE_DETAIL} TEMP-TABLE tableDiff {&TABLE_DETAIL_REF}
        FIELD i_diff   AS INTEGER
        FIELD i_idx    AS INTEGER
        FIELD c_date   AS CHARACTER FORMAT 'X(15)'
        FIELD c_time   AS CHARACTER FORMAT 'X(15)'
        FIELD c_tableA AS CHARACTER FORMAT 'X(15)'
        FIELD c_tableB AS CHARACTER FORMAT 'X(15)'
        FIELD c_field  AS CHARACTER FORMAT 'X(15)'
        FIELD c_diff   AS CHARACTER FORMAT 'X(60)'.
&ENDIF

&IF DEFINED(GRID_BLOCK) > 0 &THEN
    &SCOPED-DEFINE GRID_BLOCK {&GRID_BLOCK_SCOPE}

    DEFINE {&GRID_BLOCK} TEMP-TABLE gridBlock {&GRID_BLOCK_REF}
        FIELD i_idx    AS INTEGER
        FIELD i_x      AS INTEGER
        FIELD i_y      AS INTEGER
        FIELD c_val    AS CHARACTER
        FIELD c_loc    AS CHARACTER
        FIELD c_data   AS CHARACTER
        FIELD l_fill   AS LOGICAL.
&ENDIF

&IF DEFINED(WIN_OBJ) > 0 &THEN
    &SCOPED-DEFINE WIN_OBJ {&WIN_OBJ_SCOPE}

    DEFINE {&WIN_OBJ} TEMP-TABLE win_obj {&WIN_OBJ_REF}
        FIELD c_name     AS CHARACTER
        FIELD c_type     AS CHARACTER
        FIELD c_data     AS CHARACTER
        FIELD h_handle   AS HANDLE.

    DEFINE {&WIN_OBJ} TEMP-TABLE win_objDetail {&WIN_OBJ_REF}
        FIELD c_name   AS CHARACTER
        FIELD c_type   AS CHARACTER
        FIELD c_val    AS CHARACTER
        FIELD i_set    AS INTEGER.
&ENDIF

&IF DEFINED(C_OBJ) > 0 &THEN
    &SCOPED-DEFINE C_OBJ {&C_OBJ_SCOPE}

    DEFINE {&C_OBJ} TEMP-TABLE cObj {&C_OBJ_REF}
        FIELD i_idx    AS INTEGER
        FIELD d_theta  AS DECIMAL
        FIELD d_x      AS DECIMAL
        FIELD d_y      AS DECIMAL
        FIELD i_x      AS INTEGER
        FIELD i_y      AS INTEGER
        FIELD h_handle AS HANDLE.
&ENDIF

&IF DEFINED(FILE_DIR) > 0 &THEN
    &SCOPED-DEFINE FILE_DIR {&FILE_DIR_SCOPE}

    DEFINE {&FILE_DIR} TEMP-TABLE fileDir {&FILE_DIR_REF}
        FIELD i_idx  AS INTEGER
        FIELD c_name AS CHARACTER
        FIELD c_full AS CHARACTER
        FIELD c_type AS CHARACTER.
&ENDIF