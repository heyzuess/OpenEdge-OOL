&IF DEFINED({&TABLE_NAME}) > 0 &THEN
    &IF DEFINED({&TABLE_NAME}_SHARED) > 0 OR
        DEFINED({&TABLE_NAME}_REF)    > 0 &THEN
        &SCOPED-DEFINE {&TABLE_NAME}_SHARED SHARED
    &ENDIF

    &SCOPED-DEFINE {&TABLE_NAME} {&{&TABLE_NAME}_REF} {&{&TABLE_NAME}_SHARED}

    DEFINE {&TABLE_NAME} {&TABLE_DEFF}.
&ENDIF