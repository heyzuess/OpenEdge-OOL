DEFINE VARIABLE c_vals   AS CHARACTER INIT
"0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F".

DEFINE VARIABLE c_string AS CHARACTER FORMAT 'X(50)' LABEL 'Input String'.

DEFINE VARIABLE c_type   AS CHARACTER FORMAT 'X(15)' LABEL '  Ouput Type'
VIEW-AS COMBO-BOX SIZE 15 BY 5 LIST-ITEMS "Random", "Gradient".

DEFINE VARIABLE c_sub    AS CHARACTER FORMAT 'X(10)'
VIEW-AS COMBO-BOX SIZE 10 BY 5.

DEFINE VARIABLE c_file   AS CHARACTER.

DEFINE VARIABLE i_mod    AS INTEGER.

DEFINE VARIABLE o_hex    AS OOL.HEX.

DEFINE TEMP-TABLE tempdata
    FIELD i_idx   AS INTEGER
    FIELD c_char  AS CHARACTER
    FIELD c_color AS CHARACTER
    FIELD c_data  AS CHARACTER.

DEFINE FRAME A
    c_string AT ROW 2 COL 10
    c_type   AT ROW 4 COL 10 
    c_sub    AT ROW 6 COL 10 
WITH CENTERED ROW 4 COL 1 SIDE-LABELS
SIZE 80 BY 10.

ON 'VALUE-CHANGED' OF c_type IN FRAME A DO:
    ASSIGN c_type.

    CASE SELF:SCREEN-VALUE:
        WHEN "Gradient" THEN
        DO:
            ASSIGN c_sub:LIST-ITEMS IN FRAME A = "Left,Right"
                   c_sub:LABEL IN FRAME A      = "   Direction"
                   c_sub:ROW = c_type:ROW + 2
                   c_sub:COL = c_type:COL
                   c_sub:SIDE-LABEL-HANDLE:COL = c_sub:COL - LENGTH(c_sub:SIDE-LABEL-HANDLE:SCREEN-VALUE) - 2.

            ENABLE c_sub WITH FRAME A.
            APPLY "ENTRY" TO c_sub IN FRAME A.
        END.
        OTHERWISE
        DO:
            DISABLE c_sub WITH FRAME A.
            ASSIGN c_sub:VISIBLE IN FRAME A = FALSE.
        END.
    END CASE.
END.

ON "ENTER","RETURN","F1","PF1" OF c_sub IN FRAME A DO:
    ASSIGN c_sub.
    APPLY "GO" TO FRAME A.
END.

o_hex = NEW OOL.HEX().

ASSIGN c_sub:VISIBLE IN FRAME A = FALSE.

MAIN-BLK:
REPEAT:
    UPDATE c_string 
           c_type
    WITH FRAME A.

    RUN parseData.
END.

PROCEDURE parseData:
    DEFINE VARIABLE iv_int  AS INTEGER.
    DEFINE VARIABLE iv_set  AS INTEGER.

    EMPTY TEMP-TABLE tempdata.

    DO iv_int = 1 TO LENGTH(c_string):
        CREATE tempdata.
        ASSIGN tempdata.i_idx   = iv_int
               tempdata.c_char  = SUBSTRING(c_string,iv_int,1)
               tempdata.c_color = "#".

        IF tempdata.c_char = " " THEN ASSIGN tempdata.c_char = "~&nbsp~;~&nbsp~;".

        CASE c_type:
            WHEN "Random"   THEN
            DO iv_set = 1 TO 6:
                ASSIGN tempdata.c_color = tempdata.c_color + ENTRY(RANDOM(1, NUM-ENTRIES(c_vals)), c_vals).
            END.
            WHEN "Gradient" THEN
            DO:
                RUN getNextFromLast(iv_int - 1, i_mod, c_sub, OUTPUT tempdata.c_color).
            END.
        END CASE.

        ASSIGN tempdata.c_data = '<p style="color:'    + 
                                 tempdata.c_color      + 
                                 '~;display:inline;">' +
                                 tempdata.c_char       +
                                 '<~/p>'.
    END.

    ASSIGN c_file = "./random_color_" + STRING(TIME * ETIME) + ".txt".

    OUTPUT TO VALUE(c_file).
        PUT UNFORMATTED '<div style="background-color:black~;width:max-content~;font-size:20px~;">' SKIP.
        FOR EACH tempdata
            BY   tempdata.i_idx:
            
            PUT UNFORMATTED tempdata.c_data SKIP.
        END.
        PUT UNFORMATTED '<~/div>' SKIP.
    OUTPUT CLOSE.

    MESSAGE SEARCH(c_file).
END PROCEDURE.

PROCEDURE getNextFromLast:
    DEFINE INPUT  PARAMETER iip_last  AS INTEGER.
    DEFINE INPUT  PARAMETER iip_mod   AS INTEGER.
    DEFINE INPUT  PARAMETER cip_type  AS CHARACTER.
    DEFINE OUTPUT PARAMETER cop_color AS CHARACTER.

    DEFINE BUFFER x_tempdata FOR tempdata.

    DEFINE VARIABLE iv_int   AS INTEGER.
    DEFINE VARIABLE lv_ok    AS LOGICAL.

    IF iip_last = 0 THEN
    DO:
        RUN getStartColor(OUTPUT cop_color, OUTPUT lv_ok).
        IF NOT lv_ok THEN
        DO:
            ASSIGN cop_color = "#".
            DO iv_int = 1 TO 6:
                ASSIGN cop_color = cop_color + ENTRY(RANDOM(1, NUM-ENTRIES(c_vals)), c_vals).
            END.
        END.
        //assign cop_color = "#03EC00".
        RETURN.
    END.

    FOR FIRST x_tempdata
        WHERE x_tempdata.i_idx = iip_last:
    END.
    IF NOT AVAILABLE x_tempdata THEN
    DO:
        ASSIGN cop_color = "#0123456".
        RETURN.
    END.

    ASSIGN iv_int = o_hex:StringToHex(x_tempdata.c_color).

    CASE cip_type:
        WHEN "Left" THEN
        DO:
            ASSIGN iv_int = iv_int - iip_mod.
            IF iv_int < 0 THEN ASSIGN iv_int = o_hex:StringToHex("0xFFFFFF").
        END.
        OTHERWISE
        DO:
            ASSIGN iv_int = iv_int + iip_mod.
            IF iv_int > o_hex:StringToHex("0xFFFFFF") THEN ASSIGN iv_int = 0.
        END.
    END CASE.

    ASSIGN cop_color  = o_hex:HexToString(iv_int)
           cop_color  = REPLACE(cop_color,"0x","#").
END PROCEDURE.

PROCEDURE getStartColor:
    DEFINE OUTPUT PARAMETER cop_color AS CHARACTER.
    DEFINE OUTPUT PARAMETER lop_ok    AS LOGICAL.

    DEFINE VARIABLE cv_color AS CHARACTER.
    DEFINE VARIABLE iv_mod   AS INTEGER.
    DEFINE VARIABLE iv_int   AS INTEGER.

    DEFINE BUTTON bRandom LABEL "Random".

    DEFINE FRAME fStartColor
        cv_color FORMAT 'X(6)' LABEL 'Start Color' SKIP(1)
        iv_mod   FORMAT '>>9'  LABEL '     Degree' SKIP(2)
        bRandom
    WITH CENTERED OVERLAY ROW 4 COL 1 COLOR MESSAGE SIDE-LABELS.

    ON "GO" OF FRAME fStartColor DO:
        ASSIGN cv_color
               iv_mod.

        IF cv_color = "" THEN
        DO:
            MESSAGE "Color can not be left blank"
            VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        DO iv_int = 1 TO LENGTH(cv_color):
            IF LOOKUP(SUBSTRING(cv_color, iv_int, 1), c_vals) = 0 THEN
            DO:
                MESSAGE "Value" SUBSTRING(cv_color, iv_int, 1) "in position" iv_int
                        "is an invalid value!" SKIP(1)
                        "Must be one of the following:" SKIP
                        c_vals
                VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
        END.

        ASSIGN iv_mod = MAX(1, iv_mod).

        ASSIGN lop_ok    = TRUE
               cop_color = "#" + cv_color
               i_mod     = iv_mod.
    END.

    ON "CHOOSE" OF bRandom IN FRAME fStartColor DO:
        ASSIGN cv_color:SCREEN-VALUE = "".
        DO iv_int = 1 TO 6:
            ASSIGN cv_color:SCREEN-VALUE = cv_color:SCREEN-VALUE + ENTRY(RANDOM(1, NUM-ENTRIES(c_vals)), c_vals).
        END.

        ASSIGN iv_mod:SCREEN-VALUE = STRING(RANDOM(1, 100)).
    END.

    ENABLE bRandom WITH FRAME fStartColor.

    ASSIGN bRandom:PFCOLOR IN FRAME fStartColor = 0.

    REPEAT WHILE NOT lop_ok:
        UPDATE cv_color 
               iv_mod
        WITH FRAME fStartColor.
    END.
    HIDE FRAME fStartColor.
END PROCEDURE.