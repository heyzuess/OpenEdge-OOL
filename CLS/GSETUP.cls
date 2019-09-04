CLASS GSETUP:
    DEFINE PRIVATE VARIABLE iv_startX   AS INTEGER.
    DEFINE PRIVATE VARIABLE iv_startY   AS INTEGER.
    DEFINE PRIVATE VARIABLE iv_limitY   AS INTEGER.
    DEFINE PRIVATE VARIABLE iv_currRow  AS INTEGER.
    DEFINE PRIVATE VARIABLE iv_poolStat AS INTEGER.
    DEFINE PRIVATE VARIABLE fiv_idx     AS INTEGER.
    DEFINE BUTTON bMoveUp   LABEL " + " SIZE 3 BY 1.
    DEFINE BUTTON bMoveDown LABEL " - " SIZE 3 BY 1.

    DEFINE TEMP-TABLE optionSet
        FIELD i_idx    AS INTEGER
        FIELD i_val    AS INTEGER
        FIELD c_option AS CHARACTER.

    DEFINE FRAME overFrame
    WITH SIZE 32 BY 11 TITLE "Stats".

    DEFINE FRAME valueFrame
        iv_poolStat NO-LABEL FORMAT '>9'
        bMoveUp
        bMoveDown
    WITH OVERLAY NO-BOX.

    DEFINE BUFFER xoptionSet FOR optionSet.

    CONSTRUCTOR GSETUP():
        makeOption("Strength",5).
        makeOption("Perception",5).
        makeOption("Endurance",5).
        makeOption("Charisma",5).
        makeOption("Intelligence",5).
        makeOption("Agility",5).
        makeOption("Luck",5).
    END CONSTRUCTOR.

    /* Overloaded */
    CONSTRUCTOR GSETUP(INPUT lip_methodLib AS LOGICAL):
        makeOption("Strength",5).
        makeOption("Perception",5).
        makeOption("Endurance",5).
        makeOption("Charisma",5).
        makeOption("Intelligence",5).
        makeOption("Agility",5).
        makeOption("Luck",5).

        IF lip_methodLib THEN startSetup().
    END CONSTRUCTOR.

    DESTRUCTOR GSETUP():
    END DESTRUCTOR.

    {standard.i &METHOD_LIB = ""}

    {slib/browseM.i
      &file          = optionSet
      &framename     = optionFrame
      &browsename    = optionBrowse
      &queryname     = optionQuery
      &help          = "F1 - Complete | F4 - Exit"
      &display       = "optionSet.c_option NO-LABEL FORMAT 'X(12)'
                        optionSet.i_val    NO-LABEL FORMAT '>9'
                        WITH 7 DOWN OVERLAY"
      &position      = "ROW 4 COL 2 OVERLAY NO-BOX"
      &where         = "WHERE TRUE"
      &sfld          = c_option
      &updaction     = " "
    }

    METHOD PUBLIC LOGICAL startSetup():
        ON ANY-KEY OF optionBrowse IN FRAME optionFrame DO:
            /*MESSAGE KEYLABEL(LASTKEY).*/
            CASE KEYLABEL(LASTKEY):
                WHEN "CURSOR-UP" THEN
                DO:
                    /*MESSAGE iv_startY iv_limitY (optionSet.i_idx - 1) * 8 + iv_startY.*/
                    IF iv_startY + 8 <= (optionSet.i_idx - 1) * 8 + iv_startY THEN
                    DO:
                        ASSIGN FRAME valueFrame:Y = FRAME valueFrame:Y - 8
                               iv_currRow         = optionSet.i_idx - 1
                        NO-ERROR.
                        DISPLAY iv_poolStat WITH FRAME valueFrame.
                    END.
                END.
                WHEN "CURSOR-DOWN" THEN
                DO:
                    /*MESSAGE iv_startY iv_limitY (optionSet.i_idx + 1) * 8 + iv_startY.*/
                    IF iv_limitY >= (optionSet.i_idx + 1) * 8 + iv_startY THEN
                    DO:
                        ASSIGN FRAME valueFrame:Y = FRAME valueFrame:Y + 8
                               iv_currRow         = optionSet.i_idx + 1
                        NO-ERROR.
                        DISPLAY iv_poolStat WITH FRAME valueFrame.
                    END.
                END.
                WHEN "TAB" THEN
                DO:
                    /*RUN optionBrowse_disable.
                    RUN valueFrame_enable.
                    RUN optionBrowse_enable.*/
                    optionBrowse_disable().
                    valueFrame_enable().
                    optionBrowse_enable().
                    RETURN NO-APPLY.
                END.
                WHEN "PF1" OR WHEN "F1" THEN
                DO:
                END.
                WHEN "PF4" OR WHEN "F4" OR WHEN "ESC" THEN
                DO:
                    IF NOT messageCUI("Are you sure you want to leave~?","W",TRUE) THEN
                        RETURN NO-APPLY.
                END.
            END CASE.
        END.

        ON CHOOSE OF bMoveUp IN FRAME valueFrame DO:
            /*RUN bMoveUp_choose.*/
            bMoveUp_choose().
        END.

        ON CHOOSE OF bMoveDown IN FRAME valueFrame DO:
            /*RUN bMoveDown_choose.*/
            bMoveDown_choose().
        END.

        FOR LAST optionSet: END.
        ASSIGN FRAME overFrame:X   = 8 * 2
               FRAME overFrame:Y   = 8 * 2
               FRAME optionFrame:X = FRAME overFrame:X + 8
               FRAME optionFrame:Y = FRAME overFrame:Y + 8 * 2
               iv_startX = FRAME optionFrame:X + FRAME optionFrame:WIDTH-CHARS * 8
               iv_startY = FRAME optionFrame:Y
               iv_limitY = IF AVAILABLE optionSet THEN
                               optionSet.i_idx * 8 + iv_startY
                           ELSE
                               FRAME optionFrame:Y + FRAME optionFrame:HEIGHT-CHARS * 8
               FRAME valueFrame:X = iv_startX
               FRAME valueFrame:Y = iv_startY
               FRAME valueFrame:HIDDEN = FALSE
               iv_currRow  = 1
               iv_poolStat = 10.
        /*
        MESSAGE 'Start X' iv_startX SKIP
                'Start Y' iv_startY SKIP
                'Limit Y' iv_limitY SKIP
        VIEW-AS ALERT-BOX.*/

        PAUSE 0 BEFORE-HIDE.
        VIEW FRAME overFrame.
        DISPLAY iv_poolStat WITH FRAME valueFrame.
        /*RUN ipstartbrowse.*/
        ipStartBrowse().
        /*WAIT-FOR CLOSE, ENDKEY, WINDOW-CLOSE OF CURRENT-WINDOW.*/
    END METHOD.

    METHOD PUBLIC VOID valueFrame_enable():
        DISPLAY iv_poolStat      WITH FRAME valueFrame.
        ENABLE bMoveUp bMoveDown WITH FRAME valueFrame.
        WAIT-FOR LEAVE OF FRAME valueFrame.
    END METHOD.

    METHOD PUBLIC VOID valueFrame_disable():
        DISABLE bMoveUp bMoveDown WITH FRAME valueFrame.
    END METHOD.

    METHOD PUBLIC VOID optionBrowse_enable():
        OPEN QUERY optionQuery FOR EACH optionSet.
        ENABLE optionBrowse WITH FRAME optionFrame.
        QUERY optionQuery:REPOSITION-TO-ROW(iv_currRow).
    END METHOD.

    METHOD PUBLIC VOID optionBrowse_disable():
        DISABLE optionBrowse WITH FRAME optionFrame.
        VIEW FRAME optionFrame.
    END METHOD.

    METHOD PUBLIC VOID bMoveUp_choose():
        FOR FIRST  xoptionSet
            WHERE  xoptionSet.i_idx = iv_currRow:
            IF iv_poolStat      > 0  AND
               xoptionSet.i_val < 10 THEN
                ASSIGN xoptionSet.i_val = xoptionSet.i_val + 1
                       iv_poolStat      = iv_poolStat      - 1.
            /*MESSAGE SUBSTITUTE("Row &1 &2 1",iv_currRow,"+").*/
        END.
        HIDE FRAME optionFrame.
        VIEW FRAME optionFrame.
        DISPLAY iv_poolStat WITH FRAME valueFrame.
    END METHOD.

    METHOD PUBLIC VOID bMoveDown_choose():
        FOR FIRST  xoptionSet
            WHERE  xoptionSet.i_idx = iv_currRow:
            IF iv_poolStat      < 10 AND
               xoptionSet.i_val > 0  THEN
                ASSIGN xoptionSet.i_val = xoptionSet.i_val - 1
                       iv_poolStat      = iv_poolStat      + 1.
            /*MESSAGE SUBSTITUTE("Row &1 &2 1",iv_currRow,"-").*/
        END.
        HIDE FRAME optionFrame.
        VIEW FRAME optionFrame.
        DISPLAY iv_poolStat WITH FRAME valueFrame.
    END METHOD.

    METHOD PUBLIC LOGICAL makeOption(INPUT cip_option AS CHARACTER,
                                     INPUT iip_val    AS INTEGER):
        ASSIGN fiv_idx = 0.
        FOR LAST optionSet:
            ASSIGN fiv_idx = optionSet.i_idx.
        END.

        CREATE optionSet.
        ASSIGN optionSet.i_idx    = fiv_idx + 1
               optionSet.c_option = cip_option
               optionSet.i_val    = iip_val.

        RETURN AVAILABLE optionSet.
    END METHOD.
END CLASS.
