USING OOL.*.

DEFINE VARIABLE MATH      AS MATH.
DEFINE VARIABLE o_frame   AS WINFRAME.
DEFINE VARIABLE o_temp    AS WINFILL.
DEFINE VARIABLE o_current AS WINOBJ.
DEFINE VARIABLE h_current AS HANDLE.

DEFINE VARIABLE i_gridW AS INTEGER.
DEFINE VARIABLE i_gridH AS INTEGER.
DEFINE VARIABLE i_cellW AS INTEGER.
DEFINE VARIABLE i_cellH AS INTEGER.
DEFINE VARIABLE i_x     AS INTEGER.
DEFINE VARIABLE i_y     AS INTEGER.
DEFINE VARIABLE i_int   AS INTEGER.
DEFINE VARIABLE l_next  AS LOGICAL.

MATH = NEW MATH().
o_frame = NEW WINFRAME("Main","",40,20,0,0,0,TRUE).

ASSIGN i_cellW = 5
       i_cellH = 1
       i_gridW = MATH:FLOOR(o_frame:WIDTH  - 2, i_cellW)
       i_gridH = MATH:FLOOR(o_frame:HEIGHT - 2, i_cellH).

DO i_int = 1 TO i_gridW * i_gridH:
    ASSIGN i_x = IF i_x = i_gridW THEN 1       ELSE i_x + 1
           i_y = IF i_x = 1       THEN i_y + 1 ELSE i_y.

    o_temp = NEW WINFILL(o_frame, "", 
                         SUBSTITUTE("cell_&1_&2_&3", i_int, i_x, i_y),
                         i_cellW, i_cellH, 1, 1).
    
    o_temp:X = ((i_x - 1) * i_cellW) + 1.
    o_temp:Y = i_y.
    o_temp:COLOR = 1.
    o_temp:VALUE = FILL("_", i_cellW - 1) + "|".
    o_temp:VISIBLE = TRUE.
END.

o_frame:VISIBLE = TRUE.
o_current = o_frame:GET-FIRST-CHILD().
h_current = o_current:HANDLE.

REPEAT:
    ON 'ANY-KEY' OF h_current DO:
        CASE KEYLABEL(LASTKEY):
            WHEN "CURSOR-LEFT" THEN
            DO:
                o_current = o_frame:GET-PREV-CHILD().
                h_current = o_current:HANDLE.
                APPLY "GO" TO SELF.
            END.
            WHEN "CURSOR-RIGHT" OR WHEN "TAB" THEN
            DO:
                o_current = o_frame:GET-NEXT-CHILD().
                h_current = o_current:HANDLE.
                APPLY "GO" TO SELF.
            END.
            WHEN "CURSOR-UP" THEN
            DO:
                ASSIGN i_int = o_frame:CURRENT-CHILD - i_gridW
                       i_int = IF i_int < 1 THEN
                                   o_frame:NUM-CHILDREN + i_int
                               ELSE
                                   i_int.
                
                o_current = o_frame:GET-CHILD(i_int).
                h_current = o_current:HANDLE.
                APPLY "GO" TO SELF.
            END.
            WHEN "CURSOR-DOWN" OR WHEN "ENTER" THEN
            DO:
                ASSIGN i_int = o_frame:CURRENT-CHILD + i_gridW
                       i_int = IF i_int > o_frame:NUM-CHILDREN THEN
                                   i_int - o_frame:NUM-CHILDREN
                               ELSE
                                   i_int.

                o_current = o_frame:GET-CHILD(i_int).
                h_current = o_current:HANDLE.
                APPLY "GO" TO SELF.
            END.
            WHEN "PF1" OR WHEN "F1" THEN
            DO:
                ASSIGN l_next = TRUE.
            END.
        END CASE.
    END.

    ASSIGN l_next = FALSE.
    o_current:ENABLE().

    IF l_next THEN
        ASSIGN o_current = o_frame:GET-NEXT-CHILD()
               h_current = o_current:HANDLE.
END.