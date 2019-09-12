DEFINE VARIABLE c_propath AS CHARACTER.
DEFINE VARIABLE c_temp    AS CHARACTER.
DEFINE VARIABLE i_int     AS INTEGER.

ASSIGN c_propath = 
"C:\OpenEdge\WRK\OOL\testOOL" + "," +
"".

DO i_int = 1 TO NUM-ENTRIES(c_propath):
    ASSIGN c_temp = TRIM(ENTRY(i_int, c_propath)).
    IF c_temp = "" THEN NEXT.

    IF LOOKUP(c_temp, PROPATH) = 0 THEN
    DO:
        ASSIGN PROPATH = PROPATH + "," + c_temp.
        MESSAGE c_temp "added to PROPATH".
        PAUSE 1 NO-MESSAGE.
    END.
END.