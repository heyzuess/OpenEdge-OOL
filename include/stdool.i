&IF DEFINED(STD_OOL_) = 0 &THEN
    &GLOBAL-DEFINE STD_OOL_

    &IF DEFINED(STD_GLOBAL_) = 0 &THEN
        &GLOBAL-DEFINE STD_GLOBAL_ DEFINED

        DEFINE NEW GLOBAL SHARED VARIABLE STD AS STDLIB.
        IF NOT VALID-OBJECT(STD) THEN STD = NEW STDLIB().

        &IF DEFINED(NO_WINOBJ) = 0 &THEN
        DEFINE NEW GLOBAL SHARED VARIABLE WINPROC AS WINPROC.
        IF NOT VALID-OBJECT(WINPROC) THEN WINPROC = NEW WINPROC().
        &ENDIF
    &ENDIF
&ENDIF