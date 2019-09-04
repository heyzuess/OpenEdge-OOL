&IF DEFINED(_newApp_) = 0 &THEN
    &GLOBAL-DEFINE _newApp_
    &GLOBAL-DEFINE _newApp_PATH C:\OpenEdge\WRK\newApp\
    &IF INDEX(PROPATH,"{&_newApp_PATH}") = 0 &THEN
        ASSIGN PROPATH = PROPATH + ",{&_newApp_PATH}".
    &ENDIF
&ENDIF