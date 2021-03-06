&IF DEFINED(FRAME_ID) = 0 &THEN
    &SCOPED-DEFINE FRAME_ID FRAME_ID_DEF
&ENDIF
&IF DEFINED(FRAME_NAME) = 0 &THEN
    &SCOPED-DEFINE FRAME_NAME FRAME_DEFAULT
&ENDIF
&IF DEFINED(FRAME_DMODE) = 0 &THEN
    &SCOPED-DEFINE FRAME_DMODE CENTERED OVERLAY
&ENDIF
&IF DEFINED(FRAME_W) = 0 &THEN
    &SCOPED-DEFINE FRAME_W 40
&ENDIF
&IF DEFINED(FRAME_H) = 0 &THEN
    &SCOPED-DEFINE FRAME_H 20
&ENDIF
&IF DEFINED(FRAME_TITLE) = 0 &THEN
    &SCOPED-DEFINE FRAME_TITLE
&ENDIF
&IF DEFINED(FRAME_VARSET) = 0 &THEN
    &SCOPED-DEFINE FRAME_VARSET
&ENDIF
&IF DEFINED(FRAME_DSPINIT) = 0 &THEN
    &SCOPED-DEFINE FRAME_DSPINIT
&ENDIF
&IF DEFINED(FRAME_DISPLAY) = 0 &THEN
    &SCOPED-DEFINE FRAME_DISPLAY "Hello World"
    &SCOPED-DEFINE FRAME_DEFDISP
&ENDIF

DEFINE VARIABLE {&FRAME_ID}_handle AS HANDLE.
{&FRAME_VARSET}

DEFINE FRAME A{&FRAME_NAME}
    {&FRAME_DISPLAY}
WITH {&FRAME_DMODE} SIZE {&FRAME_W} BY {&FRAME_H}
TITLE "{&FRAME_TITLE}".

ASSIGN {&FRAME_ID}_handle = FRAME A{&FRAME_NAME}:HANDLE.
{&FRAME_DSPINIT}

FUNCTION FRAME_HANDLE RETURNS HANDLE () FORWARD.

PROCEDURE FRAME_DISPLAY:
    &IF DEFINED(FRAME_DEFDISP) = 0 &THEN
        DISPLAY {&FRAME_DISPLAY} WITH FRAME A{&FRAME_NAME}.
    &ELSEIF DEFINED(FRAME_DEFDISP) > 0 &THEN
        VIEW FRAME A{&FRAME_NAME}.
    &ENDIF
END PROCEDURE.

FUNCTION FRAME_HANDLE RETURNS HANDLE ():
    RETURN {&FRAME_ID}_handle.
END FUNCTION.