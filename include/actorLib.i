DEFINE INPUT PARAMETER cip_name  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cip_title AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iip_w     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER iip_h     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER iip_x     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER iip_y     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER cip_Vset  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cip_Disp  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cip_Init  AS CHARACTER NO-UNDO.

{actorDisplay.i
        &FRAME_NAME    = cip_name
        &FRAME_TITLE   = cip_title
        &FRAME_W       = iip_w
        &FRAME_H       = iip_h
        &FRAME_X       = iip_x
        &FRAME_Y       = iip_y
        &FRAME_VARSET  = cip_Vset
        &FRAME_DISPLAY = cip_Disp
        &FRAME_DSPINIT = cip_Init
}