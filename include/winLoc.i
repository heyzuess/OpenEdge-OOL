
{windows.i}

&IF DEFINED(WINLOC_LIB) = 0 &THEN
    &GLOBAL-DEFINE WINLOC_LIB WINLOC_LIB
    
    PROCEDURE CenterWindow :
    /*------------------------------------------------------------------------------
     Purpose:     centers window to the working area.
                  ("working area" is portion of screen not obscured by taskbar)
     Parameters:  winhandle : progress widget-handle of a window widget
     Notes:
    ------------------------------------------------------------------------------*/
     DEFINE INPUT PARAMETER winhandle AS HANDLE NO-UNDO.

     IF LOOKUP(winhandle:TYPE , "window,dialox-box":U ) = 0  THEN RETURN.

     /* calculate coordinates and dimensions of working area */
     DEFINE VARIABLE workingleft   AS INTEGER NO-UNDO.
     DEFINE VARIABLE workingtop    AS INTEGER NO-UNDO.
     DEFINE VARIABLE workingwidth  AS INTEGER NO-UNDO.
     DEFINE VARIABLE workingheight AS INTEGER NO-UNDO.
     DEFINE VARIABLE lpWorkingRect AS MEMPTR. /* RECT structure */
     DEFINE VARIABLE ReturnValue   AS INTEGER NO-UNDO.

     SET-SIZE(lpWorkingRect)=4 * {&INTSIZE}.
     RUN GetWorkingArea (winhandle:HWND, lpWorkingRect).

     /* RECT is filled with left,top,right,bottom */
     workingleft   = get-{&INT}(lpWorkingRect,1 + 0 * {&INTSIZE}).
     workingtop    = get-{&INT}(lpWorkingRect,1 + 1 * {&INTSIZE}).
     workingwidth  = get-{&INT}(lpWorkingRect,1 + 2 * {&INTSIZE}) - workingleft.
     workingheight = get-{&INT}(lpWorkingRect,1 + 3 * {&INTSIZE}) - workingtop.


     /* calculate current coordinates and dimensions of window */
     DEFINE VARIABLE windowleft   AS INTEGER NO-UNDO.
     DEFINE VARIABLE windowtop    AS INTEGER NO-UNDO.
     DEFINE VARIABLE windowwidth  AS INTEGER NO-UNDO.
     DEFINE VARIABLE windowheight AS INTEGER NO-UNDO.
     DEFINE VARIABLE hParent AS INTEGER NO-UNDO.
     DEFINE VARIABLE lpWinRect AS MEMPTR.

     SET-SIZE(lpWinRect)=4 * {&INTSIZE}.
     hParent = GetParent(winhandle:HWND).
     RUN GetWindowRect IN hpApi(hParent,
                                GET-POINTER-VALUE(lpWinRect),
                                OUTPUT ReturnValue).

     windowleft   = get-{&INT}(lpWinRect,1 + 0 * {&INTSIZE}).
     windowtop    = get-{&INT}(lpWinRect,1 + 1 * {&INTSIZE}).
     windowwidth  = get-{&INT}(lpWinRect,1 + 2 * {&INTSIZE}) - windowleft.
     windowheight = get-{&INT}(lpWinRect,1 + 3 * {&INTSIZE}) - windowtop.

     /* calculate new x and y for window */
     windowleft = workingleft + INTEGER((workingwidth  - windowwidth ) / 2 ).
     windowtop  = workingtop  + INTEGER((workingheight - windowheight ) / 2 ).

     /* perhaps you should ensure that the upper-left corner of the window
        stays visible, e.g. user can reach system-menu to close the window: */
     windowleft = MAXIMUM(workingleft, windowleft).
     windowtop  = MAXIMUM(workingtop,  windowtop).

     /* assign these values. No need to use API: */
     ASSIGN winhandle:X = windowleft
            winhandle:Y = windowtop.

     /* free memory */
     SET-SIZE(lpWorkingRect) = 0.
     SET-SIZE(lpWinRect)     = 0.

    END PROCEDURE.

    PROCEDURE GetWorkingArea :
     DEFINE INPUT PARAMETER HWND   AS INTEGER NO-UNDO.
     DEFINE INPUT PARAMETER lpRect AS MEMPTR  NO-UNDO.

     DEFINE VARIABLE hMonitor AS INTEGER NO-UNDO.
     DEFINE VARIABLE lpMonitorInfo AS MEMPTR.
     DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
     DEFINE VARIABLE SimpleArea AS LOGICAL NO-UNDO INITIAL NO.

    /*
     IF NOT (RunningWindows98() OR RunningWindows2000()) THEN*/
        SimpleArea = YES.
     /*ELSE*/ DO:
        RUN MonitorFromWindow(HWND, 2, OUTPUT hMonitor).
        IF hMonitor = 0 THEN
           SimpleArea = YES.
        ELSE DO:
           SET-SIZE(lpMonitorInfo)    = 4 + 16 + 16 + 4.
           PUT-LONG(lpMonitorInfo, 1) = GET-SIZE(lpMonitorInfo).
           RUN GetMonitorInfoA(hMonitor,
                               GET-POINTER-VALUE(lpMonitorInfo),
                               OUTPUT ReturnValue).
           IF ReturnValue = 0 THEN
              SimpleArea = YES.
           ELSE DO:
              PUT-LONG(lpRect, 1) = GET-LONG(lpMonitorInfo, 21).
              PUT-LONG(lpRect, 5) = GET-LONG(lpMonitorInfo, 25).
              PUT-LONG(lpRect, 9) = GET-LONG(lpMonitorInfo, 29).
              PUT-LONG(lpRect,13) = GET-LONG(lpMonitorInfo, 33).
           END.
           SET-SIZE(lpMonitorInfo)    = 0.
        END.
     END.

     IF SimpleArea THEN
       RUN SystemParametersInfo{&A} IN hpApi
            ( 48,  /* 48 = SPI_GETWORKAREA */
              0,
              GET-POINTER-VALUE(lpRect),
              0,
              OUTPUT ReturnValue).

    END PROCEDURE.

    PROCEDURE MonitorFromWindow EXTERNAL "user32" :
     DEFINE INPUT  PARAMETER HWND     AS LONG.
     DEFINE INPUT  PARAMETER dwFlags  AS LONG.
     DEFINE RETURN PARAMETER hMonitor AS LONG.
    END PROCEDURE.

    PROCEDURE GetMonitorInfoA EXTERNAL "user32" :
     DEFINE INPUT PARAMETER  hMonitor      AS LONG.
     DEFINE INPUT PARAMETER  lpMonitorInfo AS LONG.
     DEFINE RETURN PARAMETER ReturnValue   AS LONG.
    END PROCEDURE.
&ENDIF
