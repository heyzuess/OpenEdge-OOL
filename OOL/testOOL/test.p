USING OOL.*.

DEFINE VARIABLE o_frame AS WINFRAME.
DEFINE VARIABLE o_fname AS WINFILL.
DEFINE VARIABLE o_lname AS WINFILL.

o_frame = NEW WINFRAME().

ASSIGN o_frame:WIDTH   = 40
       o_frame:HEIGHT  = 10
       o_frame:TITLE   = "My App"
       o_frame:COLOR   = 1.
o_frame:REGISTER().

o_fname = NEW WINFILL().
ASSIGN o_fname:PARENT  = o_frame
       o_fname:VALUE   = "Enter First Name"
       o_fname:WIDTH   = LENGTH(o_fname:VALUE)
       o_fname:HEIGHT  = 1
       o_fname:X       = o_frame:WIDTH / 2 - o_fname:WIDTH / 2 - 1
       o_fname:Y       = 1
       o_fname:COLOR   = o_frame:COLOR
       o_fname:VISIBLE = TRUE.
o_fname:ADD-LABEL("First Name").
o_fname:REGISTER().

o_lname = NEW WINFILL().
ASSIGN o_lname:PARENT  = o_frame
       o_lname:VALUE   = "Enter Last Name"
       o_lname:WIDTH   = LENGTH(o_lname:VALUE)
       o_lname:HEIGHT  = 1
       o_lname:X       = o_fname:X
       o_lname:Y       = o_fname:Y + 2
       o_lname:COLOR   = o_frame:COLOR
       o_lname:VISIBLE = TRUE.
o_lname:ADD-LABEL("Last Name").
o_lname:REGISTER().

o_fname:FOCUS-OFF:SUBSCRIBE("fname_leave").
o_lname:FOCUS-OFF:SUBSCRIBE("lname_leave").
o_frame:ENABLE-ALL().

PROCEDURE fname_leave:
    DEFINE INPUT PARAMETER aip_data AS ARRAY.
    DEFINE VARIABLE cv_data AS CHARACTER.

    IF NOT VALID-OBJECT(aip_data) THEN LEAVE.

    cv_data = aip_data:INDEX-CHAR(1).

    IF TRIM(cv_data) = "" OR TRIM(cv_data) = ? THEN
    DO:
        MESSAGE "First name can not be left blank"
        VIEW-AS ALERT-BOX.

        o_fname:ENABLE().
    END.
END PROCEDURE.

PROCEDURE lname_leave:
    DEFINE INPUT PARAMETER aip_data AS ARRAY.
    DEFINE VARIABLE cv_data AS CHARACTER.

    IF NOT VALID-OBJECT(aip_data) THEN LEAVE.

    cv_data = aip_data:INDEX-CHAR(1).

    IF TRIM(cv_data) = "" OR TRIM(cv_data) = ? THEN
    DO:
        MESSAGE "Last name can not be left blank"
        VIEW-AS ALERT-BOX.

        o_lname:ENABLE().
    END.
END PROCEDURE.