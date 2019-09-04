FUNCTION getEntry RETURNS CHARACTER (INPUT X AS INTEGER,
                                     INPUT Y AS CHARACTER,
                                     INPUT Z AS CHARACTER) FORWARD.

FUNCTION getEntry RETURNS CHARACTER (INPUT iip_entry  AS INTEGER,
                                     INPUT cip_string AS CHARACTER,
                                     INPUT cip_delim  AS CHARACTER):
    IF cip_delim = "" THEN ASSIGN cip_delim = ",".
    RETURN ENTRY(iip_entry, REPLACE(cip_string,cip_delim,",")).
END FUNCTION.
