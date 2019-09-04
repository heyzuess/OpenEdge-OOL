CLASS DBOBJ:
    DEFINE PUBLIC PROPERTY c_dbName    AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY l_connected AS LOGICAL   GET. PRIVATE SET.

    CONSTRUCTOR DBOBJ (INPUT cip_name AS CHARACTER):
        ASSIGN c_dbName = cip_name.
    END CONSTRUCTOR.

    DESTRUCTOR DBOBJ ():
        IF CONNECTED(c_dbName) THEN DISCONNECT VALUE(c_dbName).
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL connectDB ():
        CONNECT VALUE(c_dbName).
        l_connected = CONNECTED(c_dbName).

        RETURN l_connected.
    END METHOD.

    METHOD PUBLIC LOGICAL disconnectDB():
        DISCONNECT VALUE(c_dbName).
        l_connected = CONNECTED(c_dbName).

        RETURN NOT l_connected.
    END METHOD.
END CLASS.