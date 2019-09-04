CLASS TESTC:
    DEFINE TEMP-TABLE test
        FIELD c_test AS CHARACTER.

    CONSTRUCTOR TESTC ():
    END CONSTRUCTOR.

    DESTRUCTOR TESTC ():
    END DESTRUCTOR.

    {slib/browseM.i
        &methodLibrary = ""
        &file          = test
        &help          = "Please select an option."
        &display       = "test.c_test NO-LABEL FORMAT 'X(10)'
                          WITH 7 DOWN OVERLAY"
        &position      = "ROW 4 COL 2 OVERLAY"
        &where         = "WHERE TRUE"
        &sfld          = c_test
        &updaction     = " "
    }

    METHOD PUBLIC LOG createTest (INPUT cip_test AS CHARACTER):
        CREATE test.
        ASSIGN test.c_test = cip_test.
        RETURN AVAILABLE test.
    END METHOD.
END CLASS.
