CLASS EXCELPROC:
    CONSTRUCTOR EXCELPROC ():
        INIT().
    END CONSTRUCTOR.

    DESTRUCTOR EXCELPROC ():
        PROC_STOP().
    END DESTRUCTOR.

    {slib/excel_procsM.i &METHOD_SET}
END CLASS.
