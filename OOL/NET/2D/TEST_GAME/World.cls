USING System.*         FROM ASSEMBLY.
USING System.Drawing.* FROM ASSEMBLY.

USING OOL.NET.2D.TEST_GAME.*.

CLASS World INHERITS ResourceLoader USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY THIS AS World
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY Boom             AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Bush             AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Clouds           AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Coin             AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Floor            AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Pipe             AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY Platform         AS Bitmap    GET. PUBLIC SET.

    CONSTRUCTOR World ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR World ():
    END DESTRUCTOR.
END CLASS.