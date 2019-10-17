USING System.*           FROM ASSEMBLY.
USING System.Drawing.*   FROM ASSEMBLY.

USING OOL.NET.2D.TEST_GAME.*.

CLASS Actor INHERITS ResourceLoader USE-WIDGET-POOL:
    DEFINE PROTECTED PROPERTY THIS AS Actor
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.
    
    DEFINE PUBLIC PROPERTY jump_l           AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY jump_r           AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY stand_l          AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY stand_r          AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY walk_l           AS Bitmap    GET. PUBLIC SET.
    DEFINE PUBLIC PROPERTY walk_r           AS Bitmap    GET. PUBLIC SET.

    CONSTRUCTOR Actor ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR Actor ():
    END DESTRUCTOR.
END CLASS.