USING System.*         FROM ASSEMBLY.
USING System.Drawing.* FROM ASSEMBLY.

USING OOL.NET.2D.TEST_GAME.*.

CLASS OOL.NET.2D.TEST_GAME.ResourceLoader ABSTRACT USE-WIDGET-POOL:
    DEFINE PUBLIC PROPERTY ResourceLocation AS CHARACTER GET. PUBLIC SET.
    
    DEFINE PROTECTED VARIABLE tempBitmap      AS Bitmap NO-UNDO.

    CONSTRUCTOR ResourceLoader ():
        SUPER().
    END CONSTRUCTOR.

    DESTRUCTOR ResourceLoader ():
    END DESTRUCTOR.
    
    METHOD PUBLIC Bitmap LoadFromDir (INPUT cip_filename AS CHARACTER):
        THIS-OBJECT:tempBitmap = ?.

        THIS-OBJECT:tempBitmap = NEW Bitmap(THIS-OBJECT:ResourceLocation + cip_filename).
        RETURN THIS-OBJECT:tempBitmap.
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.
END CLASS.