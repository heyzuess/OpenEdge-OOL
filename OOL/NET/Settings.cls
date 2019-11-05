/*****************************************************************************************
  Copyright © 2019 by Jesse Iberri <jesseiberri@gmail.com>
  Released under the GNU General Public License GPL-3.0-or-later

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
          
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
          
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>

  Name     : Settings.cls
  Author   : Jesse Iberri
  Date     : 09/01/2019
  Purpose  : OOL NET Settings Class

------------------------------------------------------------------------------------------
MODIFICATIONS:
------------------------------------------------------------------------------------------
Date        Who         Reference       Description
----------  ----------- --------------- --------- ----------------------------------------
------------------------------------------------------------------------------------------
*****************************************************************************************/
USING System.Windows.Forms.*      FROM ASSEMBLY.
USING Progress.Json.ObjectModel.* FROM ASSEMBLY.
USING OOL.NET.*.

CLASS OOL.NET.Settings USE-WIDGET-POOL:
    DEFINE PUBLIC PROPERTY THIS AS OOL.NET.Settings
    GET ():
        RETURN THIS-OBJECT.
    END GET.
    PRIVATE SET.

    DEFINE PUBLIC PROPERTY AppName         AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY ObjectName      AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY FullPathName    AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY ProgramPath     AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY ProgramName     AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY ProgramSettings AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY ProgramRESX     AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY ProgramPropath  AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY RESX            AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY PATH            AS CHARACTER GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE oParser      AS ObjectModelParser NO-UNDO.
    DEFINE PRIVATE VARIABLE oJson        AS JsonObject        NO-UNDO.
    DEFINE PRIVATE VARIABLE lcv_fileTemp AS LONGCHAR          NO-UNDO.
    DEFINE PRIVATE VARIABLE lv_pathSet   AS LOGICAL           NO-UNDO.
    DEFINE PRIVATE VARIABLE lv_makeFile  AS LOGICAL           NO-UNDO.
    DEFINE PRIVATE VARIABLE iv_int       AS INTEGER           NO-UNDO.
    DEFINE PRIVATE VARIABLE cv_temp      AS CHARACTER         NO-UNDO.

    DEFINE TEMP-TABLE AppSetting
        FIELD AppLocation      AS CHARACTER
        FIELD ResourceLocation AS CHARACTER
        FIELD ResourceRESX     AS CHARACTER
        FIELD ExtendPropath    AS CHARACTER.

    CONSTRUCTOR Settings (INPUT cip_AppName    AS CHARACTER,
                          INPUT cip_ObjectName AS CHARACTER):
        SUPER().
        ASSIGN THIS:AppName    = cip_AppName
               THIS:ObjectName = cip_ObjectName.

        IF NOT THIS:IntializeSettings() THEN DELETE OBJECT(THIS).
    END CONSTRUCTOR.

    DESTRUCTOR Settings ():
    END DESTRUCTOR.

    METHOD PRIVATE LOGICAL IntializeSettings ():
        ASSIGN FullPathName    = TRIM(REPLACE(THIS:AppName, "Initial", ""))
               ProgramName     = ENTRY(NUM-ENTRIES(FullPathName,"~\"), FullPathName, "~\")
               ProgramPath     = TRIM(REPLACE(FullPathName, ProgramName, ""))
               ProgramPath     = SUBSTRING(ProgramPath, 1, LENGTH(ProgramPath) - 1)
               ProgramSettings = ProgramPath + "~\settings.json".

        IF SEARCH(ProgramSettings) = ? THEN
        DO:
            ASSIGN lv_makeFile = TRUE.
        END.

        FIX-CODEPAGE(lcv_fileTemp) = "UTF-8".

        IF NOT lv_makeFile THEN
        DO:
            DO ON ERROR UNDO, THROW:
                COPY-LOB FROM FILE ProgramSettings TO lcv_fileTemp.
                CATCH e AS Progress.Lang.Error:
                    MessageBox:Show(e:GetMessage(1)).
                    RETURN FALSE.
                END CATCH.
            END.

            DO ON ERROR UNDO, THROW:
                ASSIGN oParser = NEW ObjectModelParser().
                ASSIGN oJson = CAST(oParser:Parse(lcv_fileTemp), JsonObject).
                CATCH e AS Progress.Lang.Error:
                    MessageBox:Show(e:GetMessage(1)).
                    RETURN FALSE.
                END CATCH.
            END.

            IF oJson:Has("AppLocation") THEN
            DO:
                ASSIGN ProgramPath = oJson:GetCharacter("AppLocation").
            END.
            ELSE
            IF oJson:Has("ResourceLocation") THEN
            DO:
                ASSIGN ProgramPath = oJson:GetCharacter("ResourceLocation").
            END.

            IF oJson:Has("ResourceRESX") THEN
            DO:
                ASSIGN ProgramRESX = oJson:GetCharacter("ResourceRESX").
            END.

            IF oJson:Has("ExtendPropath") THEN
            DO:
                ASSIGN ProgramPropath = oJson:GetCharacter("ExtendPropath").
                DO iv_int = 1 TO NUM-ENTRIES(ProgramPropath):
                    ASSIGN cv_temp = ENTRY(iv_int, ProgramPropath).

                    IF cv_temp = "" THEN NEXT.

                    IF LOOKUP(cv_temp, PROPATH) = 0 THEN
                        ASSIGN PROPATH          = PROPATH + "," + cv_temp.
                END.
            END.
        END.

        IF ProgramRESX = "" THEN ASSIGN ProgramRESX = THIS:ObjectName + ".resx".
        
        IF LOOKUP(ProgramPath, PROPATH) = 0 THEN
            ASSIGN PROPATH = PROPATH + "," + ProgramPath.

        IF lv_makeFile THEN
        DO ON ERROR UNDO, THROW:
            CREATE AppSetting.
            ASSIGN AppSetting.AppLocation      = ProgramPath
                   AppSetting.ResourceLocation = ProgramPath
                   AppSetting.ResourceRESX     = ProgramRESX
                   AppSetting.ExtendPropath    = ProgramPropath.

            TEMP-TABLE AppSetting:WRITE-JSON("LONGCHAR", lcv_fileTemp, TRUE, "UTF-8", FALSE, TRUE).
            ASSIGN lcv_fileTemp = TRIM(lcv_fileTemp)
                   lcv_fileTemp = SUBSTRING(lcv_fileTemp, 2, LENGTH(lcv_fileTemp))
                   lcv_fileTemp = SUBSTRING(lcv_fileTemp, 1, LENGTH(lcv_fileTemp) - 1).
            COPY-LOB FROM lcv_fileTemp TO FILE ProgramSettings.

            CATCH e AS Progress.Lang.Error:
                MessageBox:Show(e:GetMessage(1)).
                RETURN FALSE.
            END CATCH.
        END.

        ASSIGN THIS:RESX = THIS:ProgramRESX
               THIS:PATH = PROPATH.
        RETURN TRUE.
    END METHOD.
END CLASS.