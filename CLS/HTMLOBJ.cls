USING Progress.Lang.*.
&SCOPED-DEFINE HTML_DEC   "<!DOCTYPE HTML>"
&SCOPED-DEFINE HTML_META1 '<meta charset="utf-8">'
&SCOPED-DEFINE HTML_META2 '<meta name="viewport" content="width=device-width, initial-scale=1">'
&SCOPED-DEFINE HTML_TAG   "<html &1>,</html>"
&SCOPED-DEFINE HEAD_TAG   "<head &1>,</head>"
&SCOPED-DEFINE BODY_TAG   "<body &1>,</body>"
&SCOPED-DEFINE STYLE_TAG  "<style &1>,</style>"
&SCOPED-DEFINE SCRIPT_TAG "<script &1>,</script>"
&SCOPED-DEFINE TABLE_TAG  "<table &1>,</table>"
&SCOPED-DEFINE TABLE_HEAD "<th &1>,</th>"
&SCOPED-DEFINE TABLE_ROW  "<tr &1>,</tr>"
&SCOPED-DEFINE TABLE_COL  "<td &1>,</td>"
&SCOPED-DEFINE BUTTON_TAG "<button &1>,</button>"
&SCOPED-DEFINE SPAN_TAG   "<span &1>,</span>"
&SCOPED-DEFINE DIV_TAG    "<div &1>,</div>"
&SCOPED-DEFINE FORM_TAG   "<form &1>,</form>"
&SCOPED-DEFINE UL_TAG     "<ul &1>,</ul>"
&SCOPED-DEFINE LI_TAG     "<li &1>,</li>"
&SCOPED-DEFINE A_TAG      "<a &1>,</a>"
&SCOPED-DEFINE P_TAG      "<p &1>,</p>"
&SCOPED-DEFINE LABEL_TAG  "<label &1>,</label>"
&SCOPED-DEFINE NAV_TAG    "<nav &1>,</nav>"
&SCOPED-DEFINE HX_TAG     "<h&1 &2>,</h&1>"
&SCOPED-DEFINE INPUT_TAG  "<input &1 />,"
&SCOPED-DEFINE LINK_TAG   "<link &1 />,"
&SCOPED-DEFINE BREAK_TAG  "<br />,"
CLASS HTMLOBJ:
    DEFINE PUBLIC PROPERTY tagArray AS ARRAY     GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY fileName AS CHARACTER GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY fileSet  AS LOGICAL   GET. PRIVATE SET.

    DEFINE PRIVATE VARIABLE c_scriptS AS CHARACTER.
    DEFINE PRIVATE VARIABLE c_currTag AS CHARACTER.
    DEFINE PRIVATE VARIABLE l_temp    AS LOGICAL.
    DEFINE PRIVATE VARIABLE i_int     AS INTEGER.

    CONSTRUCTOR HTMLOBJ ():
        ASSIGN tagArray = NEW ARRAY().
        IF NOT VALID-OBJECT(tagArray) THEN DELETE OBJECT THIS-OBJECT.
        setScripStack().
    END CONSTRUCTOR.

    /* Overloaded */
    CONSTRUCTOR HTMLOBJ (INPUT aip_tagArray AS CLASS OBJECT):
        ASSIGN tagArray = CAST(aip_tagArray,ARRAY).
        IF NOT VALID-OBJECT(tagArray) THEN DELETE OBJECT THIS-OBJECT.
        setScripStack().
    END CONSTRUCTOR.

    DESTRUCTOR HTMLOBJ ():
        IF VALID-OBJECT(tagArray) THEN DELETE OBJECT tagArray.
    END DESTRUCTOR.

    METHOD PUBLIC LOG htmlTag (INPUT lip_end  AS LOGICAL,
                               INPUT cip_attr AS CHARACTER):
        ASSIGN c_currTag = ""
               cip_attr  = TRIM(cip_attr).

        IF NOT lip_end THEN tagArray:pushBack({&HTML_DEC},"CHAR").

        c_currTag = SUBSTITUTE(ENTRY(INTEGER(lip_end) + 1,{&HTML_TAG}),cip_attr).
        RETURN tagArray:pushBack(c_currTag,"CHAR").
    END METHOD.

    METHOD PUBLIC LOG headTag (INPUT lip_end  AS LOGICAL,
                               INPUT cip_attr AS CHARACTER):
        ASSIGN c_currTag = ""
               cip_attr  = TRIM(cip_attr).

        c_currTag = SUBSTITUTE(ENTRY(INTEGER(lip_end) + 1,{&HEAD_TAG}),cip_attr).
        IF NOT tagArray:pushBack(c_currTag,"CHAR") THEN RETURN FALSE.
        IF NOT lip_end THEN headerStack().
        RETURN TRUE.
    END METHOD.

    METHOD PUBLIC LOG bodyTag (INPUT lip_end  AS LOGICAL,
                               INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&BODY_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG scriptTag (INPUT lip_end  AS LOGICAL,
                                 INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&SCRIPT_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG tableTag (INPUT lip_end  AS LOGICAL,
                                INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&TABLE_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG tableHead (INPUT lip_end  AS LOGICAL,
                                 INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&TABLE_HEAD},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG tableRow (INPUT lip_end  AS LOGICAL,
                                INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&TABLE_ROW},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG tableCol (INPUT lip_end  AS LOGICAL,
                                INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&TABLE_COL},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG buttonTag (INPUT lip_end  AS LOGICAL,
                                 INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&BUTTON_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG styleTag (INPUT lip_end  AS LOGICAL,
                                INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&STYLE_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG spanTag (INPUT lip_end  AS LOGICAL,
                               INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&SPAN_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG divTag (INPUT lip_end  AS LOGICAL,
                              INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&DIV_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG formTag (INPUT lip_end  AS LOGICAL,
                               INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&FORM_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG ulTag (INPUT lip_end  AS LOGICAL,
                             INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&UL_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG liTag (INPUT lip_end  AS LOGICAL,
                             INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&LI_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG aTag (INPUT lip_end  AS LOGICAL,
                            INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&A_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG pTag (INPUT lip_end  AS LOGICAL,
                            INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&P_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG labelTag (INPUT lip_end  AS LOGICAL,
                                INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&LABEL_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG navTag (INPUT lip_end  AS LOGICAL,
                              INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&NAV_TAG},lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG hxTag (INPUT iip_size AS INTEGER,
                             INPUT lip_end  AS LOGICAL,
                             INPUT cip_attr AS CHARACTER):
        RETURN setTagByType(SUBSTITUTE({&HX_TAG},iip_size),lip_end,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG inputTag(INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&INPUT_TAG},FALSE,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG linkTag(INPUT cip_attr AS CHARACTER):
        RETURN setTagByType({&LINK_TAG},FALSE,cip_attr).
    END METHOD.

    METHOD PUBLIC LOG breakTag(INPUT iip_lines AS INTEGER):
        l_temp = TRUE.
        DO i_int = 1 TO iip_lines:
            l_temp = setTagByType({&BREAK_TAG},FALSE,"").
            IF NOT l_temp THEN RETURN FALSE.
        END.
        RETURN l_temp.
    END METHOD.

    METHOD PRIVATE LOG setTagByType (INPUT cip_type AS CHARACTER,
                                     INPUT lip_end  AS LOGICAL,
                                     INPUT cip_attr AS CHARACTER):
        ASSIGN c_currTag = ""
               cip_attr  = TRIM(cip_attr).

        c_currTag = SUBSTITUTE(ENTRY(INTEGER(lip_end) + 1,cip_type),cip_attr).
        RETURN tagArray:pushBack(c_currTag,"CHAR").
    END METHOD.

    METHOD PUBLIC LOG setContent (INPUT cip_val AS CHARACTER):
        RETURN tagArray:pushBack(cip_val,"CHAR").
    END METHOD.

    METHOD PUBLIC LOG setFilename (INPUT cip_file AS CHARACTER):
        IF fileSet THEN RETURN FALSE.
        fileName = cip_file.
        fileSet = fileName = cip_file.
        RETURN fileSet.
    END METHOD.

    METHOD PUBLIC LOG writeFile ():
        IF NOT fileSet THEN RETURN FALSE.
        OUTPUT TO VALUE(fileName).
            DO i_int = 1 TO tagArray:LENGTH:
                PUT UNFORMATTED tagArray:getIndexCHAR(i_int) SKIP.
            END.
        OUTPUT CLOSE.
        fileSet = SEARCH(fileName) <> ?.
        RETURN fileSet = FALSE.
    END METHOD.

    METHOD PRIVATE VOID setScripStack ():
        ASSIGN c_scriptS =
            '<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css" />' + "," +
            '<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>'               + "," +
            '<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.0/umd/popper.min.js"></script>'      + "," +
            '<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js"></script>'            + "," +
            '<link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css" />'.
    END METHOD.

    METHOD PRIVATE VOID headerStack ():
        tagArray:pushBack({&HTML_META1},"CHAR").
        tagArray:pushBack({&HTML_META2},"CHAR").
        DO i_int = 1 TO NUM-ENTRIES(c_scriptS):
            tagArray:pushBack(ENTRY(i_int,c_scriptS),"CHAR").
        END.
    END METHOD.
END CLASS.
