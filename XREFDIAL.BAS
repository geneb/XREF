'$SubTitle:'Dialog Box Set Up Routines for XREF'

DEFINT A-Z

'$INCLUDE: 'DialType.Bi'
'$INCLUDE: 'XrefCnst.bi'

DECLARE FUNCTION EnterFileName$ (Default$)
DECLARE FUNCTION FileNameOk (FileName$)
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION QPValI% (Number$)
DECLARE FUNCTION Power2& (Power%)

DECLARE SUB DialogBox (Title$, PrmtAns$(), Dl() AS Dialog, Mult$(), ExitCode%)

FUNCTION EnterFileName$ (Default$) STATIC

    EnterFileName$ = ""

    REDIM Dial(3) AS Dialog
    REDIM PA$(3, 1)
    REDIM Mult$(0)

    Dial(1).DRow = 2
    Dial(1).DCol = 2
    Dial(1).DLen = 50
    Dial(1).DTyp = 1
    PA$(1, 0) = "File Name: "
    PA$(1, 1) = Default$

    Dial(2).DRow = 4
    Dial(2).DCol = 14
    Dial(2).DTyp = 5
    PA$(2, 0) = "Save"

    Dial(3).DRow = 4
    Dial(3).DCol = 40
    Dial(3).DTyp = 5
    PA$(3, 0) = "Cancel"

    DO
       LOCATE 10
       DialogBox "", PA$(), Dial(), Mult$(), ExitCode

       IF ExitCode = 1 THEN
          IF FileNameOk(PA$(1, 1)) THEN
             EnterFileName$ = PA$(1, 1)
             EXIT DO
          END IF
       ELSE
          EXIT DO
       END IF
    LOOP

END FUNCTION

SUB GetRange (LowRange, HiRange, ProcName$, ObjTypes&, ObjType$(), Inf AS GenInfo)

    REDIM Dial(20) AS Dialog
    REDIM PA$(20, 1)
    REDIM Mult$(0)

    Dial(1).DRow = 1
    Dial(1).DCol = 2
    Dial(1).DTyp = 6
    PA$(1, 0) = "Use this report type to find objects that are referenced within a"

    Dial(2).DRow = 2
    Dial(2).DCol = 2
    Dial(2).DTyp = 6
    PA$(2, 0) = "range of line numbers but also used elsewhere.  This information"

    Dial(3).DRow = 3
    Dial(3).DCol = 2
    Dial(3).DTyp = 6
    PA$(3, 0) = "can be used when placing a piece of code within a subprogram or "

    Dial(4).DRow = 4
    Dial(4).DCol = 2
    Dial(4).DTyp = 6
    PA$(4, 0) = "function to find what parameters will need to be passed or shared."
    

    Dial(5).DRow = 6
    Dial(5).DCol = 5
    Dial(5).DLen = 6
    Dial(5).DTyp = 2
    PA$(5, 0) = "Between Lines: "
    PA$(5, 1) = STR$(LowRange)

    Dial(6).DRow = 6
    Dial(6).DCol = 29
    Dial(6).DLen = 6
    Dial(6).DTyp = 2
    PA$(6, 0) = "and "
    PA$(6, 1) = STR$(HiRange)

    Dial(7).DRow = 6
    Dial(7).DCol = 42
    Dial(7).DTyp = 6
    PA$(7, 0) = "Inclusive."
                    
    Dial(8).DRow = 9
    Dial(8).DCol = 2
    Dial(8).DLen = 40
    Dial(8).DTyp = 1
    PA$(8, 1) = ProcName$
    IF Inf.LineNumbers = 2 THEN
       IF LEN(ProcName$) = 0 THEN ProcName$ = "[MAIN]"
       PA$(8, 0) = "Within Procedure: "
       PA$(8, 1) = ProcName$
    ELSE
       PA$(8, 0) = "   Within Module: "
    END IF
    NLn = 11

    Dial(9).DRow = NLn
    Dial(9).DCol = 20
    Dial(9).DTyp = 6
    PA$(9, 0) = "Select object types to report:"
    NLn = NLn + 1

    FOR N = 0 TO 8
       Dial(N + 10).DRow = N + NLn
       Dial(N + 10).DCol = 20
       Dial(N + 10).DTyp = 3
       PA$(N + 10, 0) = ObjType$(N + Label) + "s"

       IF ObjTypes& AND Power2&(N + Label + 1) THEN PA$(N + 10, 1) = "û"
    NEXT

    Dial(N + 10).DRow = N + NLn
    Dial(N + 10).DCol = 20
    Dial(N + 10).DTyp = 5
    PA$(N + 10, 0) = "OK"

    Dial(N + 11).DRow = N + NLn
    Dial(N + 11).DCol = 40
    Dial(N + 11).DTyp = 5
    PA$(N + 11, 0) = "Cancel"

    LOCATE 1
    DialogBox "Object Types", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       LowRange = QPValI(PA$(5, 1))
       IF LowRange < 0 THEN LowRange = 0

       HiRange = QPValI(PA$(6, 1))
       IF HiRange < LowRange THEN HiRange = LowRange

       ProcName$ = PA$(8, 1)

       ObjTypes& = 0
       FOR N = 0 TO 8
          IF PA$(N + 10, 1) = "û" THEN
             ObjTypes& = ObjTypes& XOR Power2&(N + Label + 1)
          END IF
       NEXT
    END IF

END SUB

SUB SelectObjTypes (ObjTypes&, ObjType$(), NoKeys) STATIC

    REDIM Dial(16) AS Dialog
    REDIM PA$(16, 1)
    REDIM Mult$(0)

    FOR N = 1 TO 13
       Dial(N).DRow = N + 2
       Dial(N).DCol = 6
       Dial(N).DTyp = 3
       PA$(N, 0) = ObjType$(N - 1) + "s"

       IF ObjTypes& AND Power2&(N) THEN PA$(N, 1) = "û"
    NEXT

    IF NoKeys THEN
       Dial(1).DTyp = 6
       PA$(1, 0) = ""
    END IF

    Dial(14).DRow = 1
    Dial(14).DCol = 2
    Dial(14).DTyp = 6
    PA$(14, 0) = "Select object types to report on."

    Dial(15).DRow = 16
    Dial(15).DCol = 7
    Dial(15).DTyp = 5
    PA$(15, 0) = "OK"
  
    Dial(16).DRow = 16
    Dial(16).DCol = 20
    Dial(16).DTyp = 5
    PA$(16, 0) = "Cancel"

    LOCATE 4
    DialogBox "Object Types", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       ObjTypes& = 0
       FOR N = 1 TO 13
          IF PA$(N, 1) = "û" THEN
             ObjTypes& = ObjTypes& OR Power2&(N)
          END IF
       NEXT
    END IF

    ERASE Dial, PA$, Mult$

END SUB

SUB SetExtensions (OdbExtension$, XrfExtension$, TextExtension$) STATIC

    REDIM Dial(17) AS Dialog
    REDIM PA$(17, 1)
    REDIM Mult$(0)

    Dial(1).DRow = 1
    Dial(1).DCol = 2
    Dial(1).DTyp = 6
    PA$(1, 0) = "After a program has been read and analyzed, the information about all"

    Dial(2).DRow = 2
    Dial(2).DCol = 2
    Dial(2).DTyp = 6
    PA$(2, 0) = "the objects in the program will be saved to a database with the same"

    Dial(3).DRow = 3
    Dial(3).DCol = 2
    Dial(3).DTyp = 6
    PA$(3, 0) = "base name as the source file and the extension shown below.  This"

    Dial(4).DRow = 4
    Dial(4).DCol = 2
    Dial(4).DTyp = 6
    PA$(4, 0) = "makes it possible to run different reports without having to reread"

    Dial(5).DRow = 5
    Dial(5).DCol = 2
    Dial(5).DTyp = 6
    PA$(5, 0) = "the entire program.  If you don't want to save this information, enter"

    Dial(6).DRow = 6
    Dial(6).DCol = 2
    Dial(6).DTyp = 6
    PA$(6, 0) = "`.NUL' below."

    Dial(7).DRow = 7
    Dial(7).DCol = 13
    Dial(7).DLen = 4
    Dial(7).DTyp = 1
    PA$(7, 0) = "Object Database Extension: "
    PA$(7, 1) = OdbExtension$


    Dial(8).DRow = 9
    Dial(8).DCol = 2
    Dial(8).DTyp = 6
    PA$(8, 0) = "If you select `File' from the `Output' menu, all reports will be"

    Dial(9).DRow = 10
    Dial(9).DCol = 2
    Dial(9).DTyp = 6
    PA$(9, 0) = "written to a file with the same base name as the source file with the"

    Dial(10).DRow = 11
    Dial(10).DCol = 2
    Dial(10).DTyp = 6
    PA$(10, 0) = "extension shown below."

    Dial(11).DRow = 12
    Dial(11).DCol = 15
    Dial(11).DLen = 4
    Dial(11).DTyp = 1
    PA$(11, 0) = "`File' Output Extension: "
    PA$(11, 1) = XrfExtension$


    Dial(12).DRow = 14
    Dial(12).DCol = 2
    Dial(12).DTyp = 6
    PA$(12, 0) = "If you select `Extract Source Text' from the `Report' menu, all quoted"

    Dial(13).DRow = 15
    Dial(13).DCol = 2
    Dial(13).DTyp = 6
    PA$(13, 0) = "strings and comments will be written to a file with the same base name"

    Dial(14).DRow = 16
    Dial(14).DCol = 2
    Dial(14).DTyp = 6
    PA$(14, 0) = "as the source file with the Extension shown below."

    Dial(15).DRow = 18
    Dial(15).DCol = 2
    Dial(15).DLen = 4
    Dial(15).DTyp = 1
    PA$(15, 0) = "`Extract Source Text' File Extension: "
    PA$(15, 1) = TextExtension$

    Dial(16).DRow = 20
    Dial(16).DCol = 20
    Dial(16).DTyp = 5
    PA$(16, 0) = "OK"
 
    Dial(17).DRow = 20
    Dial(17).DCol = 41
    Dial(17).DTyp = 5
    PA$(17, 0) = "Cancel"

    LOCATE 2
    DialogBox "Output File Extensions", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       OdbExtension$ = LTRIM$(RTRIM$(UCASE$(PA$(7, 1))))
       IF INSTR(OdbExtension$, ".") = 0 THEN OdbExtension$ = "." + OdbExtension$
       XrfExtension$ = LTRIM$(RTRIM$(UCASE$(PA$(11, 1))))
       IF INSTR(XrfExtension$, ".") = 0 THEN XrfExtension$ = "." + XrfExtension$
       TextExtension$ = LTRIM$(RTRIM$(UCASE$(PA$(15, 1))))
       IF INSTR(TextExtension$, ".") = 0 THEN TextExtension$ = "." + TextExtension$
    END IF

    ERASE Dial, PA$, Mult$

END SUB

SUB SetOptions (Inf AS GenInfo) STATIC

    REDIM Dial(14) AS Dialog
    REDIM PA$(14, 1)
    REDIM Mult$(3)

    Dial(1).DRow = 1
    Dial(1).DCol = 3
    Dial(1).DTyp = 6
    PA$(1, 0) = "Display Source when reading:"

    Dial(2).DRow = 1
    Dial(2).DCol = 38
    Dial(2).DTyp = 3
    IF Inf.DispText THEN PA$(2, 1) = "û"
    

    Dial(3).DRow = 2
    Dial(3).DCol = 3
    Dial(3).DTyp = 6
    PA$(3, 0) = "Use Graphics Characters in Reports:"

    Dial(4).DRow = 2
    Dial(4).DCol = 38
    Dial(4).DTyp = 3
    IF Inf.GraphChars THEN PA$(4, 1) = "û"


    Dial(5).DRow = 4
    Dial(5).DCol = 2
    Dial(5).DTyp = 6
    PA$(5, 0) = "Make line numbers "

    Dial(6).DRow = 5
    Dial(6).DCol = 2
    Dial(6).DTyp = 6
    PA$(6, 0) = "relative to:"

    Dial(7).DRow = 4
    Dial(7).DCol = 20
    Dial(7).DLen = 3 + 256
    Dial(7).DTyp = 4
   
    PA$(7, 1) = STR$(Inf.LineNumbers + 1)
    Mult$(1) = "No Line Numbers  "
    Mult$(2) = "Beginning of File"
    Mult$(3) = "Procedures       "


    Dial(8).DRow = 9
    Dial(8).DCol = 1
    Dial(8).DTyp = 6
    PA$(8, 0) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄ[ Settings ]ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"


    Dial(9).DRow = 11
    Dial(9).DCol = 3
    Dial(9).DLen = 5
    Dial(9).DTyp = 2
    PA$(9, 0) = "Maximum objects/module:        "
    PA$(9, 1) = STR$(Inf.UBSymbols)

    Dial(10).DRow = 12
    Dial(10).DCol = 4
    Dial(10).DTyp = 6
    PA$(10, 0) = "(1000-3000), 2000 recommended"

    Dial(11).DRow = 14
    Dial(11).DCol = 3
    Dial(11).DLen = 4
    Dial(11).DTyp = 2
    PA$(11, 0) = "Maximum tokens/logical line:    "
    PA$(11, 1) = STR$(Inf.UBWords)

    Dial(12).DRow = 15
    Dial(12).DCol = 4
    Dial(12).DTyp = 6
    PA$(12, 0) = "(100-500), 300 recommended"

    Dial(13).DRow = 16
    Dial(13).DCol = 10
    Dial(13).DTyp = 5
    PA$(13, 0) = "OK"
  
    Dial(14).DRow = 16
    Dial(14).DCol = 25
    Dial(14).DTyp = 5
    PA$(14, 0) = "Cancel"

    LOCATE 4
    DialogBox "Options", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       Inf.DispText = PA$(2, 1) = "û"
       Inf.GraphChars = PA$(4, 1) = "û"
       Inf.LineNumbers = QPValI(PA$(7, 1)) - 1

       Inf.UBSymbols = QPValI(PA$(9, 1))
       IF Inf.UBSymbols < 1000 THEN
          Inf.UBSymbols = 1000
       ELSEIF Inf.UBSymbols > 3000 THEN
          Inf.UBSymbols = 3000
       END IF

       Inf.UBWords = QPValI(PA$(11, 1))
       IF Inf.UBWords < 100 THEN
          Inf.UBWords = 100
       ELSEIF Inf.UBWords > 500 THEN
          Inf.UBWords = 500
       END IF

    END IF

    ERASE Dial, PA$, Mult$

END SUB

SUB SetUpExtractText (Flag)

    REDIM Dial(5) AS Dialog
    REDIM PA$(5, 1)
    REDIM Mult$(0)

   
    Dial(1).DRow = 2
    Dial(1).DCol = 2
    Dial(1).DTyp = 3
    PA$(1, 0) = "Extract quoted strings. "
    IF Flag AND 1 THEN
       PA$(1, 1) = "û"
    ELSE
       PA$(1, 1) = " "
    END IF

   
    Dial(2).DRow = 3
    Dial(2).DCol = 2
    Dial(2).DTyp = 3
    PA$(2, 0) = "Extract REMarks."
    IF Flag AND 2 THEN
       PA$(2, 1) = "û"
    ELSE
       PA$(2, 1) = " "
    END IF

   
    Dial(3).DRow = 4
    Dial(3).DCol = 29
    Dial(3).DTyp = 6
   

    Dial(4).DRow = 5
    Dial(4).DCol = 5
    Dial(4).DTyp = 5
    PA$(4, 0) = "OK"
 
    Dial(5).DRow = 5
    Dial(5).DCol = 16
    Dial(5).DTyp = 5
    PA$(5, 0) = "Cancel"


    LOCATE 7
    DialogBox "Extract Text", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN

       Flag = 2 * ABS(PA$(2, 1) = "û") + ABS(PA$(1, 1) = "û")

    END IF

END SUB

SUB SetUpPrinter (PrinterName$, PrnSetUp$, PrnReset$, PrnWidth, PrnLength, PrnLeftMarg, PrnBotMarg) STATIC

    REDIM Dial(9) AS Dialog
    REDIM PA$(9, 1)
    REDIM Mult$(0)

    Dial(1).DRow = 2
    Dial(1).DCol = 10
    Dial(1).DLen = 5
    Dial(1).DTyp = 2
    PA$(1, 0) = "Page Width: "
    PA$(1, 1) = STR$(PrnWidth)

   
    Dial(2).DRow = 2
    Dial(2).DCol = 34
    Dial(2).DLen = 5
    Dial(2).DTyp = 2
    PA$(2, 0) = "Page Length: "
    PA$(2, 1) = STR$(PrnLength)

   
    Dial(3).DRow = 5
    Dial(3).DCol = 9
    Dial(3).DLen = 5
    Dial(3).DTyp = 2
    PA$(3, 0) = "Left Margin: "
    PA$(3, 1) = STR$(PrnLeftMarg)


    Dial(4).DRow = 5
    Dial(4).DCol = 32
    Dial(4).DLen = 5
    Dial(4).DTyp = 2
    PA$(4, 0) = "Bottom Margin: "
    PA$(4, 1) = STR$(PrnBotMarg)

   
    Dial(5).DRow = 8                    'Init String
    Dial(5).DCol = 2
    Dial(5).DLen = 40
    Dial(5).DTyp = 1
                   
    PA$(5, 0) = "Printer Init. Code: "
    PA$(5, 1) = PrnSetUp$
 
    Dial(6).DRow = 7                    'Init String Comments
    Dial(6).DCol = 24
    Dial(6).DTyp = 6

    PA$(6, 0) = "[ie. 27, M]"

   
    Dial(7).DRow = 11                   'Reset String
    Dial(7).DCol = 2
    Dial(7).DLen = 40
    Dial(7).DTyp = 1
                  
    PA$(7, 0) = "Printer Reset Code: "
    PA$(7, 1) = PrnReset$


    Dial(8).DRow = 13
    Dial(8).DCol = 18
    Dial(8).DTyp = 5
    PA$(8, 0) = "OK"
  
    Dial(9).DRow = 13
    Dial(9).DCol = 38
    Dial(9).DTyp = 5
    PA$(9, 0) = "Cancel"

    LOCATE 6
    DialogBox PrinterName$ + " Setup", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       PrnWidth = QPValI(PA$(1, 1))
       PrnLength = QPValI(PA$(2, 1))
       'IF PrnLength = 0 THEN PrnLength = -32766
       PrnLeftMarg = QPValI(PA$(3, 1))
       IF PrnLeftMarg < 1 THEN PrnLeftMarg = 1
       IF PrnLeftMarg > PrnWidth - 10 THEN PrnLeftMarg = PrnWidth - 10
       PrnBotMarg = QPValI(PA$(4, 1))
       IF PrnBotMarg < 2 THEN PrnBotMarg = 2
       'IF PrnBotMarg >= PrnLength THEN PrnBotMarg = PrnLength - 1
       PrnSetUp$ = PA$(5, 1)
       PrnReset$ = PA$(7, 1)
    END IF

    ERASE Dial, PA$, Mult$

END SUB

SUB SetupProcTree (Inf AS GenInfo) STATIC

    REDIM Dial(15) AS Dialog
    REDIM PA$(15, 1)
    REDIM Mult$(0)

    Dial(1).DRow = 1
    Dial(1).DCol = 2
    Dial(1).DTyp = 3
    PA$(1, 0) = "Use IBM line characters"
    IF Inf.GraphChars THEN
       PA$(1, 1) = "û"
    ELSE
       PA$(1, 1) = " "
    END IF

    Dial(1).DRow = 1
    Dial(1).DCol = 6
    Dial(1).DTyp = 6
    PA$(1, 0) = "Procedures can be sorted such that they appear "

    Dial(2).DRow = 2
    Dial(2).DCol = 6
    Dial(2).DTyp = 6
    PA$(2, 0) = "alphabetically and only once on any branch of"

    Dial(3).DRow = 3
    Dial(3).DCol = 6
    Dial(3).DTyp = 6
    PA$(3, 0) = "the tree.  If they are not sorted, they will"

    Dial(4).DRow = 4
    Dial(4).DCol = 6
    Dial(4).DTyp = 6
    PA$(4, 0) = "appear in the order in which they were used."

    Dial(5).DRow = 5
    Dial(5).DCol = 2
    Dial(5).DTyp = 3
    PA$(5, 0) = "Sort Procedures"
    IF Inf.SortProcTree THEN
       PA$(5, 1) = "û"
    ELSE
       PA$(5, 1) = " "
    END IF

    Dial(6).DRow = 7
    Dial(6).DCol = 6
    Dial(6).DTyp = 6
    PA$(6, 0) = "Procedures that call many other procedures and"

    Dial(7).DRow = 8
    Dial(7).DCol = 6
    Dial(7).DTyp = 6
    PA$(7, 0) = "and are in turn called many times can take up a"

    Dial(8).DRow = 9
    Dial(8).DCol = 6
    Dial(8).DTyp = 6
    PA$(8, 0) = "great deal of room in a report.  Check off the"

    Dial(9).DRow = 10
    Dial(9).DCol = 6
    Dial(9).DTyp = 6
    PA$(9, 0) = "box below to show the detail for each procedure"

    Dial(10).DRow = 11
    Dial(10).DCol = 6
    Dial(10).DTyp = 6
    PA$(10, 0) = "only once in the report."

    Dial(11).DRow = 12
    Dial(11).DCol = 2
    Dial(11).DTyp = 3
    PA$(11, 0) = "Show procedure detail only once"
    IF Inf.DetailProcTree MOD 256 THEN
       PA$(11, 1) = " "
    ELSE
       PA$(11, 1) = "û"
    END IF

    Dial(12).DRow = 14
    Dial(12).DCol = 2
    Dial(12).DTyp = 3
    PA$(12, 0) = "Insert page breaks in report"
    IF Inf.ProcTreePages THEN
       PA$(12, 1) = "û"
    ELSE
       PA$(12, 1) = " "
    END IF

    Dial(13).DRow = 16
    Dial(13).DCol = 2
    Dial(13).DTyp = 3
    PA$(13, 0) = "Exclude EXTERNAL procedure names from tree"
    IF Inf.DetailProcTree \ 256 THEN
       PA$(13, 1) = "û"
    ELSE
       PA$(13, 1) = " "
    END IF

    Dial(14).DRow = 17
    Dial(14).DCol = 14
    Dial(14).DTyp = 5
    PA$(14, 0) = "OK"
 
    Dial(15).DRow = 17
    Dial(15).DCol = 32
    Dial(15).DTyp = 5
    PA$(15, 0) = "Cancel"

    LOCATE 3
    DialogBox "Procedure Tree Setup", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       Inf.SortProcTree = PA$(5, 1) = "û"
       Inf.DetailProcTree = 256 * (PA$(13, 1) = "û") + (PA$(11, 1) <> "û")
       Inf.ProcTreePages = PA$(12, 1) = "û"
    END IF

    ERASE Dial, PA$, Mult$

END SUB

SUB SetUpSourceListing (PageTitle$, Inf AS GenInfo) STATIC

    REDIM Dial(7) AS Dialog
    REDIM PA$(7, 1)
    REDIM Mult$(3)

    Dial(1).DRow = 2
    Dial(1).DCol = 2
    Dial(1).DLen = 60
    Dial(1).DTyp = 1
    PA$(1, 0) = "Page Title: "
    PA$(1, 1) = PageTitle$

    Dial(2).DRow = 4
    Dial(2).DCol = 14
    Dial(2).DLen = 3 + 256
    Dial(2).DTyp = 4

    PA$(2, 1) = STR$(Inf.ListInclude + 1)
    Mult$(1) = "Expand all INCLUDE files      "
    Mult$(2) = "Expand INCLUDE files only once"
    Mult$(3) = "Don't expand INCLUDE files    "

    Dial(3).DRow = 9
    Dial(3).DCol = 14
    Dial(3).DTyp = 3
    PA$(3, 0) = "Print Procedures on separate pages"
    IF Inf.ProcPage THEN
       PA$(3, 1) = "û"
    ELSE
       PA$(3, 1) = " "
    END IF

    Dial(4).DRow = 11
    Dial(4).DCol = 3
    Dial(4).DLen = 3
    Dial(4).DTyp = 2

    PA$(4, 0) = "Expand Tab Characters to: "
    PA$(4, 1) = STR$(Inf.TabSpaces)

    Dial(5).DRow = 11
    Dial(5).DCol = 35
    Dial(5).DTyp = 6

    PA$(5, 0) = "spaces per tab stop."


    Dial(6).DRow = 13
    Dial(6).DCol = 24
    Dial(6).DTyp = 5
    PA$(6, 0) = "OK"
   
    Dial(7).DRow = 13
    Dial(7).DCol = 44
    Dial(7).DTyp = 5
    PA$(7, 0) = "Cancel"

    LOCATE 6
    DialogBox "Source Listing Setup", PA$(), Dial(), Mult$(), ExitCode

    IF ExitCode = 1 THEN
       PageTitle$ = PA$(1, 1)
       Inf.ListInclude = QPValI(PA$(2, 1)) - 1
       Inf.ProcPage = PA$(3, 1) = "û"
       Inf.TabSpaces = QPValI(PA$(4, 1))
    END IF

    ERASE Dial, PA$, Mult$

END SUB

