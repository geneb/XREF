'$Title:'Source Code Cross Reference Program'
'$SubTitle:'Main Module'
'****************************************************************************
'  XREF.BAS - Main Module for Cross Referencing BASIC Source files.
'  Version 2.20
'  by Don Malin
'  (c) 1989 - 1991 Crescent Software, Inc.
'
'--[ Required Modules ]------------------------------------------------------
'  Required Modules:
'     DIALOG.BAS   - Dialog box routines
'     FILESORT.BAS - File sorting module for creating indices
'     GETFILE.BAS  - File selection dialog box
'     PULLDNMS.BAS - Pull down menus.  From QuickPak Pro Version 3.00 or >
'     QMAP.BAS     - Source code reading and parsing routines
'     XREFDIAL.BAS - Dialog box set up routines
'     XREFMISC.BAS - Object database loading and saving routines
'     XREFRPT.BAS  - Reporting routines
'
'     XREFCNST.BI  - Include file for CONSTants
'     XREFTYPE.BI  - Include file for TYPE structures
'     XREFCOMN.BI  - Include file for COMMON arrays and variables
'
'     PRO.LIB      - QuickPak Professional library routines, Version 3.1 or >
'
'--[ Recommended Compiling Options ]-----------------------------------------
'    see "XREF." - MAKE.EXE script file)
'     XREF.BAS     /s/o/ot      DIALOG.BAS   /s/o
'     FILESORT.BAS /s/o/ah      GETFILE.BAS  /s/o
'     PULLDNMS.BAS /s/o         QMAP.BAS     /s/o/ot/ah
'     XREFDIAL.BAS /s/o         XREFMISC.BAS /s/o
'     XREFRPT.BAS  /s/o/ah/ot
'--[ Recommended Linking Options ]-------------------------------------------
'    see "XREF.RSP" - LINK.EXE responce file)
'
'     link /e/noe/seg:400 XREF + PULLDNMS + XREFMISC +
'          DIALOG + GETFILE + XREFDIAL + QMAP + XREFRPT + FILESORT
'          SMALLERR + NOCOM + NOFLTIN + NOEDIT + TSCNIONR,
'          , NUL, PRO + XREFMISC;
'
'  Note:  You can omit any of the "NO" stub files that you don't have, but
'         the resulting .EXE will be larger.
'****************************************************************************
'$Page
DEFINT A-Z

'$INCLUDE: 'XRefCnst.bi'
'$INCLUDE: 'XRefType.bi'
'$INCLUDE: 'XRefComn.bi'

DECLARE FUNCTION AlertUser% (Message$, Prmt1$, Prmt2$, Prmt3$)
DECLARE FUNCTION ASCII% (Char$)
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION EnterFileName$ (Default$)
DECLARE FUNCTION ErrorMsg$ (ErrorNum)
DECLARE FUNCTION ErrorPrompt% ()
DECLARE FUNCTION ExeName$ ()
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FileExtension$ (FileName$)
DECLARE FUNCTION FLInput$ (Handle, Buffer$)
DECLARE FUNCTION FLof& (Handle%)
DECLARE FUNCTION GetNybble% (BYVAL IntArray%, BYVAL Psn%)
DECLARE FUNCTION InCount% (Text$, Search$)
DECLARE FUNCTION InstrTbl% (StPos%, Temp$, Delim$)
DECLARE FUNCTION MakeDottedLine$ (LineLength%)
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Arg1, Arg2)
DECLARE FUNCTION NotInstr% (StPos%, Text$, Table$)
DECLARE FUNCTION ParsPath$ (FileSpec$)
DECLARE FUNCTION ParseString$ (CurPos%, Work$, Delim$)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION Peek1% (Segment%, Address%)
DECLARE FUNCTION Power2& (Power%)
DECLARE FUNCTION PullMenKey% (Ky$, Choice$(), Stat())
DECLARE FUNCTION QInstrB% (Start, FileName$, Find$)
DECLARE FUNCTION QPValI% (Number$)
DECLARE FUNCTION TranslatePrnSetUp$ (PrnSetUp$)
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION NoPath$ (FileName$)

DECLARE SUB AddObjType (Bits&)
DECLARE SUB BarPrint (Choice$(), Stat%())
DECLARE SUB Box0 (UlRow%, UlCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE SUB BrowseFile (FileName$, BYVAL Clr%, Ky1%, BrAction%)
DECLARE SUB BuildTables (FileName$, ObjFile$, RefFile$, RptTypes&, NoKeyWords%, NoNumConst%, TxtFile$, PageTitle$, MakeExtension$, Menu$(), Stat())
DECLARE SUB ButtonPress (Button, Status, Count, X, Y)
DECLARE SUB CreateDatabase (DBName$, ObjFile$, RefFile$)
DECLARE SUB EraseBasProcTable ()
DECLARE SUB ErrMsg (Text$)
DECLARE SUB FClose (Handle%)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGet (Handle%, Dest$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetT (Handle%, Dest AS ANY, NumBytes%)
DECLARE SUB FlushPrnBuffer ()
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FPut (Handle%, Source$)
DECLARE SUB FPutA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FPutT (Handle%, Dest AS ANY, NumBytes%)
DECLARE SUB FSeek (Handle%, Position&)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetDBStats (DBName$)
DECLARE SUB GetFile (SearchSpec$, FileSpec$)
DECLARE SUB GetRange (LowRange, HiRange, ProcName$, ObjTypes&, ObjType$(), Inf AS GenInfo)
DECLARE SUB HideCursor ()
DECLARE SUB InitBasProcTable ()
DECLARE SUB InitMouse (MouseThere%)
DECLARE SUB InitOutDev (OutDev%)
DECLARE SUB ListExternal2Range (LowRange%, HiRange%, ProcName$, ObjTypes&, ErrCode%)
DECLARE SUB MouseTrap (UlRow, UlCol, LRRow, LRCol)
DECLARE SUB MQPrint (Text$, Clr)
DECLARE SUB MScrnSave (UlRow, UlCol, LRRow, LRCol, SEG Element)
DECLARE SUB MScrnRest (UlRow, UlCol, LRRow, LRCol, SEG Element)
DECLARE SUB Print2Buffer (Text$, BYVAL CrLf%)
DECLARE SUB PrintFootNote (Text$)
DECLARE SUB PrintHeader (Header$())
DECLARE SUB PrintLine (PrnLine$, Header$())
DECLARE SUB PrnKeyWords ()
DECLARE SUB PrnObjDetail (ObjTypes&)
DECLARE SUB PrnObjSummary (ObjTypes&, UnRefedOnly%)
DECLARE SUB PrnProcObjs (ProcObjTypes&)
DECLARE SUB ProcTree (BYVAL ProcNo%)
DECLARE SUB PullDnMs (Choice$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE SUB SelectObjTypes (ObjTypes&, ObjType$(), NoKeys%)
DECLARE SUB SetCursor (Row, Col)
DECLARE SUB SetError (ErrNum%)
DECLARE SUB SetExtensions (ODBExtension$, XrfExtension$, TextExtension$)
DECLARE SUB SetMenuState ()
DECLARE SUB SetNybble (IntArray%, BYVAL Value%, BYVAL Psn%)
DECLARE SUB SetOptions (Inf AS GenInfo)
DECLARE SUB SetStatus (Status%, Truth%)
DECLARE SUB SetUpExtractText (Flag%)
DECLARE SUB SetUpPrinter (PrinterName$, PrnSetUp$, PrnReset$, PrnWidth, PrnLength, PrnLeftMarg, PrnBotMarg)
DECLARE SUB SetupProcTree (Inf AS GenInfo)
DECLARE SUB SetUpSourceListing (PageTitle$, Inf AS GenInfo)
DECLARE SUB ShowCursor ()
DECLARE SUB ShowFileName (FileName$)
DECLARE SUB TextCursor (FG%, BG%)
DECLARE SUB UpdateTabCon (Text$)
DECLARE SUB Wait4DblClick (Ky$)


CLEAR , , 4000                  'The sort routines need extra stack space
                                'REM out for P.D.Q.
Zero = 0
One = 1
Two = 2
Four = 4

'~~~~~ Define an array to hold object type names
DIM ObjType$(12)
ObjType$(0) = "BASIC Key Word"
ObjType$(1) = "DEF Fn Function"
ObjType$(2) = "FUNCTION"
ObjType$(3) = "SUB Program"
ObjType$(4) = "Label"
ObjType$(5) = "Numeric CONSTant"
ObjType$(6) = "Named CONSTant"
ObjType$(7) = "Simple Variable"
ObjType$(8) = "Simple Type Variable"
ObjType$(9) = "Static Array"
ObjType$(10) = "Static Type Array"
ObjType$(11) = "Dynamic Array"
ObjType$(12) = "Dynamic Type Array"


'~~~~~ Define arrays for the pull down menus
DIM Menu$(13, 3)                        'Menu strings (12 choices in 4 menus)
DIM Stat(13, 3)                         'Status and hot key definitions
DIM MenuHelp$(13, 3)                    'Menu help strings

Menu$(0, 0) = "File"

Menu$(1, 0) = "Open .MAK Definition File..."
MenuHelp$(1, 0) = "Displays program definition files for processing of modules specified within."
Stat(1, 0) = 256 * 6

Menu$(2, 0) = "Open Source Module..."
MenuHelp$(2, 0) = "Displays individual source modules to be opened and processed."

Menu$(3, 0) = "Load Object Database..."
MenuHelp$(3, 0) = "Displays `Object Databases' that have previously been created from source files."

Menu$(4, 0) = "Browse Report File..."
MenuHelp$(4, 0) = "Displays `XRF' report files that have previously been generated."
Menu$(5, 0) = "-"

Menu$(6, 0) = "Specify File Extensions..."
MenuHelp$(6, 0) = "Allows you to change the default file extensions used for output files."
Stat(6, 0) = 256 * 13
Menu$(7, 0) = "-"

Menu$(8, 0) = "Save Settings to File..."
MenuHelp$(8, 0) = "Saves the current settings and defaults to a specified file."

Menu$(9, 0) = "Retrieve Settings File..."
MenuHelp$(9, 0) = "Loads settings and default information from a specified file."
Menu$(10, 0) = "-"

Menu$(11, 0) = "DOS Shell"
MenuHelp$(11, 0) = "Temporarily suspends the cross reference program and invokes a DOS shell."
Menu$(12, 0) = "-"

Menu$(13, 0) = "Exit"
MenuHelp$(13, 0) = "Exits the cross reference program and returns to DOS."
Stat(13, 0) = 256



Menu$(0, 1) = "Report"

Menu$(1, 1) = "Options..."
MenuHelp$(1, 1) = "Allows you to change certain global report options."
Menu$(2, 1) = "-"

Menu$(3, 1) = "  Source Listing..."
MenuHelp$(3, 1) = "Prints all source code.  [Space] bar to select, [Enter] to view settings."
Stat(3, 1) = 256 * 9

Menu$(4, 1) = "  Extract Source Text to `*    '..."
MenuHelp$(4, 1) = "Extracts text from source files.  [Space] bar to select, [Enter] - view settings"
Stat(4, 1) = 256 * 2
Menu$(5, 1) = "-"

Menu$(6, 1) = "  Procedure Tree Diagram..."
MenuHelp$(6, 1) = "Creates a tree diagram of procedures.  [Space] - select, [Enter] - view settings"
Stat(6, 1) = 256 * 12
Menu$(7, 1) = "-"

Menu$(8, 1) = "  Object Summary Table..."
MenuHelp$(8, 1) = "Lists objects and # of references.  [Space] - select, [Enter] - view settings."
Stat(8, 1) = 256 * 9

Menu$(9, 1) = "  Object Cross Reference Table..."
MenuHelp$(9, 1) = "Lists objects and all references.  [Space] - select, [Enter] - view settings."
Stat(9, 1) = 256 * 9

Menu$(10, 1) = "  Table of Unused Objects..."
MenuHelp$(10, 1) = "Lists all unused objects.  [Space] - select, [Enter] - view settings."
Stat(10, 1) = 256 * 11

Menu$(11, 1) = "  Table of Objects Used in Procedures..."
MenuHelp$(11, 1) = "Lists objects used in each procedure.  [Space] - select, [Enter] - view settings"
Stat(11, 1) = 256 * 27

Menu$(12, 1) = "  List Objects External to a Range..."
MenuHelp$(12, 1) = "Lists objects that are used within a line number range but also used elsewhere."
Stat(12, 1) = 256 * 15



Menu$(0, 2) = "Output"

Menu$(1, 2) = "  to File "
MenuHelp$(1, 2) = "Sends report output to the specified file."
Stat(1, 2) = 256 * 5

Menu$(2, 2) = "  to Printer on LPT1..."
MenuHelp$(2, 2) = "Sends report output to `LPT1'.  [Space] - select, [Enter] - view settings."
Stat(2, 2) = 256 * 19

Menu$(3, 2) = "  to Printer on LPT2..."
MenuHelp$(3, 2) = "Sends report output to `LPT2'.  [Space] - select, [Enter] - view settings."
Stat(3, 2) = 256 * 19



Menu$(0, 3) = "Run"

MenuHelp$(2, 3) = "Resumes the cross referencing process from the point you left off."



'~~~~~ Save the DOS cursor location and screen for later.
DosRow = CSRLIN
DIM DosScr(2000)
MScrnSave One, One, 25, 80, DosScr(0)


'~~~~~ Look for a "/B" (Mono colors) in the COMMAND$ line
Cmd$ = UCASE$(COMMAND$)
IF LEN(Cmd$) THEN
   NoHi = INSTR(Cmd$, "/NOHI")
   IF NoHi THEN Cmd$ = LEFT$(Cmd$, NoHi - 1) + MID$(Cmd$, NoHi + 5)

   Mono = INSTR(Cmd$, "/B")
   IF Mono THEN Cmd$ = LEFT$(Cmd$, Mono - 1) + MID$(Cmd$, Mono + 2)
END IF

'~~~~~ Set screen color for display type or command line as above
IF Mono <> 0 OR Peek1%(Zero, &H463) = &HB4 THEN
   Inf.ScrClr = 7
   Inf.BarClr = 112
   Mono = 2
ELSEIF NoHi THEN
   Inf.ScrClr = 23
   Inf.BarClr = 48
ELSE
   Inf.ScrClr = 23
   Inf.BarClr = 63
END IF


'~~~~~ Create the programs screen
LOCATE One, One                                 'Stop the screen from scrolling
BarPrint Menu$(), Stat()                        'Show the menu bar

CALL SXRScrn(Mono)                              'Display the main screen and
CALL SXRBaner(Mono)                             '  banner
                                                


'~~~~~ Set up the mouse
InitMouse 0                                     'Define the Mouse cursor
TextCursor -Two, Four                           'Inverse foreground red background
SetCursor One, One                              'Locate it at upper right of screen



'~~~~~ Set program default parameters
OutDev = 1                                      'Output device = 1 = "File"
PrevOutDev = OutDev                             'Save it for later

                                                'Set "Procedure Tree" and
RptTypes& = 80                                  '  "Object Summary" reports
                                                '  Sets bits 4 and 6

SumObjTypes& = 32702                            'Turn on all object types for
                                                '  Object Summary report
'SumObjTypes& = SumObjTypes& XOR Power2&(6)

'DetObjTypes& = SumObjTypes&
'FOR Bit = 1 TO 5                                'Turn off BASIC .. Num CONSTs
'   DetObjTypes& = DetObjTypes& XOR Power2&(Bit) '  types for cross ref. report
'NEXT
DetObjTypes& = 32640

UnObjTypes& = SumObjTypes& XOR 2                'Set "Unreferenced" var types
ProcObjTypes& = UnObjTypes&                     'Set Objects in Proc. types

'FOR N = 7 TO 13                                 'Set External to Range types
'   ExtrnObjTypes& = ExtrnObjTypes& XOR Power2(N)
'NEXT
'ExtrnObjTypes& = ExtrnObjTypes& XOR Power2(5)
ExtrnObjTypes& = 16288


'~~~~~ Set default output file extensions
ODBExtension$ = ".ODB"                          '"Object Database" extension
XrfExtension$ = ".XRF"                          'Report output file extension
TextExtension$ = ".SPL"                         '"Extract Text" extension
CFXSpec$ = "*.CFX"
BasSpec$ = "*.BAS"
MakSpec$ = "*.MAK"

'~~~~~ Set default printer setup parameters
Prn1SetUp$ = "15"                               'Printer init. strings
Prn2SetUp$ = ""                                 '  ditto for LPT2
Prn1Reset$ = "18"                               'Printer reset code
Prn1Width = 132                                 'Printer widths
Prn2Width = 80                                  '  ditto for LPT2
Prn1LeftMarg = 1                                'Printer left margins
Prn2LeftMarg = 1                                '  ditto for LPT2
Prn1Length = 66                                 'Printer page lengths
Prn2Length = 66                                 '  ditto for LPT2
Prn1BotMarg = 6                                 'Lines to skip @ end of page
Prn2BotMarg = 6                                 '  ditto for LPT2

Inf.UBSymbols = 2000                            'Max number of objects/module
Inf.UBWords = 300                               'Max number of tokens

Inf.ProcPage = -1                               'Flag to print procedures on
                                                '  separate pages
Inf.LineNumbers = 2                             'Make line #ers relative to
                                                '  beginning of procedures
Inf.TabSpaces = 8                               'Expand tabs to 8 spaces

Inf.GraphChars = -1                             'Use IBM line chars on
                                                '   "Procedure Tree" reports
Inf.SortProcTree = -1                           '"Procedure Tree" reports will
                                                '  be sorted
Inf.DetailProcTree = 0                          'Flag to not show detail on
                                                '  same
Inf.ProcTreePages = -1                          'Flag to insert page breaks
Inf.DispText = -1                               'Display text when reading

Inf.Extract = 3                                 'Extract both Quoted strings
                                                '  and comments
Inf.ErrCode = -1                                'No errors yet (-1 = OK)

HelpBuf$ = SPACE$(80)                           'Make a buffer for printing
                                                '  menu help messages
CrLf$ = CHR$(13) + CHR$(10)



'~~~~~ Look for the default configuration file
CnfName$ = "XREF.CFX"                           'Look in the current directory
IF NOT Exist(CnfName$) THEN                     'If it wasn't found,
   CnfName$ = ParsPath$(ExeName$) + CnfName$    'Look in the EXE's directory
END IF

IF Exist(CnfName$) THEN GOSUB LoadConfigFile    'If we found it, go load it



'~~~~~ Parse up the COMMAND$ line
FileName$ = ""                                  'No file name yet
IF LEN(Cmd$) THEN                               'If there were commands
   Delim$ = " ,+;"                              'Set parsing delimiters
   CurPos = 1                                   'Set flag for parsing routine
   DO
      CurPosWas = CurPos
                                                'Get the next word from line
      Word$ = LTRIM$(RTRIM$(ParseString$(CurPos, Cmd$, Delim$)))

      IF CurPosWas = 1 THEN                     'First word on line, must be
         IF LEN(FileName$) = 0 AND LEN(Word$) THEN '  the file name.
            FileName$ = Word$                   'Use .BAS extension if absent
            IF LEN(FileExtension$(FileName$)) = 0 THEN
               FileName$ = FileName$ + ".BAS"
            END IF                              'Make output device name.
            OutDevName$ = BaseName$(FileName$) + XrfExtension$
                                                'Read source unless -
            IF INSTR(FileName$, ODBExtension$) THEN
               DoRead = 0                       '  ".ODB" extension given
            ELSE
               DoRead = -1
            END IF
         END IF
      ELSEIF LEN(Word$) THEN                    'Must be the configuration
         CnfName$ = Word$                       '  file name
         IF LEN(FileExtension$(CnfName$)) = 0 THEN
            CnfName$ = CnfName$ + ".CFX"
         END IF

         GOSUB LoadConfigFile                   'Go load it up
      END IF

      IF CurPos > LEN(Cmd$) THEN                'Past end,
         IF INSTR(Cmd$, ";") THEN               '  did they end it with ";"?
            IF LEN(FileName$) THEN              'Is there a source file?
               GOSUB DoReport                   'Yes, do the reports and end.
               GOTO EndProgram
            END IF
         END IF
         EXIT DO
      END IF
   LOOP                                         'Go back and get another word
END IF

OdbSpec$ = "*" + ODBExtension$
BrowseSpec$ = "*" + XrfExtension$


ShowFileName FileName$                          'Display the source file name


'$Page
Action = 1                                      'Set the PullDnMS sub's action
                                                '  flag to initialize

'ErrMsg STR$(FRE(""))

'~~~~~ Programs main loop for processing menu choices
DO
   ShowCursor

   SetMenuState                                 'Set menu status for choices

  
   '~~~~~ Polling loop for the pull down menu program
   DO
                                                
      PullDnMs Menu$(), Stat(), Menu, Choice, Ky$, Action
      Action = 3                                'Force "Action to 3 (idle state)))

                                                'If moving in the menus,
      IF Menu <> PrevMenu OR Choice <> PrevChoice THEN
         IF PrevMenu <> Menu THEN Choice = 1
         PrevMenu = Menu                        '  print help messages
         PrevChoice = Choice
         LSET HelpBuf$ = MenuHelp$(Choice, Menu)
         LOCATE 25, One
         MQPrint HelpBuf$, 48
      END IF

      IF Ky$ = CHR$(27) THEN
         PullDnMs Menu$(), Stat(), Zero, Choice, Ky$, 5
         Action = 1                             'Reset menu "Action" flag
         IF Inf.ErrCode > -1 THEN
            GOSUB DoReport                      'Go do the reports
         ELSE
            GOSUB DoBrowse
         END IF
      END IF
                                                'Wait for enter or space bar
   LOOP UNTIL Ky$ = CHR$(13) OR (Menu >= 1 AND Menu <= 2 AND Ky$ = " ")

   PrevOutDev = OutDev                          'Save current output device #

   IF Ky$ = CHR$(13) THEN
      LSET HelpBuf$ = "Enter=Execute, Esc=Cancel, Tab=Next Field, ^Tab=Previous Field, Arrow=Next Item"
      LOCATE 25, One
      MQPrint HelpBuf$, 48
      PrevChoice = 0
   END IF


   '~~~~~ Branch according to menu and choice
   SELECT CASE Menu

      '~~~~~ File Menu
      CASE 0
         Action = 2

         SELECT CASE Choice

            '~~~~~ Open Source File... and Load Object Database...
            CASE 1, 2, 3
               
               FileWas$ = FileName$

               SELECT CASE Choice
                  CASE 1
                     Spec$ = MakSpec$
                  CASE 2
                     Spec$ = BasSpec$
                  CASE ELSE
                     Spec$ = OdbSpec$
               END SELECT

               GetFile Spec$, FileName$

               IF LEN(FileName$) = 0 THEN
                  FileName$ = FileWas$
               ELSE
                  IF Inf.ErrCode > -1 THEN
                     GOSUB ErrExit
                     Inf.ErrCode = -1
                  END IF
                  CLOSE

                  LowRange = 0: HiRange = 0
                  FileName$ = UCASE$(FileName$)
                  Spec$ = UCASE$(Spec$)
               END IF

               PullDnMs Menu$(), Stat(), Menu, Choice, Ky$, 5
               ShowFileName FileName$
               OutDevName$ = BaseName$(FileName$) + XrfExtension$
               
               Action = 1

               SELECT CASE Choice
                  CASE 1
                     MakSpec$ = Spec$
                     PathWas$ = ParsPath$(FileName$)
                     DoRead = True
                  CASE 2
                     BasSpec$ = Spec$
                     PathWas$ = ParsPath$(FileName$)
                     DoRead = True
                  CASE ELSE
                     OdbSpec$ = Spec$
                     PathWas$ = ""
                     DoRead = False
               END SELECT


            '~~~~~ Browse Report File
            CASE 4
               GOSUB CloseBrowse
               
               BrowseFileName$ = OutDevName$

               GetFile BrowseSpec$, BrowseFileName$

               BrAction = 1
               PullDnMs Menu$(), Stat(), Zero, Choice, Ky$, 5

               GOSUB DoBrowse
               

           
            '~~~~~ Specify File Extensions...
            CASE 6
               SetExtensions ODBExtension$, XrfExtension$, TextExtension$

           
            '~~~~~ Save Settings to File
            CASE 8
               FileWas$ = CnfName$
               CnfName$ = EnterFileName$(CnfName$)

               IF LEN(CnfName$) THEN
                  FCreate CnfName$
                  FOpen CnfName$, Handle
                  IF DosError THEN
                     ErrMsg "Drive not ready"
                  ELSE
                     FPut Handle, "XRef 13 Config" + CHR$(26)
                     FPutT Handle, RptTypes&, Four
                     FPutT Handle, SumObjTypes&, Four
                     FPutT Handle, DetObjTypes&, Four
                     FPutT Handle, UnObjTypes&, Four
                     FPutT Handle, ProcObjTypes&, Four
                     FPutT Handle, ExtrnObjTypes&, Four
                     FPutT Handle, Inf.UBSymbols, Two
                     FPutT Handle, Inf.UBWords, Two

                     FPutA Handle, Inf.LineNumbers, 18

                     FPutT Handle, OutDev, Two
                     FPutT Handle, Prn1Width, Two
                     FPutT Handle, Prn2Width, Two
                     FPutT Handle, Prn1Length, Two
                     FPutT Handle, Prn2Length, Two
                     FPutT Handle, Prn1LeftMarg, Two
                     FPutT Handle, Prn2LeftMarg, Two
                     FPutT Handle, Prn1BotMarg, Two
                     FPutT Handle, Prn2BotMarg, Two
                     FPutT Handle, Inf.ListInclude, Two
                     FPutT Handle, Reserved, Two

                     FPut Handle, Prn1SetUp$ + CrLf$
                     FPut Handle, Prn2SetUp$ + CrLf$
                     FPut Handle, Prn1Reset$ + CrLf$
                     FPut Handle, Prn2Reset$ + CrLf$
                     FPut Handle, PageTitle$ + CrLf$

                     FPut Handle, ODBExtension$ + CrLf$
                     FPut Handle, XrfExtension$ + CrLf$
                     FPut Handle, TextExtension$ + CrLf$
                  END IF
                  FClose Handle
               ELSE
                  CnfName$ = FileWas$
               END IF


            '~~~~~ Retrieve Settings file...
            CASE 9
               FileWas$ = CnfName$

               GetFile CFXSpec$, CnfName$

               IF LEN(CnfName$) = 0 THEN
                  CnfName$ = FileWas$
               ELSE
                  GOSUB LoadConfigFile
               END IF

           
            '~~~~~ DOS Shell
            CASE 11
               HideCursor
               REDIM Scr(2000)                   'Save the current screen
               MScrnSave One, One, 25, 80, Scr(0)
               MScrnRest One, One, 25, 80, DosScr(0) 'Restore the DOS screen
               LOCATE DosRow, , One                  'Replace the cursor

               SHELL

               DosRow = CSRLIN                   'Save the DOS cursor
               MScrnSave One, One, 25, 80, DosScr(0) 'Save the DOS screen
               MScrnRest One, One, 25, 80, Scr(0)    'Restore the program screen
               ShowCursor
               ERASE Scr                         'Reclaim memory

           
            '~~~~~ Exit
            CASE 13
               EXIT DO

            CASE ELSE
         END SELECT

     
      '~~~~~ Report
      CASE 1

         Wait4DblClick Ky$

         IF Ky$ = CHR$(13) THEN                 'If they pressed Enter on a
                                                '  choice, do a dialog box
            SELECT CASE Choice

               '~~~~~ Options...
               CASE 1
                  SetOptions Inf
              
               '~~~~~ Source Listing...
               CASE 3
                  SetUpSourceListing PageTitle$, Inf

               '~~~~~ Extract Source Text
               CASE 4
                  SetUpExtractText Inf.Extract

               '~~~~~ Procedure Tree...
               CASE 6
                  SetupProcTree Inf

               '~~~~~ Object Summary Report...
               CASE 8
                  SelectObjTypes SumObjTypes&, ObjType$(), Zero

               '~~~~~ Object Detail Report...
               CASE 9
                  SelectObjTypes DetObjTypes&, ObjType$(), Zero

               '~~~~~ Unused Objects...
               CASE 10
                  SelectObjTypes UnObjTypes&, ObjType$(), -One

               '~~~~~ Objects used in Procedures...
               CASE 11
                  SelectObjTypes ProcObjTypes&, ObjType$(), Zero

               '~~~~~ List Objects External to a Range...
               CASE 12
                  GetRange LowRange, HiRange, ProcName$, ExtrnObjTypes&, ObjType$(), Inf
                  IF LowRange < 1 OR HiRange < 1 THEN
                     RptTypes& = RptTypes& AND (NOT Power2&(Choice - 2))
                     Action = 2
                  END IF

               CASE ELSE
            END SELECT
         ELSE                                   'If Space was pressed, toggle
                                                '  the status of choice
            IF Choice > 2 THEN
               IF Choice = 12 THEN
                  IF LowRange > 0 AND HiRange > 0 THEN
                     RptTypes& = RptTypes& XOR Power2&(Choice - 2)
                  ELSE
                     RptTypes& = RptTypes& AND (NOT Power2&(Choice - 2))
                  END IF
               ELSE
                  RptTypes& = RptTypes& XOR Power2&(Choice - 2)
               END IF
               Action = 2
            END IF
         END IF
        
     
      '~~~~~ Output
      CASE 2

         Wait4DblClick Ky$

         IF Ky$ = CHR$(13) THEN                 'If they pressed Enter on a
                                                '  choice, do a dialog box
            SELECT CASE Choice
               CASE 1
                  OutDevName$ = EnterFileName$(OutDevName$)

                  PullDnMs Menu$(), Stat(), Zero, Choice, Ky$, 5
                  Action = 1

               '~~~~~ Setup Printer on LPT1...
               CASE 2
                  SetUpPrinter "LPT1:", Prn1SetUp$, Prn1Reset$, Prn1Width, Prn1Length, Prn1LeftMarg, Prn1BotMarg

               '~~~~~ Setup Printer on LPT2...
               CASE 3
                  SetUpPrinter "LPT2:", Prn2SetUp$, Prn2Reset$, Prn2Width, Prn2Length, Prn2LeftMarg, Prn2BotMarg

               CASE ELSE
            END SELECT
         ELSE                                   'If they pressed Space,
            OutDev = Choice                     '  change the selection

            IF Inf.ErrCode > -1 THEN
               IF PrevOutDev <> OutDev THEN
                  GOSUB ErrExit
               END IF
            END IF

            Action = 2
         END IF


      '~~~~~ Run
      CASE 3
                                                'Clear the pulled down menu
         PullDnMs Menu$(), Stat(), Zero, Choice, Ky$, 5

         IF Inf.ErrCode > -1 AND Choice = 1 THEN
            GOSUB ErrExit
            Inf.ErrCode = -1
         END IF

         GOSUB DoReport                         'Go do the reports

         IF PrevMenu = 0 AND Choice = 4 THEN
            BrowseFileName$ = OutDevName$
            BrAction = 1
            GOSUB DoBrowse
         END IF

      CASE ELSE
   END SELECT

LOOP


EndProgram:

GOSUB ErrExit
HideCursor
MScrnRest One, One, 25, 80, DosScr(0) 'Restore the DOS screen
LOCATE DosRow, , One                'Replace the cursor
END



'$Page
'~~~~~ Run selected report types
DoReport:

   Menu = 0
   Choice = 1
   Action = 1

   GOSUB CloseBrowse

   ShowFileName FileName$                       'Display the source file name

   Inf.DateTime = DATE$ + " " + TIME$

   IF Inf.ErrCode = -1 THEN
      REDIM TabCon$(240)                        'Holds Table of Contents
   
      TabCon$(1) = "CROSS REFERENCE - TABLE OF CONTENTS"
      TabCon$(3) = "For file: " + FileName$
      TabCon$(4) = Inf.DateTime
      TabCon$(5) = SPACE$(72) + "Page #"
      IF Inf.GraphChars THEN
         TabCon$(6) = STRING$(78, "Ä")
      ELSE
         TabCon$(6) = STRING$(78, "-")
      END IF

      Inf.TabConLine = 7

      Inf.Page = 0                              'Init. Page number and
      Inf.LCount = 0                            '  line counter
      
      IF RptTypes& <> 4 AND RptTypes& <> 0 THEN
         InitOutDev OutDev                      'Open and initialize the
         Opn = -1                               '  output device
      ELSE
         Opn = 0
      END IF
   END IF
   
   IF Inf.ErrCode < -1 THEN                     'Bail out if any errors
      Inf.ErrCode = -1
      RETURN
   END IF

   '~~~~~ Create array to hold include file names
   IF Inf.ErrCode = -1 THEN                  'If we are not restarting,
      REDIM Make$(1)
      REDIM Incl$(100)
   END IF


   IF (RptTypes& AND 2) > 0 OR (RptTypes& AND 4) > 0 THEN
      IF INSTR(UCASE$(FileName$), ODBExtension$) THEN
         GetDBStats FileName$
         FClose Inf.DBHandle
         REDIM Incl$(100)

         FileName$ = Make$(1)

         ShowFileName FileName$
         DoRead = -1
      END IF
   END IF

  
   '~~~~~ Check to see if the file is an Object Database
   IF DoRead THEN
                                                'If it's not, it must be a
                                                '  source file.
      '~~~~~ Do they need BASIC keywords?
      N = Power2&(1)
      NoKeyWords = (SumObjTypes& AND N) = 0
      NoKeyWords = NoKeyWords AND (DetObjTypes& AND N) = 0
      NoKeyWords = NoKeyWords AND (ProcObjTypes& AND N) = 0

      '~~~~~ Do they need Numeric CONSTants?
      N = Power2&(6)
      NoNumConst = (SumObjTypes& AND N) = 0
      NoNumConst = NoNumConst AND (DetObjTypes& AND N) = 0
      NoNumConst = NoNumConst AND (UnObjTypes& AND N) = 0
      NoNumConst = NoNumConst AND (ProcObjTypes& AND N) = 0
      NoNumConst = NoNumConst AND (ExtrnObjTypes& AND N) = 0


      IF RptTypes& AND Power2&(Two) THEN        'If extracting text, make an
                                                '  "Extract" file name
         TxtFile$ = TextExtension$
      ELSE                                      'Otherwise, make it NULL so
         TxtFile$ = ""                          '  the "BuildTables" routine
      END IF                                    '  knows not to create one
      
      MouseTrap 1, 1, 1, 80


      '~~~~~ Read and parse the source file
      BuildTables FileName$, ObjFile$, RefFile$, RptTypes&, NoKeyWords, NoNumConst, TxtFile$, PageTitle$, FileExtension$(MakSpec$), Menu$(), Stat()

      DidRead = True


      MouseTrap 1, 1, 25, 80


      SELECT CASE Inf.ErrCode
         CASE -1
            FileName$ = BaseName$(FileName$) + ODBExtension$
            CreateDatabase FileName$, ObjFile$, RefFile$

            DoRead = False
            IF INKEY$ = CHR$(27) THEN Inf.ErrCode = -3

         CASE -3
            Inf.ErrCode = Menu
            RETURN
         CASE IS > -1
            Menu = ABS(Inf.ErrCode)
            RETURN
      END SELECT
     
   ELSE
      Inf.Lines = 0
      Inf.ErrCode = -1

      GetDBStats FileName$

      DidRead = False

   END IF

  
   '~~~~~ Check for errors from above
   IF Inf.ErrCode <> -1 THEN
      GOTO ErrExit
   END IF


   HideCursor


   '~~~~~ Redraw the screen
   CALL SXRInfo(Mono)
   CALL SXRBaner(Mono)
   
   ShowFileName FileName$

   IF Inf.Lines THEN
      LOCATE 19, 25
      MQPrint STR$(Inf.Lines), Inf.ScrClr + 7
   END IF
   LOCATE 21, 25
   MQPrint STR$(Inf.Objects), Inf.ScrClr + 7
   LOCATE 22, 25
   MQPrint STR$(Inf.ObjRefs), Inf.ScrClr + 7



   Make$(0) = "EXTERNAL"
   LOCATE 25, One
   


   '~~~~~ Do Procedure Tree report
   IF RptTypes& AND Power2&(Four) THEN
      ProcNo = 1

      ProcTree ProcNo

      IF Inf.ErrCode < -1 GOTO ErrExit
   END IF


   SortByType = 0
   FOR N = 6 TO 10
      IF RptTypes& AND Power2&(N) THEN
         SortByType = True
         EXIT FOR
      END IF
   NEXT


   '~~~~~ Case for all other report types
   IF SortByType THEN
      LOCATE 25, One
      MQPrint SPACE$(80), Inf.BarClr
      MQPrint "Sorting Objects", -One

      InitBasProcTable
      IF Inf.ErrCode < -1 THEN
         GOTO ErrExit
      END IF
                                                
      MQPrint SPACE$(80), Inf.BarClr
      MQPrint "Creating", -One
      LOCATE 25, 10

      '~~~~~ Object Summary report
      IF RptTypes& AND Power2&(6) THEN
         Temp$ = "Object Summary Report"
         
         MQPrint Temp$, -One

         TabCon$(Inf.TabConLine) = Temp$
         Inf.TabConLine = Inf.TabConLine + 1

         IF SumObjTypes& AND Power2&(One) THEN
            PrnKeyWords
            IF Inf.ErrCode < -1 THEN
               GOTO ErrExit
            END IF
         END IF

         PrnObjSummary SumObjTypes&, Zero

         Inf.TabConLine = Inf.TabConLine + 1

         IF Inf.ErrCode < -1 THEN
            GOTO ErrExit
         END IF
      END IF


      '~~~~~ Object Detail report (Cross Reference)
      IF RptTypes& AND Power2&(7) THEN
         TabCon$(Inf.TabConLine) = "Cross Reference Table"
         
         Inf.TabConLine = Inf.TabConLine + 1

         PrnObjDetail DetObjTypes&

         Inf.TabConLine = Inf.TabConLine + 1

         IF Inf.ErrCode < -1 THEN
            GOTO ErrExit
         END IF
      END IF


      '~~~~~ Unused Objects report
      IF RptTypes& AND Power2&(8) THEN
         TabCon$(Inf.TabConLine) = "Unused Objects Table"
         
         MQPrint SPACE$(70), -One
         MQPrint TabCon$(Inf.TabConLine), -One

         Inf.TabConLine = Inf.TabConLine + 1


         PrnObjSummary UnObjTypes&, -One

         Inf.TabConLine = Inf.TabConLine + 1

         IF Inf.ErrCode < -1 THEN
            GOTO ErrExit
         END IF
      END IF


      '~~~~~ List Objects by Procedure
      IF RptTypes& AND Power2&(9) THEN
         LOCATE 25, One
         MQPrint SPACE$(80), Inf.BarClr
      
         TabCon$(Inf.TabConLine) = "Table of Objects used in Procedures"
         Inf.TabConLine = Inf.TabConLine + 1

         PrnProcObjs ProcObjTypes&

         Inf.TabConLine = Inf.TabConLine + 1

         IF Inf.ErrCode < -1 THEN
            GOTO ErrExit
         END IF
      END IF


      '~~~~~ List objects external to a range
      IF RptTypes& AND Power2&(10) THEN

         ListExternal2Range (LowRange), (HiRange), ProcName$, ExtrnObjTypes&, ErrCode

         IF Inf.ErrCode < -1 THEN
            GOTO ErrExit
         END IF

         IF ErrCode = 1 THEN
            ErrMsg ProcName$ + " is not used by " + FileName$
         ELSEIF ErrCode = 2 THEN
            ErrMsg ProcName$ + " is not a module name!"
         ELSE
            TabCon$(Inf.TabConLine) = "List of Objects External to a Range"
            Inf.TabConLine = Inf.TabConLine + 1
         END IF
      END IF

      EraseBasProcTable

   END IF



   '~~~~~ Print a table of contents for all the reports
   IF Inf.Page > 5 AND Opn THEN                 'If more than 5 pages,
      REDIM Header$(0)                          '  make a dummy header array
      PrintHeader Header$()                     'Print it
      FOR N = 1 TO Inf.TabConLine - 1           'Print all lines of array
          PrintLine TabCon$(N), Header$()
      NEXT
      PrintFootNote ""
   END IF

   IF OutDev = 1 THEN PrevMenu = 0: Choice = 4




'~~~~~ Clean up memory and close the output file
ErrExit:

   IF Opn THEN
      IF Inf.ErrCode = -3 THEN PrintFootNote ""
      IF Inf.ErrCode > -3 THEN
         IF OutDev = 1 OR OutDev = 2 THEN
            Print2Buffer TranslatePrnSetUp$(Prn1Reset$), 0
         ELSE
            Print2Buffer TranslatePrnSetUp$(Prn2Reset$), 0
         END IF
      END IF
      FlushPrnBuffer
      FClose PrnHandle
      Opn = 0
   END IF

   ERASE Make$
   ERASE Incl$
   ERASE TabCon$

   CLOSE
   FClose Inf.DBHandle
   Inf.DBHandle = 0
   IF ODBExtension$ = ".NUL" THEN
      IF INSTR(UCASE$(FileName$), ".BAS") = 0 THEN
         IF Exist(FileName$) THEN KILL FileName$
      END IF
   END IF


   IF Inf.ObjHandle THEN
      FClose Inf.ObjHandle
      Inf.ObjHandle = 0
      KILL ObjFile$
   END IF
   IF Inf.RefHandle THEN
      FClose Inf.RefHandle
      Inf.RefHandle = 0
      KILL RefFile$
   END IF
   
   ShowCursor
   Inf.ErrCode = -1

RETURN



'$Page
'~~~~~ Look for and load previously saved settings
LoadConfigFile:

   IF Exist(CnfName$) THEN                         'If we find the file, open
      Buf$ = SPACE$(15)                            '  and load it

      FOpen CnfName$, Handle
      FGet Handle, Buf$
      Temp$ = LEFT$(Buf$, 14)
      IF LEFT$(Buf$, 5) = "XRef " THEN
         Buf$ = SPACE$(80)

         FGetT Handle, RptTypes&, Four
         FGetT Handle, SumObjTypes&, Four
         FGetT Handle, DetObjTypes&, Four
         FGetT Handle, UnObjTypes&, Four
         FGetT Handle, ProcObjTypes&, Four
         FGetT Handle, ExtrnObjTypes&, Four
         FGetT Handle, Inf.UBSymbols, Two
         IF Inf.UBSymbols = 0 THEN Inf.UBSymbols = 2000
         FGetT Handle, Inf.UBWords, Two
         IF Inf.UBWords = 0 THEN Inf.UBWords = 300

         FGetA Handle, Inf.LineNumbers, 18

         FGetT Handle, OutDev, Two
         FGetT Handle, Prn1Width, Two
         FGetT Handle, Prn2Width, Two
         FGetT Handle, Prn1Length, Two
         FGetT Handle, Prn2Length, Two
         FGetT Handle, Prn1LeftMarg, Two
         FGetT Handle, Prn2LeftMarg, Two
         FGetT Handle, Prn1BotMarg, Two
         FGetT Handle, Prn2BotMarg, Two
         FGetT Handle, Inf.ListInclude, Two
         FGetT Handle, Reserved, Two

         Prn1SetUp$ = FLInput$(Handle, Buf$)
         Prn2SetUp$ = FLInput$(Handle, Buf$)
         Prn1Reset$ = FLInput$(Handle, Buf$)
         Prn2Reset$ = FLInput$(Handle, Buf$)
         PageTitle$ = FLInput$(Handle, Buf$)

         ODBExtension$ = FLInput$(Handle, Buf$)
         XrfExtension$ = FLInput$(Handle, Buf$)
         TextExtension$ = FLInput$(Handle, Buf$)
      ELSE
         ErrMsg CnfName$ + " is not a compatible configuration file"
      END IF

      FClose Handle

      IF LEFT$(Temp$, 7) = "XRef 1b" THEN
         AddObjType SumObjTypes&
         AddObjType DetObjTypes&
         AddObjType UnObjTypes&
         AddObjType ProcObjTypes&
         AddObjType ExtrnObjTypes&
      END IF

      Buf$ = ""

   ELSE
      ErrMsg "Can't find " + CnfName$

   END IF

   RptTypes& = RptTypes& AND (NOT Power2&(10))

RETURN



DoBrowse:
   IF LEN(BrowseFileName$) THEN
      ShowFileName BrowseFileName$

      LOCATE 25, 1
      MQPrint "Use: LEFT, RIGHT, UP, DOWN, PGUP, PGDN, HOME, END to view.          ³          ", 48

      DO

         BrowseFile BrowseFileName$, Inf.ScrClr + 7, Ky1, BrAction

         SELECT CASE SGN(Ky1)
            CASE 1
               Ky$ = CHR$(Ky1)
            CASE 0
               Ky$ = ""
            CASE -1
               Ky$ = CHR$(0) + CHR$(ABS(Ky1))
         END SELECT

         Temp = PullMenKey%(Ky$, Menu$(), Stat())

         SELECT CASE LEN(Ky$)
            CASE 0
               Ky1 = 0
            CASE 1
               Ky1 = ASC(Ky$)
            CASE 2
               Ky1 = -ASC(RIGHT$(Ky$, 1))
         END SELECT

         IF Temp > -1 THEN
            Menu = Temp
            EXIT DO
         END IF

      LOOP UNTIL Ky1 = 27
      Ky1 = 0
      
   END IF

   Action = 1
RETURN


CloseBrowse:
   IF LEN(BrowseFileName$) THEN
      BrowseFile BrowseFileName$, Inf.ScrClr + 7, Ky1, 5
      BrowseFileName$ = ""
   END IF
RETURN

SUB FatalError

    IF Inf.ErrCode >= -1 THEN
       Inf.ErrCode = -WhichError

       IF Inf.ErrCode = -78 THEN
          Msg$ = STRING$(2, 20) + "Set `FILES=20' in your `CONFIG.SYS' file."
       END IF

       ErrMsg ErrorMsg$(ABS(Inf.ErrCode)) + ".  Target not built!" + Msg$
    END IF

END SUB

FUNCTION FileExtension$ (FileName$) STATIC

    FileExtension$ = ""

    N = QInstrB(-1, FileName$, ".")
    IF N <= QInstrB(-1, FileName$, "\") THEN EXIT FUNCTION

    FileExtension$ = RTRIM$(MID$(FileName$, N))

END FUNCTION

SUB FlushPrnBuffer

   SHARED PrnBuffer$
   SHARED PrnHandle
   SHARED OutDev%

   IF Inf.ErrCode < -1 THEN EXIT SUB

   DO
      IF OutDev > 1 THEN
         FOR N = 1 TO LEN(PrnBuffer$)
            FPut PrnHandle, MID$(PrnBuffer$, N, 1)
            IF DosError THEN EXIT FOR
         NEXT
      ELSEIF LEN(PrnBuffer$) THEN
         FPut PrnHandle, PrnBuffer$
      END IF

      IF DosError THEN
         IF OutDev > 1 THEN
            IF WhichError = 71 THEN SetError 25
         END IF
      ELSE
         EXIT DO
      END IF

   LOOP WHILE ErrorPrompt = 1

   IF DosError THEN Inf.ErrCode = -WhichError%

   PrnBuffer$ = ""

END SUB

FUNCTION GetNybble% (BYVAL IntArray%, BYVAL Psn%) STATIC

    SELECT CASE Psn
       CASE 1
          Temp = (IntArray MOD 256) MOD 11
       CASE 2
          Temp = (IntArray MOD 256) \ 11
       CASE 3
          Temp = (IntArray \ 256) MOD 11
       CASE 4
          Temp = (IntArray \ 256) \ 11
       CASE ELSE
          Temp = 0
    END SELECT

    GetNybble = Temp

END FUNCTION

'****************************************************************************
' Purpose: Opens the report output device, either a file or a printer port
'          and sends the device width and initialization codes.
'****************************************************************************
'
SUB InitOutDev (OutDev) STATIC

   SHARED OutDevName$
   SHARED Prn1SetUp$, Prn2SetUp$
   SHARED Prn1Width, Prn2Width
   SHARED Prn1Length, Prn2Length
   SHARED Prn1LeftMarg, Prn2LeftMarg
   SHARED Prn1BotMarg, Prn2BotMarg
   SHARED PrnBuffer$
   SHARED PrnHandle

   '~~~~~ Make the device name based on device number
   SELECT CASE OutDev
      CASE 1
         Device$ = OutDevName$
      CASE 2
         Device$ = "LPT1"
      CASE 3
         Device$ = "LPT2"
      CASE ELSE
   END SELECT

   '~~~~~ If going to a file that exists, ask the user to erase or add to it
   IF OutDev = 1 THEN
      IF Exist(Device$) THEN
         LOCATE 10
         Temp = AlertUser(Device$ + " already exists!", "Add to File", "Overwrite File", "Cancel")
         SELECT CASE Temp
            CASE 0, 3
               Inf.ErrCode = -3
               EXIT SUB
            CASE 2
               FCreate Device$
            CASE ELSE
         END SELECT
      ELSE
         FCreate Device$
      END IF
      PrnBuffer$ = ""
      FOpen Device$, PrnHandle
      FSeek PrnHandle, FLof&(PrnHandle)
   ELSE
      FOpen Device$, PrnHandle
   END IF
   
   '~~~~~ Set the appropriate settings for the output device
   SELECT CASE OutDev
      CASE 1, 2
         SetUp$ = Prn1SetUp$
         Inf.PrnWidth = Prn1Width
         Inf.PrnLength = Prn1Length
         Inf.PrnLeftMarg = Prn1LeftMarg
         Inf.PageIf = Prn1BotMarg
      CASE 3
         SetUp$ = Prn2SetUp$
         Inf.PrnWidth = Prn2Width
         Inf.PrnLength = Prn2Length
         Inf.PrnLeftMarg = Prn2LeftMarg
         Inf.PageIf = Prn2BotMarg
      CASE ELSE
   END SELECT

   IF Inf.PrnLength <= Inf.PageIf THEN Inf.PrnLength = 32766

   '~~~~~ Initialize the output device
   Print2Buffer TranslatePrnSetUp$(SetUp$), 0

   Device$ = ""
   SetUp$ = ""

END SUB

SUB Print2Buffer (Text$, BYVAL CrL) STATIC
    SHARED CrLf$
    SHARED PrnBuffer$

    IF FRE("") < LEN(Text$) + 16384 OR LEN(PrnBuffer$) + LEN(Text$) > 16382 THEN
       FlushPrnBuffer
    END IF

    PrnBuffer$ = PrnBuffer$ + Text$

    IF CrL THEN PrnBuffer$ = PrnBuffer$ + CrLf$

END SUB

'****************************************************************************
'Purpose: Prints a footnote and Form Feed character on the 5th to last line
'         of a page
'
' Inputs: Text$ - Text of the footnote
'         Inf.PrnLength - Page length of printer
'         Inf.PrnLeftMarg - Left margin
'         Inf.LineCount - Number of lines printed
'
'Outputs: None
'****************************************************************************
'
SUB PrintFootNote (Text$) STATIC

    IF LEN(Text$) THEN                          'If there is a footnote
                                                'Print to the last line
       IF Inf.PrnLength < 32766 THEN
          FOR N = Inf.LCount TO Inf.PrnLength - Inf.PageIf
             Print2Buffer "", -1
          NEXT
       END IF

       IF Inf.PrnLeftMarg > 1 THEN              'Tab out to the left margin
          Print2Buffer SPACE$(Inf.PrnLeftMarg - 1), 0
       END IF

       Print2Buffer Text$, -1
    END IF

    Print2Buffer CHR$(12), 0

    FlushPrnBuffer

END SUB

'****************************************************************************
'Purpose: Prints a page header to the output device.  Also updates the
'         page count and zeros out the line count
'
' Inputs: Header$() - String array containing the text of the header
'
'Outputs: Inf.LineCount - Number of lines printed on a page
'         Inf.Page - Number of pages printed
'****************************************************************************
'
SUB PrintHeader (Header$()) STATIC

    IF LEN(Page$) = 0 THEN Page$ = SPACE$(10)   'Init a buffer to hold the
                                                '  page number string
   
    Inf.LCount = 0                              'Reset the line count to 0
    Inf.Page = Inf.Page + 1                     'Increment the page count

    IF UBOUND(Header$) THEN                     'If there is a header,
       RSET Page$ = "Page" + STR$(Inf.Page)     'Put page number in header
       MID$(Header$(1), LEN(Header$(1)) - 10) = Page$
    END IF
   
    FOR N = 1 TO UBOUND(Header$)                'Print the header
        PrintLine Header$(N), Header$()
    NEXT

END SUB

'****************************************************************************
'Purpose: Prints a line of text to the output device.  Also checks to see
'         if a page break and header needs to be printed.
'
' Inputs: PrnLine$ - Line of text to be printed,
'         Header$() - String array containing the text of the page header
'         Inf.LineCount - Number of lines printed on a page
'         Inf.PrnLeftMarg - Left margin of page
'
'Outputs: Inf.LineCount - Increments the number of lines printed.  If a line
'                         is longer that the length of the device,  the
'                         line count will reflect the actual number of lines
'                         used by the text.
'****************************************************************************
'
SUB PrintLine (PrnLine$, Header$())
    SHARED CrLf$                                'Carriage return line feed

    Wdth = Inf.PrnWidth - Inf.PrnLeftMarg '+ 1  'Find usable width
    CurPos = 1
    DO                                          'Parse the line
       Temp$ = ParseString$(CurPos, PrnLine$, CrLf$)
       CurPos = CurPos + 1
       Temp$ = RTRIM$(Temp$)

       St = 1
       Length = LEN(Temp$)

       DO
                                                'Do we need to start new page?
          IF Inf.LCount >= Inf.PrnLength - Inf.PageIf THEN
             PrintFootNote Header$(0)        'Print the footnote
             PrintHeader Header$()           'Print the header
             IF INSTR(UCASE$(PrnLine$), "'$SKIP") THEN EXIT SUB
          END IF
             
          IF St <= Length THEN
             Print2Buffer SPACE$(Inf.PrnLeftMarg - 1), 0
          END IF

          Print2Buffer MID$(Temp$, St, Wdth), -1

          St = St + Wdth

          Inf.LCount = Inf.LCount + 1

       LOOP WHILE St <= Length

    LOOP WHILE CurPos <= LEN(PrnLine$)

END SUB

'****************************************************************************
'Purpose: Sets check marks and menu status for all the menus.
'
' Inputs: Inf.ErrCode - Current error status
'         OutDev - The current output device number
'         PrevOutDev - The previously set output device number
'         RptTypes& - Bit coded number specifying the selected report types
'         FileName$ - Name of the currently selected file
'         TextExtension$ - File extension used for extracted text files
'
'Outputs: Menu$() - Menu strings
'         Stat() - Menu status settings
'****************************************************************************
'
SUB SetMenuState STATIC

   SHARED Menu$(), MenuHelp$(), Stat()
   SHARED OutDev, PrevOutDev
   SHARED RptTypes&
   SHARED FileName$, OutDevName$
   SHARED TextExtension$, ODBExtension$

   MenHelp$ = " the cross referencing and or reporting process"

   SetStatus Stat(4, 0), FRE(-1) < 40000
  
   '~~~~~ Update check marks on the "Report" menu
   FOR N = 3 TO 12
      IF MID$(Menu$(N, 1), 1) <> "-" THEN
         IF RptTypes& AND Power2&(N - 2) THEN
            MID$(Menu$(N, 1), 1) = "û"
         ELSE
            MID$(Menu$(N, 1), 1) = " "
         END IF
      END IF
   NEXT

  
   '~~~~~ Show current file extensions in menus
   Menu$(1, 2) = "  to File `" + OutDevName$ + "'..."
   MID$(Menu$(4, 1), 28) = TextExtension$

  
   '~~~~~ Update check marks on the "Output" menu
   MID$(Menu$(PrevOutDev, 2), 1) = " "
   MID$(Menu$(OutDev, 2), 1) = "û"

  
   '~~~~~ Disable "Start" choice from the "Run" menu if no file name or
   '      report types specified
   '~~~~~ Has a file been chosen?
   IF LEN(FileName$) = 0 THEN
      Menu$(1, 3) = "Choose a file before starting!"
      Ok2Run = False

   '~~~~~ Has a report been chosen?
   ELSEIF RptTypes& = 0 AND INSTR(UCASE$(FileName$), ODBExtension$) THEN
      Menu$(1, 3) = "Choose a report before starting!"
      Ok2Run = False

   '~~~~~ Are we still processing a file?
   ELSE
      IF Inf.ErrCode > -1 THEN

         Menu$(1, 3) = "Restart"
         Stat(1, 3) = 256 * 2
         MenuHelp$(1, 3) = "Restarts" + MenHelp$ + " from the beginning."

         Menu$(2, 3) = "Resume"
      ELSE
         Menu$(1, 3) = "Start"
         Stat(1, 3) = 0
         MenuHelp$(1, 3) = "Starts" + MenHelp$ + "."

         Menu$(2, 3) = ""
      END IF
      Ok2Run = True
   END IF

   SetStatus Stat(1, 3), NOT Ok2Run

END SUB

SUB SetNybble (IntArray, BYVAL Value, BYVAL Psn) STATIC

    SELECT CASE Psn
       CASE 1
          HiByte = IntArray \ 256
          HiNybble = (IntArray MOD 256) \ 11
          IntArray = HiByte * 256 + (HiNybble * 11) + Value
       CASE 2
          HiByte = IntArray \ 256
          LoNybble = (IntArray MOD 256) MOD 11
          IntArray = HiByte * 256 + (Value * 11) + LoNybble
       CASE 3
          LoByte = IntArray MOD 256
          HiNybble = (IntArray \ 256) \ 11
          IntArray = 256 * (HiNybble * 11 + Value) + LoByte
       CASE 4
          LoByte = IntArray MOD 256
          LoNybble = (IntArray \ 256) MOD 11
          IntArray = 256 * (Value * 11 + LoNybble) + LoByte
       CASE ELSE
    END SELECT

END SUB

SUB ShowFileName (FileName$) STATIC

    Length = LEN(FileName$) + 2

    Psn = (80 - Length) \ 2

    Dif = PrevLen - Length

    IF Dif > 0 THEN
       LOCATE 2, PrevPsn
       MQPrint STRING$(PrevLen, "Ä"), Inf.ScrClr
    END IF

    IF Length > 2 THEN
       LOCATE 2, Psn
       MQPrint SPACE$(Length), Inf.BarClr
       LOCATE 2, Psn + 1
       MQPrint FileName$, Inf.BarClr
    END IF

    PrevLen = Length
    PrevPsn = Psn

END SUB

'****************************************************************************
'Purpose: Adds lines of text to the table of contents array.
'
' Inputs: Text$ - the string to add to the array
'         Inf.Page - Current printing page
'
'Outputs: Inf.TabConLine - Current line number of array
'         TabCon$() - Array holding the text of the Table of Contents
'****************************************************************************
'
SUB UpdateTabCon (Text$) STATIC
    SHARED TabCon$()

    LineLen = MinInt(Inf.PrnWidth - Inf.PrnLeftMarg, 78)

    IF LEN(Text$) THEN                          'If there is something to add,
                                                '  make a string of dots
       TabCon$(Inf.TabConLine) = MakeDottedLine$(LineLen)

       MID$(TabCon$(Inf.TabConLine), 1) = Text$ 'Overlay the text on the dots

       PgNo$ = SPACE$(5)                        'Format the page number
       RSET PgNo$ = STR$(Inf.Page)
                                                'Overlay the page number
       MID$(TabCon$(Inf.TabConLine), LineLen - 4) = PgNo$
    END IF

    Inf.TabConLine = Inf.TabConLine + 1         'Increment the line number

END SUB

SUB Wait4DblClick (Ky$)

    IF ASCII(Ky$) = 13 THEN
       T& = PDQTimer& + 3
       DO
          ButtonPress 1, Temp, 0, 0, 0
       LOOP UNTIL Temp OR PDQTimer& > T&

       IF Temp THEN
          Ky$ = " "
          DO
             GetCursor 0, 0, Temp
          LOOP WHILE Temp
       END IF
    END IF

END SUB

