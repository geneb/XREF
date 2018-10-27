DECLARE SUB InitSourceHeader ()
'****************************************************************************
'  $SubTitle:'Source Code Reading/Parsing and Listing Module'
'  QMAPD.BAS - Source Code Reading and Parsing Module for XREFD.BAS
'  by Don Malin
'  (c) 1989 - 1991 Crescent Software, Inc.
'
'  The primary routine in this module is "BuildTables"
'****************************************************************************

DEFINT A-Z

CONST UBSymbols = 2200
CONST MaxCmn = 1000

CONST Comma = 44                        'Need I say more
CONST Period = 46
CONST Colon = 58
CONST OpenParen = 40
CONST CloseParen = 41

'$INCLUDE: 'XRefCnst.bi'
'$INCLUDE: 'XRefType.bi'

TYPE KeyWords                           'Used to hold BASIC key words
   KName AS STRING * 17                 'Keyword name
   KFlag AS INTEGER                     'Floating point or P.D.Q. flag
END TYPE

TYPE WordInfo                           'Holds parsed words from a line
   Word AS STRING * NameLen             'Space for the word
   Length AS INTEGER                    'Length of the word
   BasicWord AS INTEGER                 'Flags a BASIC key word
END TYPE

TYPE SymbolInfo
   RecNo AS INTEGER
   Global AS INTEGER
   LastRef AS LONG
END TYPE

TYPE BASICWordNumber
   As              AS INTEGER
   AsCurrency      AS INTEGER
   AsDouble        AS INTEGER
   AsInteger       AS INTEGER
   AsLong          AS INTEGER
   AsSingle        AS INTEGER
   AsString        AS INTEGER
   Colon           AS INTEGER
   Common          AS INTEGER
   CommonShared    AS INTEGER
   Const           AS INTEGER
   Call            AS INTEGER
   Calls           AS INTEGER
   DeclareFunction AS INTEGER
   DeclareSub      AS INTEGER
   Def             AS INTEGER
   DefCur          AS INTEGER
   DefDbl          AS INTEGER
   DefInt          AS INTEGER
   DefLng          AS INTEGER
   DefSng          AS INTEGER
   DefStr          AS INTEGER
   Dim             AS INTEGER
   DimShared       AS INTEGER
   Else            AS INTEGER
   ElseIf          AS INTEGER
   EndDef          AS INTEGER
   EndFunction     AS INTEGER
   EndSub          AS INTEGER
   EndType         AS INTEGER
   Equals          AS INTEGER
   Erase           AS INTEGER
   ErlEqu          AS INTEGER
   Field           AS INTEGER
   For             AS INTEGER
   ForISAM         AS INTEGER
   Function        AS INTEGER
   Get             AS INTEGER
   Gosub           AS INTEGER
   Goto            AS INTEGER
   If              AS INTEGER
   Input           AS INTEGER
   Is              AS INTEGER
   LBound          AS INTEGER
   Line            AS INTEGER
   LineInput       AS INTEGER
   LineStep        AS INTEGER
   Lset            AS INTEGER
   Mid             AS INTEGER
   Put             AS INTEGER
   ReDim           AS INTEGER
   ReDimPreserve   AS INTEGER
   ReDimShared     AS INTEGER
   Restore         AS INTEGER
   Resume          AS INTEGER
   Return          AS INTEGER
   Run             AS INTEGER
   Rset            AS INTEGER
   Shared          AS INTEGER
   Static          AS INTEGER
   Step            AS INTEGER
   Sub             AS INTEGER
   Swap            AS INTEGER
   Then            AS INTEGER
   Type            AS INTEGER
   UBound          AS INTEGER
   UnScore         AS INTEGER
END TYPE



'$INCLUDE: 'XRefComn.bi'

'~~~~~ Declare routines
DECLARE FUNCTION AConstant% (BYVAL Ptr%)
DECLARE FUNCTION AlertUser% (Message$, Prmt1$, Prmt2$, Prmt3$)
DECLARE FUNCTION ASCII (Char$)
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION DoneWithModule% ()
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION ErrorMsg$ (ErrorNum)
DECLARE FUNCTION ExeName$ ()
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION ExpandTab$ (X$, BYVAL NumSpaces%)
DECLARE FUNCTION FileExtension$ (FileName$)
DECLARE FUNCTION FindExact% (BYVAL Element%, NumEls%, Search$)
DECLARE FUNCTION FLof& (Handle%)
DECLARE FUNCTION GetNybble% (BYVAL IntArray%, BYVAL Psn%)
DECLARE FUNCTION InCount% (Source$, Test$)
DECLARE FUNCTION InstrTbl% (StPos%, Temp$, Delim$)
DECLARE FUNCTION LineCount% (FileName$, Buffer$)
DECLARE FUNCTION MakeDottedLine$ (LineLength%)
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION NoPath$ (FileName$)
DECLARE FUNCTION NotInstr% (StPos%, Text$, Table$)
DECLARE FUNCTION ObjectNumber% (BYVAL Start, ObjectName$)
DECLARE FUNCTION ParsPath$ (FileName$)
DECLARE FUNCTION QPLen% (Text$)
DECLARE FUNCTION QPValI% (Number$)
DECLARE FUNCTION QPValL& (Number$)
DECLARE FUNCTION Power2& (Power%)
DECLARE FUNCTION PullMenKey% (Ky$, Choice$(), Stat())
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION XRFindKey% (SEG Enement AS ANY, NumEls)
DECLARE FUNCTION XRSearch% (SEG Element%, BYVAL NumEls%, BYVAL Number2Find%)

DECLARE SUB AdjustSourceLineNos (CommentLine%, NumDef%, ObjInf AS ObjectInfo)
DECLARE SUB AssignObjRef (BYVAL ObjNo%)
DECLARE SUB BCopyT (SEG From AS ANY, SEG ToEl AS ANY, ElSize%, NumEls%)
DECLARE SUB ClearScr0 (ULRow%, ULCol%, LRRow%, LRCol%, Colr%)
DECLARE SUB ClearSymbols ()
DECLARE SUB ErrMsg (Text$)
DECLARE SUB FatalError ()
DECLARE SUB FClose (Handle%)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetRT (Handle%, Dest AS ANY, RecNo&, RecSize%)
DECLARE SUB Find (BYVAL Address, NumEls, Search$)
DECLARE SUB FlushBuffers ()
DECLARE SUB FlushListingBuffer ()
DECLARE SUB FlushPrnBuffer ()
DECLARE SUB FlushRefBuffer ()
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FPutA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FPutRT (Handle%, Source AS ANY, RecNo&, RecSize%)
DECLARE SUB FSeek (Handle%, Position&)
DECLARE SUB InitKeyWords (KWord() AS ANY, B AS ANY, NumKeyWords%)
DECLARE SUB InitObjBuffer ()
DECLARE SUB InitRefBuffer ()
DECLARE SUB InitSourceListing (PageTitle$, FileName$)
DECLARE SUB MQPrint (Text$, Clr)
DECLARE SUB MPaintBox (ULRow%, ULCol%, LRRow%, LRCol%, Colr%)
DECLARE SUB OpenFile (FileName$)
DECLARE SUB OpenSaveText (FileName$, Mode%)
DECLARE SUB Parse ()
DECLARE SUB PrintBufferedLine (BYVAL Ln%, TxtBuf$(), Header$(), SubTitle$)
DECLARE SUB PrintFootNote (Text$)
DECLARE SUB PrintHeader (Header$())
DECLARE SUB PrintLine (PrnLine$, Header$())
DECLARE SUB PrintSourceLine (ProcInf AS ANY)
DECLARE SUB QPrintRC (Text$, Row%, Col%, Clr%)
DECLARE SUB ReadLine ()
DECLARE SUB ReadMake (FileName$, MakeExtn$)
DECLARE SUB SaveObject (BYVAL SymbolNo%)
DECLARE SUB ScrollU (UlR%, UlC%, LrR%, LrC%, Lines%, Page%)
DECLARE SUB SetNybble (IntArray%, BYVAL Value%, BYVAL Psn%)
DECLARE SUB ShowFileName (FileName$)
DECLARE SUB SrchPath (FileName$, Paths$, NotFound)
DECLARE SUB Upper (Text$)


REDIM KWord(0) AS KeyWords              'Create the keyword array.  Allocated
                                        '  later
REDIM IncNos(0)                         'Ditto for include file handles

REDIM Words(0) AS WordInfo              'Reserve space for the parsed words

REDIM Symbol$(0)
REDIM SmbInfo(0) AS SymbolInfo
REDIM ObjBuffer(1 TO 1) AS ObjectInfo
REDIM RefBuffer(1 TO 1) AS ReferenceInfo
REDIM TxtBuf$(0, 0), Header$(0)

DIM SHARED AbortMsg$

'****************************************************************************
'Purpose: Tests to see if a word is a CONSTant (numeric or named).
'         This information is required when checking the subscripts on a
'         DIM statement to see if an array is static or dynamic.
'
' Inputs: Ptr - Pointer into the Words() array to check
'         Words() - Array of words for the current source line
'         ObjInf() - Object information array
'         Inf.ModNo - The Module number currently being read
'
'Outputs: True (-1) if the word is a CONSTant.  Otherwise returns 0
'****************************************************************************
'
FUNCTION AConstant% (BYVAL Ptr) STATIC
    SHARED Words() AS WordInfo
    SHARED ObjInf AS ObjectInfo
    SHARED TempInf AS ObjectInfo
    SHARED CurrentSymbol

    IF ASC(Words(Ptr).Word) < 65 THEN           'If the word starts with a
       AConstant% = True                        '  number, it must be a
       EXIT FUNCTION                            '  constant so bail out
    END IF
    
    '~~~~~ Search the ObjInf() array for the word
    TempInf = ObjInf
    SaveCurrent = CurrentSymbol
    Start = 1                                   'Start at the first element
    DO
       Obj = ObjectNumber(Start, Words(Ptr).Word) 'Go find it
       IF Obj THEN                              'If we found a match, check
          IF ObjInf.OType = Constant THEN       '  to see if it's a constant
                                                'Was it defined in this module?
             IF ObjInf.DefModInc MOD 256 = Inf.ModNo THEN
                AConstant% = True               'Assign the function and
                EXIT DO                         '  bail out
             END IF
          END IF
       END IF
       Start = CurrentSymbol + 1                'Start at the next element
    LOOP WHILE Obj                              'Find the next occurrence

    ObjInf = TempInf
    CurrentSymbol = SaveCurrent

END FUNCTION

SUB AdjustSourceLineNos (CommentLine, NumDef, ObjInf AS ObjectInfo)

    SHARED TxtBuf$(), Make$()
    SHARED LNum$

    IF Inf.LineNumbers = 2 THEN
       SourceLine = CommentLine + NumDef - 1'2
       FOR N = 1 TO MinInt(CommentLine + 2, 64)
          L = SourceLine - N + 2
          RSET LNum$ = RTRIM$(LTRIM$(STR$(L)))
          MID$(TxtBuf$(N, 0), 1) = LNum$
       NEXT
       Temp = 3
    ELSE
       Temp = 2
    END IF

    Temp = MinInt(CommentLine + Temp, 64)
    IF Inf.ProcPage THEN
       TxtBuf$(Temp, 0) = CHR$(12) + TxtBuf$(Temp, 0)
    END IF
    TxtBuf$(Temp, 1) = NoPath$(Make$(Inf.ModNo)) + ": " + RTRIM$(ObjInf.OName)

END SUB

'****************************************************************************
'Purpose: Assigns an object reference to the Ref() array and updates the
'         total number of references and the number of references to an
'         object.  Also alerts the user if there are too many references for
'         the array
'
' Inputs: ObjNo - Object number
'         Ref() - array of reference information
'
'Outputs: Inf.ObjRefs - Total number of object references
'         ObjInf() - array of object information
'****************************************************************************
'
SUB AssignObjRef (BYVAL ObjNo) STATIC
    
    SHARED RefBuffer() AS ReferenceInfo
    SHARED ObjInf AS ObjectInfo
    SHARED SmbInfo() AS SymbolInfo
    SHARED CurrentSymbol
    SHARED MaxRefs
    SHARED BufPtr

    SHARED Ref AS ReferenceInfo
   
    Inf.ObjRefs = Inf.ObjRefs + 1               'Bump total # of references

    Ref.ObjNo = ObjNo                           'Save the object number
    Ref.Module = CHR$(Inf.ModNo)                'Save the module #
    Ref.Include = CHR$(Inf.IncNo)               'Save the include #
    Ref.InProcNo = Inf.ProcNo                   'Save the current procedure #
                                                'Save assignment flag and Type
                                                '  number
    Ref.Assign = CHR$(ObjInf.OType - (&H80 * (Inf.LineNo < 0)))
    Ref.LineNo = ABS(Inf.LineNo)                'Save the current line number

    IF CurrentSymbol THEN Ref.NextRef = 0

    IF BufPtr >= MaxRefs THEN FlushRefBuffer
    IF Inf.ErrCode < -1 THEN EXIT SUB
    BufPtr = BufPtr + 1

    RefBuffer(BufPtr) = Ref

    IF CurrentSymbol THEN
       LastRef& = SmbInfo(CurrentSymbol).LastRef

       IF LastRef& THEN
          Ptr& = BufPtr - (Inf.ObjRefs - LastRef&)
          IF Ptr& > 0 THEN
             RefBuffer(Ptr&).NextRef = Inf.ObjRefs
          ELSE
             FGetRT Inf.RefHandle, Ref, LastRef&, LEN(Ref)
             Ref.NextRef = Inf.ObjRefs
             FPutRT Inf.RefHandle, Ref, LastRef&, LEN(Ref)
          END IF
       END IF

       SmbInfo(CurrentSymbol).LastRef = Inf.ObjRefs
    END IF

    IF DosError THEN FatalError

    IF Inf.ObjRefs MOD 4 = 0 THEN
       QPrintRC STR$(Inf.ObjRefs), 25, 27, Inf.BarClr
    END IF

END SUB

'****************************************************************************
'Purpose: This is the main controlling routine for reading, parsing and
'         saving object information from source files.  Printing of source
'         text is also controlled from this sub.
'
' Inputs: FileName$ - is the name of the main module of the source file
'                     to be read.
'         RptTypes& - bit coded variable flagging various report types.
'         NoKeyWords- Flag to not save BASIC keywords
'         NoNumConst- Flag to not save numeric constants
'         TextFile$ - Name of the file to write quoted strings and comments
'         PageTitle$ - Title line for source listing headers
'         See shared variables below
'
'Outputs: ObjInf() - array of object information
'         Ref() array of object reference information
'         Make$() array of associated module names
'         Incl$() array of referenced include file names
'         TabCon$() array holding table of contents information
'****************************************************************************
'
SUB BuildTables (FileName$, ObjFile$, RefFile$, RptTypes&, NoKeyWords, NoNumConst, TextFile$, PageTitle$, MakeExtn$, Menu$(), Stat()) STATIC

   '~~~~~ Arrays common to the the main module
   SHARED ObjInf AS ObjectInfo          'Object information
   SHARED Ref AS ReferenceInfo          'Object reference information
   SHARED Make$()                       'Names of associated modules
   SHARED Incl$()                       'Names of include files
   
   '~~~~~ Internal variables used within this module only
   SHARED KWord() AS KeyWords           'BASIC keyword tables
   SHARED NumKeyWords                   'Number of BASIC keywords
   SHARED Words() AS WordInfo           'Array of parsed words from a line
   SHARED IncNos()                      'Include file handles
   SHARED Symbol$()
   SHARED SmbInfo() AS SymbolInfo
   SHARED RefBuffer() AS ReferenceInfo
   SHARED LastSymbol
   SHARED ObjBuffer() AS ObjectInfo
   SHARED MaxObjs

   SHARED TripOnComma                   'Flag to finalize a DIM statement
   SHARED LastWord
   SHARED VarTypeChrs$                  'Variable type identifier characters
   SHARED VarTypeTbl$                   'Table of type chars. for current DEFs
   SHARED TypeFlag                      'Flags a TYPE variable
   SHARED Handle                        'Currently opened input file handle
   SHARED Path$
   SHARED Delim$                        'Table of word delimiters
   SHARED PrnLine$                      'Screen printing buffer
   SHARED SourceText$                   'Copy of the current source line
   SHARED SubTitle$
   SHARED PrintSource
   SHARED NewInclude
   SHARED FlushListBuff
   SHARED Text$
   SHARED SaveText                      'Flag to save quoted strings, comments
   SHARED TxtExtn$
   SHARED NewObject
   SHARED CurrentSymbol
   SHARED LNum$


   '~~~~~ Show the user what we are doing
   Temp$ = SPACE$(80)
   RSET Temp$ = "[Esc] to Quit "
   MID$(Temp$, 1) = "Objects:"
   MID$(Temp$, 16) = "References:"
   LOCATE 25, 1, 0
   MQPrint Temp$, 48
   LOCATE , 12
   
                                        'Set flag to print source text
   PrintSource = RptTypes& AND Power2&(1)

   IF RptTypes& AND Power2&(2) THEN     'Set flag to extract text
      SaveText = Inf.Extract
      TxtExtn$ = TextFile$
   ELSE
      SaveText = 0
   END IF
   NewInclude = 0

                                        'Skip initialization if flag set
   IF Inf.ErrCode > -1 THEN GOTO ReEnter


   '~~~~~ Setup for source listing
   '      Note: Source text must be buffered since line numbers will be
   '            reset when a SUB or FUNCTION is encountered.  Reordered
   '            line numbers must begin at the top of any comment block
   '            for a procedure.
   IF PrintSource THEN
      InitSourceListing PageTitle$, FileName$
   END IF

   '~~~~~ Initialize variables
   REDIM Words(Inf.UBWords) AS WordInfo 'Reserve space for the parsed words
   REDIM Symbol$(Inf.UBSymbols)
   REDIM SmbInfo(Inf.UBSymbols) AS SymbolInfo
   REDIM CommonList(1 TO MaxCmn)

   Temp$ = ""

   LastSymbol = 0
   SmbInfo(0).RecNo = 1

   REDIM IncNos(100)                    'Make array to hold Include handles
   DIM BlankObjInf AS ObjectInfo
   BlankObjInf.OName = ""

   DIM TempInf AS ObjectInfo
   DIM ProcInf AS ObjectInfo
   DIM DefFnInf AS ObjectInfo

   Delim$ = " :();,=<>+-/\*^'_" + CHR$(9) 'Set delimiter characters
   Skip$ = " ();," + CHR$(9)            'Set characters to skip (non words)

   AbortMsg$ = CHR$(20) + "Cannot continue."
   VarTypeChrs$ = "%&!#$@"              'Variable type identifier characters
   
   Zero = False
  
   Inf.Lines = Zero                     'Init. number of lines
   Inf.Objects = Zero                   'Init. number of objects
   Inf.ObjRefs = Zero                   'No references yet
   Inf.Incs = Zero                      'No includes yet
   Inf.IncNo = Zero
   Inf.MaxNameLen = Zero
                        
   Words(0).Word = ""                   'Force Word(0) to all spaces
   CommentLine = 0
   IF Inf.LineNumbers THEN
      LNum$ = "     "
   END IF


   '~~~~~ Clear the main window
   IF Inf.DispText THEN
      ClearScr0 3, 2, 23, 79, Inf.ScrClr + 7
   END IF

  
   '~~~~~ Initialize key word tables
   DIM B AS BASICWordNumber
   InitKeyWords KWord(), B, NumKeyWords
   IF Inf.ErrCode < -1 THEN EXIT SUB
   

   '~~~~~ Initialize buffers
   REDIM ObjBuffer(1 TO 1) AS ObjectInfo
   REDIM RefBuffer(1 TO 1) AS ReferenceInfo
   InitObjBuffer
   InitRefBuffer


   '~~~~~ Parse out file's path name
   Path$ = ParsPath$(FileName$)

  
   '~~~~~ Read the .MAK file if there is one
   IF LEN(RTRIM$(Make$(1))) = 0 THEN
      ReadMake FileName$, MakeExtn$
      IF Inf.ErrCode < -1 THEN EXIT SUB
   END IF


   '~~~~~ Create the table files
   ObjFile$ = "Xref1111.OBJ"
   FCreate ObjFile$
   FOpen ObjFile$, Inf.ObjHandle

   RefFile$ = "Xref2222.REF"
   FCreate RefFile$
   FOpen RefFile$, Inf.RefHandle


   GOSUB MakeNewObject
   ObjInf.OName = "[MAIN]"
   ObjInf.OType = -1
   ObjInf.OnLineNo = 1
   ObjInf.DefModInc = 1
   ObjInf.FirstRef = 1
   SaveObject 1

   Handle = 0

  
   '~~~~~ Read and process each module
   FOR M = 1 TO Inf.Modules

      '~~~~~ Clear variables for the new module
      Inf.ModNo = M
      Inf.LineNo = Zero
      Inf.Dynamic = Zero
      Inf.InType = Zero
      Inf.ProcNo = 1
      ProcInf.OnLineNo = Inf.ProcNo     'The main module is defined on line 1
      NumDef = Inf.ProcNo

      VarTypeTbl$ = STRING$(26, "!")    'Default to singles from A-Z for now

      REDIM CommonList(1 TO MaxCmn)
      CmnPtr = 0
      SharedList$ = " "                 'Nothing shared yet
      StaticList$ = " "                 'Nothing static yet
      SubTitle$ = ""

     
      '~~~~~ Show the user what module we are examining
      ShowFileName Make$(Inf.ModNo)


      '~~~~~ Open the source module file
      OpenFile Make$(Inf.ModNo)
      IF Inf.ErrCode < -1 THEN
         FOR N = 1 TO Handle
            CLOSE N
         NEXT
         Handle = 0
         CLOSE #99
         EXIT SUB
      END IF


      '~~~~~ Open the extract text file
      IF SaveText THEN
         OpenSaveText BaseName$(Make$(Inf.ModNo)) + TxtExtn$, Zero
      END IF


      '~~~~~ Read until end of module
      DO UNTIL DoneWithModule                   'Read another line of text

         '~~~~~ Check the keyboard
         Ky$ = INKEY$                           'Check for Ctrl C

         IF QPLen%(Ky$) THEN
            IF ASC(Ky$) = 3 OR ASC(Ky$) = 27 THEN
               Inf.ErrCode = -3
               EXIT SUB
            END IF
         END IF

         Temp = PullMenKey%(Ky$, Menu$(), Stat())
         IF Temp > -1 THEN
            Inf.ErrCode = Temp
            EXIT SUB
         END IF


ReEnter: '~~~~~ Reentry point for aborted and restarted run
         Inf.ErrCode = -1
        
         '~~~~~ Read a line of source
         ReadLine

         '~~~~~ Parse the line into words (Words() array)
         Parse

         IF Inf.ErrCode < -1 THEN
            FOR N = 1 TO Handle
               CLOSE N
            NEXT
            Handle = 0
            CLOSE #99
            EXIT SUB
         END IF
         Inf.Lines = Inf.Lines + 1


         '~~~~~ Print source line if requested
         IF PrintSource THEN
            SELECT CASE Inf.ListInclude
               CASE 0                           'Don't Expand includes
                  Ok2Print = True
               CASE 1                           'Expand includes only once
                  Ok2Print = Inf.IncNo = 0 OR NewInclude OR INSTR(UCASE$(SourceText$), "$INCLUDE")
               CASE ELSE
                  Ok2Print = Inf.IncNo = 0 OR INSTR(UCASE$(SourceText$), "$INCLUDE")
            END SELECT
         ELSE
            Ok2Print = 0
         END IF

         IF Ok2Print THEN PrintSourceLine ProcInf

        
         '~~~~~ Process each word in a source line
         Ptr = 1                        'Current word pointer
         PrevWordPtr = Zero
         PrevBASICWord = Zero           'Previously encountered BASIC keyword
         TripOnComma = Zero             'No DIM statements yet
         OnProcLine = Zero
         OnCall = Zero
         OnIF = Zero
         OnGraph = Zero
         OnLine = Zero


         DO WHILE Ptr <= LastWord

            NewObject = False


            '~~~~~ Process BASIC keywords
            IF Inf.InType THEN
               IF Words(Ptr).BasicWord THEN
                  SELECT CASE Words(Ptr).BasicWord
                     CASE B.EndType, B.AsCurrency, B.AsDouble, B.AsInteger, B.AsLong, B.AsSingle, B.AsString
                        'Only the above words are legit.
                     CASE ELSE
                        Words(Ptr).BasicWord = 0
                  END SELECT
               END IF
            END IF
            
            IF Words(Ptr).BasicWord THEN

               IF NOT NoKeyWords THEN
                  Obj = ObjectNumber(1, Words(Ptr).Word)

                  IF Obj = 0 THEN
                     GOSUB MakeNewObject
                     ObjInf.OName = Words(Ptr).Word
                     ObjInf.OType = BASIC
                     SetNybble ObjInf.Flag, KWord(Words(Ptr).BasicWord).KFlag, Atrb
                  END IF

                  ObjInf.NumRefs = ObjInf.NumRefs + 1
                  SaveObject Obj

                  AssignObjRef Obj
               END IF

               SELECT CASE Words(Ptr).BasicWord

                  CASE B.EndSub, B.EndFunction, B.EndDef
                     Inf.ProcNo = 1
                     Inf.DefFnProc = Zero
                     SharedList$ = " "
                     StaticList$ = " "

                  CASE B.If, B.ElseIf, B.Is
                     OnIF = -1

                  CASE B.Then, B.Goto, B.Gosub
                     OnIF = Zero

                  CASE B.Get, B.Put, B.Step
                     IF Ptr < LastWord THEN
                        IF ASC(Words(Ptr + 1).Word) = 40 THEN
                           OnGraph = -1
                        END IF
                     END IF

                  CASE B.Line, B.LineStep
                     OnLine = True

                  CASE B.Equals
                     IF Inf.DefFnProc THEN
                        IF DefFnInf.OnLineNo = Inf.LineNo THEN
                           Inf.DefFnProc = 0
                        END IF
                     END IF

                  CASE B.EndType
                     Inf.InType = False

                  CASE B.Field, B.Input, B.LineInput, B.Erase
                     PrevBASICWord = Ptr
                     TripOnComma = Ptr

                  CASE B.Colon
                     PrevBASICWord = Zero
                     OnCall = False
                     OnIF = False
                     OnGraph = False
                     OnLine = False

                  CASE B.Else
                     PrevBASICWord = Zero

                  CASE B.Common, B.CommonShared
                     '~~~~~ Skip /BlockName/
                     IF Ptr < LastWord THEN
                        IF ASC(Words(Ptr + 1).Word) = 47 THEN
                           Words(Ptr + 3).Word = ","
                           PrevBASICWord = Ptr
                           TripOnComma = Ptr
                           Ptr = Ptr + 3
                        END IF
                     END IF

                  CASE B.UnScore
                     IF Inf.LineNumbers < 2 THEN Inf.LineNo = Inf.LineNo + 1

                  CASE ELSE
               END SELECT

           
            '~~~~~ Process all Non-BASIC objects
            ELSEIF NOT Inf.InType AND ASC(Words(Ptr).Word) <> Period THEN
                                                 
               '~~~~~ Adjust line number if an underscore is encountered.
               IF Words(PrevWordPtr).BasicWord = B.UnScore THEN
                  PrevWordPtr = PrevWordPtr - 1
               END IF

               '~~~~~ If processing a DIM/REDIM statement and done with it,
               '      set "PrevWordPtr" to point to the DIM/REDIM keyword
               IF PrevBASICWord THEN
                  IF Ptr > TripOnComma THEN
                     IF ASC(Words(Ptr - 1).Word) = Comma OR ASC(Words(PrevWordPtr).Word) = Comma THEN
                        PrevWordPtr = PrevBASICWord
                     END IF
                  END IF
               END IF

               

               PrevWord = Words(PrevWordPtr).BasicWord


               '~~~~~ Words after AS are normally skipped, except when on a
               '      FIELD statement
               IF PrevWord = B.As AND PrevBASICWord > 0 THEN
                  IF Words(PrevBASICWord).BasicWord = B.Field THEN
                     Words(PrevWordPtr).BasicWord = 0
                  END IF
               END IF


               '~~~~~ Find the objects type
               TypeFlag = False
               VarType = False
               N = Ptr
               NoType = False

               HasTypeChar = InstrTbl%(2, Words(Ptr).Word, VarTypeChrs$)
               AscVal = ASC(Words(Ptr).Word)
               IF AscVal = 32 THEN AscVal = ASC(LTRIM$(Words(Ptr).Word))
               IF AscVal >= 97 AND AscVal <= 122 THEN AscVal = AscVal - 32

               IF AscVal >= 65 THEN

                  IF N < LastWord AND ASC(Words(N + 1).Word) = OpenParen THEN
                     Lvl = 0
                     DO
                        N = N + 1
                        SELECT CASE ASC(Words(N).Word)
                           CASE OpenParen
                              Lvl = Lvl + 1
                           CASE CloseParen
                              IF Lvl = 1 THEN EXIT DO
                              Lvl = Lvl - 1
                           CASE ELSE
                        END SELECT
                     LOOP WHILE Words(N).Length
                     TripOnComma = N
                  END IF

                  DimedType = False
                  IF HasTypeChar THEN
                     VarType = ASC(MID$(Words(Ptr).Word, Words(Ptr).Length, 1))

                  ELSE
                     IF N < LastWord AND INSTR(UCASE$(Words(N + 1).Word), "AS ") = 1 AND PrevWord <> B.Field THEN
                        TripOnComma = N + 1

                        SELECT CASE Words(N + 1).BasicWord
                           CASE B.AsString:   VarType = 36   '"$"
                           CASE B.AsInteger:  VarType = 37   '"%"
                           CASE B.AsLong:     VarType = 38   '"&"
                           CASE B.AsSingle:   VarType = 33   '"!"
                           CASE B.AsDouble:   VarType = 35   '"#"
                           CASE B.AsCurrency: VarType = 64   '"@"
                           CASE ELSE
                              VarType = 0
                              TypeFlag = True
                              TripOnComma = N + 2
                        END SELECT
                        IF VarType THEN DimedType = True

                     ELSE
                        VarType = ASC(MID$(VarTypeTbl$, AscVal - 64, 1))
                        NoType = True
                     END IF
                  END IF

               END IF

               IF HasTypeChar THEN
                  MID$(Words(Ptr).Word, Words(Ptr).Length) = " "
                  Words(Ptr).Length = Words(Ptr).Length - 1
               END IF

               Array = False
               IF OnGraph THEN
                  IF ASC(Words(Ptr - 1).Word) = Comma THEN
                     IF ASC(Words(Ptr - 2).Word) = 41 THEN
                        Array = True
                     END IF
                  END IF
               END IF

               IF Ptr < LastWord THEN
                  Array = Array OR ASCII(Words(Ptr + 1).Word) = OpenParen
               END IF

               Array = Array OR PrevWord = B.Erase OR PrevWord = B.UBound OR PrevWord = B.LBound


               '~~~~~ Branch according to the previous BASIC keyword if any
               SELECT CASE PrevWord
                      
                  CASE IS = B.As AND Words(PrevBASICWord).BasicWord <> B.Field
                     '~~~~~ Do nothing case
                      
                  CASE B.Common, B.CommonShared
                    
                     '~~~~~ Search for any prior references
                     Obj = 32767
                     Start = 1
                     DO
                        Temp = ObjectNumber(Start, Words(Ptr).Word)
                        IF Temp = 0 THEN EXIT DO
                        IF Temp < Obj THEN
                           ObjType = ObjInf.Share \ 256
                           IF VarType = ObjType OR (ObjType = 0 AND NoType) THEN
                              IF GetNybble(ObjInf.Flag, Commn) <> 0 OR ObjInf.DefModInc MOD 256 = Inf.ModNo THEN
                                 IF (ObjInf.OType >= NearArray) = Array THEN
                                    Obj = Temp
                                    TempInf = ObjInf
                                    SaveCurrent = CurrentSymbol
                                    'EXIT DO
                                 END IF
                              END IF
                           END IF
                        END IF
                        Start = CurrentSymbol + 1
                     LOOP
                     IF Obj = 32767 THEN Obj = 0

                     Start = 1
                     IF Obj THEN
                        DO
                           Temp = ObjectNumber(Start, Words(Ptr).Word)

                           '~~~~~ If we found a reference,
                           IF Temp THEN
                              IF Temp <> Obj THEN

                                 ObjType = ObjInf.Share \ 256
                                 IF VarType = ObjType OR (ObjType = 0 AND NoType) THEN

                                    IF ObjInf.DefModInc MOD 256 = Inf.ModNo THEN

                                       TempInf.NumRefs = TempInf.NumRefs + ObjInf.NumRefs
                                       TempInf.Share = ObjInf.Share

                                       Symbol$(CurrentSymbol) = ""
                                       SmbInfo(CurrentSymbol).Global = 0
                                       ObjInf.DefModInc = -1
                                       SaveObject Temp

                                       FlushRefBuffer

                                       FGetRT Inf.RefHandle, Ref, ObjInf.FirstRef, LEN(Ref)
                                       Ref.ObjNo = Obj
                                       FPutRT Inf.RefHandle, Ref, ObjInf.FirstRef, LEN(Ref)

                                       FGetRT Inf.RefHandle, Ref, SmbInfo(SaveCurrent).LastRef, LEN(Ref)
                                       Ref.NextRef = ObjInf.FirstRef
                                       FPutRT Inf.RefHandle, Ref, SmbInfo(SaveCurrent).LastRef, LEN(Ref)
                                       SmbInfo(SaveCurrent).LastRef = ObjInf.FirstRef

                                       ObjInf = TempInf
                                       NextNewObj = Temp

                                       EXIT DO
                                    END IF
                                 END IF
                              END IF
                           END IF

                           Start = CurrentSymbol + 1
                        LOOP WHILE Temp
                     END IF


                     '~~~~~ If it's a new variable
                     IF Obj = 0 THEN
                        GOSUB MakeNewObject
                 
                        ObjInf.OName = Words(Ptr).Word

                        '~~~~~ Determine the type of object
                        IF Array THEN
                           ObjInf.OType = FarArray
                        ELSE
                           ObjInf.OType = Simple
                        END IF

                        IF TypeFlag THEN
                           IF ObjInf.OType = Simple THEN
                              ObjInf.OType = SimpleType
                           ELSE
                              ObjInf.OType = FarTypeArray
                           END IF
                        END IF
              
                        '~~~~~ Show where it was defined
                        SetNybble ObjInf.Flag, ABS(DimedType), DimType
                        ObjInf.DefModInc = 256 * Inf.IncNo + Inf.ModNo
                        ObjInf.InProcNo = Inf.ProcNo
                        ObjInf.OnLineNo = Inf.LineNo
                     ELSE
                        ObjInf = TempInf
                        Symbol$(CurrentSymbol) = ""
                        SmbInfo(CurrentSymbol).Global = 0
                        CurrentSymbol = SaveCurrent
                     END IF

                     SetNybble ObjInf.Flag, 1, Commn
                     ObjInf.Share = VarType * 256 + ABS((ObjInf.Share MOD 256) OR PrevWord = B.CommonShared)

                     ObjInf.NumRefs = ObjInf.NumRefs + 1

                     SaveObject Obj

                     AssignObjRef Obj

                     '~~~~~ Add the name to the list of COMMON variables
                     CmnPtr = CmnPtr + 1
                     IF CmnPtr > MaxCmn THEN
                        ErrMsg "Too many COMMON variables!" + AbortMsg$
                        Inf.ErrCode = -9
                        EXIT FOR
                     ELSE
                        CommonList(CmnPtr) = Obj

                        PrevBASICWord = PrevWordPtr
                     END IF

                      
                  CASE B.Const
                    
                     '~~~~~ No identifier, determine the type from expression
                     IF NoType THEN
                        VarType = 0

                        '~~~~~ If something after the equals sign,
                        IF Ptr + 2 <= LastWord THEN

                           '~~~~~ Is it a number?
                           IF ASC(Words(Ptr + 2).Word) < 65 THEN

                              '~~~~~ Is it a float?
                              IF INSTR(Words(Ptr + 2).Word, ".") THEN

                                 '~~~~~ Is it a double?
                                 IF INSTR(UCASE$(Words(Ptr + 2).Word), "D") OR INSTR(Words(Ptr + 2).Word, "#") THEN
                                    VarType = 35        '"#"
                                 ELSE
                                    VarType = 33        '"!"
                                 END IF
                              ELSE

                                 '~~~~~ Is it a long or an int?
                                 Temp& = QPValL&(Words(Ptr + 2).Word)
                                 IF Temp& > 32766 OR Temp& < -32767 THEN
                                    VarType = 38        '"&"
                                 ELSE
                                    VarType = 37        '"%"
                                 END IF
                              END IF
                          
                           '~~~~~ Must be another constant
                           ELSE

                              '~~~~~ Remove any type identifier
                              IF INSTR(VarTypeChrs$, MID$(Words(Ptr + 2).Word, Words(Ptr + 2).Length, 1)) THEN
                                 Temp$ = LEFT$(Words(Ptr + 2).Word, Words(Ptr + 2).Length - 1)
                              ELSE
                                 Temp$ = LEFT$(Words(Ptr + 2).Word, Words(Ptr + 2).Length)
                              END IF

                              '~~~~~ Search for it's information
                              Start = 1
                              DO
                                 Temp = ObjectNumber(Start, Temp$)

                                 IF Temp THEN
                                    Module = ObjInf.DefModInc MOD 256
                                    IF Module = Inf.ModNo THEN
                                       IF ObjInf.InProcNo = Inf.ProcNo THEN
                                          EXIT DO
                                       END IF
                                       IF ObjInf.Share MOD 256 THEN
                                          EXIT DO
                                       END IF
                                    END IF
                                 END IF
                                 Start = CurrentSymbol + 1
                              LOOP WHILE Temp

                              '~~~~~ Use the other constant's type
                              IF Temp THEN
                                 VarType = ObjInf.Share \ 256
                              END IF


                           END IF

                        '~~~~~ Must be a string constant
                        ELSE
                           VarType = 36                 '"$"

                        END IF
                     END IF


                     '~~~~~ Can only be a new object
                     GOSUB MakeNewObject

                     ObjInf.OName = Words(Ptr).Word
                     ObjInf.OType = Constant
                     ObjInf.Share = 256 * VarType - (Inf.ProcNo = 1)

                     '~~~~~ Show where it was defined
                     ObjInf.DefModInc = 256 * Inf.IncNo + Inf.ModNo
                     ObjInf.InProcNo = Inf.ProcNo
                     ObjInf.OnLineNo = Inf.LineNo
                     ObjInf.NumRefs = 1

                     SaveObject Obj


                     '~~~~~ Always an assignment
                     Inf.LineNo = -Inf.LineNo

                     AssignObjRef Obj
                     Inf.LineNo = ABS(Inf.LineNo)
                     PrevBASICWord = PrevWordPtr
                     TripOnComma = Ptr

                      
                  CASE B.DefInt, B.DefLng, B.DefSng, B.DefDbl, B.DefCur, B.DefStr

                     NumDef = NumDef + 1
                    
                     '~~~~~ Set a type identifier character based on the DEF type
                     SELECT CASE PrevWord
                        CASE B.DefInt
                           Temp$ = "%"
                        CASE B.DefLng
                           Temp$ = "&"
                        CASE B.DefSng
                           Temp$ = "!"
                        CASE B.DefDbl
                           Temp$ = "#"
                        CASE B.DefCur
                           Temp$ = "@"
                        CASE B.DefStr
                           Temp$ = "$"
                     END SELECT

                     '~~~~~ Parse up the DEF expression ie. "DEFINT I" or
                     '      "DEFINT I, X" or "DEFINT A-Z" or "DEFDBL A-H, O-Z"
                     DO WHILE Ptr <= LastWord
                        DefChar$ = RTRIM$(Words(Ptr).Word)

                        '~~~~~ Look for a "-"
                        IF Ptr < LastWord THEN
                           IF Words(Ptr + 1).Length = 1 THEN
                              IF INSTR(Words(Ptr + 1).Word, "-") THEN
                                 DefChar$ = DefChar$ + RTRIM$(Words(Ptr + 1).Word) + RTRIM$(Words(Ptr + 2).Word)
                                 Ptr = Ptr + 2
                              END IF
                           END IF
                        END IF
                        Upper DefChar$

                        '~~~~~ Get table position from character value
                        St = ASCII(DefChar$) - 64

                        '~~~~~ is this a range of characters?
                        Through = INSTR(DefChar$, "-")
                        IF Through THEN
                           En = ASCII(MID$(DefChar$, Through + 1)) - 64
                           MID$(VarTypeTbl$, St) = STRING$(En - St + 1, Temp$)
                        ELSE
                           MID$(VarTypeTbl$, St) = Temp$
                        END IF

                        '~~~~~ No comma in next position, bail out
                        IF Ptr < LastWord THEN
                           IF ASC(Words(Ptr + 1).Word) <> Comma THEN EXIT DO
                        END IF
                        Ptr = Ptr + 2
                     LOOP

            
                  CASE B.DeclareSub, B.DeclareFunction, B.Call, B.Calls, B.Def, B.Sub, B.Function

                     '~~~~~ Look for previous references
                     Obj = ObjectNumber(1, Words(Ptr).Word)

                     '~~~~~ If we didn't find any, assign the new proc. name
                     IF Obj = False THEN
                        GOSUB MakeNewObject
                        ObjInf.OName = Words(Ptr).Word
                        
                        IF PrevWord = B.Function OR PrevWord = B.DeclareFunction THEN
                           ObjInf.OType = FuncProc
                           ObjInf.Share = 256 * VarType

                        ELSEIF PrevWord = B.Def THEN
                           IF UCASE$(RTRIM$(ObjInf.OName)) = "FN" THEN
                              ObjInf.OName = RTRIM$(ObjInf.OName) + Words(Ptr + 1).Word
                              Ptr = Ptr + 1
                           END IF
                           ObjInf.OType = DefFnProc
                           ObjInf.Share = 256 * VarType
                           CommentLine = 0

                        ELSE
                           ObjInf.OType = SubProc
                        END IF
                     END IF
                
                     '~~~~~ Calls are always a reference
                     IF PrevWord = B.Call OR PrevWord = B.Calls THEN
                        
                        ObjInf.NumRefs = ObjInf.NumRefs + 1
                        IF ObjInf.FirstRef = 0 THEN
                           ObjInf.FirstRef = Inf.ObjRefs + 1
                        END IF
                        OnCall = True

                        SaveObject Obj
                        AssignObjRef Obj

                     '~~~~~ If this is not a declaration
                     ELSEIF PrevWord <> B.DeclareSub AND PrevWord <> B.DeclareFunction THEN
                        SharedList$ = " "
                        StaticList$ = " "
                        ObjInf.DefModInc = 256 * Inf.IncNo + Inf.ModNo

                        '~~~~~ Handle DEF FN definitions
                        IF PrevWord = B.Def THEN
                           ObjInf.InProcNo = Inf.ProcNo
                           Inf.DefFnProc = Obj
                           ObjInf.OnLineNo = Inf.LineNo

                           DefFnInf = ObjInf
                       
                        '~~~~~ Handle SUB or FUNCTION definitions
                        ELSE
                           OnProcLine = True

                           '~~~~~ Is this a STATIC procedure?
                           SetNybble ObjInf.Flag, Automatic, Atrb
                           Temp = Ptr + 1
                           DO WHILE Temp <= LastWord
                              IF Words(Temp).BasicWord = B.Static THEN
                                 SetNybble ObjInf.Flag, 0, Atrb
                                 EXIT DO
                              END IF
                              Temp = Temp + 1
                           LOOP

                           IF Inf.LineNumbers = 2 THEN
                              ObjInf.OnLineNo = Inf.LineNo - (CommentLine + NumDef - 1)
                           ELSE
                              ObjInf.OnLineNo = Inf.LineNo
                           END IF

                           Inf.ProcNo = Obj
                           ProcInf = ObjInf


                           '~~~~~ Adjust source listing line numbers
                           IF PrintSource THEN
                              AdjustSourceLineNos CommentLine, NumDef, ObjInf
                           END IF

                        END IF

                        '~~~~~ Point .FirstRef at this reference and the
                        '      current ref's pointer to the original .FirstRef
                        FirstRef& = ObjInf.FirstRef
                        ObjInf.FirstRef = Inf.ObjRefs + 1
                        SaveObject Obj

                        IF NewObject THEN FirstRef& = 0
                        Ref.NextRef = FirstRef&
                        IF FirstRef& THEN CurrentSymbol = 0

                        AssignObjRef Obj

                     ELSE

                        IF NewObject THEN ObjInf.FirstRef = 0

                        SaveObject Obj
                     END IF
                     
               

                  CASE B.Dim, B.DimShared, B.ReDim, B.ReDimShared, B.ReDimPreserve, B.Shared, B.Static
                     Test$ = " " + UCASE$(RTRIM$(Words(Ptr).Word)) + " "

                     '~~~~~ Look for previous references
                     Start = 1
                     DO
                        Obj = ObjectNumber(Start, Words(Ptr).Word)

                        '~~~~~ If we found a match,
                        IF Obj THEN
                           ObjType = ObjInf.Share \ 256
                           IF VarType = ObjType OR (ObjType = 0 AND NoType) THEN

                              Module = ObjInf.DefModInc MOD 256
                              Cmmn = GetNybble(ObjInf.Flag, Commn) > 0 AND XRSearch%(CommonList(1), CmnPtr, Obj) > 0

                              IF (ObjInf.OType >= NearArray) = Array THEN
                          
                                 '~~~~~ Is it in the same module or a COMMON var.?
                                 IF Module = Inf.ModNo OR Cmmn THEN

                                    '~~~~~ Are we in a DEF FN proc.?
                                    IF Inf.DefFnProc THEN

                                       '~~~~~ Is it a static variable?
                                       IF INSTR(StaticList$, Test$) THEN
                                          IF ObjInf.InProcNo = Inf.DefFnProc THEN EXIT DO
                                       ELSEIF ObjInf.InProcNo = Inf.ProcNo THEN
                                          EXIT DO
                                       END IF
                                    ELSE
                                       '~~~~~ In same procedure, must be same var.
                                       IF ObjInf.InProcNo = Inf.ProcNo THEN EXIT DO
                                    END IF
                                    IF ObjInf.Share MOD 256 THEN EXIT DO
                                    IF INSTR(SharedList$, Test$) THEN EXIT DO
                                    IF PrevWord = B.Shared THEN EXIT DO
                                 END IF
                              END IF
                           END IF
                        END IF
                       
                        '~~~~~ Look again at next object
                        Start = CurrentSymbol + 1
                     LOOP WHILE Obj

                    
                     '~~~~~ If this is a new object, assign it
                     IF Obj = 0 THEN
                        GOSUB MakeNewObject
                  
                        ObjInf.OName = Words(Ptr).Word
                     END IF

                     Temp = TypeFlag
                     SaveTrip = TripOnComma

                     GOSUB GetAttribute


                     '~~~~~ Determine the object type for the variable
                     SELECT CASE PrevWord

                        CASE B.Dim, B.DimShared, B.Shared, B.Static
                                     
                           '~~~~~ Not an array
                           IF NOT Array THEN
                              ObjInf.OType = Simple

                           ELSEIF ObjInf.OType < FarArray THEN

                              '~~~~~ '$DYNAMIC in effect
                              IF Inf.Dynamic THEN
                                 ObjInf.OType = FarArray
                             
                              '~~~~~ Variable used as the subscript
                              ELSEIF NOT AConstant%(Ptr + 2) THEN
                                 ObjInf.OType = FarArray

                              ELSE
                                 ObjInf.OType = NearArray
                                 IF Ptr + 3 <= LastWord THEN
                                    IF UCASE$(RTRIM$(Words(Ptr + 3).Word)) = "TO" THEN
                                       IF NOT AConstant%(Ptr + 4) THEN
                                          ObjInf.OType = FarArray
                                       END IF
                                    END IF
                                 END IF
                                 
                                 IF NewObject THEN
                                    IF GetNybble(ObjInf.Flag, Atrb) = Automatic THEN
                                       ObjInf.OType = FarArray
                                    END IF
                                 END IF
                              END IF
                           END IF

                        CASE B.ReDim, B.ReDimShared, B.ReDimPreserve
                           ObjInf.OType = FarArray

                     END SELECT


                     TripOnComma = SaveTrip

                     '~~~~~ Is this a TYPE variable?
                     TypeFlag = Temp
                     IF TypeFlag THEN
                        IF ObjInf.OType = Simple THEN
                           ObjInf.OType = SimpleType
                        ELSEIF ObjInf.OType = FarArray THEN
                           ObjInf.OType = FarTypeArray
                        ELSEIF ObjInf.OType = NearArray THEN
                           ObjInf.OType = NearTypeArray
                        END IF
                     END IF

                     IF NewObject THEN
                        '~~~~~ Show where it was defined
                        SetNybble ObjInf.Flag, ABS(DimedType), DimType
                        ObjInf.Share = VarType * 256 + ABS((ObjInf.Share MOD 256) OR PrevWord = B.Shared OR PrevWord = B.DimShared OR PrevWord = B.ReDimShared)
                        ObjInf.DefModInc = 256 * Inf.IncNo + Inf.ModNo
                        ObjInf.InProcNo = Inf.ProcNo
                        ObjInf.OnLineNo = Inf.LineNo
                     END IF

                     SELECT CASE PrevWord
                        CASE B.Shared
                     
                           SharedList$ = SharedList$ + UCASE$(RTRIM$(Words(Ptr).Word)) + " "
                           TripOnComma = Ptr

                        CASE B.Static

                           StaticList$ = StaticList$ + UCASE$(RTRIM$(Words(Ptr).Word)) + " "
                           TripOnComma = Ptr
                           SetNybble ObjInf.Flag, 0, Atrb

                        CASE ELSE
                           ObjInf.NumRefs = ObjInf.NumRefs + 1

                     END SELECT

                     SaveObject Obj
                     
                     AssignObjRef Obj

                     PrevBASICWord = PrevWordPtr


                  CASE B.ForISAM


                  CASE B.Goto, B.Gosub, B.Restore, B.Resume, B.Return, B.ErlEqu

                     IF ASC(LTRIM$(Words(Ptr).Word)) <> 48 OR (PrevWord <> B.Goto AND PrevWord <> B.Resume) THEN

                        '~~~~~ Put the colon back on line labels
                        'IF ASC(Words(Ptr).Word) >= 65 THEN
                        '   MID$(Words(Ptr).Word, Words(Ptr).Length + 1) = ":"
                        '   Words(Ptr).Length = Words(Ptr).Length + 1
                        'END IF

                        '~~~~~ Look for previous references
                        Start = 1
                        DO
                           Obj = ObjectNumber(Start, Words(Ptr).Word)
                           IF Obj THEN
                              IF ObjInf.OType = Label THEN
                                 IF ObjInf.DefModInc MOD 256 = Inf.ModNo THEN
                                    EXIT DO
                                 END IF
                              END IF
                              Start = CurrentSymbol + 1
                           END IF
                        LOOP WHILE Obj

                        '~~~~~ Add to list if it's new (Forward reference)
                        IF Obj = 0 THEN
                           GOSUB MakeNewObject
                           ObjInf.OName = Words(Ptr).Word
                           ObjInf.OType = Label
                           ObjInf.DefModInc = 256 * Inf.IncNo + Inf.ModNo
                           ObjInf.InProcNo = Inf.ProcNo
                        END IF

                        ObjInf.NumRefs = ObjInf.NumRefs + 1

                        SaveObject Obj

                        AssignObjRef Obj

                        IF Ptr < LastWord THEN
                           IF ASCII(Words(Ptr + 1).Word) = Comma AND (PrevWord = B.Goto OR PrevWord = B.Gosub) THEN
                              PrevBASICWord = PrevWordPtr
                              TripOnComma = Ptr
                           END IF
                        END IF
                     END IF


                  CASE B.Type
                     Inf.InType = True


                  CASE ELSE

                     IF OnLine THEN
                        IF INSTR("BF", UCASE$(RTRIM$(Words(Ptr).Word))) GOTO BOnLINE
                     END IF

                     Test$ = " " + UCASE$(RTRIM$(Words(Ptr).Word)) + " "

                     Lbl = False

                     IF Ptr = 1 OR PrevWord = B.Then OR PrevWord = B.Else OR PrevWord = B.Run THEN
                        IF AscVal >= 48 AND AscVal <= 57 THEN
                           Lbl = True
                           VarType = 0
                        ELSEIF Ptr < LastWord AND ASC(Words(Ptr + 1).Word) = Colon THEN
                           Lbl = True
                           VarType = 0
                        END IF
                     END IF


                     Start = 1
                     DO
                        Temp$ = ""
                        Obj = ObjectNumber(Start, Words(Ptr).Word)
                        IF Obj = 0 THEN
                           IF Lbl = False THEN
                              Per = INSTR(Words(Ptr).Word, ".")
                              IF Per THEN
                                 DO
                                    Obj = ObjectNumber(Start, LEFT$(Words(Ptr).Word, Per - 1))
                                    IF Obj THEN
                                       IF ObjInf.OType = SimpleType THEN
                                          Words(Ptr).Word = LEFT$(Words(Ptr).Word, Per - 1)
                                          Test$ = " " + UCASE$(RTRIM$(Words(Ptr).Word)) + " "
                                          VarType = 0
                                          EXIT DO
                                       END IF
                                    END IF
                                    Start = CurrentSymbol + 1
                                 LOOP WHILE Obj
                              END IF
                           END IF
                        END IF
                        
                        IF Obj THEN
                           Cmmn = GetNybble(ObjInf.Flag, Commn) > 0 AND XRSearch%(CommonList(1), CmnPtr, Obj) > 0
                           Module = ObjInf.DefModInc MOD 256
                           OType = ObjInf.OType

                           IF OType = FuncProc OR OType = SubProc THEN
                              IF Lbl THEN Lbl = 0
                              EXIT DO

                           ELSEIF Module = Inf.ModNo OR Cmmn THEN
                              
                              SELECT CASE OType
                                 CASE Label
                                    IF Lbl THEN EXIT DO

                                 CASE DefFnProc
                                    EXIT DO

                                 CASE Constant
                                    IF ObjInf.InProcNo = 1 OR ObjInf.InProcNo = Inf.ProcNo THEN
                                       EXIT DO
                                    END IF

                                 CASE ELSE

                                    ObjType = ObjInf.Share \ 256
                                    IF NoType THEN
                                       WasDimedType = GetNybble(ObjInf.Flag, DimType)
                                    ELSE
                                       WasDimedType = 0
                                    END IF

                                    IF VarType = ObjType OR WasDimedType = 1 OR (ObjType = 0 AND NoType) THEN
                                       IF NOT Lbl OR OType = Label THEN

                                          IF (OType >= NearArray) = Array THEN

                                             IF Inf.DefFnProc THEN
                                                IF INSTR(StaticList$, Test$) THEN
                                                   IF ObjInf.InProcNo = Inf.DefFnProc THEN EXIT DO
                                                ELSEIF ObjInf.InProcNo = Inf.ProcNo THEN
                                                   EXIT DO
                                                END IF
                                             ELSE
                                                IF ObjInf.InProcNo = Inf.ProcNo THEN EXIT DO
                                             END IF

                                             IF ObjInf.Share MOD 256 THEN EXIT DO
 
                                             IF INSTR(SharedList$, Test$) THEN
                                                EXIT DO
                                             END IF
                                          END IF
                                       END IF
                                    END IF

                              END SELECT
                           END IF
                           Start = CurrentSymbol + 1
                        END IF
                        
                     LOOP WHILE Obj


                     IF Obj = 0 THEN

                        IF AscVal < 65 THEN
                           IF Lbl = False THEN
                              IF NOT NoNumConst THEN
                                 GOSUB MakeNewObject
                                 ObjInf.OName = Words(Ptr).Word
                                 ObjInf.OType = NumConst
                                 ObjInf.Share = 1
                              END IF
                              ObjInf.OType = NumConst
                           ELSE
                              GOSUB MakeNewObject
                              ObjInf.OName = Words(Ptr).Word
                           END IF
                        ELSE

                           GOSUB MakeNewObject
                           ObjInf.OName = Words(Ptr).Word

                           IF Lbl = False THEN GOSUB GetAttribute

                           IF Array THEN
                              IF TypeFlag THEN
                                 ObjInf.OType = NearTypeArray
                              ELSE
                                 ObjInf.OType = NearArray
                              END IF
                           ELSE
                              IF TypeFlag THEN
                                 ObjInf.OType = SimpleType
                              ELSE
                                 ObjInf.OType = Simple
                              END IF
                           END IF
                        END IF

                        ObjInf.DefModInc = 256 * Inf.IncNo + Inf.ModNo
                        IF Inf.DefFnProc THEN
                           IF INSTR(StaticList$, Test$) THEN
                              ObjInf.InProcNo = Inf.DefFnProc
                           ELSE
                              ObjInf.InProcNo = Inf.ProcNo
                           END IF
                           ObjInf.OnLineNo = Inf.LineNo
                        ELSE
                           IF INSTR(SharedList$, Test$) THEN
                              ObjInf.InProcNo = 1
                              ObjInf.OnLineNo = 1
                           ELSE
                              ObjInf.InProcNo = Inf.ProcNo
                              ObjInf.OnLineNo = Inf.LineNo
                           END IF
                        END IF

                        SetNybble ObjInf.Flag, ABS(DimedType), DimType
                        ObjInf.Share = VarType * 256 + (ObjInf.Share MOD 256)

                     END IF

                     IF Lbl THEN
                        ObjInf.OType = Label
                        ObjInf.InProcNo = Inf.ProcNo
                        IF Ptr = 1 THEN ObjInf.OnLineNo = Inf.LineNo
                     ELSE
                        IF ObjInf.OType = SubProc THEN
                           OnCall = Inf.UBWords
                        ELSEIF ObjInf.OType = FuncProc THEN
                           IF TripOnComma > OnCall THEN OnCall = TripOnComma
                        END IF

                        IF ObjInf.OType <> SubProc THEN
                           SELECT CASE PrevWord
                              CASE B.Lset, B.Rset, B.Mid, B.Swap, B.Then, B.For, B.Colon, B.Input, B.LineInput
                                 Inf.LineNo = -Inf.LineNo
                                 IF PrevWord = B.Swap THEN
                                    PrevBASICWord = PrevWordPtr
                                    TripOnComma = Ptr
                                 ELSEIF PrevWord = B.Mid THEN
                                    IF Ptr > 2 THEN                 '"="
                                       IF ASC(Words(Ptr - 2).Word) = 61 THEN
                                          Inf.LineNo = ABS(Inf.LineNo)
                                       END IF
                                    END IF
                                 END IF
                              CASE ELSE                        '"="
                                 IF Ptr < LastWord AND ASC(Words(Ptr + 1).Word) = 61 THEN
                                    IF NOT OnIF THEN
                                       Inf.LineNo = -Inf.LineNo
                                    END IF
                                 END IF
                           END SELECT
                        END IF
                     END IF

                     IF NOT NoNumConst OR ObjInf.OType <> NumConst THEN
                        IF PrevWord = B.For THEN
                           ObjInf.NumRefs = ObjInf.NumRefs + 2
                        ELSEIF NOT ObjInf.OType = FuncProc OR NOT Obj = Inf.ProcNo THEN
                           ObjInf.NumRefs = ObjInf.NumRefs + 1
                        END IF

                        IF ObjInf.FirstRef = 0 THEN
                           ObjInf.FirstRef = Inf.ObjRefs + 1
                        END IF

                        SaveObject Obj

                        AssignObjRef Obj
                     END IF

                     Inf.LineNo = ABS(Inf.LineNo)
BOnLINE:
               END SELECT
            END IF

            IF Inf.ErrCode < -1 THEN EXIT FOR
            
            PrevWordPtr = Ptr
            DO
               Ptr = Ptr + 1
               IF Ptr >= Inf.UBWords THEN EXIT DO
            LOOP WHILE Ptr <= LastWord AND INSTR(Skip$, RTRIM$(Words(Ptr).Word)) > 0

         LOOP

         
         Word$ = UCASE$(RTRIM$(Words(1).Word))
         IF LastWord > 0 AND (Word$ = "'" OR Word$ = "REM") THEN
            CommentLine = CommentLine + 1
         ELSE
            CommentLine = 0
         END IF

      LOOP


      '~~~~~ Clear out all non-global symbol names
      ClearSymbols

   NEXT                                         'Read the next module file

   FOR N = 1 TO Handle
       CLOSE N
   NEXT


   '~~~~~ Flush the Object and reference buffers
   FlushBuffers


   IF PrintSource OR FlushListBuff THEN
      FlushListingBuffer
   END IF
   
   IF SaveText THEN OpenSaveText "", 0

   FClose Inf.ObjHandle
   FClose Inf.RefHandle
   Inf.ObjHandle = 0
   Inf.RefHandle = 0

   ERASE KWord, IncNos, Symbol$, SmbInfo, ObjBuffer
   ERASE RefBuffer, CommonList
   
   DefChar$ = ""
   Delim$ = ""
   LNum$ = ""
   PrnLine$ = ""
   SharedList$ = ""
   Skip$ = ""
   SourceText$ = ""
   StaticList$ = ""
   Temp$ = ""
   Test$ = ""
   Text$ = ""
   VarTypeChrs$ = ""
   VarTypeTbl$ = ""
   Word$ = ""
   
   IF Inf.ErrCode >= -3 THEN Inf.ErrCode = -1

   EXIT SUB


'~~~~~ Get the variables attribute (Static, Automatic, Parameter)
GetAttribute:

   '~~~~~ Are we in a procedure?
   IF Inf.ProcNo > 1 THEN

      '~~~~~ Are we on the proc definition line?
      IF OnProcLine THEN

         '~~~~~ Must be a parameter
         SetNybble ObjInf.Flag, Parameter, Atrb

      '~~~~~ Is this a dynamic procedure?
      ELSEIF GetNybble(ProcInf.Flag, Atrb) = Automatic THEN

         '~~~~~ If the variable wasn't in a STATIC statement
         IF INSTR(StaticList$, Test$) = 0 THEN

            '~~~~~ If it wasn't in a SHARED statement
            IF INSTR(SharedList$, Test$) = 0 THEN

               '~~~~~ It must be an automatic variable
               SetNybble ObjInf.Flag, Automatic, Atrb
            END IF
         END IF
      END IF
   END IF

RETURN

MakeNewObject:

    ObjInf = BlankObjInf

    IF NextNewObj THEN
       Obj = NextNewObj
       NextNewObj = 0
    ELSE
       IF Inf.Objects < 32766 THEN
          Inf.Objects = Inf.Objects + 1
       ELSE
          ErrMsg "Too many objects!  Try eliminating key words and numeric constants."
          Inf.ErrCode = -9
       END IF
       Obj = Inf.Objects
    END IF
    NewObject = True

    ObjInf.FirstRef = Inf.ObjRefs + 1

    QPrintRC STR$(Inf.Objects), 25, 9, Inf.BarClr
RETURN


END SUB

SUB ClearSymbols STATIC

    SHARED Symbol$()
    SHARED SmbInfo() AS SymbolInfo
    SHARED LastSymbol

    Prev = 0
    Temp = LastSymbol
    FOR N = 1 TO Temp
       IF NOT SmbInfo(N).Global THEN
          Symbol$(N) = ""
          SmbInfo(N).LastRef = 0
          SmbInfo(Prev).RecNo = N
          Prev = N
       ELSE
          LastSymbol = N
       END IF
    NEXT

    SmbInfo(Prev).RecNo = N
    
END SUB

FUNCTION DoneWithModule STATIC
    SHARED Handle, LineWas, Make$(), IncNos(), Incl$(), SaveText, TxtExtn$

    DO WHILE EOF(Handle)                'Close include file when done
       CLOSE Handle
       Handle = Handle - 1
       Inf.IncNo = IncNos(Handle)

       SELECT CASE Handle
          CASE 0
             EXIT DO

          CASE 1                        'Redisplay module name
             IF SaveText THEN
                OpenSaveText BaseName$(Make$(Inf.ModNo)) + TxtExtn$, 1
             END IF

             Inf.LineNo = LineWas

             ShowFileName Make$(Inf.ModNo)
             'EXIT DO

          CASE ELSE
             IF SaveText THEN
                OpenSaveText BaseName$(Incl$(Inf.IncNo)) + TxtExtn$, 1
             END IF

       END SELECT
       
    LOOP

    DoneWithModule = Handle = 0

END FUNCTION

FUNCTION ExpandTab$ (X$, BYVAL NumSpaces) STATIC

    I = InCount(X$, CHR$(9))                    'Find number of Tab Chars.
    IF I THEN                                   'Are there any?
                                                'make room for new string
       Work$ = SPACE$(QPLen%(X$) + 1 + (NumSpaces - 1) * I)

       LSET Work$ = X$ + CHR$(0)                'Put existing string in it
                                                '  and a CHR$(0) for later
       X = 1                                    'Set search position to 1

       FOR N = 1 TO I                           'Do each Tab
                                                'find the next Tab character
           X = INSTR(X, Work$, CHR$(9)) + 1

           Length = X - 2                       'Calc length of left part
                                                'Calc spaces to next tab stop
           Sp = Length + NumSpaces - (Length MOD NumSpaces) - Length
                                                'Insert the spaces
           IF X > 1 THEN MID$(Work$, X - 1) = SPACE$(Sp) + MID$(Work$, X)
       NEXT                                     'Assign the function looking
                                                '  for the CHR$(0)
       ExpandTab$ = LEFT$(Work$, INSTR(Work$, CHR$(0)) - 1)
       Work$ = ""
    ELSE                                        'No tabs, just assign the
       ExpandTab$ = X$                          '  function
    END IF

END FUNCTION

'~~~~~ Flush the Object information buffer
'
SUB FlushBuffers
   
   SHARED ObjBuffer() AS ObjectInfo
   SHARED MaxObjs

   IF MaxObjs > 0 THEN
      Records = MinInt(MaxObjs, Inf.Objects)

      '~~~~~ Flush the Objects buffer
      FSeek Inf.ObjHandle, 0&
      RecsSaved = 0
      Recs2Save = 65536 \ LEN(ObjBuffer(1))

      DO UNTIL RecsSaved >= Records OR DosError
         IF RecsSaved + Recs2Save > Records THEN
            Recs2Save = Records - RecsSaved
         END IF
         FPutA Inf.ObjHandle, ObjBuffer(RecsSaved + 1), CLNG(Recs2Save * LEN(ObjBuffer(1)))

         RecsSaved = RecsSaved + Recs2Save
      LOOP

      IF DosError THEN FatalError
   END IF

   '~~~~~ Flush the References buffer
   FlushRefBuffer

END SUB

SUB FlushListingBuffer

   SHARED PushedLines
   SHARED SubTitle$
   SHARED TxtBuf$(), Header$()

   IF PushedLines < 64 THEN PrintHeader Header$()
   FOR N = PushedLines TO 1 STEP -1
      PrintBufferedLine N, TxtBuf$(), Header$(), SubTitle$
   NEXT

   PrintFootNote ""
   Inf.TabConLine = Inf.TabConLine + 1

   ERASE TxtBuf$, Header$
   SubTitle$ = ""

END SUB

SUB FlushRefBuffer

   SHARED RefBuffer() AS ReferenceInfo
   SHARED BufPtr, MaxRefs

   IF MaxRefs <= 0 THEN EXIT SUB

   '~~~~~ Flush the References buffer
   FSeek Inf.RefHandle, FLof&(Inf.RefHandle)

   Recs2Save = 65535 \ LEN(RefBuffer(1))
   RecsSaved = 0

   DO UNTIL RecsSaved >= BufPtr
      IF RecsSaved + Recs2Save > BufPtr THEN
         Recs2Save = BufPtr - RecsSaved
      END IF

      FPutA Inf.RefHandle, RefBuffer(RecsSaved + 1), CLNG(Recs2Save) * LEN(RefBuffer(1))
      IF DosError THEN EXIT DO

      RecsSaved = RecsSaved + Recs2Save
   LOOP

   IF DosError THEN
      FatalError
   ELSE
      BufPtr = 0
   END IF

END SUB

SUB InitKeyWords (KWord() AS KeyWords, B AS BASICWordNumber, NumKeyWords)

   '~~~~~ Find the keyword table file
   KeyTable$ = "XRef.Key"
   IF NOT Exist(KeyTable$) THEN
      Try$ = ParsPath$(ExeName$) + KeyTable$
      IF Exist(Try$) THEN
         KeyTable$ = Try$
      ELSE
         SrchPath KeyTable$, ENVIRON$("PATH"), NotFound
         IF NotFound THEN
            ErrMsg "Can't find " + KeyTable$ + AbortMsg$
            Inf.ErrCode = -53
            EXIT SUB
         END IF
      END IF
   END IF


   '~~~~~ Open and load the keyword table file
   FOpen KeyTable$, Handle%
   IF DosError THEN
      FatalError
      EXIT SUB
   END IF
   Bytes& = FLof&(Handle%)
   NumKeyWords = Bytes& \ 19
   REDIM KWord(NumKeyWords) AS KeyWords
   FGetA Handle%, KWord(1), Bytes&
   FClose Handle%


   '~~~~~ Initialize Special Keyword TYPE "B"
   REDIM Temp$(67)
   REDIM Temp(67)

   Temp$(1) = "AS"
   Temp$(2) = "AS CURRENCY"
   Temp$(3) = "AS DOUBLE"
   Temp$(4) = "AS INTEGER"
   Temp$(5) = "AS LONG"
   Temp$(6) = "AS SINGLE"
   Temp$(7) = "AS STRING"
   Temp$(8) = ":"
   Temp$(9) = "COMMON"
   Temp$(10) = "COMMON SHARED"
   Temp$(11) = "CONST"
   Temp$(12) = "CALL"
   Temp$(13) = "CALLS"
   Temp$(14) = "DECLARE FUNCTION"
   Temp$(15) = "DECLARE SUB"
   Temp$(16) = "DEF"
   Temp$(17) = "DEFCUR"
   Temp$(18) = "DEFDBL"
   Temp$(19) = "DEFINT"
   Temp$(20) = "DEFLNG"
   Temp$(21) = "DEFSNG"
   Temp$(22) = "DEFSTR"
   Temp$(23) = "DIM"
   Temp$(24) = "DIM SHARED"
   Temp$(25) = "ELSE"
   Temp$(26) = "ELSEIF"
   Temp$(27) = "END DEF"
   Temp$(28) = "END FUNCTION"
   Temp$(29) = "END SUB"
   Temp$(30) = "END TYPE"
   Temp$(31) = "="
   Temp$(32) = "ERASE"
   Temp$(33) = "ERL ="
   Temp$(34) = "FIELD"
   Temp$(35) = "FOR"
   Temp$(36) = "FOR ISAM"
   Temp$(37) = "FUNCTION"
   Temp$(38) = "GET"
   Temp$(39) = "GOSUB"
   Temp$(40) = "GOTO"
   Temp$(41) = "IF"
   Temp$(42) = "INPUT"
   Temp$(43) = "IS"
   Temp$(44) = "LBOUND"
   Temp$(45) = "LINE"
   Temp$(46) = "LINE INPUT"
   Temp$(47) = "LINE STEP"
   Temp$(48) = "LSET"
   Temp$(49) = "MID$"
   Temp$(50) = "PUT"
   Temp$(51) = "REDIM"
   Temp$(52) = "REDIM PRESERVE"
   Temp$(53) = "REDIM SHARED"
   Temp$(54) = "RESTORE"
   Temp$(55) = "RESUME"
   Temp$(56) = "RETURN"
   Temp$(57) = "RUN"
   Temp$(58) = "RSET"
   Temp$(59) = "SHARED"
   Temp$(60) = "STATIC"
   Temp$(61) = "STEP"
   Temp$(62) = "SUB"
   Temp$(63) = "SWAP"
   Temp$(64) = "THEN"
   Temp$(65) = "TYPE"
   Temp$(66) = "UBOUND"
   Temp$(67) = "_"

   FOR N = 1 TO UBOUND(Temp)
      KWord(0).KName = Temp$(N)
      Temp(N) = XRFindKey%(KWord(0), NumKeyWords)
      IF Temp(N) = 0 THEN STOP
   NEXT
   
   BCopyT Temp(1), B, -1, N - 1

END SUB

SUB InitObjBuffer

   SHARED ObjInf AS ObjectInfo
   SHARED ObjBuffer() AS ObjectInfo
   SHARED MaxObjs

   Mem& = FRE(-1) - 32767 - 4096
                           '^ Leave room for the reference buffer
   Mem& = UBOUND(ObjBuffer) * LEN(ObjInf) + Mem&
   IF Mem& > 131000 THEN Mem& = 131000

   MaxObjs = Mem& \ LEN(ObjInf)
   IF MaxObjs > 0 THEN
      REDIM ObjBuffer(1 TO MaxObjs) AS ObjectInfo
   END IF

END SUB

SUB InitRefBuffer

    SHARED Ref AS ReferenceInfo
    SHARED RefBuffer() AS ReferenceInfo
    SHARED MaxRefs

    Mem& = FRE(-1) - 32767
    Mem& = UBOUND(RefBuffer) * LEN(Ref) + Mem&
    IF Mem& > 131059 THEN Mem& = 131059
    MaxRefs = Mem& \ LEN(Ref)
    IF MaxRefs < 1 THEN MaxRefs = 1

    REDIM RefBuffer(1 TO MaxRefs) AS ReferenceInfo

END SUB

SUB InitSourceHeader

    SHARED VertLine$
    SHARED Header$()
    SHARED PageTtl$, FlName$

    REDIM Header$(3)                    'Make a header for printing
    Temp = Inf.PrnWidth - Inf.PrnLeftMarg
    Header$(1) = SPACE$(Temp)
    MID$(Header$(1), 1) = PageTtl$

    Header$(2) = SPACE$(Temp)
    MID$(Header$(2), 1) = NoPath$(FlName$) + ": MAIN|"
    MID$(Header$(2), Temp - 18) = Inf.DateTime

    IF Inf.GraphChars THEN
       Header$(3) = STRING$(Temp, "Ä")
       IF Inf.LineNumbers THEN MID$(Header$(3), 6) = "Â"
       VertLine$ = "³"
    ELSE
       Header$(3) = STRING$(Temp, "-")
       VertLine$ = "|"
    END IF

END SUB

SUB InitSourceListing (PageTitle$, FileName$)

    SHARED PushedLines
    SHARED Title$, SubTitle$, VertLine$
    SHARED TxtBuf$(), Header$()
    SHARED TabCon$()                    'Table of contents text array
    SHARED LastMod
    SHARED PageTtl$, FlName$


    Title$ = ""
    SubTitle$ = ""
    REDIM TxtBuf$(64, 1)                'Create a buffer for printed source
    PushedLines = 0                     'Nothing in buffer yet
    TxtBuf$(1, 1) = "[MAIN]"
    LastMod = 0                         'No modules yet
    PageTtl$ = PageTitle$
    FlName$ = FileName$

    InitSourceHeader

    TabCon$(Inf.TabConLine) = "Source Listing "
    Inf.TabConLine = Inf.TabConLine + 1

END SUB

FUNCTION ObjectNumber (BYVAL Start, Test$) STATIC

    SHARED ObjInf AS ObjectInfo
    SHARED Symbol$()
    SHARED SmbInfo() AS SymbolInfo
    SHARED ObjBuffer() AS ObjectInfo
    SHARED MaxObjs
    SHARED LastSymbol
    SHARED CurrentSymbol
    
    ObjectNumber = False

    T$ = UCASE$(LTRIM$(RTRIM$(Test$)))

    NumEls = LastSymbol - Start + 1
    CurrentSymbol = FindExact%(VARPTR(Symbol$(Start)), NumEls, T$)
    
    IF CurrentSymbol > -1 THEN
       CurrentSymbol = CurrentSymbol + Start
       ObjectNumber = SmbInfo(CurrentSymbol).RecNo

       RecNo = SmbInfo(CurrentSymbol).RecNo

       IF RecNo <= MaxObjs THEN
          ObjInf = ObjBuffer(RecNo)
       ELSE
          FGetRT Inf.ObjHandle, ObjInf, CLNG(RecNo), LEN(ObjInf)
       END IF
    ELSE
       CurrentSymbol = 0
    END IF

END FUNCTION

SUB OpenFile (FileName$) STATIC
    SHARED Handle, IncNos()

    Handle = Handle + 1
    IncNos(Handle) = Inf.IncNo

    IF NOT Exist%(FileName$) THEN
       ErrMsg "Cannot find " + FileName$ + "!" + AbortMsg$
       Inf.ErrCode = -53
       EXIT SUB
    END IF

    Test$ = " "
    OPEN FileName$ FOR BINARY AS #Handle
    GET #Handle, , Test$
    CLOSE #Handle


    IF ASCII(Test$) = 252 THEN
       ErrMsg "Cannot process QuickBASIC - Fast Load and Save files!" + AbortMsg$
       Inf.ErrCode = -54
       EXIT SUB
    END IF

    OPEN FileName$ FOR INPUT AS #Handle

END SUB

SUB OpenSaveText (FileName$, Mode) STATIC

    IF LEN(PrevName$) THEN
       IF LOF(99) = 0 THEN
          CLOSE #99
          KILL PrevName$
       ELSE
          CLOSE #99
       END IF
    END IF

    IF LEN(FileName$) THEN
       IF Mode = 0 THEN
          OPEN FileName$ FOR OUTPUT AS #99
       ELSE
          OPEN FileName$ FOR APPEND AS #99
       END IF
    END IF

    PrevName$ = FileName$

END SUB

SUB Parse STATIC

   SHARED Text$
   SHARED KWord() AS KeyWords
   SHARED NumKeyWords
   SHARED Words() AS WordInfo
   SHARED Delim$
   SHARED LastWord
  
   Words(0).Word = ""
   IF LEN(Skip$) = 0 THEN Skip$ = " " + CHR$(9)
   StPos = 1
   EnPos = 1
   LastWord = 1
   Length = LEN(Text$)
   WasBWord = False

   DO UNTIL StPos > Length OR LastWord > Inf.UBWords

      StPos = NotInstr%(StPos, Text$, Skip$)
      IF StPos = 0 THEN EXIT DO

      EnPos = InstrTbl(StPos, Text$, Delim$) - 1
      IF EnPos = -1 THEN EnPos = Length
      IF EnPos < StPos THEN EnPos = StPos

      WordLen = EnPos - StPos + 1
      IF WordLen >= 40 THEN WordLen = 39

      Words(LastWord).Word = MID$(Text$, StPos, WordLen)
      Words(LastWord).Length = WordLen

      Temp = ASC(Words(LastWord).Word)
      IF Temp >= 48 AND Temp <= 57 THEN
         RSET Words(LastWord).Word = RTRIM$(Words(LastWord).Word)
         Words(LastWord).Length = LEN(Words(LastWord).Length)
      END IF
      

      IF Temp = 35 THEN
         Words(LastWord + 1).Word = MID$(Words(LastWord).Word, 2)
         Words(LastWord + 1).Length = Words(LastWord).Length - 1
         Words(LastWord + 1).BasicWord = False
         IF ASC(Words(LastWord + 1).Word) >= 48 AND ASC(Words(LastWord + 1).Word) <= 57 THEN
            RSET Words(LastWord + 1).Word = RTRIM$(Words(LastWord + 1).Word)
            Words(LastWord + 1).Length = LEN(Words(LastWord + 1).Length)
         END IF

         Words(LastWord).Word = "#"
         Words(LastWord).Length = 1
         SkipFlag = -1
      ELSE
         SkipFlag = 0
      END IF

     
      IF WordLen > 17 THEN
         BWord = False
      ELSE
         KWord(0).KName = UCASE$(Words(LastWord).Word)
         BWord = XRFindKey%(KWord(0), NumKeyWords)

         IF Temp = 47 AND LastWord > 1 THEN
            IF INSTR(UCASE$(Words(LastWord - 1).Word), "COMMON ") = 1 THEN
               BWord = False
            END IF
         END IF
      END IF
      Words(LastWord).BasicWord = BWord

      IF BWord > 0 AND WasBWord THEN
         
         IF PLength + WordLen + 1 > 17 THEN
            Sentence = False
         ELSE
            KWord(0).KName = UCASE$(Words(LastWord - 1).Word)
            MID$(KWord(0).KName, PLength + 2) = UCASE$(Words(LastWord).Word)
            Sentence = XRFindKey%(KWord(0), NumKeyWords)
         END IF

         IF Sentence THEN
            Words(LastWord).Word = ""
            IF SkipFlag THEN
               Words(LastWord).Word = Words(LastWord + 1).Word
               Words(LastWord).Length = Words(LastWord + 1).Length
               Words(LastWord).BasicWord = 0
            END IF
            LastWord = LastWord - 1
            Words(LastWord).Word = KWord(0).KName
            Words(LastWord).Length = PLength + WordLen + 1
            WordLen = Words(LastWord).Length
            Words(LastWord).BasicWord = Sentence
         END IF
      END IF

      IF BWord THEN
         IF INSTR(KWord(0).KName, "DATA ") = 1 THEN
            LastWord = LastWord + 1
            EXIT DO
         END IF
      ELSEIF WasBWord THEN
         IF INSTR(UCASE$(Words(LastWord - 1).Word), "DECLARE ") = 1 THEN
            LastWord = LastWord + 1
            EXIT DO
         END IF
      END IF

      WasBWord = BWord > False AND ASC(Words(LastWord).Word) <> Colon

      LastWord = LastWord + 1
      IF SkipFlag THEN LastWord = LastWord + 1

      IF LastWord >= Inf.UBWords THEN
         Temp$ = "Too many tokens on line:" + STR$(Inf.LineNo) + "!" + CHR$(20) + CHR$(20)
         Temp$ = Temp$ + "Use the `Options' dialog box on the `Report' menu to" + CHR$(20)
         Temp$ = Temp$ + "increase the maximum number of objects/logical line," + CHR$(20)
         Temp$ = Temp$ + "and then restart the reporting process."
         ErrMsg Temp$

         Inf.ErrCode = -9
         EXIT DO
      END IF

      StPos = EnPos + 1
      PLength = WordLen

   LOOP

   LastWord = LastWord - 1

   IF LastWord < Inf.UBWords THEN
      Words(LastWord + 1).Length = 0
   END IF

END SUB

SUB PrintBufferedLine (BYVAL Ln, TxtBuf$(), Header$(), SubTitle$) STATIC

    SHARED Make$()                       'Names of associated modules
    SHARED TabCon$()                     'Table of contents text array

    IF LEN(FF$) = 0 THEN
       FF$ = CHR$(12)
       PgNo$ = SPACE$(5)                 'Buffer for page #ers in
    END IF                               '  table of contents


    IF LEN(TxtBuf$(Ln, 1)) THEN
       MID$(Header$(2), 1) = SPACE$(Inf.PrnWidth - Inf.PrnLeftMarg - 19)
       MID$(Header$(2), 1) = TxtBuf$(Ln, 1) + "| " + SubTitle$
    END IF


    '~~~~~ Look for a form feed
    Temp = INSTR(TxtBuf$(Ln, 0), FF$)
    IF Temp THEN
       TxtBuf$(Ln, 0) = LEFT$(TxtBuf$(Ln, 0), Temp - 1) + MID$(TxtBuf$(Ln, 0), Temp + 1)
    END IF

    PrintLine TxtBuf$(Ln, 0), Header$()
    IF Inf.ErrCode < -1 THEN EXIT SUB

    IF Temp THEN
       PrintFootNote ""
       IF Inf.ErrCode < -1 THEN EXIT SUB
       PrintHeader Header$()
    END IF

    '~~~~~ Look for a page number for the Table of Contents
    IF LEN(TxtBuf$(Ln, 1)) THEN
       TabCon$(Inf.TabConLine) = MakeDottedLine$(78)

       IF INSTR(TxtBuf$(Ln, 1), ": [MAIN]") THEN
          MID$(TabCon$(Inf.TabConLine), 1) = SPACE$(4) + TxtBuf$(Ln, 1) + " "
       ELSE
          Temp$ = MID$(TxtBuf$(Ln, 1), INSTR(TxtBuf$(Ln, 1), ":") + 2)
          MID$(TabCon$(Inf.TabConLine), 1) = SPACE$(8) + Temp$ + " "
       END IF
       RSET PgNo$ = STR$(Inf.Page)
       MID$(TabCon$(Inf.TabConLine), 74) = PgNo$
       Inf.TabConLine = Inf.TabConLine + 1
    END IF

END SUB

SUB PrintSourceLine (ProcInf AS ObjectInfo)

    SHARED PushedLines, SourceText$, LastMod
    SHARED Title$, SubTitle$, VertLine$, LNum$
    SHARED TxtBuf$(), Header$()
    SHARED TabCon$()                    'Table of contents text array
    SHARED Make$()


    IF PushedLines >= 63 THEN

       FOR N = PushedLines TO PushedLines - 2 STEP -1

          Temp = INSTR(UCASE$(TxtBuf$(N, 0)), "$SUBTITLE:")
          IF Temp THEN
             Temp = INSTR(Temp, TxtBuf$(N, 0), "'") + 1
             IF Temp > 1 THEN
                Inc2 = INSTR(Temp, TxtBuf$(N, 0), "'")
                IF Inc2 = 0 THEN Inc2 = LEN(TxtBuf$(N, 0)) + 1
                SubTitle$ = MID$(TxtBuf$(N, 0), Temp, Inc2 - Temp)
                Temp = INSTR(Header$(2), "|")
                MID$(Header$(2), Temp + 1) = SPACE$(Inf.PrnWidth - Inf.PrnLeftMarg - 19 - Temp)
                MID$(Header$(2), Temp + 2) = SubTitle$
             END IF
          END IF

          Temp = INSTR(UCASE$(TxtBuf$(N, 0)), "$TITLE:")
          IF Temp THEN
             Temp = INSTR(Temp, TxtBuf$(N, 0), "'") + 1
             IF Temp > 1 THEN
                Inc2 = INSTR(Temp, TxtBuf$(N, 0), "'")
                IF Inc2 = 0 THEN Inc2 = LEN(TxtBuf$(N, 0)) + 1
                Title$ = MID$(TxtBuf$(N, 0), Temp, Inc2 - Temp)
                Temp = INSTR(Header$(1), "Page")
                MID$(Header$(1), 1) = SPACE$(Inf.PrnWidth - Inf.PrnLeftMarg - Temp)
                MID$(Header$(1), 1) = Title$
             END IF
          END IF

       NEXT

    END IF


    IF PushedLines = 63 THEN
       PrintHeader Header$()

    '~~~~~ If 64 lines accumulated, print the 64th buffer line
    ELSEIF PushedLines = 64 THEN

       PrintBufferedLine PushedLines, TxtBuf$(), Header$(), SubTitle$
       IF Inf.ErrCode < -1 THEN EXIT SUB
    END IF

    '~~~~~ Slide buffer down
    FOR N = 64 TO 2 STEP -1
       TxtBuf$(N, 0) = TxtBuf$(N - 1, 0)
       TxtBuf$(N, 1) = TxtBuf$(N - 1, 1)
    NEXT
    PushedLines = MinInt(PushedLines + 1, 64)

    '~~~~~ Put the line number in the text
    IF Inf.LineNumbers THEN
       SourceLine = Inf.LineNo
       IF Inf.LineNumbers = 2 THEN
          IF Inf.IncNo = 0 OR INSTR(UCASE$(SourceText$), "$INCLUDE") THEN
             SourceLine = SourceLine - ProcInf.OnLineNo + 1
          END IF
       END IF

       RSET LNum$ = LTRIM$(STR$(SourceLine))

       TxtBuf$(N, 0) = LNum$ + VertLine$ + SourceText$

    ELSE
       TxtBuf$(N, 0) = SourceText$
    END IF

    IF Inf.IncNo THEN
       IF INSTR(UCASE$(SourceText$), "$INCLUDE") = 0 THEN
          TxtBuf$(N, 0) = "$  " + TxtBuf$(N, 0)
       END IF
    END IF

    TxtBuf$(N, 1) = ""

    '~~~~~ Start a new page if this is a new module
    IF Inf.ModNo <> LastMod THEN
       LastMod = Inf.ModNo
       IF Inf.ModNo > 1 THEN
          TxtBuf$(N + 1, 0) = CHR$(12) + TxtBuf$(N + 1, 0)
          TxtBuf$(N + 1, 1) = NoPath$(Make$(Inf.ModNo)) + ": [MAIN]"
       ELSE
          TxtBuf$(N, 1) = NoPath$(Make$(Inf.ModNo)) + ": [MAIN]"
       END IF
    END IF

END SUB

SUB ReadLine STATIC

    SHARED Text$
    SHARED Handle, LineWas
    SHARED SourceText$
    SHARED PrintSource
    SHARED FlushListBuff
    SHARED SaveText, TxtExtn$
    SHARED Incl$()
    SHARED Path$, PrnLine$
    SHARED NewInclude
          
    IF LEN(PrnLine$) = 0 THEN
       PrnLine$ = SPACE$(78)
       IF Inf.LineNumbers THEN
          IF Inf.GraphChars THEN
             LNum$ = SPACE$(5) + "³"
          ELSE
             LNum$ = SPACE$(5) + "|"
          END IF
       ELSE
          LNum$ = ""
       END IF
       SLNum$ = SPACE$(5)
       TabChar$ = CHR$(9)
       Blanks$ = " " + TabChar$
       Quote$ = CHR$(34)
       CrLf$ = CHR$(13) + CHR$(10)
       Test4REM$ = "REM"
       Test4Apos$ = "'"
    END IF

    Temp = 0
    SourceText$ = ""
    DO
       Temp = Temp + 1
       LINE INPUT #Handle, Text$                'Read a line of text
       SourceText$ = SourceText$ + RTRIM$(Text$)

       IF Inf.DispText THEN
          ScrollU 3, 2, 23, 79, 1, 0
          IF Inf.IncNo THEN
             Clr = Inf.ScrClr + 8
          ELSEIF Inf.ModNo <> PrevMod THEN
             Clr = 112
          ELSE
             Clr = Inf.ScrClr + 7
          END IF

          LSET PrnLine$ = SourceText$
          QPrintRC PrnLine$, 23, 2, Clr

          PrevMod = Inf.ModNo
       END IF

       US = INSTR(Text$, "_")
       GetMore = 0
       IF LEN(Text$) > 0 AND US > 0 THEN

          IF FRE("") < 8192 THEN FlushPrnBuffer

          R = INSTR(Text$, Test4REM$ + " ")
          IF R = 0 OR R > US THEN
             R = INSTR(Text$, Test4Apos$)
          END IF
          IF R = 0 OR R > US THEN
             R = InCount%(LEFT$(SourceText$, US), Quote$)
             IF R MOD 2 = 0 THEN
                SourceText$ = SourceText$ + CrLf$ + LNum$
                GetMore = -1
             END IF
          END IF
       END IF
    LOOP WHILE GetMore

    IF INSTR(SourceText$, TabChar$) THEN
       SourceText$ = ExpandTab$(SourceText$, Inf.TabSpaces)
    END IF

    IF Temp > 1 THEN

       IF FRE("") < 8192 THEN FlushPrnBuffer
       Text$ = SourceText$

       US = 0
       DO
          US = INSTR(US + 1, Text$, "_")
          CR = INSTR(Text$, CrLf$)
          IF US > 0 AND CR > US THEN
             Temp = (Inf.LineNumbers > 0) * -6 + 2
             Text$ = LEFT$(Text$, US) + LTRIM$(MID$(Text$, CR + Temp))
          ELSE
             EXIT DO
          END IF
       LOOP
    END IF

    IF StartInc THEN
       Inf.LineNo = 1
       StartInc = False
    ELSE
       Inf.LineNo = Inf.LineNo + 1
    END IF
   
    FullLen = LEN(Text$)

    IF FullLen THEN                             'If its not a Nul string,

       X = 1
       GOSUB GetLength
       N = INSTR(Text$, Quote$)                 'Remove quoted strings
       DO WHILE N > 0 AND N <= Length

          X = INSTR(N + 1, Text$, Quote$)

          IF X = False THEN X = FullLen
         
          IF Length < X THEN
             GOSUB GetLength
          END IF
            
          IF SaveText AND 1 THEN
             RSET SLNum$ = LTRIM$(STR$(Inf.LineNo))
             PRINT #99, SLNum$; ": ";
             PRINT #99, Quote$; MID$(Text$, N + 1, X - N - 1); Quote$
          END IF

          Text$ = LEFT$(Text$, N) + " " + MID$(Text$, X + 1)
          Length = Length - (X - N) + 1
         
          N = INSTR(N + 1, Text$, Quote$)
       LOOP

       IF (SaveText AND 2) > 0 AND Cmnt > 0 THEN
          RSET SLNum$ = LTRIM$(STR$(Inf.LineNo))
          PRINT #99, SLNum$; ": ";
          PRINT #99, MID$(Text$, Length - Cmnt + 1)
       END IF
      

       IF INSTR(Length + 1, Text$, "$") THEN
          '~~~~~ Is "$" the first non blank character in comment?           $
          IF ASCII(MID$(Text$, NotInstr%(Length + 1, Text$, Blanks$), 1)) = 36 THEN

             Temp$ = UCASE$(Text$)

             IF INSTR(Length, Temp$, "$DYNAMIC") THEN Inf.Dynamic = True
             IF INSTR(Length, Temp$, "$STATIC") THEN Inf.Dynamic = False


             '~~~~~ Look for Source listing meta commands
             Temp = INSTR(Length, Temp$, "$LIST")
             IF Temp THEN
                IF PrintSource AND (MID$(Text$, Temp + 5, 1) = "-" OR MID$(Text$, Temp + 6, 3) = "OFF") THEN
                   PrintSource = False
                   FlushListBuff = True
                ELSEIF PrintSource = 0 AND FlushListBuff THEN
                   PrintSource = True
                END IF
             END IF

             IF PrintSource OR FlushListBuff THEN
                Temp = INSTR(Length, Temp$, "$LINESIZE:")
                IF Temp THEN
                   Inf.PrnWidth = QPValI(MID$(Temp$, Temp + 10))
                   InitSourceHeader
                END IF

                Temp = INSTR(Length, Temp$, "$PAGE")
                IF Temp THEN
                   IF MID$(Temp$, Temp + 5, 1) = " " OR Temp + 4 = FullLen THEN
                      SourceText$ = SourceText$ + CHR$(12)
                   END IF
                END IF

                Temp = INSTR(Length, Temp$, "$PAGEIF")
                IF Temp THEN
                   Temp = INSTR(Temp, Temp$, ":") + 1
                   Inf.PageIf = QPValI(MID$(Temp$, Temp))
                END IF

                Temp = INSTR(Length, Temp$, "$PAGESIZE")
                IF Temp THEN
                   Temp = INSTR(Temp, Temp$, ":") + 1
                   Inf.PrnLength = QPValI(MID$(Temp$, Temp))
                END IF

                Temp = INSTR(Length, Temp$, "$SKIP")
                IF Temp THEN
                   Temp = INSTR(Temp, Temp$, ":") + 1
                   Temp = QPValI(MID$(Temp$, Temp))
                   FOR N = 1 TO Temp
                      SourceText$ = SourceText$ + " " + CrLf$
                   NEXT
                END IF
             END IF


             '~~~~~ Look for INCLUDE files
             Inc = INSTR(Temp$, "$INCLUDE")
             IF Inc > Length THEN

                IF INSTR(Length + 1, Temp$, "'") > Inc THEN
                   Inc = INSTR(Inc, Temp$, "'") + 1
                   Inc2 = INSTR(Inc, Temp$, "'")
                   IF Inc2 > Inc THEN
                      IncName$ = MID$(Temp$, Inc, Inc2 - Inc)

                      IF LEN(FileExtension$(IncName$)) = 0 THEN
                         IncName$ = IncName$ + ".BAS"
                      END IF

                      IF QPLen%(Path$) THEN     'Add path to include name
                         IF InstrTbl(1, IncName$, "\:") = False THEN
                            IncName$ = Path$ + IncName$
                         ELSEIF INSTR(IncName$, ":") = False THEN
                            IncName$ = LEFT$(Path$, INSTR(Path$, ":")) + IncName$
                         END IF
                      END IF

                      IF NOT Exist%(IncName$) THEN
                         IncName$ = NoPath$(IncName$)
                                                'Check environment path
                         SrchPath IncName$, ENVIRON$("INCLUDE"), NotFound
                         IF NotFound THEN
                            ErrMsg "Include file " + IncName$ + " not found!" + AbortMsg$
                            Inf.ErrCode = -53
                            EXIT SUB
                         END IF
                      END IF
            
                      IF Inf.IncNo = 0 THEN LineWas = Inf.LineNo
                      StartInc = True
             

                      Inf.IncNo = Inf.Incs
                      Find VARPTR(Incl$(1)), Inf.IncNo, IncName$
                      IF Inf.IncNo = -1 THEN
                         Inf.Incs = Inf.Incs + 1
                         Inf.IncNo = Inf.Incs
                         NewInclude = True
                      ELSE
                         Inf.IncNo = Inf.IncNo + 1
                         NewInclude = False
                      END IF
                      Incl$(Inf.IncNo) = IncName$

                      ShowFileName IncName$      'Display the name of INCLUDE

                      OpenFile IncName$
                      IF Inf.ErrCode < -1 THEN EXIT SUB

                      IF SaveText THEN
                         OpenSaveText BaseName$(IncName$) + TxtExtn$, 0
                      END IF
               
                      IncName$ = ""
                   END IF
                END IF
             END IF
          END IF
       END IF
    ELSE
       Length = 0
    END IF

    
    Text$ = LEFT$(Text$, Length)                'Trim remark

    EXIT SUB


'~~~~~ Get Length without comments
GetLength:

    Length = INSTR(X, Text$, Test4Apos$)
    Temp = X
    DO
       RM = INSTR(Temp, Text$, Test4REM$)
       IF RM THEN
          SELECT CASE ASCII(MID$(Text$, RM + 3, 1))
             CASE 48 TO 57, 65 TO 90, 97 TO 122
                Temp = RM + 1
             CASE ELSE
                IF RM > 1 THEN
                   SELECT CASE ASCII(MID$(Text$, RM - 1, 1))
                      CASE 9, 32, 58, 95
                      CASE ELSE
                         Temp = RM + 1
                   END SELECT
                END IF
          END SELECT
       END IF
    LOOP WHILE RM > 0 AND Temp > RM
    
    IF Length OR RM THEN
       IF Length > 0 AND RM > 0 THEN
          IF Length < RM THEN
             Cmnt = 1
          ELSE
             Length = RM + 3
             Cmnt = 4
          END IF
       ELSEIF RM THEN
          Length = RM + 3
          Cmnt = 4
       ELSE
          Cmnt = 1
       END IF
    ELSE
       Length = FullLen
       Cmnt = 0
    END IF

RETURN

END SUB

SUB ReadMake (FileName$, MakeExtn$)

    SHARED Make$(), Path$

    '~~~~~ Parse out file's path name
    Path$ = ParsPath$(FileName$)
    Inf.Modules = 0

    '~~~~~ Load up the .MAK file if there is one.
    IF INSTR(FileName$, MakeExtn$) THEN
       IF NOT Exist%(FileName$) THEN
          ErrMsg FileName$ + " not found!" + AbortMsg$
          Inf.ErrCode = -53
          EXIT SUB
       END IF

       Lns = LineCount%(FileName$, SPACE$(4096))
       REDIM Make$(Lns)

       OPEN FileName$ FOR INPUT AS #1

       DO UNTIL EOF(1)                          'Read the module names
          Inf.Modules = Inf.Modules + 1
          LINE INPUT #1, Make$(Inf.Modules)
          IF QPLen%(Make$(Inf.Modules)) THEN
                                                'Add a path name if needed
             IF InstrTbl%(1, Make$(Inf.Modules), ":\") = False THEN
                Make$(Inf.Modules) = Path$ + Make$(Inf.Modules)
             ELSEIF INSTR(Make$(Inf.Modules), ":") = False THEN
                Make$(Inf.Modules) = LEFT$(Path$, INSTR(Path$, ":")) + Make$(Inf.Modules)
             END IF

             IF LEN(FileExtension$(Make$(Inf.Modules))) = 0 THEN
                Make$(Inf.Modules) = Make$(Inf.Modules) + ".BAS"
             END IF
          ELSE
             Inf.Modules = Inf.Modules - 1
          END IF
       LOOP

       CLOSE #1

    ELSE                                        'One module

       Inf.Modules = 1
       REDIM Make$(1)
       Make$(1) = FileName$

    END IF

    FOR N = 1 TO Inf.Modules
       IF NOT Exist%(Make$(N)) THEN
          ErrMsg Make$(N) + " not found!" + AbortMsg$
          Inf.ErrCode = -53
          EXIT FOR
       END IF
    NEXT

END SUB

SUB SaveObject (BYVAL SymbolNo) STATIC

   SHARED ObjInf AS ObjectInfo          'Object information
   SHARED NewObject

   SHARED ObjBuffer() AS ObjectInfo
   SHARED MaxObjs

   SHARED Symbol$()
   SHARED SmbInfo() AS SymbolInfo
   SHARED LastSymbol
   SHARED CurrentSymbol

   IF NewObject THEN
      CurrentSymbol = SmbInfo(0).RecNo

      IF CurrentSymbol > Inf.UBSymbols THEN
         Temp$ = "Too many objects in this module!" + CHR$(20) + CHR$(20)
         Temp$ = Temp$ + "Use the `Options' dialog box on the `Report' menu to" + CHR$(20)
         Temp$ = Temp$ + "increase the maximum number of objects/module, and" + CHR$(20)
         Temp$ = Temp$ + "then restart the reporting process."
         ErrMsg Temp$

         Inf.ErrCode = -9
         CurrentSymbol = 0
         EXIT SUB
      END IF
      IF SmbInfo(CurrentSymbol).RecNo THEN
         SmbInfo(0).RecNo = SmbInfo(CurrentSymbol).RecNo
      ELSE
         SmbInfo(0).RecNo = CurrentSymbol + 1
      END IF
      LastSymbol = MaxInt(LastSymbol, CurrentSymbol)

      Symbol$(CurrentSymbol) = UCASE$(LTRIM$(RTRIM$(ObjInf.OName)))
      SmbInfo(CurrentSymbol).RecNo = SymbolNo


      Temp = ObjInf.Share \ 256
      Temp$ = RTRIM$(ObjInf.OName)
      IF Temp THEN
         Temp$ = Temp$ + CHR$(Temp)
      END IF

      Temp = ObjInf.OType
      SELECT CASE Temp
         CASE NearArray, FarArray, NearTypeArray, FarTypeArray
            Temp$ = Temp$ + "()"
         CASE ELSE
      END SELECT

      SELECT CASE Temp
         CASE SimpleType, NearTypeArray, FarTypeArray
            Temp$ = Temp$ + "."
         CASE ELSE
      END SELECT
      
      Inf.MaxNameLen = MaxInt(Inf.MaxNameLen, LEN(Temp$))
      LSET ObjInf.OName = Temp$

   END IF


   IF SymbolNo <= MaxObjs THEN
      ObjBuffer(SymbolNo) = ObjInf
   ELSE
      FPutRT Inf.ObjHandle, ObjInf, CLNG(SymbolNo), LEN(ObjInf)
   END IF

   IF DosError THEN FatalError

   Global = ObjInf.OType <= SubProc OR GetNybble(ObjInf.Flag, Commn) <> 0
   SmbInfo(CurrentSymbol).Global = Global

END SUB

'~~~~~ Search an environment path for a file
SUB SrchPath (FileName$, Paths$, NotFound) STATIC

    NotFound = True                             'Guilty until proven otherwise
    Path$ = ""                                  'No Path yet
    PP = 1                                      'Present position

    DO UNTIL Exist(Path$ + FileName$)           'Loop until we find the file
       IF PP > QPLen%(Paths$) THEN EXIT SUB     'Bail out if no more paths

       PCP = INSTR(PP, Paths$, ";")             'Find Semicolon position
       IF PCP = 0 THEN PCP = QPLen%(Paths$) + 1 'Last path
                                                'Parse out the path
       Path$ = LTRIM$(RTRIM$(MID$(Paths$, PP, PCP - PP)))
                                                'Ensure there's a "\" at end
       IF RIGHT$(Path$, 1) <> "\" THEN Path$ = Path$ + "\"
                                                'Bump position for next path
       PP = PCP + 1
    LOOP

    FileName$ = Path$ + FileName$               'Add the path to the file name
    NotFound = False

END SUB

