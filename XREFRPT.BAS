'$SubTitle:'Reporting Routines Module'

DEFINT A-Z

'$INCLUDE: 'XRefCnst.bi'
'$INCLUDE: 'XRefType.bi'
'$INCLUDE: 'XRefComn.bi'

DECLARE FUNCTION ASCII% (Char$)
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION ErrorMsg$ (ErrorNum)
DECLARE FUNCTION FileExtension$ (FileName$)
DECLARE FUNCTION GetNybble% (BYVAL IntArray%, BYVAL Psn%)
DECLARE FUNCTION MakeDottedLine$ (LineLength%)
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION QPValI% (Number$)
DECLARE FUNCTION Power2& (Power%)
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION NoPath$ (FileName$)
DECLARE FUNCTION XRSearch% (SEG Element%, BYVAL NumEls%, BYVAL Number2Find%)

DECLARE SUB ErrMsg (Text$)
DECLARE SUB FatalError ()
DECLARE SUB FGetT (Handle%, Dest AS ANY, NumBytes%)
DECLARE SUB FSeek (Handle%, Position&)
DECLARE SUB GetInProcInfo (BYVAL ProcNo%, ProcName$, ProcLineNo%)
DECLARE SUB GetObjRec (BYVAL Index%, RecNo%, ObjInf AS ANY)
DECLARE SUB GetRefRec (BYVAL RecNo&, Ref AS ANY)
DECLARE SUB KeySort (SEG Element AS ANY, ElSize%, NumEls%, SEG TableEl AS ANY, NumKeys%)
DECLARE SUB MQPrint (Text$, Clr)
DECLARE SUB PrintFootNote (Text$)
DECLARE SUB PrintHeader (Header$())
DECLARE SUB PrintLine (PrnLine$, Header$())
DECLARE SUB ProcTree (BYVAL ProcNo%)
DECLARE SUB QPrintRC (X$, Row%, Col%, Clr%)
DECLARE SUB SetNybble (IntArray%, BYVAL Value%, BYVAL Psn%)
DECLARE SUB SortT2 (SEG Element AS ANY, NumEls, Dir, ElSize, MemberOffset, MemberSize)
DECLARE SUB UpdateTabCon (Text$)

CONST NameNdx = 1
CONST TypeNameNdx = 2

TYPE SubSort
   TName AS STRING * 30
   TIndex AS INTEGER
END TYPE

TYPE TreeSort
   ProcNo AS INTEGER
   IndexNo AS INTEGER
END TYPE

TYPE TypeNameSort
   ObjType AS INTEGER
   ObjName AS STRING * 28
   ObjNo AS INTEGER
END TYPE

TYPE ProcInfo
   ObjNo AS INTEGER
   OName AS STRING * 40
   DefModInc AS INTEGER
   OnLineNo AS INTEGER
END TYPE

REDIM Proc(0) AS ProcInfo

SUB EraseBasProcTable
    SHARED Proc() AS ProcInfo

    ERASE Proc
END SUB

SUB GetInProcInfo (BYVAL ProcNo, ProcName$, ProcLineNo) STATIC
    SHARED Proc() AS ProcInfo
  
    ProcName$ = ""
    ProcLineNo = 0

    Temp = UBOUND(Proc)
    FOR N = 0 TO Temp
       IF Proc(N).ObjNo = ProcNo THEN EXIT FOR
    NEXT

    IF N <= Temp THEN
       ProcName$ = RTRIM$(Proc(N).OName)
       ProcLineNo = Proc(N).OnLineNo
    ELSE
       ProcName$ = ""
       ProcLineNo = 0
    END IF

END SUB

SUB GetObjRec (BYVAL Index, RecNo, ObjInf AS ObjectInfo) STATIC

   IF Index THEN
      IF Index = 1 THEN
         Offset& = (RecNo - 1) * 4& + Inf.NameData
      ELSE
         Offset& = (RecNo - 1) * 4& + Inf.TypeNameData
      END IF
      FSeek Inf.DBHandle, Offset&
      FGetT Inf.DBHandle, Offset&, 4
      RecNo = Offset&
   ELSE
      Offset& = RecNo
   END IF

   Offset& = (Offset& - 1) * LEN(ObjInf) + Inf.ObjData
   FSeek Inf.DBHandle, Offset&

   FGetT Inf.DBHandle, ObjInf, LEN(ObjInf)

   IF DosError THEN FatalError

END SUB

SUB GetRefRec (BYVAL RecNo&, Ref AS ReferenceInfo) STATIC

   FSeek Inf.DBHandle, (RecNo& - 1) * LEN(Ref) + Inf.RefData

   FGetT Inf.DBHandle, Ref, LEN(Ref)

   IF DosError THEN FatalError

END SUB

'----- Create an array to hold all BASIC procedure names and line numbers
'
SUB InitBasProcTable
    SHARED ObjInf AS ObjectInfo
    SHARED Proc() AS ProcInfo
    SHARED MaxProcLen

    BasProcs = 1
    FOR N = 2 TO Inf.Objects
       GetObjRec TypeNameNdx, (N), ObjInf
       IF ObjInf.DefModInc > 0 THEN
          IF ObjInf.OType > SubProc THEN EXIT FOR
          IF ObjInf.OType >= DefFnProc AND ObjInf.OnLineNo THEN
             BasProcs = BasProcs + 1
          END IF
       END IF
       IF Inf.ErrCode < -1 THEN EXIT SUB
    NEXT

    REDIM Proc(BasProcs) AS ProcInfo
    Proc(0).OName = "EXTERNAL"

    GetObjRec 0, 1, ObjInf
    Proc(1).ObjNo = 1
    Proc(1).OName = ObjInf.OName
    Proc(1).OnLineNo = ObjInf.OnLineNo

    MaxProcLen = 0
    BasProcs = 1
    FOR N = 2 TO Inf.Objects
       Rec = N
       GetObjRec TypeNameNdx, Rec, ObjInf
       IF ObjInf.DefModInc > 0 THEN
          IF ObjInf.OType > SubProc THEN EXIT FOR
          IF ObjInf.OType >= DefFnProc AND ObjInf.OnLineNo THEN
             BasProcs = BasProcs + 1
             Proc(BasProcs).ObjNo = Rec
             Proc(BasProcs).OName = ObjInf.OName
             MaxProcLen = MaxInt(MaxProcLen, LEN(RTRIM$(ObjInf.OName)))
             Proc(BasProcs).DefModInc = ObjInf.DefModInc
             Proc(BasProcs).OnLineNo = ObjInf.OnLineNo
          END IF
       END IF
       IF Inf.ErrCode < -1 THEN EXIT SUB
    NEXT

END SUB

SUB ListExternal2Range (LowRange, HiRange, ProcName$, ObjTypes&, ErrCode)
    SHARED ObjInf AS ObjectInfo
    SHARED Ref AS ReferenceInfo
    SHARED Make$(), Incl$()
    SHARED Proc() AS ProcInfo

    '~~~~~ Set up the Report Header
    REDIM Header$(4)
    LineLen = 80 - Inf.PrnLeftMarg

    Header$(1) = SPACE$(LineLen)
    LSET Header$(1) = "LIST OF OBJECTS EXTERNAL TO A RANGE"

    Header$(2) = SPACE$(LineLen)
    LSET Header$(2) = "Within"
    MID$(Header$(2), LineLen - 18) = Inf.DateTime

    Header$(3) = "Between Lines" + STR$(LowRange) + " and" + STR$(HiRange) + " inclusive."

    IF Inf.GraphChars THEN
       Header$(4) = STRING$(LineLen, "Ä")
    ELSE
       Header$(4) = STRING$(LineLen, "-")
    END IF


    '~~~~~ Get Module number and adjust range
    ErrCode = 0

    ProcName$ = UCASE$(ProcName$)

    IF Inf.LineNumbers = 2 THEN                 'If Line numbers relative to
                                                '  procedures?
       '~~~~~ Find the procedure's Object Number
       FOR N = 1 TO UBOUND(Proc)
          IF UCASE$(RTRIM$(Proc(N).OName)) = ProcName$ THEN EXIT FOR
       NEXT
       IF N > UBOUND(Proc) THEN
          ErrCode = 1                           '  bail out
          EXIT SUB
       ELSE
          '~~~~~ Get module number and adjust range
          GetObjRec 0, Proc(N).ObjNo, ObjInf
          Module = ObjInf.DefModInc MOD 256
          LowRange = LowRange + ObjInf.OnLineNo - 1
          HiRange = HiRange + ObjInf.OnLineNo - 1
       END IF

       '~~~~~ Put info. in header
       MID$(Header$(2), 9) = NoPath$(Make$(Module)) + ": " + ProcName$
    ELSE

       '~~~~~ Search the Make$ array for the module specified
       IF UCASE$(ProcName$) = "MAIN" THEN ProcName$ = Make$(Module)

       IF LEN(FileExtension$(ProcName$)) = 0 THEN
          ProcName$ = ProcName$ + ".BAS"
       END IF

       FOR Module = 1 TO Inf.Modules
          
          P = INSTR(Make$(Module), ProcName$)
          IF P THEN
             IF P = 1 THEN
                EXIT FOR
             ELSEIF INSTR(":\", MID$(Make$(Module), P - 1, 1)) THEN
                EXIT FOR
             END IF
          END IF
       NEXT

       IF Module > Inf.Modules THEN
          ErrCode = 2                           '  bail out
          EXIT SUB
       END IF

       MID$(Header$(2), 9) = NoPath$(RTRIM$(Make$(Module)))
    END IF

    PrintHeader Header$()

    LOCATE , 1
    MQPrint SPACE$(80), Inf.BarClr
    MQPrint "Creating List of Objects External to a Range.", -1


    '~~~~~ Find all objects used within the range
    REDIM I(1 TO Inf.Objects)
    Matches = 0

    FOR N& = 1 TO Inf.ObjRefs
       GetRefRec N&, Ref

       IF INKEY$ = CHR$(27) THEN
          Inf.ErrCode = -3
          EXIT SUB
       END IF

       LineNo = Ref.LineNo
       IF LineNo >= LowRange AND LineNo <= HiRange THEN
          IF ASC(Ref.Module) = Module THEN
             IF XRSearch(I(1), Matches, Ref.ObjNo) = 0 THEN
                P = ASC(Ref.Assign) AND &H7F
                IF ObjTypes& AND Power2&(P + 1) THEN
                   Matches = Matches + 1
                   I(Matches) = Ref.ObjNo
                END IF
             END IF
          END IF
       END IF
    NEXT
    
    

    '~~~~~ Check each unique object
    FOR N = 1 TO Matches
       GetObjRec 0, I(N), ObjInf
       Rec& = ObjInf.FirstRef
       DO WHILE Rec&
          GetRefRec Rec&, Ref

          'IF Ref.ObjNo <> I(N) THEN STOP

          IF INKEY$ = CHR$(27) THEN
             Inf.ErrCode = -3
             EXIT SUB
          END IF

          LineNo = Ref.LineNo
          
          IF LineNo < LowRange OR LineNo > HiRange OR ASC(Ref.Module) <> Module THEN
             GetObjRec 0, Ref.ObjNo, ObjInf

             '~~~~~ Print the Object Info
             Temp$ = RTRIM$(ObjInf.OName)
             P = ObjInf.OType

             IF P = SimpleType OR P = NearTypeArray OR P = FarTypeArray THEN
                Temp$ = LEFT$(Temp$, LEN(Temp$) - 1) + " AS type"
             END IF

             IF LEN(LineBuf$) THEN LineBuf$ = LineBuf$ + ", "

             IF LEN(LineBuf$) + LEN(Temp$) + 2 > LineLen THEN
                PrintLine LineBuf$, Header$()
                IF Inf.ErrCode < -1 THEN EXIT FOR
                LineBuf$ = ""
             END IF

             LineBuf$ = LineBuf$ + Temp$

             EXIT DO
          END IF

          Rec& = Ref.NextRef
       LOOP
    NEXT

    IF LEN(LineBuf$) THEN PrintLine LineBuf$, Header$()

    PrintFootNote Header$(0)

    
END SUB

FUNCTION MakeDottedLine$ (LineLength)

   Work$ = STRING$(LineLength, ".")

   FOR N = 1 TO LineLength STEP 2
      MID$(Work$, N, 1) = " "
   NEXT

   MakeDottedLine$ = Work$

END FUNCTION

SUB PrnKeyWords
    SHARED ObjInf AS ObjectInfo
    SHARED ObjType$()
    SHARED FileName$

    LineLen = Inf.PrnWidth - Inf.PrnLeftMarg
    Cols = (LineLen + 1) \ 27
    
    PrnLine$ = SPACE$(LineLen)

    REDIM Header$(5)
    Header$(0) = "(p) = Unsupported P.D.Q. keyword,  (f) Floating point keyword"
    Header$(1) = PrnLine$
    LSET Header$(1) = "BASIC KEYWORD USAGE TABLE"
  
    Header$(2) = PrnLine$
    MID$(Header$(2), 1) = FileName$
    MID$(Header$(2), LineLen - 18) = Inf.DateTime

    Temp$ = "pf Key Word     # of uses"
    Header$(4) = Temp$
    FOR N = 2 TO Cols
       Header$(4) = Header$(4) + "  " + Temp$
    NEXT

    IF Inf.GraphChars THEN
       Temp$ = "ÄÄ " + STRING$(22, "Ä")
    ELSE
       Temp$ = "-- " + STRING$(22, "-")
    END IF
    Header$(5) = Temp$

    FOR N = 2 TO Cols
       Header$(5) = Header$(5) + "  " + Temp$
    NEXT

    Temp$ = SPACE$(5)

    KeyWords = 0
    FOR N = 2 TO Inf.Objects
       GetObjRec TypeNameNdx, (N), ObjInf
       IF ObjInf.OType <> BASIC THEN EXIT FOR
    NEXT
    KeyWords = N - 2

    
    REDIM Table(KeyWords) AS ObjectInfo

    FOR N = 2 TO KeyWords + 1
       GetObjRec TypeNameNdx, (N), Table(N - 1)
    NEXT

    LinesPerCol = MinInt((KeyWords - 1) \ Cols + 1, Inf.PrnLength - Inf.PageIf - 5)
    ItemsPerPage = LinesPerCol * Cols

    FOR PageOffset = 0 TO KeyWords - 1 STEP ItemsPerPage
                               
       PrintHeader Header$()

       IF PageOffset = 0 THEN UpdateTabCon SPACE$(4) + "BASIC Keywords "

       FOR N = 1 TO LinesPerCol
          Ky$ = INKEY$
          IF ASCII(Ky$) = 27 THEN
             Inf.ErrCode = -3
             PrintFootNote Header$(0)
             EXIT SUB
          END IF

          LSET PrnLine$ = ""
          FOR C = 0 TO Cols - 1
             X = C * LinesPerCol + N + PageOffset
             IF X <= KeyWords THEN
                Offset = C * 27

                RSET Temp$ = LTRIM$(STR$(Table(X).NumRefs))
                MID$(PrnLine$, Offset + 4) = LEFT$(Table(X).OName, 17) + Temp$

                Temp = GetNybble(Table(X).Flag, Atrb)
                IF Temp AND 1 THEN
                   MID$(PrnLine$, Offset + 2) = "f"
                END IF
                IF Temp AND 2 THEN
                   MID$(PrnLine$, Offset + 1) = "p"
                END IF
             END IF
          NEXT
          PrintLine RTRIM$(PrnLine$), Header$()
          IF Inf.ErrCode < -1 THEN EXIT FOR

       NEXT

       IF Inf.ErrCode < -1 THEN EXIT FOR
       PrintFootNote Header$(0)

    NEXT

    ERASE Table, Header$
    PrnLine$ = ""
    Temp$ = ""

END SUB

SUB PrnObjDetail (ObjTypes&)
    SHARED ObjInf AS ObjectInfo
    SHARED Ref AS ReferenceInfo
    SHARED Make$(), Incl$()
    SHARED ObjType$()
    SHARED FileName$

    LineLen = Inf.PrnWidth - Inf.PrnLeftMarg '+ 1

    REDIM Header$(6)
    Header$(1) = SPACE$(LineLen)

    Header$(2) = SPACE$(LineLen)
    MID$(Header$(2), 1) = FileName$
    MID$(Header$(2), LineLen - 18) = Inf.DateTime

    Header$(4) = "Object Name, Defined in Module"
    Header$(5) = "    PROCEDURE NAME: Line#, NextLine#, etc..."
    IF Inf.GraphChars THEN
       Header$(6) = STRING$(LineLen, "Í")
       DivLine$ = STRING$(LineLen, "Ä")
    ELSE
       Header$(6) = STRING$(LineLen, "=")
       DivLine$ = STRING$(LineLen, "-")
    END IF

    Temp$ = SPACE$(LineLen)

    LOCATE , 10
    MQPrint "Cross Reference Table", -1

    N = 1

    FOR ObjTypeNo = 0 TO FarTypeArray

       IF ObjTypes& AND Power2&(ObjTypeNo + 1) THEN

          N = N - 1
          DO
             N = N + 1
             IF N > Inf.Objects THEN EXIT DO
             ProcNo = N
             GetObjRec TypeNameNdx, ProcNo, ObjInf
          LOOP UNTIL ObjInf.OType >= ObjTypeNo AND ObjInf.DefModInc > -1

          DoFooter = 0

          IF ObjInf.OType = ObjTypeNo THEN
             SELECT CASE ObjTypeNo
                CASE BASIC, SubProc, NumConst
                   Header$(0) = ""
                CASE Label
                   Header$(0) = " * " + ObjType$(ObjTypeNo) + " was defined on this line."
                CASE ELSE
                   Header$(0) = " = " + ObjType$(ObjTypeNo) + " was assigned on this line."
             END SELECT
             Header$(1) = SPACE$(LineLen)
             LSET Header$(1) = ObjType$(ObjTypeNo) + " CROSS REFERENCE TABLE"
             PrintHeader Header$()

             LSET Header$(1) = ObjType$(ObjTypeNo) + " CROSS REFERENCE TABLE Continued."

             UpdateTabCon SPACE$(4) + ObjType$(ObjTypeNo) + "s "

             DoFooter = -1
          END IF

          DO UNTIL N > Inf.Objects OR ObjInf.OType <> ObjTypeNo

             Ky$ = INKEY$
             IF ASCII(Ky$) = 27 THEN
                Inf.ErrCode = -3
                EXIT DO
             END IF
                
             Temp1$ = LTRIM$(RTRIM$(ObjInf.OName)) + ", "
             Temp1$ = Temp1$ + RTRIM$(NoPath$(Make$(ObjInf.DefModInc MOD 256)))
             LSET Temp$ = Temp1$

             PrevProc = -1
             PrevModule = ObjInf.DefModInc MOD 256

             Rec& = ObjInf.FirstRef
             IF ObjTypeNo >= DefFnProc AND ObjTypeNo <= SubProc AND Rec& > 0 THEN
                IF PrevModule THEN
                   GetRefRec Rec&, Ref
                   Rec& = Ref.NextRef
                END IF
             END IF

             DO WHILE Rec&
                
                GetRefRec Rec&, Ref
                Rec& = Ref.NextRef

                Module = ASC(Ref.Module)
                IF Module <> PrevModule THEN
                   
                   PrintLine Temp$, Header$()

                   LSET Temp$ = "  " + NoPath$(Make$(Module))
                END IF
      
                IF Ref.InProcNo <> PrevProc OR Module <> PrevModule THEN
                   PrevProc = Ref.InProcNo
                   PrevModule = Module

                   PrintLine Temp$, Header$()

                   LSET Temp$ = ""

                   GetInProcInfo Ref.InProcNo, Temp1$, PrevProcLine

                   MID$(Temp$, 5) = Temp1$ + ":"
                   Ptr = 5 + LEN(Temp1$) + 2
                   Frst = -1
                END IF
                'IF Ref.ObjNo <> ProcNo THEN STOP

                Temp1$ = ""

                LineNo = Ref.LineNo
                IF ASC(Ref.Include) THEN
                   Temp1$ = NoPath$(Incl$(ASC(Ref.Include))) + ": "
                ELSEIF Inf.LineNumbers = 2 THEN
                   LineNo = LineNo - PrevProcLine + 1
                END IF

                IF Frst THEN
                   Frst = 0
                ELSE
                   MID$(Temp$, Ptr) = ","
                   Ptr = Ptr + 2
                END IF
         
                IF ASC(Ref.Assign) > 127 THEN
                   Temp1$ = Temp1$ + "="
                ELSEIF ObjInf.OType = Label AND Ref.LineNo = ObjInf.OnLineNo THEN
                   Temp1$ = Temp1$ + "*"
                END IF

                Temp1$ = Temp1$ + LTRIM$(STR$(LineNo))

                IF Ptr + LEN(Temp1$) > LineLen THEN
                   PrintLine Temp$, Header$()

                   LSET Temp$ = ""
                   Ptr = 7
                END IF
         
                MID$(Temp$, Ptr) = Temp1$
                Ptr = Ptr + LEN(Temp1$)

                IF Inf.ErrCode < -1 THEN EXIT DO
             LOOP

             PrintLine Temp$, Header$()

             '~~~~~ UnREM the following 2 lines to add seperation lines
             '      between objects
             'LSET Temp$ = DivLine$
             'PrintLine Temp$, Header$()

             DO
                N = N + 1
                ProcNo = N
                IF ProcNo > Inf.Objects THEN EXIT DO
                GetObjRec TypeNameNdx, ProcNo, ObjInf
             LOOP UNTIL ObjInf.DefModInc > -1

             IF Inf.ErrCode < -1 THEN EXIT DO

          LOOP

          IF DoFooter THEN
             PrintFootNote Header$(0)
          END IF
       END IF

       IF Inf.ErrCode < -1 THEN EXIT FOR
    NEXT

    ERASE Header$
    DivLine$ = ""
    Temp$ = ""
    Temp1$ = ""

END SUB

'Prints Object names and number of references
'
SUB PrnObjSummary (ObjTypes&, UnRefedOnly)
    SHARED ObjInf AS ObjectInfo
    SHARED Ref AS ReferenceInfo
    SHARED Make$(), Incl$()
    SHARED ObjType$()
    SHARED FileName$
    SHARED MaxProcLen

    FOR ObjTypeNo = DefFnProc TO FarTypeArray
       IF ObjTypes& AND Power2&(ObjTypeNo + 1) THEN EXIT FOR
    NEXT
    IF ObjTypeNo = FarTypeArray + 1 THEN EXIT SUB

    Spac$ = " "
    ObjLen = MaxInt(12, Inf.MaxNameLen)
    ProcLen = MaxInt(13, MaxProcLen + 1)
    InclLen = -13 * (Inf.Incs > 0)


    '~~~~~ UnREM the following to more accuratly determine the minimum
    '      lengths for each field in the report.
    'N = 1
    'First = 0
    'FOR ObjTypeNo = DefFnProc TO FarTypeArray
    '   IF ObjTypes& AND Power2&(ObjTypeNo + 1) THEN
    '
    '      N = N - 1
    '      DO
    '         N = N + 1
    '         IF N > Inf.Objects THEN EXIT FOR
    '         GetObjRec TypeNameNdx, (N), ObjInf
    '      LOOP UNTIL ObjInf.OType >= ObjTypeNo AND ObjInf.DefModInc > -1
    '
    '      IF First = 0 THEN First = N
    '
    '      DO UNTIL ObjInf.OType <> ObjTypeNo
    '
    '         GOSUB CheckUnRefed
    '         IF NOT UnRefedOnly OR UnRefed THEN
    '            Temp = LEN(RTRIM$(ObjInf.OName)) + 1
    '            IF Temp > ObjLen THEN
    '               ObjLen = Temp
    '            END IF
    '
    '            GetInProcInfo ObjInf.InProcNo, ProcName$, ProcLineNo
    '            Temp = LEN(ProcName$) + 1
    '
    '            IF Temp > ProcLen THEN
    '               ProcLen = Temp
    '            END IF
    '
    '            IF ObjInf.DefModInc > 256 THEN InclLen = 13
    '         END IF
    '
    '         DO
    '            N = N + 1
    '            IF N > Inf.Objects THEN EXIT FOR
    '
    '            GetObjRec TypeNameNdx, (N), ObjInf
    '         LOOP UNTIL ObjInf.DefModInc > -1
    '      LOOP
    '   END IF
    'NEXT

    LineLen = 80 - Inf.PrnLeftMarg

    IF Inf.Modules > 1 THEN
       ModLen = 13
    ELSE
       ModLen = 0
    END IF
    LNumLen = 6
    NRefLen = 6

    Needed = 2 + ObjLen + ProcLen + ModLen + InclLen + LNumLen + NRefLen + 1

    IF Needed > LineLen THEN
       LineLen = Inf.PrnWidth - Inf.PrnLeftMarg
       IF Needed > LineLen THEN
          Test = LineLen - (2 + ObjLen + ModLen + InclLen + LNumLen + NRefLen + 1)
          IF Test < 12 THEN
             IF InclLen THEN
                InclLen = False
             ELSE
                ModLen = False
             END IF
             Test = Test + 13
             IF Test < 12 AND ModLen > 0 THEN
                ModLen = False
                Test = Test + 13
             END IF
          END IF
          ProcLen = Test
       END IF
    END IF

    IF Needed < LineLen THEN
       ObjLen = ObjLen + ((LineLen - Needed + 1) \ 2)
    END IF

    NamePos = 3
    ProcPos = NamePos + ObjLen
    ModPos = ProcPos + ProcLen
    InclPos = ModPos + ModLen
    LNumPos = LineLen - 12
    NRefPos = LineLen - 6

    Nums$ = SPACE$(LNumLen)

    REDIM Header$(5)

    Header$(1) = SPACE$(LineLen)
    IF UnRefedOnly THEN
       LSET Header$(1) = "UNUSED OBJECT TABLE"
    ELSE
       LSET Header$(1) = "OBJECT SUMMARY TABLE "
    END IF

    Header$(2) = SPACE$(LineLen)
    MID$(Header$(2), 1) = FileName$
    MID$(Header$(2), LineLen - 18) = Inf.DateTime

    Header$(4) = SPACE$(LineLen)
    MID$(Header$(4), NamePos) = "Object Name"
    MID$(Header$(4), ProcPos) = "In Procedure"
    IF ModLen THEN MID$(Header$(4), ModPos) = "Module"
    IF InclLen THEN MID$(Header$(4), InclPos) = "Include"
    MID$(Header$(4), LNumPos) = "Line #"
    MID$(Header$(4), NRefPos) = "  Refs."

    IF Inf.GraphChars THEN
       Header$(5) = STRING$(LineLen, "Ä")
    ELSE
       Header$(5) = STRING$(LineLen, "-")
    END IF

    MID$(Header$(5), NamePos - 1) = Spac$
    MID$(Header$(5), ProcPos - 1) = Spac$
    MID$(Header$(5), ModPos - 1) = Spac$
    MID$(Header$(5), InclPos - 1) = Spac$
    MID$(Header$(5), LNumPos - 1) = Spac$
    MID$(Header$(5), NRefPos) = Spac$

    PrintHeader Header$()

    Temp$ = SPACE$(LineLen)
    
    N = 1

    FOR ObjTypeNo = DefFnProc TO FarTypeArray

       IF Inf.ErrCode < -1 THEN EXIT FOR

       IF ObjTypes& AND Power2&(ObjTypeNo + 1) THEN

          N = N - 1
          DO
             N = N + 1
             IF N > Inf.Objects THEN EXIT FOR
             GetObjRec TypeNameNdx, (N), ObjInf
          LOOP UNTIL ObjInf.OType >= ObjTypeNo AND ObjInf.DefModInc > -1
     
          IF ObjInf.OType = ObjTypeNo THEN

             IF Inf.LCount >= Inf.PrnLength - 12 THEN
                PrintFootNote Header$(0)
                PrintHeader Header$()
             END IF

             IF Inf.LCount > UBOUND(Header$) THEN PrintLine "", Header$()
             PrintLine ObjType$(ObjTypeNo) + "s:", Header$()

             UpdateTabCon SPACE$(4) + ObjType$(ObjTypeNo) + "s "

             IF Inf.ErrCode < -1 THEN EXIT FOR
          END IF

          DO UNTIL ObjInf.OType <> ObjTypeNo

             Ky$ = INKEY$
             IF ASCII(Ky$) = 27 THEN
                Inf.ErrCode = -3
                EXIT FOR
             END IF

             GOSUB CheckUnRefed

             IF NOT UnRefedOnly OR UnRefed THEN
                                    
                LSET Temp$ = "  " + MakeDottedLine$(LineLen - 3)

                MID$(Temp$, ProcPos - 1) = Spac$
                MID$(Temp$, ModPos - 1) = Spac$
                MID$(Temp$, InclPos - 1) = Spac$
                MID$(Temp$, NamePos, ObjLen - 1) = LTRIM$(RTRIM$(ObjInf.OName))

                LineNo = ObjInf.OnLineNo
                IF ObjTypeNo <> SubProc AND ObjTypeNo <> FuncProc THEN
                   GetInProcInfo ObjInf.InProcNo, ProcName$, ProcLineNo
                   MID$(Temp$, ProcPos, ProcLen - 1) = ProcName$
                   IF Inf.LineNumbers = 2 AND ObjInf.DefModInc \ 256 = 0 THEN
                      LineNo = LineNo - ProcLineNo + 1
                   END IF
                END IF
                RSET Nums$ = STR$(LineNo)
                MID$(Temp$, LNumPos) = Nums$

                MID$(Temp$, ModPos, ModLen) = NoPath$(Make$(ObjInf.DefModInc MOD 256))
                MID$(Temp$, InclPos, InclLen) = NoPath$(Incl$(ObjInf.DefModInc \ 256))
                 
                RSET Nums$ = STR$(ObjInf.NumRefs)
                MID$(Temp$, NRefPos) = Nums$
                IF UnRefed THEN
                   MID$(Temp$, LineLen) = "*"
                   Header$(0) = " * Object was defined but never used."
                END IF

                IF Inf.LCount >= Inf.PrnLength - Inf.PageIf THEN
                   PrintLine ObjType$(ObjTypeNo) + "s continued.", Header$()
                   Header$(0) = ""
                END IF

                PrintLine Temp$, Header$()

                IF Inf.ErrCode < -1 THEN EXIT FOR
             END IF

             DO
                N = N + 1
                IF N > Inf.Objects THEN EXIT FOR

                GetObjRec TypeNameNdx, (N), ObjInf
             LOOP UNTIL ObjInf.DefModInc > -1
          LOOP
       END IF
    NEXT

    PrintFootNote Header$(0)

    ERASE Header$
    Temp$ = ""
    Nums$ = ""

    EXIT SUB


CheckUnRefed:
    IF ObjTypeNo > SubProc THEN
       UnRefed = ObjInf.NumRefs <= 1
       IF ObjTypeNo = NumConst THEN UnRefed = 0
    ELSE
       UnRefed = ObjInf.NumRefs = 0
    END IF
RETURN

END SUB

SUB PrnProcObjs (ProcObjTypes&)
    SHARED ObjInf AS ObjectInfo
    SHARED Ref AS ReferenceInfo
    SHARED Make$(), Incl$()
    SHARED Proc() AS ProcInfo
    SHARED ObjType$()
    SHARED FileName$


    MQPrint "Sorting References", -1

    '----- Sort the procedure names alphabeticaly (not by type first)
    IF UBOUND(Proc) > 1 THEN
       SortT2 Proc(2), UBOUND(Proc) - 1, 0, 46, 2, 40
    END IF

    Temp$ = "Table of Objects Used in Procedures"
    MQPrint "Creating " + Temp$, -1


    REDIM Header$(6)

    Header$(1) = SPACE$(79)
    LSET Header$(1) = UCASE$(Temp$) + " for "
    MID$(Header$(1), 41) = NoPath$(FileName$)

    Header$(2) = SPACE$(79)
    MID$(Header$(2), 79 - 18) = Inf.DateTime

    Header$(4) = "Procedure Name: Defined in Module                                Procedure Type"

                 '         1         2         3         4         5         6         7         8
                 '12345678901234567890123456789012345678901234567890123456789012345678901234567890
    Header$(5) = "       Object Type:           Object Name                        Variable Scope"

    IF Inf.GraphChars THEN
       Header$(6) = STRING$(79, "Í")
       DivLine$ = STRING$(79, "Ä")
    ELSE
       Header$(6) = STRING$(79, "=")
       DivLine$ = STRING$(79, "-")
    END IF


    'PrintHeader Header$()


    '----- Set up the sorting table for sorts below
    '----- Sorts by Object type then Name
    REDIM Table(1 TO 2, 1 TO 3)
    Table(1, 1) = 0
    Table(1, 2) = -1

    Table(2, 1) = 2
    Table(2, 2) = 28

    MaxN& = 0
    LineBuf$ = SPACE$(79)

    '----- For each module
    FOR Module = 1 TO Inf.Modules

       '----- Examine each procedure in alpha order to see if they where
       '      defined in this module.
       FOR ProcNo = 1 TO UBOUND(Proc)

          '----- Was it defined in this module?
          IF ProcNo = 1 OR Proc(ProcNo).DefModInc MOD 256 = Module THEN
                   
             '----- Print the Procedure Name, Module and Type
             Temp$ = RTRIM$(Proc(ProcNo).OName) + ": "
             Temp$ = Temp$ + NoPath$(Make$(Module))

             MID$(Header$(2), 1) = SPACE$(59)
             MID$(Header$(2), 1) = Temp$

             IF ProcNo = 1 THEN
                PrintHeader Header$()
             END IF

             MID$(Header$(2), 1) = Temp$ + " continued."

             LSET LineBuf$ = Temp$

             IF ProcNo = 1 THEN
                Temp$ = SPACE$(4) + NoPath$(Make$(Module)) + ": "
             ELSE
                Temp$ = SPACE$(8)
             END IF

             UpdateTabCon Temp$ + RTRIM$(Proc(ProcNo).OName)

       
             '----- Put the unique objects in a temporary array for sorting
             REDIM Sort(1000) AS TypeNameSort
             NumObjs = 0
             Recursive = False

             IF Module = 1 OR ProcNo > 1 THEN
                GetObjRec 0, Proc(ProcNo).ObjNo, ObjInf
                SaveFlags = ObjInf.Flag
                Start& = ObjInf.FirstRef + 1
             ELSE
                Start& = MaxN&
             END IF

             FOR N& = Start& TO Inf.ObjRefs
                GetRefRec N&, Ref
                
                IF ASC(Ref.Module) > Module THEN EXIT FOR

                IF Ref.InProcNo = Proc(ProcNo).ObjNo THEN

                   IF Ref.ObjNo = Proc(ProcNo).ObjNo THEN
                      IF ASC(Ref.Assign) <= 127 THEN
                         Recursive = True
                      END IF
                   END IF

                   FOR X = 1 TO NumObjs
                      IF Sort(X).ObjNo = Ref.ObjNo THEN EXIT FOR
                   NEXT

                   IF X > NumObjs THEN

                      IF Ref.ObjNo <> Proc(ProcNo).ObjNo OR Recursive THEN
                         X = ASC(Ref.Assign) AND &H7F
                         IF ProcObjTypes& AND Power2&(X + 1) THEN
                            GetObjRec 0, Ref.ObjNo, ObjInf
                            NumObjs = NumObjs + 1
                            Sort(NumObjs).ObjType = ObjInf.OType
                            Sort(NumObjs).ObjName = ObjInf.OName
                            Sort(NumObjs).ObjNo = Ref.ObjNo
                         END IF
                      END IF
                   END IF

                ELSEIF ProcNo > 1 THEN
                   EXIT FOR
                END IF

                IF Inf.ErrCode < -1 THEN EXIT FOR
                
             NEXT

             IF N& > MaxN& THEN MaxN& = N&
             

             '----- Sort the temp array by Object type and name
             IF NumObjs > 1 THEN
                KeySort Sort(1), LEN(Sort(1)), NumObjs, Table(1, 1), 2
             END IF


             VertLine$ = " "
             IF ProcNo > 1 THEN
             
                IF GetNybble(SaveFlags, Atrb) = Automatic THEN
                   IF Recursive THEN
                      Temp$ = "Recursive"
                      IF Inf.GraphChars THEN
                         VertLine$ = "³"
                      ELSE
                         VertLine$ = "|"
                      END IF
                   ELSE
                      Temp$ = "Dynamic"
                   END IF
                ELSE
                   Temp$ = "Static"
                END IF

                MID$(LineBuf$, 71) = Temp$
             END IF

             PrintLine LineBuf$, Header$()

             IF Inf.ErrCode < -1 THEN EXIT FOR

             '----- Compose and print each line of objects
             LastType = -1
             FOR X = 1 TO NumObjs
                Ky$ = INKEY$
                IF ASCII(Ky$) = 27 THEN
                   Inf.ErrCode = -3
                   PrintFootNote ""
                   EXIT FOR
                END IF

                LSET LineBuf$ = ""

                IF Sort(X).ObjType <> LastType THEN
                   MID$(LineBuf$, 8) = ObjType$(Sort(X).ObjType) + "s"
                   LastType = Sort(X).ObjType
                END IF

                GetObjRec 0, Sort(X).ObjNo, ObjInf
                MID$(LineBuf$, 31) = LTRIM$(ObjInf.OName)

                IF ProcNo > 1 THEN
                   IF LastType > Constant THEN
                      IF ObjInf.Share MOD 256 THEN
                         Temp$ = "Global"
                      ELSEIF ObjInf.InProcNo = 1 THEN
                         Temp$ = "Shared"
                      ELSE
                         Temp = GetNybble(ObjInf.Flag, Atrb)
                         SELECT CASE Temp
                            CASE Parameter
                               Temp$ = "Parameter"
                            CASE Automatic
                               Temp$ = "Automatic"
                            CASE ELSE
                               Temp$ = "Static"
                         END SELECT
                      END IF
                      MID$(LineBuf$, 71) = Temp$

                   ELSEIF Sort(X).ObjNo = Proc(ProcNo).ObjNo THEN
                      IF Inf.GraphChars THEN
                         MID$(LineBuf$, 71) = "Ù"
                      ELSE
                         MID$(LineBuf$, 71) = "*"
                      END IF
                      Temp = LEN(RTRIM$(ObjInf.OName))
                      MID$(LineBuf$, 31 + Temp, 40 - Temp) = "<" + DivLine$

                      VertLine$ = " "
                   ELSEIF LastType = FuncProc OR LastType = SubProc THEN
                      MID$(LineBuf$, 71) = VertLine$
                   END IF
                END IF

                PrintLine LineBuf$, Header$()
                IF Inf.ErrCode < -1 THEN EXIT FOR
             NEXT

             IF Inf.ErrCode < -1 THEN EXIT FOR

             PrintLine DivLine$, Header$()

          END IF
       NEXT

       PrintFootNote ""

       IF Inf.ErrCode < -1 THEN EXIT FOR
    NEXT

    ERASE Table, Sort, Header$
    LineBuf$ = ""
    Temp$ = ""

END SUB

SUB ProcTree (BYVAL ProcNo)

    SHARED ObjInf AS ObjectInfo
    SHARED Ref AS ReferenceInfo
    SHARED Make$(), FileName$

    '----- No need to recreate these on each invokation
    STATIC Index(), Level, Buf$, Temp, NObjs, Module
    STATIC I, Header$(), Ky$, FirstRec&, Rec&
    STATIC Leader$, ContDown$, Branch$, LeftCor$

    DIM TempInf AS ObjectInfo
 
    '----- Level of 0 means we should initialize
    IF Level = 0 THEN
       MQPrint SPACE$(80), Inf.BarClr           'Show the user whats going on
       MQPrint "Creating Procedure Tree", -1
       LOCATE , 10

       Temp = MinInt(1000, Inf.Objects)
       REDIM Index(1 TO Temp, 1)                'Create a sorting index

       NObjs = 0
       FOR I = 1 TO Inf.Objects
          RecNo = I
          GetObjRec NameNdx, RecNo, ObjInf

          IF ObjInf.OType >= DefFnProc AND ObjInf.OType <= SubProc THEN
             NObjs = NObjs + 1
             Index(NObjs, 0) = RecNo
          END IF
       NEXT

       '----- Create the report header
       REDIM Header$(3)
       Header$(1) = SPACE$(78)
       LSET Header$(1) = "PROCEDURE TREE DIAGRAM"
       Header$(2) = SPACE$(78)
       MID$(Header$(2), 1) = FileName$
       MID$(Header$(2), 78 - 18) = Inf.DateTime
       IF Inf.GraphChars THEN
          Header$(3) = STRING$(78, "Ä")
       ELSE
          Header$(3) = STRING$(78, "-")
       END IF

       PrintHeader Header$()

       UpdateTabCon "Procedure Tree "

       '----- Initialize variables
       IF NOT Inf.ProcTreePages THEN            'If no page breaks, set the
          Inf.LCount = -32767                   '  line count to low to show.
       END IF

       IF Inf.GraphChars THEN                   'Using IBM graphic characters?
          Leader$ = "ÄÄÄÄ"                      'Yes, set graphics line
          ContDown$ = "³    "                   '  components.
          Branch$ = "Ã"
          LeftCor$ = "À"
       ELSE                                     'No, set text (low ASCII)
          Leader$ = "----"                      '  line components.
          ContDown$ = "|    "
          Branch$ = "|"
          LeftCor$ = "^"
       END IF
       Leader$ = Leader$ + SPACE$(64)
                                                'Create a line buffer for text
       Buf$ = SPACE$(Inf.PrnWidth - Inf.PrnLeftMarg)
       GetObjRec 0, ProcNo, ObjInf
       LSET Buf$ = ObjInf.OName                 'Put starting proc. name in
       PrintLine Buf$, Header$()                '  buffer and print it.
    END IF


    Level = Level + 1                           'Bump the tree branch level
    IF Level * 5 > (Inf.PrnWidth - Inf.PrnLeftMarg) - 5 THEN
       Level = Level - 1
       EXIT SUB
    END IF

    '----- Find then save all procedures for this branch
    GetObjRec 0, ProcNo, ObjInf
    FirstRec& = ObjInf.FirstRef + 1
    Module = ObjInf.DefModInc MOD 256
    
    FOR I = 0 TO 1
       
       IF I THEN
          REDIM TBL(Last) AS TreeSort           'Create a sorting table for
       END IF                                   '  this branch.

       Last = 0

       FOR Rec& = FirstRec& TO Inf.ObjRefs
          GetRefRec Rec&, Ref

          IF ASC(Ref.Module) <> Module THEN EXIT FOR
          IF Ref.InProcNo <> ProcNo THEN EXIT FOR

          Temp = ASC(Ref.Assign) AND &H7F       'Get the object's type and
                                                '  see if it's a procedure
          IF Temp >= DefFnProc AND Temp <= SubProc THEN
             '----- Don't process function assignments
             IF Ref.ObjNo <> ProcNo OR ASC(Ref.Assign) <= 127 THEN
                Last = Last + 1                 'Bump number of nodes
                'REDIM PRESERVE TBL(Last) AS TreeSort
                IF I THEN
                   TBL(Last).ProcNo = Ref.ObjNo 'Save object number and it's
                                                '  subscript in the index.
                   TBL(Last).IndexNo = XRSearch%(Index(1, 0), NObjs, Ref.ObjNo)
                END IF
             END IF
          END IF
       NEXT

       IF Last = 0 THEN EXIT FOR
    NEXT

    

    '----- Sort this branch
    IF Inf.SortProcTree AND Last > 1 THEN
       SortT2 TBL(1), Last, 0, 4, 2, -1
    END IF


    '----- Create the report lines for this branch
    FOR N = 1 TO Last                           'Examine each node of branch

       '----- Don't keep processing the same object
       IF TBL(N).ProcNo <> TBL(N - 1).ProcNo THEN

          GetObjRec 0, TBL(N).ProcNo, ObjInf

          IF Inf.DetailProcTree \ 256 = 0 OR ObjInf.DefModInc MOD 256 > 0 THEN

             '----- Continue lines from previous branches to the left
             FOR I = 0 TO Level - 1
                Temp = ASC(MID$(Buf$, I * 5 + 1, 1))

                IF Temp = ASC(Branch$) OR Temp = ASC(ContDown$) THEN
                   MID$(Buf$, I * 5 + 1) = ContDown$
                ELSE
                   MID$(Buf$, I * 5 + 1) = "     "
                END IF
             NEXT

             '----- Insert our branch line and this procedure name
             Temp = (Level - 1) * 5
             MID$(Buf$, Temp + 2) = Leader$
          
             MID$(Buf$, Temp + 6) = RTRIM$(ObjInf.OName) + ": " + LCASE$(NoPath$(Make$(ObjInf.DefModInc MOD 256)))

             '----- If this procedure name is same as branch procedure name,
             '      Show that it is a recursive procedure.
             IF TBL(N).ProcNo = ProcNo THEN MID$(Buf$, Temp + 3) = "<"

             '----- Find beginning subscript of next procedure in list
             FOR I = N + 1 TO Last
                IF TBL(I).ProcNo <> TBL(N).ProcNo THEN
                   IF Inf.DetailProcTree \ 256 THEN
                      GetObjRec 0, TBL(I).ProcNo, TempInf
                      IF TempInf.DefModInc MOD 256 > 0 THEN EXIT FOR
                   ELSE
                      EXIT FOR
                   END IF
                END IF
             NEXT
             IF I <= Last THEN                     'If not on last object,
                MID$(Buf$, Temp + 1) = Branch$     '  use branch character
             ELSE                                  'Otherwise,
                MID$(Buf$, Temp + 1) = LeftCor$    '  use left corner character
             END IF

             '----- Print this node's line
             PrintLine RTRIM$(Buf$), Header$()
             IF Inf.ErrCode < -1 THEN EXIT FOR

             '----- See if this node starts a branch
             IF ObjInf.DefModInc AND TBL(N).ProcNo <> ProcNo THEN

                IF Inf.DetailProcTree MOD 256 THEN  'Doing a detailed listing?

                   ProcTree TBL(N).ProcNo

                ELSE
                   '----- Has this procedure's branch already been shown?
                   IF Index(TBL(N).IndexNo, 1) = 0 THEN
                      Index(TBL(N).IndexNo, 1) = -1

                      ProcTree TBL(N).ProcNo

                   END IF
                END IF
             END IF
          END IF
       END IF

       '----- Bail out if the user presses Escape
       Ky$ = INKEY$
       IF ASCII(Ky$) = 27 THEN Inf.ErrCode = -3

       IF Inf.ErrCode < -1 THEN EXIT FOR

    NEXT

    ERASE TBL                                   'Erase this branches table
    Level = Level - 1                           'Decrement the branch level

    IF Level = 0 THEN                           'If back to the root,
       PrintFootNote Header$(0)                 '  print foot notes.
       Inf.TabConLine = Inf.TabConLine + 1

       ERASE Index, Header$                     'Clean up memory
       Buf$ = ""
       Leader$ = ""
    END IF

END SUB

