'$SubTitle:'Routines for Loading and Saving Object Databases'

DEFINT A-Z

'$INCLUDE: 'XRefCnst.bi'
'$INCLUDE: 'XRefType.bi'
'$INCLUDE: 'XRefComn.bi'

'$LINESIZE:95

DECLARE FUNCTION DosError% ()
DECLARE FUNCTION ErrorMsg$ (ErrorNum)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FLInput$ (Handle, Buffer$)
DECLARE FUNCTION FLof& (Handle%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION ParseString$ (CurPos%, Work$, Delim$)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION QPValI% (Number$)
DECLARE FUNCTION TranslatePrnSetUp$ (PrnSetUp$)
DECLARE FUNCTION WhichError% ()

DECLARE SUB ButtonPress (Button, Status, Count, X, Y)
DECLARE SUB Copy2Database (FileName$)
DECLARE SUB ErrMsg (Text$)
DECLARE SUB FatalError ()
DECLARE SUB FClose (Handle%)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGet (Handle%, Dest$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetT (Handle%, Dest AS ANY, NumBytes%)
DECLARE SUB FileSort (DataFile$, IndexFile$, First&, Last&, Table(), RecLength)
DECLARE SUB FileView (FileName$, Ky%, Action%, VFL AS ANY, SEG Array)
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FPut (Handle%, Source$)
DECLARE SUB FPutA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FPutT (Handle%, Dest AS ANY, NumBytes%)
DECLARE SUB FSeek (Handle%, Position&)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB HideCursor ()
DECLARE SUB MQPrint (Text$, Clr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Element)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Element)
DECLARE SUB QPrintRC (X$, Row%, Col%, Clr%)
DECLARE SUB SetError (ErrNum%)
DECLARE SUB ShowCursor ()


TYPE FileViewInfo

    Colr        AS INTEGER      '7      ;Text color (white on black default)
    ULRow       AS INTEGER      '1      ;window's corners
    ULCol       AS INTEGER      '1
    LRRow       AS INTEGER      '25
    LRCol       AS INTEGER      '80
    HorizOffset AS INTEGER      '0      ;Horizontal offset for display (scrolling)
    LoPtr       AS INTEGER      '0      ;set SI to this for the first screen line
    FileHandle  AS INTEGER      '0      ;Use for saving file handle
    EndOfFile   AS INTEGER      '-1     ;Points within buffer
    LineNumber  AS LONG         '1      ;Line No of the top line
    BufferMid   AS INTEGER      '16384  ;midpoint of the buffer in bytes
    FileSeek    AS LONG         '0      ;absolute file LOC to begin display (polled only!)
    FileOffset  AS LONG         '-1     ;Address within file of buffer data

END TYPE

'$Page

SUB AddObjType (Bits&)

   Low = Bits& AND 63

   Bits& = (Bits& * 2) AND &HFF80

   Bits& = Bits& OR Low

END SUB

SUB BrowseFile (FileName$, BYVAL Clr, Ky1, Action) STATIC

    IF Action = 1 THEN

       Eighty = 80
       OneTwelve = 112

       QPrintRC CHR$(24), 3, Eighty, OneTwelve
       QPrintRC CHR$(8), 4, Eighty, OneTwelve
       FOR N = 5 TO 22
          QPrintRC "°", N, Eighty, OneTwelve
       NEXT
       QPrintRC CHR$(25), 23, Eighty, OneTwelve
    
       FileLen& = FileSize&(FileName$) + 1
       PrevBarPos = 0
       WaitTime = 1

       REDIM VFL(1 TO 1) AS FileViewInfo
       Ky1% = 0

       VFL(1).Colr = Clr
       VFL(1).ULRow = 3
       VFL(1).ULCol = 2
       VFL(1).LRRow = 23
       VFL(1).LRCol = 79
       VFL(1).BufferMid = 16384

       REDIM Array1(VFL(1).BufferMid)            '32K
       LNum$ = SPACE$(10)
       
    ELSEIF Action = 5 THEN

       FileView FileName$, Ky1%, Action%, VFL(1), Array1(0)
       ERASE VFL, Array1
       EXIT SUB
    END IF

    
    FileView FileName$, Ky1%, Action%, VFL(1), Array1(0)

    '----- Calculate the scroll pointer line
    Offset& = VFL(1).FileOffset + VFL(1).LoPtr
    IF VFL(1).FileOffset <> 0 THEN
       Offset& = Offset& - 16384
    END IF
    BarPos = Offset& * 19 \ FileLen&
    IF BarPos > 18 THEN BarPos = 18

    '----- Mouse code
    ButtonPress 1, Down, Presses, MCol, MRow
    IF Presses = 0 THEN GetCursor MCol, MRow, Button
    MRow = (MRow \ 8) + 1
    MCol = (MCol \ 8) + 1

    '----- If the mouse button was pressed or is down
    IF Presses OR Button THEN
       CurTime& = PDQTimer&
       IF Presses THEN
          TimeOut& = 0
          InitDelay = 7
          PrRow = MRow
          PrCol = MCol
          IF PrCol = 80 AND PrRow = PrevBarPos + 4 THEN
             DoingPointer = -1
          ELSE
             DoingPointer = 0
          END IF
       END IF

       IF PrCol = 80 THEN
          '----- Pressed on the Scroll pointer or above or below?
          IF DoingPointer THEN
             IF MRow <> PrRow THEN
                IF MRow < 4 THEN MRow = 4
                IF MRow > 22 THEN MRow = 22
                VFL(1).FileSeek = (MRow - 4) * FileLen& \ 19 + 1
                IF VFL(1).FileSeek > 1 THEN
                   VFL(1).FileSeek = VFL(1).FileSeek + 132
                END IF
                PrRow = MRow
             END IF

          ELSEIF CurTime& >= TimeOut& THEN
             TimeOut& = CurTime& + WaitTime + InitDelay
             InitDelay = 0

             '----- Pressed on the arrow icons?
             IF PrRow = 3 THEN
                Ky1 = -72
             ELSEIF PrRow = 23 THEN
                Ky1 = -80
             ELSEIF MRow < PrevBarPos + 4 AND MRow > 3 THEN
                Ky1 = -73
             ELSEIF MRow > PrevBarPos + 4 AND MRow < 23 THEN
                Ky1 = -81
             END IF
          END IF

       '----- Pressed in the browse window
       ELSEIF MCol > 1 THEN
          IF MRow > PrRow THEN
             Ky1 = -72
             IF MRow < 24 THEN PrRow = PrRow + 1
          ELSEIF MRow < PrRow THEN
             Ky1 = -80
             IF MRow > 2 THEN PrRow = PrRow - 1
          END IF
             
          IF MCol > PrCol THEN
             IF MCol = 80 THEN MCol = 79
             VFL(1).HorizOffset = VFL(1).HorizOffset - (MCol - PrCol)
             IF VFL(1).HorizOffset < 0 THEN VFL(1).HorizOffset = 0
             PrCol = MCol
             Action = 2
          ELSEIF MCol < PrCol THEN
             VFL(1).HorizOffset = VFL(1).HorizOffset + PrCol - MCol
             IF VFL(1).HorizOffset > 32766 THEN VFL(1).HorizOffset = 0
             PrCol = MCol
             Action = 2
          END IF
       END IF
    END IF

    IF BarPos <> PrevBarPos THEN
       HideCursor
       QPrintRC "°", 4 + PrevBarPos, Eighty, OneTwelve
       QPrintRC CHR$(8), 4 + BarPos, Eighty, OneTwelve
       ShowCursor
       PrevBarPos = BarPos
    END IF

    IF VFL(1).LineNumber <> PrevLine OR VFL(1).HorizOffset <> PrevCol THEN
       RSET LNum$ = STR$(ABS(VFL(1).LineNumber)) + ":" + STR$(VFL(1).HorizOffset + 1)
       QPrintRC LNum$, 25, 70, 48
       PrevLine = VFL(1).LineNumber
       PrevCol = VFL(1).HorizOffset
    END IF

    
END SUB

SUB Copy2Database (FileName$)

    FOpen FileName$, Handle

    IF FRE(-1) < 65534 THEN
       BlockSize& = FRE(-1) - 4096
    ELSE
       BlockSize& = 65534
    END IF

    REDIM Buffer(BlockSize& \ 2 - 1)

    BytesCopied& = 0
    FileLen& = FLof&(Handle)

    DO WHILE BytesCopied& < FileLen& AND NOT DosError
       IF FileLen& - BytesCopied& < BlockSize& THEN
          Bytes& = FileLen& - BytesCopied&
       ELSE
          Bytes& = BlockSize&
       END IF

       FGetA Handle, Buffer(0), Bytes&
       FPutA Inf.DBHandle, Buffer(0), Bytes&

       BytesCopied& = BytesCopied& + Bytes&
    LOOP

    SaveError = WhichError
    
    FClose Handle
    KILL FileName$

    SetError SaveError

END SUB

SUB CreateDatabase (DBName$, ObjFile$, RefFile$)

    SHARED ObjInf AS ObjectInfo
    SHARED Ref AS ReferenceInfo
    SHARED Make$()
    SHARED Incl$()

    Term$ = CHR$(13) + CHR$(10)

    LOCATE 25, 1
    MQPrint SPACE$(80), Inf.BarClr
    MQPrint "Creating Object Database " + DBName$ + ".", -1
    LOCATE , 28 + LEN(DBName$)


    '~~~~~ Create indexes for the Object information file
    MQPrint "Sorting Data...", -1

    NumberOfKeys = 3
    REDIM KeyInfo(1 TO NumberOfKeys, 1 TO 3)
    KeyInfo(1, 1) = 40: KeyInfo(1, 2) = -1
    KeyInfo(2, 1) = 0:  KeyInfo(2, 2) = 40
    KeyInfo(3, 1) = 48: KeyInfo(3, 2) = -1

    TypeNameNdx$ = "OBJTNP11.NDX"

    FileSort ObjFile$, TypeNameNdx$, 0&, 0&, KeyInfo(), -LEN(ObjInf)
    IF DosError THEN FatalError: EXIT SUB

    NumberOfKeys = 1
    REDIM KeyInfo(1 TO NumberOfKeys, 1 TO 3)
    KeyInfo(1, 1) = 0: KeyInfo(1, 2) = 40

    NameNdx$ = "OBJN2222.NDX"

    FileSort ObjFile$, NameNdx$, 0&, 0&, KeyInfo(), -LEN(ObjInf)
    IF DosError THEN FatalError: EXIT SUB


    
    MQPrint "Writing", -1

    FCreate DBName$
    FOpen DBName$, Inf.DBHandle


    '~~~~~ Write out the header
    Header = 54                                         '54 Bytes

    FPut Inf.DBHandle, "XrfDataBase" + CHR$(26)         'File identifier

    Inf.ObjData = Header
    Inf.NameData = CLNG(Inf.Objects) * LEN(ObjInf) + Inf.ObjData
    Inf.TypeNameData = Inf.Objects * 4& + Inf.NameData

    Inf.RefData = Inf.Objects * 4& + Inf.TypeNameData

    Inf.ModuleData = Inf.ObjRefs * LEN(Ref) + Inf.RefData
    Temp = 0
    FOR N = 1 TO Inf.Modules
       Temp = Temp + LEN(Make$(N)) + 2
    NEXT
    Inf.IncData = Temp + Inf.ModuleData

    FPutA Inf.DBHandle, Inf.Modules, 36

    FPut Inf.DBHandle, STRING$(6, 0)


    '~~~~~ Copy/Move object info and indexes to the database
    Copy2Database ObjFile$
    IF DosError THEN FatalError: EXIT SUB

    Copy2Database NameNdx$
    IF DosError THEN FatalError: EXIT SUB
    
    Copy2Database TypeNameNdx$
    IF DosError THEN FatalError: EXIT SUB
    

    '~~~~~ Copy/Move reference info to the database
    Copy2Database RefFile$
    IF DosError THEN FatalError: EXIT SUB
    

    '~~~~~ Copy Module and Include names to the database
    FOR N = 1 TO Inf.Modules
        FPut Inf.DBHandle, Make$(N) + Term$
        IF DosError THEN FatalError: EXIT SUB
    NEXT

    FOR N = 1 TO Inf.Incs
        FPut Inf.DBHandle, Incl$(N) + Term$
        IF DosError THEN FatalError: EXIT SUB
    NEXT


    MQPrint SPACE$(80), 48

END SUB

SUB GetDBStats (DBName$)

    SHARED Make$(), Incl$()

    FOpen DBName$, Inf.DBHandle                         'Open the database

    IF DosError THEN FatalError: EXIT SUB
    
    Temp$ = SPACE$(12)

    FGet Inf.DBHandle, Temp$                            'Read the ID$
    IF Temp$ <> "XrfDataBase" + CHR$(26) THEN
       ErrMsg DBName$ + " is not an XREF v2 compatible database."
       Inf.ErrCode = -54
       EXIT SUB
    END IF

    FGetA Inf.DBHandle, Inf.Modules, 36                 'Read database stats.


    '~~~~~ Read module and include file names
    Temp$ = SPACE$(66)
    FSeek Inf.DBHandle, Inf.ModuleData

    REDIM Make$(Inf.Modules)
    FOR N = 1 TO Inf.Modules
        Make$(N) = FLInput$(Inf.DBHandle, Temp$)
        IF DosError THEN FatalError: EXIT SUB
    NEXT

    REDIM Incl$(Inf.Incs)
    FOR N = 1 TO Inf.Incs
       Incl$(N) = FLInput$(Inf.DBHandle, Temp$)
       IF DosError THEN FatalError: EXIT SUB
    NEXT

END SUB

'****************************************************************************
'Purpose: Translates printer setup codes (ie. 27, M) to a string
'         of characters
'
' Inputs: PrnSetUp$ - Printer setup code
'
'Outputs: Translated printer setup string
'
'****************************************************************************
'
FUNCTION TranslatePrnSetUp$ (PrnSetUp$) STATIC

   Trans$ = ""
   St = 1
   DO UNTIL St > LEN(PrnSetUp$)
      Word$ = ParseString$(St, PrnSetUp$, " ,")

      IF QPValI(Word$) THEN                     'If it's a number,
         Trans$ = Trans$ + CHR$(QPValI(Word$))  '  make it a character
      ELSE                                      'Otherwise,
         Trans$ = Trans$ + Word$                '  just add it as is
      END IF
   LOOP

   TranslatePrnSetUp$ = Trans$                  'Assign the function

END FUNCTION

