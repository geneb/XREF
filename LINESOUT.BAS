'****************************************************************************
'  LINESOUT.BAS - Source for LINESOUT.COM
'  by Don Malin
'  (c) 1990, 1991 Crescent Software, Inc.
'
'  Purpose:  Removes unused line numbers and labels from a source file that
'  has been XREFed.  For each module (source file) listed in an Object
'  Database, this program will look for and remove line numbers and labels
'  that are never referenced.  When the program has completed a module, it
'  will make a backup copy (.BAK) of the original source and then write out
'  the new source to the module's name (.BAS).
'
'  From DOS type:  LINESOUT FileName[.ODB]
'----------------------------------------------------------------------------
'  Compile and link with P.D.Q. as follows:
'  bc LINESOUT /O/AH;
'  link /E/NOE/NOD LINESOUT _KILL _NOREAD _NOVAL,, NUL, PDQ;
'  exe2com LINESOUT
'****************************************************************************
DEFINT A-Z

DECLARE FUNCTION ChangeExtension$ (FileName$, Extension$)
DECLARE FUNCTION BufIn$ (FileName$, Done%)
DECLARE FUNCTION PDQExist% (FileName$)
DECLARE SUB BigSave ALIAS "B$PUT3" (BYVAL Handle, BYVAL Segment, BYVAL Address, BYVAL NumBytes)
DECLARE SUB PDQInput (Work$)

'$INCLUDE: 'XRefCnst.bi'
'$INCLUDE: 'XRefType.bi'

CONST BufWidth = 16382                          'Width of source buffer el.

TYPE Buffer                                     'Source buffer layout
   Length AS INTEGER                            'Length of buffered text
   Buffer AS STRING * BufWidth                  'Buffered text
END TYPE

DIM Obj AS ObjectInfo                           'Holds Object information

CrLf$ = CHR$(13) + CHR$(10)                     'Chariage return line feed


PRINT "Line Number Removal Utility"
PRINT "(c) 1990, 1991 Crescent Software, Inc."
PRINT


'~~~~~ Get the name of the ODB file to use.
IF LEN(COMMAND$) THEN FileName$ = COMMAND$      'Use command line argument


DO

   IF LEN(FileName$) = 0 THEN                   'No command line argument
      PRINT "Object Database Name: ";           'Ask the user
      PDQInput FileName$
      IF LEN(FileName$) = 0 THEN END            'Nothing entered?  Quit
   END IF
   
   IF NOT PDQExist%(FileName$) THEN             'Can't find it, try ODB exten.
      FileName$ = ChangeExtension$(FileName$, ".ODB")
   END IF

   IF PDQExist%(FileName$) THEN                 'If we found it,

      OPEN FileName$ FOR BINARY AS #1           'Open it in binary mode

      Test$ = SPACE$(12)                        'Read the first 12 bytes
      GET #1, , Test$

      '~~~~~ Check for valid Object Database file
      IF Test$ <> "XrfDataBase" + CHR$(26) THEN
         PRINT FileName$; " is not a valid Object Database!"
         FileName$ = ""
         CLOSE #1
      ELSE
         EXIT DO                                'All set, bail out of input
      END IF                                    '  loop.
   ELSE
      PRINT "Can't find - "; FileName$
      FileName$ = ""
   END IF

LOOP

'~~~~~ Read the object database's header information
GET #1, , NumModules                            'Number of modules
GET #1, , ModuleData&                           'Offset to module name data
GET #1, 25, NumObjects                          'Number of objects
GET #1, , ObjData&                              'Offset to object info. data
GET #1, 35, TypeNameNdx&                        'Offset to object index data
                                                '  sorted by type and name.

CLOSE #1

OPEN FileName$ FOR INPUT AS #1                  're-open file for input
SEEK #1, ModuleData& + 1                        'Seek to maodule name data

REDIM Module$(NumModules)                       'Make an array to hold module
FOR N = 1 TO NumModules                         '  names and then read them.
   INPUT #1, Module$(N)
NEXT

CLOSE #1

'~~~~~ Open the object database twice. First handle reads object information.
'      The second handle reads from the index portion.
OPEN FileName$ FOR BINARY AS #1                 'Re-open the file in binary

OPEN FileName$ FOR BINARY AS #3                 'Open it again for index

Record = 1                                      'Object database rec. pointer

'~~~~~ See how much memory we have to buffer lines of source.
MaxEls = FRE(-1) \ (BufWidth + 2&) - 1          'How many buffer lines.
MaxSize& = MaxEls * CLNG(BufWidth)              'How much memory.


'~~~~~ Read all source in each module.
FOR Module = 1 TO NumModules                    'Read each module

   SEEK #3, TypeNameNdx& + 1                    'Move to start of Obj. index

   REDIM B(1 TO MaxEls) AS Buffer               'Create the source buffer

   BPtr = 1                                     'Point to first element
   Modified = False                             'Nothing changed yet
   Found = True                                 'Flag that we found something

   OPEN Module$(Module) FOR INPUT AS #2         'Open the source module
   IF LOF(2) > MaxSize& THEN                    'Will it fit in memory?
      PRINT "Not enough memory!"
      END
   END IF
   'CLOSE #2

   DO UNTIL EOF(2)

      '~~~~~ Get the next line number to find in the source.
      IF Found THEN                             'Flag set to get another L #?
         DO WHILE Record <= NumObjects
            GET #3, , ObjRec&                   'Read object number from index
                                                '  then the object itself
            GET #1, (ObjRec& - 1) * LEN(Obj) + ObjData& + 1, Obj
            Record = Record + 1
                                                'Is it an unreferenced label?
            IF Obj.OType = Label AND Obj.NumRefs <= 1 THEN
               IF Obj.DefModInc = Module THEN   'From this module?
                                                'Make string to search source
                  Search$ = UCASE$(LTRIM$(RTRIM$(Obj.OName)))
                  Found = False                 'We havn't found it in source
                  EXIT DO                       '  yet.  Bail out
               END IF
            ELSEIF Obj.OType > Label THEN       'Did we read past label data?
               Record = NumObjects + 1          'Set counter to past end
            END IF
         LOOP
      END IF

      'Source$ = BufIn$(Module$(Module), Done)   'Read next line of source
      'IF Done THEN EXIT DO
      Source$ = ""
      DO
         LINE INPUT #2, Temp$
         Source$ = Source$ + Temp$
      LOOP WHILE ERR = 83
      Temp$ = ""

      '~~~~~ If there is a line number to find, look for it in the source
      IF NOT Found THEN                         'See if its in this line.
         IF INSTR(UCASE$(Source$), Search$) = 1 THEN
            IF INSTR(" " + CHR$(9), MID$(Source$, LEN(Search$) + 1, 1)) THEN
               LSET Search$ = ""                'Erase it if it is
               MID$(Source$, 1) = Search$
               Source$ = RTRIM$(Source$)
               Modified = True                  'We modified it so set flag
               Found = True                     'We also found the line
            END IF
         END IF
      END IF

      '~~~~~ Save the source line to the buffer.
      SLength = LEN(Source$) + 2                'Calc this line's length
                                                'Will it fit in this bufffer
      IF B(BPtr).Length + CLNG(SLength) > BufWidth THEN
         BPtr = BPtr + 1                        '  line?  Yes bump line #
      END IF
                                                'Put source line in buffer
      MID$(B(BPtr).Buffer, B(BPtr).Length + 1) = Source$
      MID$(B(BPtr).Buffer, B(BPtr).Length + SLength - 1) = CrLf$
      B(BPtr).Length = B(BPtr).Length + SLength 'Adjust the buffered length

   LOOP

   CLOSE 2                                      'Close the source file

   '~~~~~ Write out the new file if changes were made above.
   IF Modified THEN                             'See if the backup file exists
      BakFile$ = ChangeExtension$(Module$(Module), ".BAK")
      IF PDQExist(BakFile$) THEN
         PRINT "Backup file `"; BakFile$; "' already exists.  Overwrite it [Y/N]? ";
         DO
            Q$ = UCASE$(INKEY$)
         LOOP UNTIL INSTR("~YN", Q$) > 1
         PRINT Q$

         IF Q$ = "Y" THEN                       'Should we erase it?
            KILL BakFile$                       'Erase the existing file.
            NAME Module$(Module) AS BakFile$    'Rename the original to the
         END IF                                 '  backup name.
      ELSE
         NAME Module$(Module) AS BakFile$
      END IF

      OPEN Module$(Module) FOR BINARY AS #2     'Open the original file name.

      FOR N = 1 TO BPtr                         'Write each buffered line
         BigSave 2, VARSEG(B(N).Buffer), VARPTR(B(N).Buffer), B(N).Length
         
         IF ERR THEN
            CLOSE #2
            PRINT "Error writing new file `"; Module$(Module); "'.  Program terminated."
            KILL Module$(Module)
            NAME BakFile$ AS Module$(Module)
            END
         END IF
      NEXT

      CLOSE #2
   ELSE                                         'Nothing changed?  Tell em
      PRINT "`"; Module$(Module); "' was not modified."
   END IF

NEXT

FUNCTION ChangeExtension$ (FileName$, Extension$) STATIC

   Per = LEN(FileName$) + 1
   FOR N = Per - 1 TO Per - 4 STEP -1
      IF N THEN
         Temp = ASC(MID$(FileName$, N, 1))
         IF Temp = 46 THEN
            Per = N
            EXIT FOR
         ELSEIF Temp = 92 THEN
            EXIT FOR
         END IF
      END IF
   NEXT

   ChangeExtension$ = LEFT$(FileName$, Per - 1) + Extension$

END FUNCTION

