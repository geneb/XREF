'ÚÄÄÄÄÄ´ STR2CODE.BAS - Object String maker utility for BASIC programs ÃÄÄÄÄÄ¿
'³                                                                           ³
'³Copyright (c) 1990 - 1991 Crescent Software                                ³
'³Written by Paul Passarelli                                                 ³
'³with a little help from:                                                   ³
'³      Anders Ericsson  for identifying the need for this utility           ³
'³      Chris May  for the concept of String$() to Object File               ³
'³      Don Malin's  MakeQLB                                                 ³
'³                                                                           ³
'³                                                                           ³
'³      These are the six parameters for STR2CODE explained in order:        ³
'³                                                                           ³
'³Note:                                                                      ³
'³      As with LIB and LINK, a semicolon (;) can be used to force           ³
'³      STR2CODE to use its defaults for the remaining paramerers.           ³
'³                                                                           ³
'³                                                                           ³
'³      SourceFile -- A program file you wish to extract strings from        ³
'³                    The default extension is ".BAS".                       ³
'³                    This supplies the default for the other files.         ³
'³                    This file must be saved in Text format.                ³
'³                                                                           ³
'³      TargetFile -- A substitute program file containing the Function      ³
'³                    replacements of the literal strings in SourceFile.     ³
'³                    The default extension is ".BA$".                       ³
'³                                                                           ³
'³      ObjectFile -- BINARY file in Microsoft / Intel Object Module Format  ³
'³                    will contain the Logical Enumerated Data of the literal³
'³                    strings from SourceFile assigned to a CODE segment.    ³
'³                    The default extension is ".OB$".                       ³
'³                                                                           ³
'³     IncludeFile -- ASCII file will show each extracted string and the     ³
'³                    Function value to which it is assigned.  Along with    ³
'³                    instructions to use this file in the QB environment.   ³
'³                    The default extension is ".BI$"                        ³
'³                                                                           ³
'³     ShortString -- The default value is 5  Range [1 to 255]               ³
'³                    A number indicating the shortest string to extract.    ³
'³                    Since each substitution uses four bytes in DGroup      ³
'³                    nothing is gained in replacing shorter strings.        ³
'³                                                                           ³
'³     MaxLiterals -- The default value is 750  Range [1 to 8192]            ³
'³                    This is the maximum number of literal strings you      ³
'³                    expect to extract from this file.  If you choose the   ³
'³                    default value, and it is exceeded, STR2CODE will       ³
'³                    calculate a new value and re invoke itself.  It is     ³
'³                    rarely nessasary to specify this value.                ³
'³                                                                           ³
'³                                                                           ³
'³ STR2CODE SourceFile[.BAS], [TargetFile[.BA$]], [ObjectFile[.OB$]],_       ³
'³          [IncludeFile[.BI$]], [ShortString], [MaxLiterals] [;]            ³
'³                                                                           ³
'³ Example:                                                                  ³
'³      STR2CODE MyProg, , , ,12;                                            ³
'³                                                                           ³
'³                                                                           ³
'³      Later you may quickly re-create a new Object file by using:          ³
'³                                                                           ³
'³ STR2CODE IncludeFile.BI$, NUL, , NUL;                                     ³
'³                                                                           ³
'³      After editing the extracted strings.                                 ³
'³                                                                           ³
'³                                                                           ³
'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
'³Compile STR2CODE.BAS and Link as follows:                                  ³
'³>BC /o/s str2code;                                                         ³
'³                                                                           ³
'³>LINK /e/noe str2code [nocom][noedit][noevent][nofltin][nolpt]_            ³
'³             [smallerr][tscnionr],,nul,pro;                                ³
'³                                                                           ³
'³      [] = optional items. "smallerr" is available with BASIC 6 and 7 PDS, ³
'³      "no" object files may be included with your compiler.                ³
'³                                                                           ³
'³  -- or --                                                                 ³
'³                                                                           ³
'³>LINK /e/nod/noe str2code [_noval][str61500],,nul,[basic7]PDQ PRO;         ³
'³                                                                           ³
'³      The P.D.Q. version will be ÷14K smaller.                             ³
'³                                                                           ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
DEFINT A-Z

'Basic Subs
DECLARE SUB FatalErr (Message$)
DECLARE SUB GetParms (Source$, Target$, Object$, Include$, CutOff%, MaxLiterals%)
DECLARE SUB ExtractStr (Source$, Target$, Include$, CutOff%, KnownStrings%, Substitutions%, ExcessStrings%)
DECLARE SUB Str2Obj (Object$, NumEls%, NumBytes&, BaseName$, Literal$())
DECLARE SUB WriteInclude (Include$, KnownStrings%)

'Basic Functions
DECLARE FUNCTION CommentEOL% (Work$)
DECLARE FUNCTION SkipKeyWord% (Test$, EndOfLine%)
DECLARE FUNCTION UserInp$ (NoSemi%, Prompt$, Default$)

'QuickPak Professional Subs
DECLARE SUB QPrintRC (Text$, Row%, Col%, Colr%)

'QuickPak Professional Functions
DECLARE FUNCTION ASCII% (Character$)
DECLARE FUNCTION CheckSum% (A$)
DECLARE FUNCTION ErrorMsg$ (ErrNum%)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FindExact% (BYVAL VarPointer, NumEls%, Search$)
DECLARE FUNCTION InCount% (Source$, Search$)
DECLARE FUNCTION MinInt% (A%, B%)
DECLARE FUNCTION MaxInt% (A%, B%)
DECLARE FUNCTION Null% (Test$)
DECLARE FUNCTION QInstr2% (Start%, Source$, Search$)
DECLARE FUNCTION QInstrB% (Start%, Source$, Search$)
DECLARE FUNCTION QPValI% (Num$)
DECLARE FUNCTION Valid% (FileName$)



'Basic DEF FN
DEF fnNoPath$ (FileSpec$)
    Bug% = QInstrB%(-1, FileSpec$, "\")      'for a bug in BC7
    fnNoPath$ = MID$(FileSpec$, 1 + MaxInt%(QInstrB%(-1, FileSpec$, ":"), Bug%))
END DEF

DEF fnNoExtn$ (FileSpec$)
    Bug% = LEN(FileSpec$) * -(INSTR(QInstrB%(-1, FileSpec$, "\") + 1, FileSpec$, ".") = 0)
    fnNoExtn$ = LEFT$(FileSpec$, MaxInt%(INSTR(QInstrB%(-1, FileSpec$, "\") + 1, FileSpec$, ".") - 1, Bug%))
END DEF

DEF fnChkExt$ (FileSpec$, DefaultXtn$)
    Bug% = -4 * (INSTR(QInstrB%(-1, FileSpec$, "\") + 1, FileSpec$, ".") = 0)
    fnChkExt$ = FileSpec$ + LEFT$(DefaultXtn$, Bug%)
END DEF


DEFINT A-Z
'----- Print Banner
PRINT
PRINT "Literal String Conversion Utility  Version 1.05"
PRINT "STR2CODE  Copyright (c) 1990-1991 Crescent Software, Inc."
PRINT "32 Seventy Acres, West Redding, CT 06896  (203)438-5300"
PRINT


DIM SHARED BaseName$
DIM SHARED Quote$

Quote$ = CHR$(34)
Report$ = SPACE$(6)


DIM SHARED KeyWord$(4), KeyWord%(4)
KeyWord$(1) = "CONST"
KeyWord$(2) = "DATA"
KeyWord$(3) = "INPUT"
KeyWord$(4) = "ALIAS"


'----- Get input parameters from COMMAND$ or by prompting the User
CALL GetParms(Source$, Target$, Object$, Include$, MinLength%, MaxLiterals%)

Process:        'this is the re-entry point for the 2nd invocation

IF (FRE(BaseName$) - (1026 + 256)) \ 4 < MaxLiterals% THEN
   FatalErr "Insufficient Memory to process " + STR$(MaxLiterals%) + " Strings"
END IF
REDIM SHARED Literal$(1 TO MaxLiterals)         'to hold all the "Strings"


'----- Read Source.BAS and Write Target.BA$, return info
CALL ExtractStr(Source$, Target$, Include$, MinLength, KnownStrings, Substitutions, ExcessStrings)


'----- Create the Object file
CALL Str2Obj(Object$, KnownStrings, StrBytes&, BaseName$, Literal$())


'----- Write the Include File
CALL WriteInclude(Include$, KnownStrings)


'----- Report warning for un-processed lines
IF SkipKeyWord%("", 0) THEN STOP

FOR I = 1 TO UBOUND(KeyWord, 1)
    IF KeyWord(I) THEN
       PRINT
       RSET Report$ = STR$(KeyWord(I))
       PRINT Report$; " Lines containing "; KeyWord$(I); " statements were not processed."
    END IF
NEXT



'----- Report the results of the extraction / conversion
PRINT
RSET Report$ = STR$(StrBytes&)
PRINT Report$; " Bytes of Literal String data were extracted from DGroup."

RSET Report$ = STR$(KnownStrings)
PRINT Report$; " Unique literal strings were extracted."

RSET Report$ = STR$(Substitutions)
PRINT Report$; " Literal strings were substituted by their function equivalents."

RSET Report$ = STR$(StrBytes& - Substitutions * 4)
PRINT Report$; " Bytes of DGroup were freed."

QPrintRC SPACE$(80), CSRLIN, 1, -1      '.001 .007



'----- Report incomplete extraction if nessasary.
IF ExcessStrings% THEN
   BEEP
   PRINT : PRINT
   RSET Report$ = STR$(ExcessStrings%)
   PRINT Report$; " Strings were encountered after `MaxLiterals' Strings were extracted."

   IF MaxLiterals = 750 THEN    'our default value
      '----- Calculate the maximum reprocessing quantity
      MaxLiterals = MaxLiterals + ExcessStrings
      PRINT SPC(5); " REPROCESSING  with MaxLiterals ="; MaxLiterals
      PRINT : PRINT

      '----- Clear the previous totals
      ERASE Literal$, KeyWord%
      KnownStrings = 0
      Substitutions = 0
      ExcessStrings = 0
      StrBytes& = 0

      GOTO Process
   END IF

   '----- Print alternate message for user defined explicit MaxLiterals.
   PRINT SPC(5); " You may run STR2CODE again using a larger number for `MaxLiterals'."
   PRINT
   END 1
END IF

END

'Date           Version Comment:
'02/04/91       .000    Original
'02/06/91               Tighten code
'02/07/91       .001    Improved behavior for output redirection
'03/13/91       .003    Identified "INPUT" as a No Extract key word
'03/14/91       .004    Separate CS$ Function from key word ie PRINT"No Space"
'03/14/91       .005    Report excess strings not processed, and re run
'                       Version Number 1.02
'08/05/91       .006    Change NumBytes% to Numbytes& in Str2Obj.
'                       Change EnDatOff% to EnDatOff& in Str2Obj.
'                       Change length of Report$ from 5 to 6.
'08/07/91       .007    Added LOCATE statements to support [tscnionr.obj]
'                       Change QPrint0 to QPrintRC as above.
'                       Note: for PDQ STR61500 was used, STR61750 is too big.
'                       Version Number 1.03
'08/19/19       .008    Add "ALIAS" to the KeyWord List     (G.H. 23838)
'                       Version Number 1.04
'12/23/91       .009    Removed GRPDEF from Str2Obj to eliminate error message
'                       for users of RTLINK.  It was unnessasary anyhow.       


FUNCTION CommentEOL% (Test$) STATIC
'----- Finds the location of the comment portion (if any) of Test$,
'      returns this as an End of Line position.

    EoL = 0
    DO
       Start = EoL + 1
       Apostr = 32767 - INSTR(Start, Test$, "'")
       'IF Apostr = 0 THEN Apostr = 32767
       Remark = 32767 - INSTR(Start, UCASE$(Test$), "REM")
       'IF Remark = 0 THEN Remark = 32767
       LinCon = 32767 - INSTR(Start, Test$, "_")
       'IF LinCon = 0 THEN LinCon = 32767

       EoL = MinInt%(MinInt%(Apostr, Remark), LinCon)

       IF EoL = 32767 THEN EXIT DO      'there are no comments on this line
       EoL = 32767 - EoL

       IF InCount%(LEFT$(Test$, EoL), Quote$) MOD 2 THEN
          'part of a quoted string.
       ELSEIF EoL = 32767 - Apostr OR EoL = 32767 - LinCon THEN
          'the EOL is from here to the end of the line
          EXIT DO
       ELSEIF EoL = Remark THEN
          'look for chars following "REM" that make it CODE!
          SELECT CASE ASCII%(MID$(Test$, Remark + 3, 1))
             CASE 36, 46                        '"$" and  "." are code.
                ' Why not "%", "&", "!", or "#"?  I don't know?
             CASE 48 TO 57                      'the numeric digits
             CASE 65 TO 90                      'the upper case characters
             CASE 97 TO 122                     'the lower case characters
             CASE ELSE
                EXIT DO                         'now its a comment
          END SELECT
       END IF
    LOOP
    CommentEOL% = EoL

    IF Null%(LEFT$(Test$, EoL - 1)) THEN        'the whole line is a comment
       CommentEOL% = 1                          'for faster service
    END IF

END FUNCTION

SUB ExtractStr (Source$, Target$, Include$, CutOff%, KnownStrings%, Substitutions%, ExcessStrings%) STATIC
'----- Identifys all occurrences of literal strings, substituting the
'      function.  Duplicates are checked for in Literal$().

SHARED BaseName$, Literal$()

   
    '----- Assign some values
    MaxLiterals% = UBOUND(Literal$, 1)  'if we reach this we bail out
    KnownStrings% = Zero                'make sure these start at 0
    Substitutions% = Zero
  

    '----- Test for binary file (Fast Load and Save)
    Test$ = " "
    OPEN Source$ FOR BINARY AS #1       'Open the module
    GET #1, , Test$
    CLOSE #1
    IF ASC(Test$) = 252 THEN
       FatalErr "Cannot process QuickBASIC - Fast Load and Save files!"
    END IF
   
   
    '----- Open the Source and destination files
    OPEN Source$ FOR INPUT AS #1 LEN = 512
    OPEN Target$ FOR OUTPUT AS #2 LEN = 256
    IF ERR THEN FatalErr "Cannot Create " + Target$
   

    '----- Write some header info to the new ".BA$" file
    PRINT #2, "Declare Function "; BaseName$; "CS$ (BYVAL LiteralString%)   'Converted "; DATE$
    IF Include$ <> "NUL" THEN
       PRINT #2, "''$INCLUDE: '"; Include$; "'   'Move this line and Un-Rem for testing in QB"
    END IF
    PRINT #2,
    IF ERR THEN FatalErr ""
   

    '----- Process the Source file a line at a time
    DO
       LINE INPUT #1, Output$                   'get a line of code
   
       IF LEN(Output$) = 0 THEN                 'see if it is white space
          GOTO Send
       ELSEIF INSTR(Output$, Quote$) = 0 THEN   'are there any literals?
          GOTO Send
       ELSEIF INSTR(Output$, "'$N") THEN        '$N -- Do Not Extract!
          GOTO Send
       END IF
 
       EoL = CommentEOL%(Output$)               'find the comment (if any)
       IF EoL = 1 THEN                          'the whole line is a comment
          GOTO Send
       ELSEIF INSTR(Output$, Quote$) >= EoL THEN        'any literals left?
          GOTO Send
       ELSEIF SkipKeyWord%(Output$, EoL) THEN   '"CONST" or "DATA" or "INPUT"
          GOTO Send
       ELSE
          Work$ = Output$                               'we have something to do.
          Output$ = ""
       END IF
     
       Start = 1                                        'find the literals
       DO WHILE INSTR(Start, Work$, Quote$)
          Q1 = INSTR(Start, Work$, Quote$)
          IF Q1 >= EoL THEN EXIT DO                     'part of a comment
          Q2 = INSTR(Q1 + 1, Work$, Quote$)
          IF (Q2 = 0) OR (Q2 >= EoL) GOTO Send          'the odd Quote "
          Length = Q2 - Q1 - 1
          IF Length >= CutOff THEN                      'found an extractable

             '--------------------------------------------------------
             'Isolate the line into 3 portions, Left, Right, and what we want
             '--------------------------------------------------------
             EoL = EoL - Q2
             LeftSide$ = LEFT$(Work$, Q1 - 1)
             RightSide$ = MID$(Work$, Q2 + 1)
             this$ = MID$(Work$, Q1 + 1, Length)

             '--------------------------------------------------------
             'Is this a new or previously encountered string?
             '--------------------------------------------------------
             Element = 1 + FindExact%(VARPTR(Literal$(1)), (KnownStrings), this$)
             IF Element = 0 THEN                        'new literal string.
                
                IF KnownStrings < MaxLiterals THEN
                   '--------------------------------------------------
                   'Add this string to the array, allow it to be substituted
                   '--------------------------------------------------
                   KnownStrings = KnownStrings + 1
                   Element = KnownStrings
                   Literal$(Element) = this$

                ELSE
                   '--------------------------------------------------
                   'There is no room for this string.  Count it, do not substitute.
                   '--------------------------------------------------
                   ExcessStrings = ExcessStrings + 1    '.005
                   S2C$ = Quote$ + this$ + Quote$
                END IF
             END IF

             IF Element THEN
                '-----------------------------------------------------
                'The string exists in the array, proceed with the substitition
                '-----------------------------------------------------
                S2C$ = " " + BaseName$ + "CS$(" + LTRIM$(STR$(Element)) + ")"      '.004
                Substitutions = Substitutions + 1
             END IF

             '--------------------------------------------------------
             'Add the extracted or unextracted string to the output buffer
             '--------------------------------------------------------
             Output$ = Output$ + LeftSide$ + S2C$
             

             '--------------------------------------------------------
             'See if there is more work do do on this line
             '--------------------------------------------------------
             IF LEN(RightSide$) THEN
                Work$ = RightSide$
                Start = 1
             ELSE
                GOTO Send
             END IF

          ELSE
             '--------------------------------------------------------
             'This string is shorter than the cutoff length given.  Look
             'for more literals in this line.
             '--------------------------------------------------------
             Start = Q2 + 1

          END IF

       LOOP

       Output$ = Output$ + Work$

Send:  PRINT #2, Output$
       IF ERR THEN FatalErr ""
   
    LOOP WHILE NOT EOF(1)
   
    CLOSE #1, 2

END SUB

SUB FatalErr (Message$) STATIC
'----- When something goes wrong!

    BEEP                                'get their attention
    PRINT                               'make some room

    IF LEN(Message$) = 0 THEN
       Message$ = ErrorMsg$(ERR)
    END IF

    'this can't be redirected           '.007
    QPrintRC Message$ + "  Program terminated." + SPACE$(16), CSRLIN, 1, -1

    PRINT
    END 1                               'set the ErrorLevel for MAKE & BATCH

END SUB

SUB GetParms (Source$, Target$, Object$, Include$, CutOff%, MaxLiterals%) STATIC
'----- Examines COMMAND$ and provides default responses where required.
'      Prompts the user for parameters not given by default.

SHARED BaseName$

    Source$ = ".BAS"

    NoSemi = 1
    

    '~~~~~ Get command line parameters from COMMAND$
    IF LEN(COMMAND$) THEN

       Param = 1
       P = 1
       CMD$ = COMMAND$

       IF CMD$ = "STR2CODE" THEN
          'My COMMAND$ to rebuild from QBX:
          CMD$ = "\bas\notpro\str2code, \bas\notpro\str2code, \objb\str2code, \inc\str2code, 5, 58;"
       END IF
      
       DO
           '~~~~~ Parse out parameter looking for [,] or [;] or EOL
           PC = INSTR(P, CMD$, ",")
           IF PC = 0 THEN PC = INSTR(P, CMD$, ";")
           IF PC = 0 THEN PC = LEN(CMD$) + 1
           Temp$ = UCASE$(LTRIM$(RTRIM$(MID$(CMD$, P, PC - P))))
          
           '~~~~~ Assign parameters
           SELECT CASE Param
              CASE 1                            'Source.BAS File Name
                 Source$ = Temp$
                 IF LEN(Source$) = 0 THEN FatalErr "No Source File Name!"
                 BaseName$ = fnNoPath$(fnNoExtn$(Source$))  'the name of the Source file
                 NoSemi = 1
              CASE 2                            'New Target.BA$ file
                 Target$ = Temp$
                 NoSemi = 2
              CASE 3                            'Coded string object file
                 Object$ = Temp$
                 NoSemi = 3
              CASE 4                            'List file name (for re-run)
                 Include$ = Temp$
                 NoSemi = 4
              CASE 5                            'Shortest string to process
                 CutOff$ = Temp$
                 NoSemi = 5
              CASE 6                            'Maximum number of strings
                 MaxLiterals$ = Temp$
                 NoSemi = 6
              CASE ELSE
           END SELECT

           Param = Param + 1                    'Bump parameter number
           P = PC + 1
       LOOP UNTIL PC >= LEN(CMD$) OR MID$(CMD$, PC, 1) = ";"   'Get another

    END IF


    '----- Prompt User for parameters
    IF INSTR(CMD$, ";") = 0 THEN                'No semicolon, prompt User
       LOCATE CSRLIN, POS(0), 1                 '.007 for bottom line
       DO
          ON NoSemi GOSUB GetSource, GetTarget, GetObject, GetListFile, GetCutOffValue, GetMaxLiterals
       LOOP WHILE NoSemi AND (NoSemi <= 6)
       PRINT
    END IF



    ErrDupFil$ = "Duplicate filenames: "        '1 copy of part of the message
    '----- Make any nessasary adjustments to the arguments obtained
   
    IF LEN(Source$) = 0 THEN FatalErr "No Source File Name!"
    Source$ = fnChkExt$(Source$, ".BAS")
    'IF NOT Valid%(Source$) THEN FatalErr "`" + Source$ + "' is not a valid file name!"
    IF NOT Exist%(Source$) THEN FatalErr "Source File Not Found"
   
    
    IF LEN(Target$) = 0 OR RIGHT$(Target$, 1) = "\" THEN
       Target$ = Target$ + BaseName$ + ".BA$"
    END IF
    Target$ = fnChkExt$(Target$, ".BA$")
    IF NOT Valid%(Target$) THEN FatalErr "`" + Target$ + "' is not a valid file name!"
    IF Target$ = Source$ THEN FatalErr ErrDupFil$ + "Target = Source"
    IF UCASE$(Target$) = "NUL.BA$" THEN Target$ = "NUL"


    IF LEN(Object$) = 0 OR RIGHT$(Object$, 1) = "\" THEN
       Object$ = Object$ + BaseName$ + ".OB$"
    END IF
    Object$ = fnChkExt$(Object$, ".OB$")
    IF NOT Valid%(Object$) THEN FatalErr "`" + Object$ + "' is not a valid file name!"
    IF Object$ = Source$ THEN FatalErr ErrDupFil$ + "Object = Source"
    IF Object$ = Target$ THEN FatalErr ErrDupFil$ + "Object = Target"
    IF UCASE$(Object$) = "NUL.OB$" THEN Object$ = "NUL"

   
    IF LEN(Include$) = 0 OR RIGHT$(Include$, 1) = "\" THEN
       Include$ = Include$ + BaseName$ + ".BI$"
    END IF
    Include$ = fnChkExt$(Include$, ".BI$")
    IF NOT Valid%(Include$) THEN FatalErr "`" + Include$ + "' is not a valid file name!"
    IF Include$ = Source$ THEN FatalErr ErrDupFil$ + "Include = Source"
    IF Include$ = Target$ THEN FatalErr ErrDupFil$ + "Include = Target"
    IF Include$ = Object$ THEN FatalErr ErrDupFil$ + "Include = Object"
    IF UCASE$(Include$) = "NUL.BI$" THEN Include$ = "NUL"

   
    CutOff% = QPValI(CutOff$)
    IF CutOff <= 0 THEN CutOff = 5
    IF CutOff >= 255 THEN CutOff = 255
    IF RIGHT$(Source$, 4) = ".BI$" THEN CutOff = 1
    

    MaxLiterals% = QPValI(MaxLiterals$)
    IF MaxLiterals >= 8192 THEN FatalErr "Cannot process >8192 strings"
    IF MaxLiterals <= 0 THEN MaxLiterals = 750

    'This can't be redirected           '.007
    QPrintRC "Processing:  " + Source$ + SPACE$(68 - LEN(Source$)), CSRLIN, 1, -1 '.001
    PRINT                               'this can

EXIT SUB




'~~~~~ Get Source file name
GetSource:
    Source$ = UserInp$(NoSemi, "Source File Name", Source$)
    IF Source$ = ".BAS" THEN FatalErr "No Source File Name!"
    BaseName$ = fnNoPath$(fnNoExtn$(Source$))
RETURN


'~~~~~ Get Target file name
GetTarget:
    IF LEN(Target$) = 0 OR RIGHT$(Target$, 1) = "\" THEN
       Target$ = Target$ + BaseName$ + ".BA$"
    END IF
    Target$ = UserInp$(NoSemi, "Target File Name", Target$)
RETURN


'~~~~~ Get Object file name
GetObject:
    IF LEN(Object$) = 0 OR RIGHT$(Object$, 1) = "\" THEN
       Object$ = Object$ + BaseName$ + ".OB$"
    END IF
    Object$ = UserInp$(NoSemi, "Object File Name", Object$)
RETURN


'~~~~~ Get Include file name
GetListFile:
    IF LEN(Include$) = 0 OR RIGHT$(Include$, 1) = "\" THEN
       Include$ = Include$ + BaseName$ + ".BI$"
    END IF
    Include$ = UserInp$(NoSemi, "Include / Listfile", Include$)
RETURN


GetCutOffValue:
    IF LEN(CutOff$) = 0 THEN
       CutOff$ = "5"
    END IF
    CutOff$ = UserInp$(NoSemi, "Ignore Strings Shorter than", CutOff$)
RETURN


GetMaxLiterals:
    IF LEN(MaxLiterals$) = 0 THEN
       MaxLiterals$ = "750"
    END IF
    MaxLiterals$ = UserInp$(NoSemi, "Maximum Literals", MaxLiterals$)
RETURN


END SUB

FUNCTION SkipKeyWord% (Work$, EoL%) STATIC
'----- Scans Work$ for the KEY words "CONST" & "DATA" & "INPUT".  Any
'      occurrence of either is counted, returning -1.  The line is simply
'      re -emitted.

SHARED KeyWord$(), KeyWord%()

    SkipKeyWord% = 0                    'assume no Conflicting Key Word

    FOR I = 1 TO UBOUND(KeyWord%, 1)    '.003
        KeyWord$ = KeyWord$(I)
        LenKey = LEN(KeyWord$)
   
        Start = 1
        DO WHILE QInstr2%(Start, Work$, KeyWord$)
           AKey = QInstr2%(Start, Work$, KeyWord$)
           IF AKey >= EoL THEN EXIT DO
           Start = AKey + LenKey
           PostChar = ASCII%(MID$(Work$, Start, 1))

           IF InCount%(LEFT$(Work$, AKey), Quote$) MOD 2 THEN
              'part of a quoted string.
           ELSEIF PostChar = 32 OR PostChar = 0 THEN
              IF AKey = 1 GOTO FoundOne
              PreChar = ASCII%(MID$(Work$, AKey - 1, 1))
              IF PreChar = 0 OR PreChar = 32 OR PreChar = 58 THEN
FoundOne:        SkipKeyWord% = -1
                 KeyWord%(I) = KeyWord%(I) + 1
                 EXIT DO
              END IF
           'ELSE part of a variable
           END IF
        LOOP
    NEXT

END FUNCTION

SUB Str2Obj (Object$, NumEls%, NumBytes&, BaseName$, Literal$()) STATIC
'----- Creates the Object File with the literal strings in a CODE segment.
' Based on Routine to create object file - CWM 8/1/1990
' modifications: Paul Passarelli
   

    CALL KILLFile(Object$)
    OPEN Object$ FOR BINARY AS #3
    IF ERR THEN FatalErr "Cannot Open " + Object$
   
    BaseLen = LEN(BaseName$)
 
    FOR I = 1 TO NumEls                         'total bytes in Literal$()
        NumBytes& = NumBytes& + LEN(Literal$(I))
    NEXT I
  
 
 
    '------------------------------------------------------------------------
    'THEADR record
    A$ = CHR$(128) + MKI$(BaseLen + 6)
    A$ = A$ + CHR$(BaseLen + 4) + LCASE$(BaseName$) + ".ASM"
    A$ = A$ + CHR$(CheckSum%(A$))
    PUT #3, , A$
    IF ERR GOTO ObjErr
 
 
    '------------------------------------------------------------------------
    'LNAMES record
    A$ = CHR$(150) + MKI$(BaseLen + 31)
    A$ = A$ + CHR$(0)                           'this is a Null Name
    A$ = A$ + CHR$(6) + "DGROUP"
    A$ = A$ + CHR$(BaseLen + 5) + UCASE$(BaseName$) + "_TEXT"
    A$ = A$ + CHR$(4) + "DATA"
    A$ = A$ + CHR$(4) + "CODE"
    A$ = A$ + CHR$(5) + "_DATA"
    A$ = A$ + CHR$(CheckSum%(A$))
    PUT #3, , A$
    IF ERR GOTO ObjErr
 
  
    '------------------------------------------------------------------------
    'First SEGDEF        .CODE
    A$ = CHR$(152) + MKI$(7)
    A$ = A$ + CHR$(72)
    A$ = A$ + LEFT$(MKL$(41& + NumBytes& + NumEls), 2) 'Segment Length
    A$ = A$ + CHR$(3) + CHR$(5) + CHR$(1)
    A$ = A$ + CHR$(CheckSum%(A$))
    PUT #3, , A$
    IF ERR GOTO ObjErr
 

    '-------------------------------------------------------------------------
    'GRPDEF record
'    A$ = CHR$(154) + MKI$(4)
'    A$ = A$ + CHR$(2) + CHR$(255) + CHR$(2)
'    A$ = A$ + CHR$(95)
'    PUT #3, , A$
'    IF ERR GOTO ObjErr
 
 
    '------------------------------------------------------------------------
    'ExtDef record
    A$ = CHR$(140) + MKI$(15)
    A$ = A$ + CHR$(12) + "STR2CODEKRNL" + CHR$(0)
    A$ = A$ + CHR$(220)
    PUT #3, , A$
    IF ERR GOTO ObjErr
 
 
    '------------------------------------------------------------------------
    'PUBDEF record
    A$ = CHR$(144) + MKI$(BaseLen + 9)
    A$ = A$ + CHR$(0) + CHR$(1)
    A$ = A$ + CHR$(BaseLen + 2) + UCASE$(BaseName$) + "CS"
    A$ = A$ + MKI$(0) + CHR$(0)
    A$ = A$ + CHR$(CheckSum%(A$))
    PUT #3, , A$
    IF ERR GOTO ObjErr

 
    '------------------------------------------------------------------------
    'COMENT record
    A$ = CHR$(136) + MKI$(4)
    A$ = A$ + CHR$(0) + CHR$(162) + CHR$(1)
    A$ = A$ + CHR$(209)
    PUT #3, , A$
    IF ERR GOTO ObjErr
 
 
    '------------------------------------------------------------------------
    'LEDATA record
    A$ = CHR$(160) + MKI$(45) + CHR$(1) + MKI$(0)
    '                Length     Seg Ndx   Enumerated Data Offset
 
    A$ = A$ + CHR$(139) + CHR$(220)                     'Mov  BX,SP
    A$ = A$ + CHR$(139) + CHR$(79) + CHR$(4)            'Mov  CX,[BX+4]
    A$ = A$ + CHR$(227) + CHR$(28)                      'Jcxz $+28
 
    A$ = A$ + CHR$(129) + CHR$(249) + MKI$(NumEls)      'Cmp  CX,NumEls
    A$ = A$ + CHR$(118) + CHR$(4)                       'Jbe  $+04
    A$ = A$ + CHR$(51) + CHR$(201)                      'Xor  CX,CX
    A$ = A$ + CHR$(235) + CHR$(18)                      'Jmp  $+18
 
    A$ = A$ + CHR$(252)                                 'Cld
    A$ = A$ + CHR$(190) + MKI$(0)       '1st FIXUPP     'Mov  SI,Offset NULL$
    A$ = A$ + CHR$(50) + CHR$(228)                      'Xor  AH,AH
    A$ = A$ + CHR$(73)                                  'Dec  CX
 
    A$ = A$ + CHR$(227) + CHR$(7)                       'Jcxz $+07
    A$ = A$ + CHR$(46) + CHR$(172)                      'Lods CS:Byte
    A$ = A$ + CHR$(3) + CHR$(240)                       'Add  SI,AX
    A$ = A$ + CHR$(73)                                  'Dec  CX
    A$ = A$ + CHR$(235) + CHR$(247)                     'Jmp  $-09

    A$ = A$ + CHR$(140) + CHR$(201)                     'Mov  CX,CS
    A$ = A$ + CHR$(234) + MKL$(0)       '2nd FIXUPP     'Jmp Far STR2CODEKRNL
                                                        
    A$ = A$ + CHR$(0)                                   'NULL$

    A$ = A$ + CHR$(CheckSum%(A$))
    PUT #3, , A$
    IF ERR GOTO ObjErr

    EnDatOff& = LEN(A$) - 8     'don't use LEDATA header or NULL & CheckSum
                                'keep tally of the bytes in the LEDATA record
 
 
    '------------------------------------------------------------------------
    'FIXUPP record
    A$ = CHR$(156) + MKI$(13)
    '1st FIXUPP
    A$ = A$ + CHR$(196) + CHR$(19) + CHR$(0) + CHR$(1) + CHR$(1) + MKI$(40)
    '2nd FIXUPP
    A$ = A$ + CHR$(204) + CHR$(36) + CHR$(38) + CHR$(1) + CHR$(1)
    A$ = A$ + CHR$(62)
    PUT #3, , A$
    IF ERR GOTO ObjErr
 
 
    '------------------------------------------------------------------------
    'another LEDATA record
    A$ = CHR$(160) + MKI$(1024) + CHR$(1) + LEFT$(MKL$(EnDatOff&), 2)
 
    Start = 1                                   'Start at element number 1
    DO
       B$ = ""
       FOR I = Start TO NumEls
           A$ = A$ + CHR$(LEN(Literal$(I))) + Literal$(I)
           IF LEN(A$) > 1026 THEN
              B$ = RIGHT$(A$, LEN(A$) - 1026)   'keep the excess
              A$ = LEFT$(A$, 1026)              'work with 1026 bytes
              EXIT FOR
           END IF
       NEXT I
       Start = I + 1                            'resume the next Literal$()
    

       MID$(A$, 2, 2) = MKI$(LEN(A$) - 2)
       A$ = A$ + CHR$(CheckSum%(A$))
       PUT #3, , A$
       IF ERR GOTO ObjErr

    
       IF Start < NumEls OR LEN(B$) THEN        'do we have more to do yet?
          EnDatOff& = EnDatOff& + 1020
          'yet another LEDATA record
          A$ = CHR$(160) + MKI$(1024) + CHR$(1) + LEFT$(MKL$(EnDatOff&), 2)
       END IF
       A$ = A$ + B$
    LOOP WHILE Start < NumEls OR LEN(B$)

 
    '------------------------------------------------------------------------
    'MODEND record
    A$ = CHR$(138) + MKI$(2)
    A$ = A$ + CHR$(0)
    A$ = A$ + CHR$(116)
    PUT #3, , A$
    IF ERR GOTO ObjErr
   
    CLOSE #3
    EXIT SUB

ObjErr:
    FatalErr ""



'----------------------------------------------------------------------------
' Equivalent assembly source code
'----------------------------------------------------------------------------
'.MODEL Medium,BASIC
'
'    NumEls     Equ     7FFF
'
'    Extrn      Str2CodeKrnl: Proc
'
'.CODE
'BaseName Proc
'    Mov    BX,SP                ;use BX for the 1 stack frame operation
'    Mov    CX,[BX+4]            ;CX gets the string number to reproduce
'    Jcxz   NullString           ;if they ask for the zeroth its null
'
'    Cmp    CX,NumEls            ;7FFFh forces a word compare
'    Jbe    FindString
'    Xor    CX,CX                ;if they ask for more than we have its null
'    Jmp Short NullString
'
' FindString:
'    Cld                         ;do forward string moves please
'    Mov    SI,Offset StrTable   ;point to the first extracted string
'    Xor    AH,AH                ;no string longer than 255
'    Dec    CX                   ;reduce base 1 to base 0
'
' NextEl:
'    Jcxz   HaveEl               ;if CX = 0 then we are on the right element
'    Lods   CS:StrTable          ;load the current string length into AL
'    Add    SI,AX                ;move ahead to the next element
'    Dec    CX                   ;reduce counter by one
'    Jmp    NextEl               ;continue til we get correct element
'
' HaveEl:
'    Mov    CX,CS                ;CX:SI points to the desired string
' NullString:
'    Jmp    Str2CodeKrnl         ;this Proc will return to BASIC
'
' StrTable  DB   0               ;the extracted strings will begin here
'
'BaseName EndP
'END
END SUB

FUNCTION UserInp$ (NoSemi, Prompt$, Default$) STATIC
'----- Prompts the user for a single item, showing the suggested default.

    Prompt$ = Prompt$ + " [" + Default$ + "]: "
    QPrintRC Prompt$ + SPACE$(80 - LEN(Prompt$)), CSRLIN, 1, -1 '.007
    PRINT Prompt$;                              'show default if Enter is hit
    LOCATE , LEN(Prompt$)                       '.007
    LINE INPUT ""; Temp$                        'allow entry of new info

    Temp$ = UCASE$(LTRIM$(RTRIM$(Temp$)))       'clean it up
   
    NoSemi = NoSemi + 1                         '1 more param was obtained
    IF RIGHT$(Temp$, 1) = ";" THEN
       Temp$ = RTRIM$(LEFT$(Temp$, LEN(Temp$) - 1))
       NoSemi = 0                               'so we can default the rest
    END IF
    IF LEN(Temp$) = 0 THEN Temp$ = Default$     'they decided to accept
   
    UserInp$ = Temp$                            'asign the function output

END FUNCTION

SUB WriteInclude (Include$, KnownStrings%) STATIC
'----- Writes the Include file showing all known strings.
'      Also provides the Redim Shared array for use in QB.

SHARED BaseName$, Literal$()

    OPEN Include$ FOR OUTPUT AS #4
    IF ERR THEN FatalErr "Cannot Create " + Include$

    PRINT #4, "'***** If you are using this file be sure to rem out the the line"
    PRINT #4, "' `Declare Function "; BaseName$; "CS$ (BYVAL LiteralString%)'"
    PRINT #4,
    PRINT #4, "Redim Shared "; BaseName$; "CS$(1 to "; STR$(KnownStrings); ")  'the number of strings replaced"
    PRINT #4,
    FOR LineNo = 1 TO KnownStrings
        PRINT #4, BaseName$; "CS$("; STR$(LineNo); ") = "; Quote$; Literal$(LineNo); Quote$
        IF ERR THEN FatalErr ""
    NEXT

    CLOSE #4
END SUB

