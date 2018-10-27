'****************************************************************************
'  TEXTOUT.BAS - Source for TEXTOUT.COM
'  by Don Malin
'  (c) 1990, 1991 Crescent Software, Inc.
'
'  Purpose: Extracts quoted strings and optionaly comments from a BASIC
'  source file and writes them to a file with the same base name as the
'  source file but with an ".SPL" extension.  This file can be run through
'  any spell checking program or used to translate program messages into
'  another language.
'
'  From DOS type:  TEXTOUT FileName[.BAS] [/c]
'
'  "/c" specifies extracting comments
'----------------------------------------------------------------------------
'  Compile and link with P.D.Q. and QuickPak Pro as follows:
'  bc TEXTOUT /O;
'  link /NOD TEXTOUT,, NUL, PDQ PRO;
'  exe2com TEXTOUT
'****************************************************************************
DEFINT A-Z

DECLARE FUNCTION Null% (Text$)                  'Pro routine determines if a
                                                '  string is blank
'$INCLUDE: '\pdq\pdqdecl.bas'
'$LIST OFF
Qt$ = CHR$(34)                                  'Quotation mark character "

PRINT "Source Text Extraction Program"
PRINT "(c) 1990, 1991 Crescent Software, Inc."
PRINT


'~~~~~ Get the names of files to work with
Source$ = COMMAND$                              'Copy the command line arg.

GOSUB FixSourceName                             'Extract "/C" command switch

IF LEN(Source$) = 0 THEN                        'No file given?
   PRINT "Source file name: ";                  'Ask the user for a name
   PDQInput Source$
   GOSUB FixSourceName
   IF LEN(Source$) = 0 THEN END
END IF


Per = INSTR(Source$, ".")                       'Look for a period in name
IF Per THEN                                     'Create the names for files
   Dest$ = LEFT$(Source$, Per) + "SPL"
ELSE
   Dest$ = Source$ + ".SPL"
   Source$ = Source$ + ".BAS"
END IF


'~~~~~ Open the needed files
OPEN Source$ FOR INPUT AS #1                    'Open the Source file (.BAS)
OPEN Dest$ FOR OUTPUT AS #2                     'Open the Text file (.SPL)

PRINT "Extracting text from " + Source$ + " to " + Dest$ + " ...";

CurLine = 0                                     'Initialize line number


'~~~~~ Read the source file until done
DO UNTIL EOF(1)

   Text$ = ""                                   'Initialize the source line $
   DO
      LINE INPUT #1, Buf$                       'Read a line of source code
      Text$ = Text$ + Buf$                      'Add chars. to Source line
   LOOP WHILE RIGHT$(Text$, 1) = "_"            'Go back for more if needed

   CurLine = CurLine + 1                        'Bump the source line number

   RemPos = 1                                   'Find remarks or end of line
   GOSUB FindRemark

   '~~~~~ Look for quoted strings to extract
   Quote = 0                                    'No quote yet
   DO
      Quote = INSTR(Quote + 1, Text$, Qt$)      'Look for left quote
                                                'If there is one and it's to
      IF Quote > 0 AND Quote < Remark THEN      '  the left of remarks,
         Quote2 = INSTR(Quote + 1, Text$, Qt$)  '  find the right-hand quote
         IF Quote2 > Remark THEN                'If there is a remark char.
            RemPos = Quote2                     '  inside quotes, find next
            GOSUB FindRemark                    '  remark char.
         END IF
         IF Quote2 = 0 THEN Quote2 = Remark     'No right quote, use length
        
         IF Quote2 - Quote - 1 THEN             'If it's not a NUL string ("")
                                                'Extract the quoted text
            QStr$ = MID$(Text$, Quote + 1, Quote2 - Quote - 1)
            'IF NOT Null(QStr$) THEN             'If it's not blank, write it
                                                '  to the ".SPL" file
               PRINT #2, STR$(CurLine); ": "; Qt$; QStr$; Qt$
            'END IF
         END IF

         Quote = Quote2                         'Set position to look for
                                                '  another quoted string.
      END IF
   LOOP WHILE Quote

   IF RemSwitch THEN
      IF Remark < LEN(Text$) THEN
         PRINT #2, STR$(CurLine); ": "; MID$(Text$, Remark)
      END IF
   END IF

LOOP


LOCATE CSRLIN, 1                                'Tell the user what we did.
PRINT Dest$ + " has been created.                                       "

END


'~~~~~ Find remarks or end of line
FindRemark:

   Remark = INSTR(RemPos, Text$, "'")
   IF Remark = 0 THEN Remark = INSTR(RemPos, UCASE$(Text$), "REM")
   IF Remark = 0 THEN Remark = LEN(Text$)

RETURN


'~~~~~ Look for "/C" switch and trim the source file name
FixSourceName:

   RemSwitch = INSTR(UCASE$(Source$), "/C")
   IF RemSwitch THEN
      Source$ = LEFT$(Source$, RemSwitch - 1) + MID$(Source$, RemSwitch + 2)
      Source$ = UCASE$(LTRIM$(RTRIM$(Source$)))
   END IF

RETURN
