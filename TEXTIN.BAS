'****************************************************************************
'  TEXTIN.BAS - Source for TEXTIN.COM
'  by Don Malin
'  (c) 1990, 1991 Crescent Software, Inc.
'
'  Purpose: Replaces text in a source file (.BAS) with text from a ".SPL"
'  file created by the "TEXTOUT.COM" program.  The modified source is
'  writen to a file with the same base name as the source file but with
'  an extension of ".TRN".
'
'  From DOS type:  TEXTIN FileName[.BAS]
'----------------------------------------------------------------------------
'  Compile and link with P.D.Q. as follows:
'  bc TEXTIN /O;
'  link /EX/NOD TEXTIN,, NUL, PDQ;
'  exe2com TEXTIN
'****************************************************************************
DEFINT A-Z

DECLARE SUB GetNewText (Repl$, LineNo%, Quote%) 'Reads a line from ".SPL" file

'$INCLUDE: '\pdq\pdqdecl.bas'

Qt$ = CHR$(34)                                  'Quotation mark character "
CrLf$ = CHR$(13) + CHR$(10)                     'Chariage return line feed

PRINT "Source Text Merging Program"
PRINT "(c) 1990, 1991 Crescent Software, Inc."
PRINT


'~~~~~ Get the names of files to work with
Source$ = COMMAND$                              'Copy the command line arg.

IF LEN(Source$) = 0 THEN                        'No file given?
   PRINT "Source file name: ";                  'Ask the user for a name
   PDQInput Source$
   IF LEN(Source$) = 0 THEN END
END IF

Source$ = UCASE$(Source$)

N = INSTR(Source$, ".")                         'Look for a period in name
IF N THEN                                       'Create the names for files
   SPL$ = LEFT$(Source$, N) + "SPL"
   NewFile$ = LEFT$(Source$, N) + "TRN"
ELSE
   SPL$ = Source$ + ".SPL"
   NewFile$ = Source$ + ".TRN"
   Source$ = Source$ + ".BAS"
END IF


'~~~~~ Open the needed files
OPEN Source$ FOR INPUT AS #1                    'Open the Source file (.BAS)
OPEN SPL$ FOR INPUT AS #2                       'Open the Text file (.SPL)
OPEN NewFile$ FOR OUTPUT AS #3                  'Open the new file (.TRN)

PRINT "Merging: " + Source$ + " and " + SPL$ + " into " + NewFile$ + " ...";

CurLine = 0                                     'Initialize line number

GetNewText Repl$, LineNo, Quote                 'Read a line from the text
                                                '  file (.SPL)

'~~~~~ Read the source file until done
DO UNTIL EOF(1)

   Text$ = ""                                   'Initialize the source line $
   DO
      LINE INPUT #1, Buf$                       'Read a line of source
      Text$ = Text$ + Buf$                      'Add chars. to Source line
      IF RIGHT$(Text$, 1) = "_" THEN            'Is there a line continuation
         Text$ = Text$ + CrLf$                  'Add chariage return line feed
         More = -1                              'Set flag to get more
      ELSE
         More = 0
      END IF
   LOOP WHILE More                              'Go back for more if needed

   CurLine = CurLine + 1                        'Bump the source line number
   
  
   '~~~~~ Look for lines to replace
   Q = 0                                        'Init. leftmost " location
   TotDif = 0                                   'Init. line length difference
   DO WHILE CurLine = LineNo                    'Is this a line to replace?
      IF Quote THEN
         Q = INSTR(Q + 1, Text$, Qt$)           'Look for left quotation mark
         IF Q = 0 THEN GOTO BadFile             'If we don't find one, Quit

         Q2 = INSTR(Q + 1, Text$, Qt$)          'Look for the right quote
         IF Q2 = 0 THEN Q2 = LEN(Text$)         'If not found, use the length
                                                'Replace the new text in line
         Text$ = LEFT$(Text$, Q) + Repl$ + MID$(Text$, Q2)

         Dif = LEN(Repl$) - (Q2 - Q - 1)        'Get the difference in length
         Q = Q2 + Dif                           'Set next position to look
                                                '  for more quoted strings.
         TotDif = TotDif + Dif                  'Accumulate the line length
                                                '  difference.
      ELSE                                      'Replace remark
         GOSUB FindRemark                       'Find remark in Text
         IF Remark = 0 THEN GOTO BadFile        'If not remark, bail out
         Text$ = LEFT$(Text$, Remark - 1) + Repl$
      END IF

      GetNewText Repl$, LineNo, Quote           'Read another line of text to
                                                '  use for next replacement.
   LOOP


   '~~~~~ Adjust the position of comments if the line length changed
   IF TotDif THEN                               'If the line length changed,
      GOSUB FindRemark                          '  look for a comment.

      IF Remark THEN                            'If there is one, did the
         IF TotDif < 0 THEN                     '  line get smaller
            Text$ = LEFT$(Text$, Remark - 1) + SPACE$(ABS(TotDif)) + MID$(Text$, Remark)
         ELSE                                   '  or bigger?
            FOR N = Remark - 1 TO Remark - TotDif STEP -1
                IF MID$(Text$, N, 1) <> " " THEN EXIT FOR
            NEXT
            Text$ = LEFT$(Text$, N) + MID$(Text$, Remark)
         END IF
      END IF
   END IF

   PRINT #3, Text$                              'Write the new line to the
                                                '  ".TRN" file

LOOP                                            'Get the next source line


LOCATE CSRLIN, 1                                'Tell the user what we did.
PRINT NewFile$ + " has been created.                                      "

END


'~~~~~ Find comment position
FindRemark:
   IF Q = 0 THEN Q = 1
   Remark = INSTR(Q, Text$, "'")                'Look for either comment type
   IF Remark = 0 THEN Remark = INSTR(Q, UCASE$(Text$), "REM")
RETURN


'~~~~~ Show problem and end
BadFile:
   PRINT Text$                                  '  show the user the text and
   PRINT Repl$                                  '  end the program.  No way
   PRINT                                        '  to recover from this.
   PRINT "Files do not match.  Program terminated."
   END

'****************************************************************************
'GetNewText - Reads a line of text from the ".SPL" file
'
'   Repl$ - Returns the text only of the line read
'  LineNo - Returns the source line number associated with the text
'   Quote - Flags (-1) a quoted strings, otherwise (0) it's a comments
'****************************************************************************
'
SUB GetNewText (Repl$, LineNo, Quote) STATIC
   SHARED Qt$, Buf$, SaveErr                    'Share the Quotation mark
                                                '  character and the string
                                                '  used for reading from files
   Repl$ = ""                                   'Initialize the text line
   LineNo = 0                                   '   "       the line number

   DO UNTIL EOF(2)                              'Read until end of file
      LINE INPUT #2, Repl$

      IF LEN(Repl$) THEN                        'Did we get something?
         LineNo = PDQValI(Repl$)                'Read the source line number

         Quote = INSTR(Repl$, Qt$)              'Find the first quote
         IF Quote THEN                          'Just return the text
            Repl$ = MID$(Repl$, Quote + 1, INSTR(Quote + 1, Repl$, Qt$) - Quote - 1)
         ELSE
            Repl$ = MID$(Repl$, INSTR(Repl$, ":") + 2)
         END IF
         EXIT DO                                'Bail out of the loop
      END IF
   LOOP                                         'Go back for more if the line
                                                '  we read was invalid
END SUB
