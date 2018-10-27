'****************************************************************************
'MAKEKEYS.BAS - Program that creates the XREF.KEY file for XREF.EXE.
'
'  The XREF.KEY file contains a list of all BASIC's keywords.
'
'  To create the above file, run this program.  To modify keyword
'  information, edit the KEYWORDS.BI include file and then rerun
'  this program.
'****************************************************************************

DEFINT A-Z

DECLARE FUNCTION XRFindKey% (SEG Element AS ANY, NumKeyWords)

CONST NumKeyWords = 388

TYPE KeyWords
   KName AS STRING * 17
   KFlag AS INTEGER
END TYPE

DECLARE SUB FClose (Handle%)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FPutA (Handle%, SEG Element AS ANY, NumBytes&)

KWords:
'$INCLUDE: 'KeyWords.bi'

REDIM KWord(NumKeyWords) AS KeyWords
KWord(1).KName = CHR$(34)
RESTORE KWords
FOR N = 2 TO NumKeyWords
    READ KWord(N).KName, KWord(N).KFlag
NEXT

CLS
FOR N = 1 TO NumKeyWords
   KWord(0).KName = KWord(N).KName
   BWord = XRFindKey%(KWord(0), NumKeyWords)
   IF BWord = 0 THEN
      PRINT KWord(N).KName; "is out of order"
   END IF
NEXT

FCreate "XRef.Key"
FOpen "XRef.Key", Handle%
FPutA Handle%, KWord(1), NumKeyWords * LEN(KWord(0))
FClose Handle%

