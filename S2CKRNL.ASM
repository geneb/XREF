; S2CKrnl.asm -
; Adapted from FUNCNAME.ASM Copyright (c) 1990 by Chris May (8/1/1990)
; BC6 (Near String) version
;----------------------------------------------------------------------------
;
;   Calling Sample:
;
;           .
;           .
;           .
;          Mov  CX,CS
;          Mov  SI Offset CS:{StringNumber}
;          Jmp  Str2CodeKrnl            ;will return to BASIC if successful
; ??CS Proc EndP
;

.MODEL Medium, BASIC

.DATA

    CodeStrBuf  DB      255 Dup (?)     ;space to transfer string to
    CodeStrLen  DW      ?               ;length of the string to return
    CodeStrAdr  DW Offset CodeStrBuf    ;address of the string to return

.CODE

Str2CodeKrnl Proc Far
    Cld                             ;so string moves are foreward
    Push    ES                      ;save ES
    Push    DS                      ;set ES = DS
    Pop     ES

    Mov     CodeStrLen,CX           ;it might be a null string
    Jcxz    Exit
    Mov     DS,CX
    
    LodSb                           ;first get the length of the string
    Xor     AH,AH                   ;255 max so clear AH
    Mov     CX,AX                   ;we'll use Rep MovSb to copy the string
    Mov     SS:CodeStrLen,AX        ;put the length in the descriptor
    Mov     AX,Offset SS:CodeStrLen ;address of string to return to Basic
    Mov     DI,Offset SS:CodeStrBuf ;we'll use Rep MovSb to copy the string
    Jcxz    Exit
    Rep MovSb

 Exit:
    Push    SS
    Pop     DS                      ;restore DS to its original condition
    Pop     ES                      ;restore ES to its original condition
    RetF 2                          ;return to Basic

Str2CodeKrnl EndP

End
