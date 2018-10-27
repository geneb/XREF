; S2CKrnlF.asm -
; Adapted from FuncName.ASM Copyright (c) 1990 by Chris May (8/1/1990)
; Adapted from S2CKrnl.asm
; BC7 (Far String) version
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

    Extrn  Stringassign:proc

.DATA

    Descriptor  DD      ?       ;a string descriptor
    

.CODE

Str2CodeKrnl Proc Far

    Push   CX                   ;Push Segment of this CS$
    Inc    SI
    Push   SI                   ;Push Address of this CS$

    Push   ES                   ;save ES
    Mov    ES,CX                ;for segment override
    Xor    AH,AH
    Mov    AL,Byte Ptr ES:[SI-1];get the 1 byte length in AX
    Pop    ES                   ;restore ES
    Push   AX                   ;Push length of this CS$


    Push   DS                   ;Push Segment of destination descriptor
    Mov    AX,Offset Descriptor
    Push   AX                   ;Push Address of destination descriptor
    Xor    AX,AX
    Push   AX                   ;Push length of destination (0 for Var Len)

    Call   Stringassign         ;go assign string

    Mov    AX,Offset Descriptor ;address of string to return to Basic

    RetF 2                      ;clean up the stack, return to Basic

Str2CodeKrnl EndP

End
