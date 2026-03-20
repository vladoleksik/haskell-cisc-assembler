start: MOV (R6), 0xA
    SUB R6, 2
    MOV R2, 2(R6)
loop:
    DEC R2
    CMP 1, R2
    NOP
  
redo: BNE loop #loop condition
    HALT