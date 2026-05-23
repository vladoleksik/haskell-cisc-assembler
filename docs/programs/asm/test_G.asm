MOV R0, 0xA #Tests POP_PC. Meant to load from 0x0. Expected to halt with R2=7.
PUSH R0
POP_PC
HALT
MOV R2, 7
HALT