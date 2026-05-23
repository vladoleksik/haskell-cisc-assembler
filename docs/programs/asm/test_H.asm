BR main #This program busily waits until IRQ0 is activated, then halts with R1=2.
NOP #Expected to be loaded from 0x0
NOP
NOP
HALT #Here will be the address of int (0xA)!
int: POP R0
ADD R0, 2
PUSH R0
RETI
main: MOV R0, 0xA
MOV R1, 0x8 #Address of IV[IRQ0] (0x8)
MOV (R1), 0xA #Address of int handler for IRQ0 (0xA)
WAIT
MOV R1, 2
HALT