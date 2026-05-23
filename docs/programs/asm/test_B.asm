MOV 2(R2), 3 #Tests branches and labels
SEZ
BEQ after
HALT
after: PUSH 2(R2)
POP R5
NOP
SUB R1, 2(R2)
HALT