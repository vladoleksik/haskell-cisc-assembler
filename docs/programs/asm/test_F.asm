MOV R0, 3 #Tests shift and rotate ops, flags. Expected: halt with R5=7, R0=6
ASL R0
MOV R1, 0x8001
ASR R1
MOV R2, 0x8001
LSR R2
MOV R3, 0x8001
ROL R3
MOV R4, 0x8000
RLC R4
SEV
et1: BVC et2
MOV R0, 1
CLV
BR et1
et2: MOV R5, 7
HALT





