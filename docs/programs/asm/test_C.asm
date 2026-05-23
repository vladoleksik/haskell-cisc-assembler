BR main #Tests calls and functions. Program should be loaded at 0x0. Expected: R0=0x13BA
gauss: MOV R1, R0
MOV R2, 0
loop: CMP R1, 0
BEQ end
ADD R2, R1
DEC R1
BR loop
end: MOV R0, R2
RET

main: MOV R3, 0x100
MOV (R3), 99
INC (R3)
MOV R0, (R3)
MOV R1, 2
CALL R1
MOV 2(R3), R0
MOV R1, R0
HALT

