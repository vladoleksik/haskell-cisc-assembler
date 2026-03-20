# Custom ISA Assembler

A lightweight, modular assembler written in **Haskell** for a custom 16-bit processor. This tool translates human-readable assembly code into binary bytecode using a two-pass assembly process.

## Features
* **Two-Pass Assembly:** Resolves forward-referencing labels automatically.
* **Semantic Validation:** Prevents using immediate values as destinations for mutator instructions.
* **Flexible Endianness:** Supports both Big-Endian (default) and Little-Endian output.
* **Detailed IR & Symbol Views:** Debug your code by inspecting the intermediate representation and the symbol table.

### Prerequisites
* [GHC](https://www.haskell.org/ghc/) (9.4.x or later recommended)
* [Cabal](https://www.haskell.org/cabal/)

### Build
Clone the repository and run:
```bash
cabal build
cabal install
```

## Assembly Syntax
The assembler supports four instruction classes. Lines can contain labels, instructions, or both. Comments start with `#`.

```asm
start: MOV (R6), 0x4  # Indirect addressing
       SUB R6, 2      # Two-operand arithmetic
loop:  DEC R2         # Single-operand mutator
       CMP 1, R2      # Non-destructive comparison
       BNE loop       # Branch to label
       HALT           # No-operand instruction
```

## Instruction Encoding

#### Class B1: Two Operands (e.g., `MOV`, `SUB`, `CMP`)
These are the most complex because they need to fit a destination and a source.
* **Format:** `[Opcode:4][SrcMode:2][SrcReg:4][DstMode:2][DstReg:4]`
* **Addressing Modes:**
    * `00`: Immediate (the value is found in the following word).
    * `01`: Direct (the `SrcReg`/`DstReg` encodes the register).
    * `10`: Indirect (the `SrcReg`/`DstReg` points to a register for `(Rn)`).
    * `11`: Index (the `SrcReg`/`DstReg` encoded the register for `(offset(Rn))`. The offset is found in the following word).

#### Class B2: One Operand (e.g., `DEC`, `INC`, `PUSH`)
Since there is only one operand, you have more bits available for the address or value.
* **Format:** `[Opcode:12][DestMode:2][DestReg:4]`

#### Class B3: Control Flow / Branching (e.g., `BNE`, `BR`)
These don't use registers; they use a relative **Offset**.
* **Format:** `[Opcode:8][Offset:8]`
* **Offset Calculation:** The CPU takes the current Program Counter ($PC$) and adds the signed 8-bit offset to find the next instruction. This allows jumping roughly 256 bytes forward or backward.

#### Class B4: Zero Operands (e.g., `NOP`, `HALT`, `RET`)
The simplest to decode.
* **Format:** `[Opcode:16]`
* **Logic:** The decoder sees the opcode to immediately execute the corresponding action.

---

## Usage

```bash
assembler-exe -i <input_file> -o <output_file> [FLAGS]
```

### Options
* `-i` **(Required)**: Path to the `.asm` source file.
* `-o` **(Required)**: Path for the generated `.bin` file.
* `--le`: Output in Little-Endian format (default is Big-Endian).
* `-s`: Print the Symbol Table to the console during assembly.
* `-a`: Print the Intermediate Representation (parsed instructions).
* `-v`: Show version and author information.
* `-h`: Show the help menu.

## Project Structure
The project is organized into functional modules:
* `ISA/`: Definitions for Registers, Operands, and Instructions.
* `Parser/`: Megaparsec logic for assembly syntax.
* `Assembler.hs`: Logic for address calculation and bit-packing.