\ --------------------------------------------------------------
\ Test of Assembler, Disassembler and Simulator for RISC-V (RV32I and RV32C)
\ (c) Klaus Kohl-Schoepe
\ Licence: LGPLv3 - see https://www.gnu.org/licenses/lgpl-3.0.de.html
\ --------------------------------------------------------------
\ - requires 32-Bit Forth - testet on mcForth (using Createp/End-Create/p,/$,p/@p)
\ --------------------------------------------------------------

Include rv32imc_asm.f
Include rv32imc_dasm.f
Include rv32imc_sim.f

\ --------------------------------------------------------------
\ Test of RV32I and RV32M
\ --------------------------------------------------------------
-1 [IF]
Proc Test32
  herep 1 and allotp  \ Align code address
  x3 , x4 , x5 add,
  x3 , x4 , x5 sub,
  x3 , x4 , x5 sll,
  x3 , x4 , x5 slt,
  x3 , x4 , x5 sltu,
  x3 , x4 , x5 xor,
  x3 , x4 , x5 srl,
  x3 , x4 , x5 sra,
  x3 , x4 , x5 or,
  x3 , x4 , x5 and,
  x3 ,( 291 x4 lb,
  x3 ,( -291 x4 lh,
  x3 ,( 291 x4 lw,
  x3 ,( 291 x4 lbu,
  x3 ,( 291 x4 lhu,
  x3 , x4 , #291 addi,
  x3 , x4 , 1 slli,
  x3 , x4 , 2 slti,
  x3 , x4 , 4 sltiu,
  x3 , x4 , #-291 xori,
  x3 , x4 , 8 srli,
  x3 , x4 , #16 srai,
  x3 , x4 , #291 ori,
  x3 , x4 , #-291 andi,
  x3 , x4 , #291 jalr,
  x5 ,( $123 x4 sb,
  x5 ,( $456 x4 sh,
  x5 ,( $-123 x4 sw,
  x2 , x4 , herep 2 + beq,
  x0 , x4 , herep #32 + bne,
  x1 , x4 , herep #2048 + blt,
  x31 , x4 , herep 2 - bge,
  x1 , x4 , herep #32 - bltu,
  x1 , x4 , herep #2048 - bgeu,
  x3 , $12345 auipc,
  x3 , $56789 lui,
  x1 , $00001234 jal,
  fence.i,
  fence,
  ecall,
  ebreak,
  uret,
  sret,
  mret,
  wfi,
  x4 , x5 sfence.vma,
  x3 , $123 , x4 csrrw,
  x3 , $123 , $1A csrrwi,
  x3 , $123 , x4 csrrs,
  x3 , $123 , $1A csrrsi,
  x3 , $123 , x4 csrrc,
  x3 , $123 , $1A csrrci,
  x3 , x4 , x5 mul,
  x3 , x4 , x5 mulh,
  x3 , x4 , x5 mulhsu,
  x3 , x4 , x5 mulhu,
  x3 , x4 , x5 div,
  x3 , x4 , x5 divu,
  x3 , x4 , x5 rem,
  x3 , x4 , x5 remu,
  x3 , $12345678 li,
End-Code

Test32 dup 1 and +  #63 ndis  drop
[THEN]

\ --------------------------------------------------------------
\ Test of RV32C
\ --------------------------------------------------------------
-1 [IF]
Proc Test16
  herep 1 and allotp  \ Align code address
  x3 , x5 c.mv,
  x3 , x5 c.add,
  x6 c.jr,
  x8 , x10 c.sub,
  x3 c.jalr,
  c.ebreak,
  x3 ,( #20 sp c.lwsp,
  x3 , #18 c.li,
  x3 , #18 c.lui,
  x2 , #32 c.addi16sp,
  c.nop,
  x3 , #18 c.addi,
  x3 , 3 c.slli,
  x10 ,( #20 x11 c.sw,
  x8 , x10 c.xor,
  x8 , x10 c.or,
  x8 , x10 c.and,
  x5 ,( #20 x2 c.swsp,
  x10 , sp , 4 c.addi4spn,
  x10 ,( #20 x11 c.lw,
  x9 , herep 2 + c.beqz,
  x10 , herep 2 - c.bnez,
  x8 , #18 c.andi,
  x8 , 1 c.srli,
  x8 , 2 c.srai,
  herep 2 - c.j,
  herep 2 + c.jal,
End-Proc

Test16 dup 1 and +  #27 ndis  drop
[THEN]
\ --------------------------------------------------------------
\ Test of Labels
\ --------------------------------------------------------------
Proc Labels
  herep 1 and allotp  \ Align code address
1$:
  x9 , 1$ c.beqz,
  x9 , 2$ c.beqz,
  x10 , 1$ c.bnez,
  x10 , 2$ c.bnez,
  x9 , x10 , 1$ beq,
  x9 , x10 , 2$ beq,
  x9 , x10 , 1$ bne,
  x9 , x10 , 2$ bne,
  x9 , x10 , 1$ blt,
  x9 , x10 , 2$ blt,
  x9 , x10 , 1$ bge,
  x9 , x10 , 2$ bge,
  x9 , x10 , 1$ bltu,
  x9 , x10 , 2$ bltu,
  x9 , x10 , 1$ bgeu,
  x9 , x10 , 2$ bgeu,
  x9 , 1$ jal,
  x9 , 2$ jal,
  x3 , 1$ li,
  x3 , 2$ li,
2$:
End-Proc

\ --------------------------------------------------------------
\ A small test for the Simulator
\ --------------------------------------------------------------
1 [IF]
Proc simtest
  herep 1 and allotp  \ Align code address
  x8 , 0 c.li,
1$:
  x8 , 1 c.addi,
  1$ c.j,
End-Proc

: simulator     ( addr n -- ) \ Simulator with Disassembler and Debugging
  siminit  
  ?FOR dup dis drop  sim IF rdrop drop exit THEN NEXT
  drop ;

simtest  dup 1 and  +  20  simulator
[THEN]

\ --------------------------------------------------------------
\ Test with mecrisp
\ --------------------------------------------------------------
\ - !!! For test the RISC-V programm starts at here !!! -
\ - !!! Change dis to use offset here               !!! -
\ --------------------------------------------------------------
0 [IF]
\ Download mecrisp-quintus-gd32vf103cb.bin (18800 Bytes) to here
    s" mecrisp-quintus-gd32vf103cb.bin"  r/w fopen
    dup  here >x  rot  #18800  swap  fread drop
    fclose
\ Reset-Vector
 0 dis drop
\ Reset Code
 $4736  80 ndis
[THEN]

\ --------------------------------------------------------------
\ Test with assembler code
\ --------------------------------------------------------------
\ - !!! For test the RISC-V programm starts at here !!! -
\ - !!! Change dis to use offset here               !!! -
\ --------------------------------------------------------------
0 [IF]
\ Download x.bin (296 Bytes) to here
  s" x.bin"  r/w fopen
  dup  here >x  rot  #296  swap  fread drop
  fclose
  0  #88 ndis drop
[THEN]
