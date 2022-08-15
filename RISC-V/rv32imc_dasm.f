\ --------------------------------------------------------------
\ Disassembler for RISC-V (RV32I and RV32C)
\ (c) Klaus Kohl-Schoepe
\ Licence: LGPLv3 - see https://www.gnu.org/licenses/lgpl-3.0.de.html
\ --------------------------------------------------------------
\ Comment:
\ - look and feel adapted to GNU disassembler
\ - requires 32-Bit Forth - testet on mcForth (using Createp/End-Create/p,/$,p/@p)
\ - !!! <= change this if you use a system which don't align code
\
\ Logbook:
\ - 05.08.21 KKS: first tests (RV32I)
\ - 12.08.21 KKS: many corrections and extensions (RV32I/M/C)
\                 actual no error known but not all bit combination testet
\ - 17.08.21 KKS: First version - verified with rv32imc_asm.f and rv32imc_test.f
\ --------------------------------------------------------------

onlyforth

[UNDEFINED] Dassembler   Vocabulary Dassembler [THEN]
Dassembler Definitions

\ --------------------------------------------------------------
\ Tools:
\ --------------------------------------------------------------
Variable opcode

: findop    ( opcode table -- 0 | entry -1 ) \ Find opcode in table
  BEGIN  dup @p                               \ end of table ?
  WHILE  over  over @p  and   over 4 +  @p  = \ compare mask opcode with table
         IF  nip  -1  exit THEN               \ found
         #24 +                                \ next entry
  REPEAT drop  drop  0 ;                      \ not found

\ Print address and opcode
: addr.         ( addr -- addr )
  dup  8 0u.r  space ;
: opcode16.     ( addr -- addr )
  opcode @  $ffff and  4 0u.r  5 spaces ;
: opcode32.     ( addr -- addr )
  opcode @  8 0u.r  space ;

\ Print registers
: ,.
  [char] ,  emit ;
: u.-           ( u -- ) \ u. without space
  0 <# #s #> type ;
: reg.          ( reg -- )
  ." x"  base @ decimal  swap u.-  base ! ;
: n.            ( n -- )
  base @ decimal
  swap dup 0< IF ." -" abs THEN u.-
  base ! ;
: 0x.           ( n -- )
  dup 0< IF ." -0x" abs ELSE ." 0x" THEN u.- ;
: 0x.r          ( n len -- )
  >r  dup 0< IF ." -0x" abs ELSE ." 0x" THEN r> 0u.r ;
: u0x.r         ( u len -- ; unsigned )
  ." 0x" 0u.r ;
: rd.           ( -- ; bits [11: 7] )
  opcode @  #07 rshift  $1f and  reg. ;
: rs1.          ( -- ; bits [20:15] )
  opcode @  #15 rshift  $1f and  reg. ;
: rs2.          ( -- ; bits [24:20] )
  opcode @  #20 rshift  $1f and  reg. ;

: rd'_c.        ( -- ; bits [9:7]+8 )
  opcode @  #07 rshift  $07 and  8 +  reg. ;
: rs2_c.        ( -- ; bits [6:2] )
  opcode @  #02 rshift  $1f and  reg. ;
: rs2'_c.       ( -- ; bits [4:2]+8 )
  opcode @  #02 rshift  $07 and  8 +  reg. ;

\ --------------------------------------------------------------
\ Disassembler for 32-bit opcode:
\ --------------------------------------------------------------
\ Print mnemonics with immediate value from opcode
: rd,rs1,rs2        ( -- )
  rd.  ,.  rs1.  ,.  rs2. ;

: rd,rs1,uimm4_0_i  ( -- ; uimm = [24:20] => [4:0] )
  rd.  ,.  rs1. ,.  opcode @
        #20 rshift    $0000001f and          \ bit 24:20 => 4:0
  n. ;

: rd,imm11_0_i(rs1) ( -- ; imm = [31:20] => [11:0] )
  rd.  ,.  opcode @
        #20 rshift    $00000fff and          \ bit 31:20 => 11:0
  dup   $0800 and  IF $fffff000      or THEN \ Sign
  n.  ." ("  rs1. ." )" ;

: rd,imm11_0_i  ( -- ; imm = [31:20] => [11:0] )
  rd.  ,.  opcode @
        #20 rshift    $00000fff and          \ bit 31:20 => 11:0
  dup   $0800 and  IF $fffff000      or THEN \ Sign
  n. ;

: rd,rs1,imm11_0_i  ( -- ; imm = [31:20] => [11:0] )
  rd.  ,.  rs1. ,.  opcode @
        #20 rshift    $00000fff and          \ bit 31:20 => 11:0
  dup   $0800 and  IF $fffff000      or THEN \ Sign
  n. ;

: rs2,imm11_0(rs1)_s ( -- ; imm = [31:25,11:7] => [11:0] )
  rs2.  ,.  opcode @  0
  over  #20 rshift    $00000fe0 and  or      \ bit 31:25 => 11: 5
  swap  #07 rshift    $0000001f and  or      \ bit 11: 7 =>  4: 0
  dup   $0800 and  IF $fffff000      or THEN \ Sign
  0x.  ." (" rs1. ." )" ;

: rs1,rs2,imm12_1_sb ( addr -- addr ; imm = [31,7,30:25,11:8] => [12:1] )
  rs1.  ,.  rs2.  ,.  opcode @  0
  over  0<         IF $fffff000      or THEN \ bit 31    =>    12+Sign
  over  $0080 and  IF $00000800      or THEN \ bit  7    =>    11
  over  #20 rshift    $000007e0 and  or      \ bit 30:25 => 10: 5
  swap  #07 rshift    $0000001e and  or      \ bit 11: 8 =>  4: 1
  dup   $1000 and  IF $ffffe000      or THEN \ Sign
  over +  8 u0x.r ;

: rd,imm31_12_u     ( -- ; imm = [31:12] => [19:0] )
  rd.  ,.  opcode @
         #12 rshift   $000fffff and          \ bit 31:12 => 19: 0
  5 u0x.r ;

: rd,imm20_1_uj     ( -- ; imm = [31,19:12,20,30:21] => [20:1] )
  rd.  ,.  opcode @   0
  over  0<         IF $fff00000      or THEN \ bit 31    =>    20+Sign
  over                $000ff000 and  or      \ bit 19:12 => 19:12
  over  #09 rshift    $00000800 and  or      \ bit    20 =>    11
  swap  #20 rshift    $000007fe and  or      \ bit 30:21 => 10: 1
  over +  8 u0x.r ;

: rs1,rs2           ( -- )
  rs1.  ,.  rs2. ;

: addr,rs1          ( -- addr = [31:20] => [11:0] )
  opcode @
        #20 rshift    $00000fff and
  3 u0x.r  ,.  rs1. ;

: rd,addr,rs1       ( -- addr = [31:20] => [11:0] )
  rd.  ,.  opcode @
        #20 rshift    $00000fff and
  3 u0x.r  ,.  rs1. ;

: addr,uimm         ( -- addr = [31:20] => [11:0]; imm = [19:15] => 5:0 )
  opcode @
  dup   #20 rshift    $00000fff and          \ bit 31:20 => 11:0
  3 u0x.r  ,.
        #15 rshift    $0000001f and          \ bit 19:15 =>  5:0
  n. ;

: rd,addr,uimm      ( -- addr = [31:20] => [11:0]; imm = [19:15] => 5:0 )
  rd.  ,.  opcode @
  dup   #20 rshift    $00000fff and          \ bit 31:20 => 11:0
  3 u0x.r  ,.
        #15 rshift    $0000001f and          \ bit 19:15 =>  5:0
  2 u0x.r ;

Createp rv32i1_table
\ mask          opcode        mnemonics (12char)  operands
  $0000707f ,p  $00000003 ,p  s" lb        " $,p  ' rd,imm11_0_i(rs1)  ,p
  $0000707f ,p  $00001003 ,p  s" lh        " $,p  ' rd,imm11_0_i(rs1)  ,p
  $0000707f ,p  $00002003 ,p  s" lw        " $,p  ' rd,imm11_0_i(rs1)  ,p
  $0000707f ,p  $00004003 ,p  s" lbu       " $,p  ' rd,imm11_0_i(rs1)  ,p
  $0000707f ,p  $00005003 ,p  s" lhu       " $,p  ' rd,imm11_0_i(rs1)  ,p
  $ffffffff ,p  $0000100f ,p  s" fence.i   " $,p  ' nop                ,p
  $ffffffff ,p  $0ff0000f ,p  s" fence     " $,p  ' nop                ,p
  $0000707f ,p  $00000013 ,p  s" addi      " $,p  ' rd,rs1,imm11_0_i   ,p
  $fe00707f ,p  $00001013 ,p  s" slli      " $,p  ' rd,rs1,uimm4_0_i   ,p
  $0000707f ,p  $00002013 ,p  s" slti      " $,p  ' rd,rs1,imm11_0_i   ,p
  $0000707f ,p  $00003013 ,p  s" sltiu     " $,p  ' rd,rs1,imm11_0_i   ,p
  $0000707f ,p  $00004013 ,p  s" xori      " $,p  ' rd,rs1,imm11_0_i   ,p
  $fe00707f ,p  $00005013 ,p  s" srli      " $,p  ' rd,rs1,uimm4_0_i   ,p
  $fe00707f ,p  $40005013 ,p  s" srai      " $,p  ' rd,rs1,uimm4_0_i   ,p
  $0000707f ,p  $00006013 ,p  s" ori       " $,p  ' rd,rs1,imm11_0_i   ,p
  $0000707f ,p  $00007013 ,p  s" andi      " $,p  ' rd,rs1,imm11_0_i   ,p
  $0000007f ,p  $00000017 ,p  s" auipc     " $,p  ' rd,imm31_12_u      ,p
  $0000707f ,p  $00000023 ,p  s" sb        " $,p  ' rs2,imm11_0(rs1)_s ,p
  $0000707f ,p  $00001023 ,p  s" sh        " $,p  ' rs2,imm11_0(rs1)_s ,p
  $0000707f ,p  $00002023 ,p  s" sw        " $,p  ' rs2,imm11_0(rs1)_s ,p
  $fe00707f ,p  $00000033 ,p  s" add       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02000033 ,p  s" mul       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $40000033 ,p  s" sub       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00001033 ,p  s" sll       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02001033 ,p  s" mulh      " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00002033 ,p  s" slt       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02002033 ,p  s" mulhsu    " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00003033 ,p  s" sltu      " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02003033 ,p  s" mulhu     " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00004033 ,p  s" xor       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02004033 ,p  s" div       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00005033 ,p  s" srl       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02005033 ,p  s" divu      " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $40005033 ,p  s" sra       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00006033 ,p  s" or        " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02006033 ,p  s" rem       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $00007033 ,p  s" and       " $,p  ' rd,rs1,rs2         ,p
  $fe00707f ,p  $02007033 ,p  s" remu      " $,p  ' rd,rs1,rs2         ,p
\ End  
  $00000000 ,p
End-Create

Createp rv32i2_table
  $0000007f ,p  $00000037 ,p  s" lui       " $,p  ' rd,imm31_12_u      ,p
  $0000707f ,p  $00000063 ,p  s" beq       " $,p  ' rs1,rs2,imm12_1_sb ,p
  $0000707f ,p  $00001063 ,p  s" bne       " $,p  ' rs1,rs2,imm12_1_sb ,p
  $0000707f ,p  $00004063 ,p  s" blt       " $,p  ' rs1,rs2,imm12_1_sb ,p
  $0000707f ,p  $00005063 ,p  s" bge       " $,p  ' rs1,rs2,imm12_1_sb ,p
  $0000707f ,p  $00006063 ,p  s" bltu      " $,p  ' rs1,rs2,imm12_1_sb ,p
  $0000707f ,p  $00007063 ,p  s" bgeu      " $,p  ' rs1,rs2,imm12_1_sb ,p
  $0000707f ,p  $00000067 ,p  s" jalr      " $,p  ' rd,rs1,imm11_0_i   ,p
  $0000007f ,p  $0000006f ,p  s" jal       " $,p  ' rd,imm20_1_uj      ,p
  $ffffffff ,p  $00000073 ,p  s" ecall     " $,p  ' nop                ,p
  $ffffffff ,p  $00100073 ,p  s" ebreak    " $,p  ' nop                ,p
  $ffffffff ,p  $00200073 ,p  s" uret      " $,p  ' nop                ,p
  $ffffffff ,p  $10200073 ,p  s" sret      " $,p  ' nop                ,p
  $ffffffff ,p  $10500073 ,p  s" wfi       " $,p  ' nop                ,p
  $fe00707f ,p  $12000073 ,p  s" sfence.vma" $,p  ' rs1,rs2            ,p
  $ffffffff ,p  $30200073 ,p  s" mret      " $,p  ' nop                ,p
  $00007fff ,p  $00001073 ,p  s" csrw      " $,p  ' addr,rs1           ,p
  $0000707f ,p  $00001073 ,p  s" csrrw     " $,p  ' rd,addr,rs1        ,p
  $00007fff ,p  $00005073 ,p  s" csrwi     " $,p  ' addr,uimm          ,p
  $0000707f ,p  $00005073 ,p  s" csrrwi    " $,p  ' rd,addr,uimm       ,p
  $00007fff ,p  $00002073 ,p  s" csrs      " $,p  ' addr,rs1           ,p
  $0000707f ,p  $00002073 ,p  s" csrrs     " $,p  ' rd,addr,rs1        ,p
  $00007fff ,p  $00006073 ,p  s" csrsi     " $,p  ' addr,uimm          ,p
  $0000707f ,p  $00006073 ,p  s" csrrsi    " $,p  ' rd,addr,uimm       ,p
  $00007fff ,p  $00003073 ,p  s" csrc      " $,p  ' addr,rs1           ,p
  $0000707f ,p  $00003073 ,p  s" csrrc     " $,p  ' rd,addr,rs1        ,p
  $00007fff ,p  $00007073 ,p  s" csrci     " $,p  ' addr,uimm          ,p
  $0000707f ,p  $00007073 ,p  s" csrrci    " $,p  ' rd,addr,uimm       ,p
\ End
  $00000000 ,p
End-Create

: dis32         ( addr -- addr+4 )
  cr  addr.  opcode32.
  opcode @  rv32i1_table  findop
  IF   8 +  dup countp typep space   #12 +   @p execute  4 +  exit THEN
  opcode @  rv32i2_table  findop
  IF   8 +  dup countp typep space   #12 +   @p execute  4 +  exit THEN
  ." unimp " 4 + ;

\ --------------------------------------------------------------
\ Disassembler for 16-bit opcode:
\ --------------------------------------------------------------
: rd,rs2_c          ( -- )
  rd.  ,.  rs2_c. ;

: rd,uimm5_0_c      ( -- ; imm = [12,4:0] => [5:0] )
  rd.  ,.   opcode @  0
  over  $1000 and  IF $00000020      or THEN \ bit 12    =>     5
  swap  #02 rshift    $0000001f and  or      \ bit  6: 2 =>  4: 0
  n. ;

: rd,imm5_0_c       ( -- ; imm = [12,4:0] => [5:0] )
  rd.  ,.   opcode @  0
  over  $1000 and  IF $ffffffe0      or THEN \ bit 12    =>     5
  swap  #02 rshift    $0000001f and  or      \ bit  6: 2 =>  4: 0
  n. ;

: rd,uimm7_2_ci(sp) ( -- ; imm = [3:2,12,6:4] => [7:2] )
  rd.  ,.   opcode @  0
  over  #04 lshift    $000000c0 and  or      \ bit  3: 2 =>  7: 6
  over  $1000 and  IF $00000040      or THEN \ bit    12 =>     5
  swap  #02 rshift    $0000001c and  or      \ bit  6: 4 =>  4: 2
  n.  ." (sp)";

: sp,imm9_4_ci         ( -- ; imm = [12,4:3,5,2,6] => [9:4] )
  ." sp,"   opcode @  0
  over  $1000 and  IF $00000200      or THEN  \ bit   12 =>     9
  over  #04 lshift    $00000180 and  or       \ bit 4: 3 =>  8: 7
  over  $0020 and  IF $00000040      or THEN  \ bit    5 =>     6
  over  $0004 and  IF $00000020      or THEN  \ bit    2 =>     5
  swap  $0040 and  IF $00000010      or THEN  \ bit    6 =>     4
  n. ;

: rd,imm17_12_ci       ( -- ; imm = [12,6:2] => [17:12] )
  rd.  ,.  opcode @  0
  over  $1000 and  IF $fffe0000      or THEN \ bit 12    =>    17+sign
  swap  #10 lshift    $0001f000 and  or      \ bit  6: 2 => 16:12
  #12 ashift  n. ;                        \ but shown on  5: 0

: rd',rs2'_c           ( -- )
  rd'_c.  ,.  rs2'_c. ;

: rd',uimm5_0_c        ( -- ; imm = [12,4:0] => [5:0] )
  rd'_c.  ,.  opcode @  0
  over  $1000 and  IF $00000020      or THEN \ bit    12 =>     5
  swap  #02 rshift    $0000001f and  or      \ bit  6: 2 =>  4: 0
  n. ;

: rd',imm5_0_c         ( -- ; imm = [12,4:0] => [5:0] )
  rd'_c.  ,.  opcode @  0
  over  $1000 and  IF $ffffffe0      or THEN \ bit    12 =>     5+Sign
  swap  #02 rshift    $0000001f and  or      \ bit  6: 2 =>  4: 0
  n. ;

: rs',addr8_1_cb       ( addr -- addr ; imm = [12,6:5,2,11:10,4:3] => [8:1] )
  rd'_c.  ,.  opcode @  0
  over  $1000 and  IF $00000100      or THEN \ bit 12    =>     8
  over  #01 lshift    $000000c0 and  or      \ bit  6: 5 =>  7: 6
  over  $0004 and  IF $00000020      or THEN \ bit  2    =>     5
  over  #07 rshift    $00000018 and  or      \ bit 11:10 =>  4: 3
  swap  #02 rshift    $00000006 and  or      \ bit  4: 3 =>  2: 1
  dup   $0100 and  IF $fffffe00      or THEN \ Sign
  over +  8 0x.r ;

: addr11_1_cb          ( addr -- addr ; imm = [12,8,10:9,6,7,2,11,5:3] => [11:1] )
  opcode @  0
  over  $1000 and  IF $00000800      or THEN \ bit 12    =>    11
  over  $0100 and  IF $00000400      or THEN \ bit  8    =>    10
  over  #01 rshift    $00000300 and  or      \ bit 10: 9 =>  9: 8
  over  $0040 and  IF $00000080      or THEN \ bit  6    =>     7
  over  $0080 and  IF $00000040      or THEN \ bit  7    =>     6
  over  $0004 and  IF $00000020      or THEN \ bit  2    =>     5
  over  $0800 and  IF $00000010      or THEN \ bit 11    =>     4
  swap  #02 rshift    $0000000e and  or      \ bit  5: 3 =>  3: 1
  dup   $0800 and  IF $fffff000      or THEN \ Sign
  over +  8 0x.r ;

: x1,addr11_1_cb       ( addr -- addr ; imm = [12,8,10:9,6,7,2,11,5:3] => [11:1] )
  ." x1,"  addr11_1_cb ;

: rs2,uimm7_2(sp)_css  ( -- ; imm = [8:7,12:9] => [7:2] )
  rs2_c.  ,.   opcode @  0
  over  #01 rshift    $000000c0 and  or      \ bit  8: 7 =>  7: 6
  swap  #07 rshift    $0000003c and  or      \ bit 12: 9 =>  5: 2
  n.  ." (sp)" ;

: rs2',sp,uimm9_2_ciw  ( -- ; imm = [10:7,12:11,5,6] => [9:2] )
  rs2'_c.  ." ,sp,"  opcode @  0
  over  #01 rshift    $000003c0 and  or      \ bit 10: 7 =>  9: 6
  over  #07 rshift    $00000030 and  or      \ bit 12:11 =>  5: 4
  over  $0020 and  IF $00000008      or THEN \ bit  5    =>     3
  swap  $0040 and  IF $00000004      or THEN \ bit  6    =>     2
  n. ;

: rs2',uimm6_2(rs')_c  ( -- ; uimm = [5,12:10,6] => [6:2] )
  rs2'_c.  ,.  opcode @  0
  over  $0020 and  IF $00000040      or THEN \ bit  5    =>     6
  over  #07 rshift    $00000078 and  or      \ bit 12:10 =>  5: 3
  swap  $0040 and  IF $00000004      or THEN \ bit  6    =>     2
  n.  ." ("  rd'_c.  ." )" ;

Createp rv32c_table
\ mask         opcode       mnemonics        operands
\ bit 16=1 if rd<>0; bit 17=1 if rs2<>0
  $0000ffff ,p  $00000000 ,p  s" unimp     " $,p  ' nop                ,p
  $0000e003 ,p  $00000000 ,p  s" c.addi4spn" $,p  ' rs2',sp,uimm9_2_ciw ,p
  $0000e003 ,p  $00004000 ,p  s" c.lw      " $,p  ' rs2',uimm6_2(rs')_c ,p
  $0000e003 ,p  $0000c000 ,p  s" c.sw      " $,p  ' rs2',uimm6_2(rs')_c ,p
  $0000ffff ,p  $00000001 ,p  s" c.nop     " $,p  ' nop                ,p
  $0001e003 ,p  $00010001 ,p  s" c.addi    " $,p  ' rd,imm5_0_c        ,p
  $0000e003 ,p  $00002001 ,p  s" c.jal     " $,p  ' x1,addr11_1_cb     ,p
  $0001e003 ,p  $00014001 ,p  s" c.li      " $,p  ' rd,imm5_0_c        ,p
  $0001ef83 ,p  $00016101 ,p  s" c.addi16sp" $,p  ' sp,imm9_4_ci       ,p
  $0001e003 ,p  $00016001 ,p  s" c.lui     " $,p  ' rd,imm17_12_ci     ,p
  $0000ec03 ,p  $00008001 ,p  s" c.srli    " $,p  ' rd',uimm5_0_c      ,p
  $0000ec03 ,p  $00008401 ,p  s" c.srai    " $,p  ' rd',uimm5_0_c      ,p
  $0000ec03 ,p  $00008801 ,p  s" c.andi    " $,p  ' rd',imm5_0_c       ,p
  $0000fc63 ,p  $00008c01 ,p  s" c.sub     " $,p  ' rd',rs2'_c         ,p
  $0000fc63 ,p  $00008c21 ,p  s" c.xor     " $,p  ' rd',rs2'_c         ,p
  $0000fc63 ,p  $00008c41 ,p  s" c.or      " $,p  ' rd',rs2'_c         ,p
  $0000fc63 ,p  $00008c61 ,p  s" c.and     " $,p  ' rd',rs2'_c         ,p
  $0000e003 ,p  $0000a001 ,p  s" c.j       " $,p  ' addr11_1_cb        ,p
  $0000e003 ,p  $0000c001 ,p  s" c.beqz    " $,p  ' rs',addr8_1_cb     ,p
  $0000e003 ,p  $0000e001 ,p  s" c.bnez    " $,p  ' rs',addr8_1_cb     ,p
  $0001e003 ,p  $00010002 ,p  s" c.slli    " $,p  ' rd,uimm5_0_c       ,p
  $0000e003 ,p  $0000c002 ,p  s" c.swsp    " $,p  ' rs2,uimm7_2(sp)_css ,p
  $0001f07f ,p  $00018002 ,p  s" c.jr      " $,p  ' rd.                ,p
  $0001f003 ,p  $00018002 ,p  s" c.mv      " $,p  ' rd,rs2_c           ,p
  $0000ffff ,p  $00009002 ,p  s" c.ebreak  " $,p  ' nop                ,p
  $0001f07f ,p  $00019002 ,p  s" c.jalr    " $,p  ' rd.                ,p
  $0003f003 ,p  $00039002 ,p  s" c.add     " $,p  ' rd,rs2_c           ,p
  $0001e003 ,p  $00014002 ,p  s" c.lwsp    " $,p  ' rd,uimm7_2_ci(sp)  ,p
\ End  
  $00000000 ,p
End-Create

: dis16         ( addr -- addr+2 )
  opcode @  $ffff and                   \ only 16 bit
        dup $0f80 or  IF $10000 or THEN \ rd<>0  => bit 16
        dup $007c or  IF $20000 or THEN \ rs2<>0 => bit 17
  opcode !
  cr  addr.  opcode16.
  opcode @  rv32c_table  findop
  IF   8 +  dup countp typep space  #12 +   @p execute  2 +  exit THEN
  ." unimp "  2 + ;

\ --------------------------------------------------------------
\ Disassembler for all opcode:
\ --------------------------------------------------------------
: opcode@       ( addr -- opcode ) \ read 32 bit opcode
  dup  3 +  c@p     8 lshift
  over 2 +  c@p  +  8 lshift
  over 1 +  c@p  +  8 lshift
  swap      c@p  + ;

Forth Definitions  Dassembler

: dis           ( addr -- addr+x )
\ dup 1 and IF 1 + THEN \ !!! tests for aligned address !!!
  base @  >r  hex
  dup ( here + ) \ !!! expect program address 0 at here !!!
  opcode@   dup opcode !
  dup $ffff and  $ffff =             \ special case $ffff
  IF   drop  cr  addr.  opcode16.  ." 0xffff "  2 +
  ELSE 3 and  3 =   IF dis32 ELSE dis16 THEN
  THEN r>  base ! ;

: ndis          ( addr n -- addr+x )
  FOR dis NEXT ;

onlyforth
