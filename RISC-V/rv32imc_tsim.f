\ --------------------------------------------------------------
\ Simulator for RISC-V (RV32I and RV32C) - here GD32F103CB
\ (c) Klaus Kohl-Schoepe
\ Licence: LGPLv3 - see https://www.gnu.org/licenses/lgpl-3.0.de.html
\ --------------------------------------------------------------
\ Comment:
\ - Support x0..x31
\ - requires 32-Bit Forth - testet on mcForth (using Createp/End-Create/p,/$,p/@p)
\
\ Logbook:
\ - 17.09.21 KKS: Start with base rv32imc_tdis.f
\ - 17.09.21 KKS: Adapted to GD32F103CB
\
\ Info:
\ - 0x0000 0000 - 0x0001 FFFF Flash =  128KByte (default $FF)
\ - 0x2000 0000 - 0x2000 7FFF SRAM  =   32KByte (default $00)
\ - 0x4001 3800 - 0x4001 3BFF USART 0 (used on Longan Nano Board)
\ 
\ --------------------------------------------------------------

\ --------------------------------------------------------------
\ General used flags
\ --------------------------------------------------------------
\ Stop-Flag if any error
Variable endflag

\ Flag for simulator output
\ 1=wait for key; 2=disassembler; 4=all read/write, 8= only reg/external
Variable info
: ikeywait      ( -- ) \ debug wait
  info @  3 and  1 = IF key drop THEN ;
: icr           ( -- ) \ CR if only read/write/reg
  info @  dup 2 and 0=  swap $0c and  and IF cr THEN ;
  
: imem?         ( addr -- f ) \ flag for memory access output
  info @  $06 and 0<>  ?dup ?exit  \ all r/w or debug
  info @  $08 and                  \ only ext. memory + Reg ?
  IF   dup  $00020000           u<            \ not in Flash
       over $20000000 $20008000 within or     \ or RAM
       over $40013800 =                or     \ or UART_State
       over $40013804 =                or  0= \ or UART_Data
  ELSE 0
  THEN ;

\ Count of executed opcodes
Variable opcounter

\ --------------------------------------------------------------
\ Handling GD32F103 Memory
\ --------------------------------------------------------------
\ Speicherverwaltung und Target-Dictionary
2Variable seg_f       \ Target-Flash-Segment
2Variable seg_r       \ Target-RAM-Segment

: mem_alloc   ( -- ) ( abort if error )
  seg_f 2@ or ?exit                 \ Flash still allocated
  $20000. xalloc  ?abort" XALLOC"  seg_f 2!
  seg_r 2@ or ?exit                 \ RAM still allocated
   $8000. xalloc  ?abort" XALLOC"  seg_r 2! ;

: mem_free    ( -- ) ( Abort if error )
  seg_r 2@ or 0=exit                \ RAM not used
  seg_r 2@  xfree  ?abort" XFREE"
  0. seg_r 2!
  seg_f 2@ or 0=exit                \ RAM not used
  seg_f 2@  xfree  ?abort" XFREE"
  0. seg_f 2! ;

: tc@           ( addr -- char ) \ only Flash or RAM allowed
  dup  $00000000  $00020000 within \ Flash
  IF                0  seg_f 2@  d+  c@x exit THEN
  dup  $20000000  $20008000 within \ RAM
  IF  $20000000 -   0  seg_r 2@  d+  c@x exit THEN
  dup  $40000000  $00000000 within \ ignore the rest
  IF  drop                           0   exit THEN
  ."  Illegal read access from $"
  base @ hex  swap 8 0u.r  base !
  abort ;

: tc!           ( char addr -- ) \ only Flash or RAM allowed
  dup $00000000  $00020000 within \ Flash
  IF               0  seg_f 2@  d+  c!x exit THEN
  dup $20000000  $20008000 within \ RAM
  IF  $20000000 -  0  seg_r 2@  d+  c!x exit THEN
  dup $08000000  $08020000 within \ Flash
  IF  $08000000 -  0  seg_r 2@  d+  c!x exit
  THEN
  dup   $40000000  $42000000 within     \ Peripheral
  over  $d2000000  $d3000000 within  or \ Flash
  over  $e0000000  $f0000000 within  or \ ???
  IF  drop  drop                        exit THEN
  ."  Illegal write access: $"
  base @ >r  hex  8 0u.r  ."  <= $"  2 0u.r  r> base !
  abort ;

\ --------------------------------------------------------------
\ Load disassembler and compile simulator
\ --------------------------------------------------------------
Include rv32imc_tdasm.f

onlyforth

[UNDEFINED] Simulator   Vocabulary Simulator [THEN]
Simulator Definitions

\ use this variable for decoding
Variable opcode

\ --------------------------------------------------------------
\ Register access:
\ --------------------------------------------------------------
\ Supports 32 registers - Reg 0 is PC
#32 Constant #regs
Create regs   #regs cells allot   End-Create
Variable mepc

: reg@          ( reg -- n )
  ?dup IF cells regs +  @ ELSE 0 THEN ;
: reg!          ( n reg -- )
  info @  $06 and  0<>   over  and
  IF  base @  >r
      decimal icr ."   x" dup .  hex ." <= $"  over 8 0u.r
      r> base !  THEN
  ?dup IF cells regs +  ! ELSE drop THEN ;

: rd?           ( -- reg ; bits [11: 7] )
  opcode @  #07 rshift  $1f and ;
: rd@           ( -- n )
  rd?  reg@ ;
: rd!           ( n -- ; bits [11: 7] )
  rd?  reg! ;
: rd'?        ( -- reg ; bits [9:7]+8 )
  opcode @  #07 rshift  $07 and  8 + ;
: rd'@        ( -- n ; bits [9:7]+8 )
  rd'?  reg@ ;
: rd'!        ( n -- ; bits [9:7]+8 )
  rd'?  reg! ;

: rs1?          ( -- reg ; bits [20:15] )
  opcode @  #15 rshift  $1f and ;
: rs1@          ( -- n )
  rs1? reg@ ;

: rs2'?       ( -- reg ; bits 4:2 + 8 )
  opcode @  #02 rshift  $07 and  8 + ;
: rs2'@       ( -- ; bits [4:2]+8 )
  rs2'?  reg@ ;
: rs2'!       ( n -- )
  rs2'?  reg! ;

: rs2@          ( -- ; bits [24:20] )
  opcode @  #20 rshift  $1f and  reg@ ;
: rs2_c@        ( -- ; bits [6:2] )
  opcode @  #02 rshift  $1f and  reg@ ;

: regs.         ( -- ) \ Display register
  base @ >r  hex
  cr  ." PC   :   " regs @  8 0u.r
     ."  MEPC :   " mepc @  8 0u.r
  0  #32 FOR
    dup 7 and 0= IF cr THEN  dup reg@  8 0u.r space  1+
  NEXT drop
  r> base ! ;

\ --------------------------------------------------------------
\ Memory access:
\ --------------------------------------------------------------
Variable uart_state
Variable uart_data

\ Read access (Byte, Half, Word) with UART handling
: c@s           ( addr -- char ) \ read byte
  imem?
  IF   base @ >r  hex
       icr ."   Byte ($" dup 8 0u.r  ." )  "
       dup $40013800 = IF ." UART_State  " THEN
       dup $40013804 = IF ." UART_Data  " THEN
       r> base !
  THEN
  dup  $40013800  = \ read uart state
  IF   drop  uart_state @                           exit THEN
  dup  $40013804  = \ read uart data reset RX state
  IF   drop  uart_data @
       uart_state @  $20 invert  and  uart_state !  exit THEN
  tc@ ;
: cs@s          ( addr -- n ) \ Sign extend to 32-bit
  c@s  dup $0080 and  IF $ffffff00 or THEN ;
: h@s           ( addr -- hword ) \ read 16 bit
  dup 1 and IF ."  Misaligned Half !!! "  -1 endflag !  THEN
  imem?
  IF   base @ >r  hex
       icr ."   Half ($" dup 8 0u.r  ." )  "
       dup $40013800 = IF ." UART_State  " THEN
       dup $40013804 = IF ." UART_Data  " THEN
       r> base !
  THEN
  dup  $40013800  = \ read uart state
  IF   drop  uart_state @                           exit THEN
  dup  $40013804  = \ read uart data reset RX state
  IF   drop  uart_data @
       uart_state @  $20 invert  and  uart_state !  exit THEN
  dup  1 +  tc@  8 lshift
  swap      tc@  + ;
: hs@s          ( addr -- n ) \ Sign extend to 32-bit
  h@s  dup $8000 and  IF $ffff0000 or THEN ;
: (@s           ( addr -- word ) \ read 32 bit
  dup 1 and IF ."  Misaligned Word !!! "  -1 endflag !  THEN
  dup  3 +  tc@     8 lshift
  over 2 +  tc@  +  8 lshift
  over 1 +  tc@  +  8 lshift
  swap      tc@  + ;
: @s            ( addr -- word ) \ read 32 bit with debug
  imem?
  IF   base @ >r  hex
       icr ."   Word ($" dup 8 0u.r  ." )  "
       dup $40013800 = IF ." UART_State  " THEN
       dup $40013804 = IF ." UART_Data  " THEN
       r> base !
  THEN
  dup  $40013800  = \ read uart state
  IF   drop  uart_state @                           exit THEN
  dup  $40013804  = \ read uart data reset RX state
  IF   drop  uart_data @
       uart_state @  $20 invert  and  uart_state !  exit THEN
  (@s ;

\ Write access (Byte, Half, Word) with UART handling
: c!s           ( char addr -- ) \ write byte
  imem?
  IF  base @ >r  hex
      icr  ."   mem($" dup 8 0u.r ." ) <= $"  over 2 0u.r
      dup $40013800 = IF ."   UART_State  " THEN
      dup $40013804 = IF ."   UART_Data  " THEN
      r> base !  THEN
  dup  $40013800 = \ write uart state
  IF drop  uart_state !  exit THEN
  dup  $40013804 = \ output writen uart data
  IF drop  emit  exit THEN
  tc! ;
: h!s           ( hword addr -- ) \ read 16 bit
  dup 1 and IF ."  Misaligned !!! " ikeywait  THEN
  imem?
  IF  base @ >r  hex
      icr ."   mem($" dup 8 0u.r ." ) <= $"  over 4 0u.r
      dup $40013800 = IF ."   UART_State  " THEN
      dup $40013804 = IF ."   UART_Data  " THEN
      r> base !  THEN
  dup  $40013800 = \ write uart state
  IF drop  uart_state !  exit THEN
  dup  $40013804 = \ output writen uart data
  IF drop  emit  exit THEN
  over            over      tc!
  swap  8 rshift  swap 1 +  tc! ;
: !s            ( hword addr -- ) \ write 32 bit
  dup 1 and IF ."  Misaligned !!! " ikeywait  THEN
  imem?
  IF  base @ >r  hex
      icr ."   mem($" dup 8 0u.r ." ) <= $"  over 8 0u.r
      dup $40013800 = IF ."   UART_State  " THEN
      dup $40013804 = IF ."   UART_Data  " THEN
      r> base !  THEN
  dup  $40013800 = \ write uart state
  IF drop  uart_state !  exit THEN
  dup  $40013804 = \ output writen uart data
  IF drop  emit  exit THEN
  >r        dup  r@      tc!
  8 rshift  dup  r@ 1 +  tc!
  8 rshift  dup  r@ 2 +  tc!
  8 rshift       r> 3 +  tc! ;

\ --------------------------------------------------------------
\ Tools:
\ --------------------------------------------------------------
: findop    ( opcode table -- 0 | entry -1 ) \ Find opcode in table
  BEGIN  dup @p                              \ end of table ?
  WHILE  over over @p and  over cell+ @p  =  \ compare mask opcode with table
         IF  nip  -1  exit THEN              \ found
         3 cells +                           \ next entry
  REPEAT drop  drop  0 ;                     \ not found

\ --------------------------------------------------------------
\ Get literal/addr out of 32-bit opcode
\ --------------------------------------------------------------
: uimm4_0_i     ( -- u ; uimm = [24:20] => [4:0] )
  opcode @  #20 rshift    $0000001f and ;        \ bit 24:20 => 4:0

: imm11_0_i     ( -- n ; imm = [31:20] => [11:0] )
  opcode @
        #20 rshift    $00000fff and            \ bit 31:20 => 11:0
  dup   $0800 and  IF $fffff000      or THEN ; \ Sign

: imm11_0_i(rs1) ( -- addr ; imm = [31:20] => [11:0] )
  imm11_0_i  rs1@  + ;

: imm11_0(rs1)_s ( -- addr ; imm = [31:25,11:7] => [11:0] )
  opcode @  0
  over  #20 rshift    $00000fe0 and  or      \ bit 31:25 => 11: 5
  swap  #07 rshift    $0000001f and  or      \ bit 11: 7 =>  4: 0
  dup   $0800 and  IF $fffff000      or THEN \ Sign
  rs1@  + ;

: imm12_1_sb ( -- n ; imm = [31,7,30:25,11:8] => [12:1] )
  opcode @  0
  over  0<         IF $fffff000      or THEN \ bit 31    =>    12+Sign
  over  $0080 and  IF $00000800      or THEN \ bit  7    =>    11
  over  #20 rshift    $000007e0 and  or      \ bit 30:25 => 10: 5
  swap  #07 rshift    $0000001e and  or      \ bit 11: 8 =>  4: 1
  dup   $1000 and  IF $ffffe000      or THEN ; \ Sign

: imm31_12_u     ( -- u ; imm = [31:12] )
  opcode @            $fffff000 and ;        \ bit 31:12

: imm20_1_uj     ( -- n ; imm = [31,19:12,20,30:21] => [20:1] )
  opcode @   0
  over  0<         IF $fff00000      or THEN \ bit 31    =>    20+Sign
  over                $000ff000 and  or      \ bit 19:12 => 19:12
  over  #09 rshift    $00000800 and  or      \ bit    20 =>    11
  swap  #20 rshift    $000007fe and  or ;    \ bit 30:21 => 10: 1

: addr          ( -- addr = [31:20] => [11:0] )
  opcode @
        #20 rshift    $00000fff and ;

: addr,uimm         ( -- addr = [31:20] => [11:0]; imm = [19:15] => 5:0 )
  opcode @
  dup   #20 rshift    $00000fff and          \ bit 31:20 => 11:0
  swap  #15 rshift    $0000001f and ;        \ bit 19:15 =>  5:0

\ execute 32-bit opcode ( pc on stack )
: i.lb                  imm11_0_i(rs1) cs@s   rd!  4 + ;
: i.lh                  imm11_0_i(rs1) hs@s   rd!  4 + ;
: i.lw                  imm11_0_i(rs1)   @s   rd!  4 + ;
: i.lbu                 imm11_0_i(rs1)  c@s   rd!  4 + ;
: i.lhu                 imm11_0_i(rs1)  h@s   rd!  4 + ;
: i.fence.i       4 + ;
: i.fence         4 + ;
: i.addi          rs1@  imm11_0_i  +          rd!  4 + ;
: i.slli          rs1@  uimm4_0_i  lshift     rd!  4 + ;
: i.slti          rs1@  imm11_0_i  <  1 and   rd!  4 + ;
: i.sltiu         rs1@  imm11_0_i  u<  1 and  rd!  4 + ;
: i.xori          rs1@  imm11_0_i  xor        rd!  4 + ;
: i.srli          rs1@  uimm4_0_i  rshift     rd!  4 + ;
: i.srai          rs1@  uimm4_0_i  ashift     rd!  4 + ;
: i.ori           rs1@  imm11_0_i  or         rd!  4 + ;
: i.andi          rs1@  imm11_0_i  and        rd!  4 + ;
: i.auipc         dup   imm31_12_u  +         rd!  4 + ;
: i.sb            rs2@  imm11_0(rs1)_s        c!s  4 + ;
: i.sh            rs2@  imm11_0(rs1)_s        h!s  4 + ;
: i.sw            rs2@  imm11_0(rs1)_s        !s   4 + ;
: i.add           rs1@  rs2@       +          rd!  4 + ;
: i.mul           rs1@  rs2@       *          rd!  4 + ;
: i.sub           rs1@  rs2@       -          rd!  4 + ;
: i.sll           rs1@  rs2@       lshift     rd!  4 + ;
: i.mulh          rs1@  rs2@       m* nip     rd!  4 + ;
: i.slt           rs1@  rs2@       <  1 and   rd!  4 + ;
: i.mulhsu        rs2@  rs1@  dup 0<  dup >r
  IF negate THEN um*  r> IF dnegate THEN nip  rd!  4 + ;
: i.sltu          rs1@  rs2@       u<  1 and  rd!  4 + ;
: i.mulhu         rs1@  rs2@       um* nip    rd!  4 + ;
: i.xor           rs1@  rs2@       xor        rd!  4 + ;
: i.div           rs1@  rs2@       /          rd!  4 + ;
: i.srl           rs1@  rs2@       rshift     rd!  4 + ;
: i.divu          rs1@ 0  rs2@   um/mod nip   rd!  4 + ;
: i.sra           rs1@  rs2@       ashift     rd!  4 + ;
: i.or            rs1@  rs2@       or         rd!  4 + ;
: i.rem           rs1@  rs2@       mod        rd!  4 + ;
: i.and           rs1@  rs2@       and        rd!  4 + ;
: i.remu          rs1@ 0  rs2@   um/mod drop  rd!  4 + ;
: i.lui           imm31_12_u                  rd!  4 + ;
: i.beq           rs1@  rs2@  =   IF imm12_1_sb  + ELSE 4 + THEN ;
: i.bne           rs1@  rs2@  =   IF 4 + ELSE imm12_1_sb  + THEN ;
: i.blt           rs1@  rs2@  <   IF imm12_1_sb  + ELSE 4 + THEN ;
: i.bge           rs1@  rs2@  <   IF 4 + ELSE imm12_1_sb  + THEN ;
: i.bltu          rs1@  rs2@  u<  IF imm12_1_sb  + ELSE 4 + THEN ;
: i.bgeu          rs1@  rs2@  u<  IF 4 + ELSE imm12_1_sb  + THEN ;
: i.jalr              4 +  rd!  rs1@  imm11_0_i  +  -2 and ;
: i.jal           dup 4 +  rd!        imm20_1_uj +         ;
: i.ecall         mepc !   $11 cells ;
: i.ebreak        mepc !   $03 cells ;
: i.uret          4 + ;
: i.sret          4 + ;
: i.wfi           4 + ;
: i.sfence.vma    4 + ;
: i.mret          4 + ;
\ Maybe here are some register to handle
: i.csrw
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  <= $" rs1@ 8 0u.r  ikeywait
      r> base !
  THEN 4 + ; \ addr
: i.csrrw
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  <= $" rs1@ 8 0u.r  ikeywait
      r> base !
  THEN 0  rd!  4 + ; \ addr
: i.csrwi
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  <= $" rs1? 8 0u.r  ikeywait
      r> base !
  THEN 4 + ; \ addr,uimm
: i.csrrwi
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  <= $" rs1? 8 0u.r  ikeywait
      r> base !
  THEN 0  rd!  4 + ; \ addr,uimm
: i.csrs
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  |= $" rs1@ 8 0u.r  ikeywait
      r> base !
  THEN 4 + ; \ addr
: i.csrrs
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  |= $" rs1@ 8 0u.r  ikeywait
      r> base !
  THEN 0  rd!  4 + ; \ addr
: i.csrsi
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  |= $" rs1? 8 0u.r  ikeywait
      r> base !
  THEN 4 + ; \ addr,uimm
: i.csrrsi
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  |= $" rs1? 8 0u.r  ikeywait
      r> base !
  THEN 0  rd!  4 + ; \ addr,uimm
: i.csrc
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  &= $" rs1@ 8 0u.r  ikeywait
      r> base !
  THEN 4 + ; \ addr
: i.csrrc
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  &= $" rs1@ 8 0u.r  ikeywait
      r> base !
  THEN 0  rd!  4 + ; \ addr
: i.csrci
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  &= $" rs1? 8 0u.r  ikeywait
      r> base !
  THEN 4 + ; \ addr,uimm
: i.csrrci
  info @  $0c  and
  IF  base @  >r  hex
      icr ."   Reg: " imm11_0_i 3 0u.r  ."  &= $" rs1? 8 0u.r  ikeywait
      r> base !
  THEN 0  rd!  4 + ; \ addr,uimm

Createp rv32i1_table
\ mask          opcode        command
  $0000707f ,p  $00000003 ,p  ' i.lb         ,p
  $0000707f ,p  $00001003 ,p  ' i.lh         ,p
  $0000707f ,p  $00002003 ,p  ' i.lw         ,p
  $0000707f ,p  $00004003 ,p  ' i.lbu        ,p
  $0000707f ,p  $00005003 ,p  ' i.lhu        ,p
  $ffffffff ,p  $0000100f ,p  ' i.fence.i    ,p
  $ffffffff ,p  $0ff0000f ,p  ' i.fence      ,p
  $0000707f ,p  $00000013 ,p  ' i.addi       ,p
  $fe00707f ,p  $00001013 ,p  ' i.slli       ,p
  $0000707f ,p  $00002013 ,p  ' i.slti       ,p
  $0000707f ,p  $00003013 ,p  ' i.sltiu      ,p
  $0000707f ,p  $00004013 ,p  ' i.xori       ,p
  $fe00707f ,p  $00005013 ,p  ' i.srli       ,p
  $fe00707f ,p  $40005013 ,p  ' i.srai       ,p
  $0000707f ,p  $00006013 ,p  ' i.ori        ,p
  $0000707f ,p  $00007013 ,p  ' i.andi       ,p
  $0000007f ,p  $00000017 ,p  ' i.auipc      ,p
  $0000707f ,p  $00000023 ,p  ' i.sb         ,p
  $0000707f ,p  $00001023 ,p  ' i.sh         ,p
  $0000707f ,p  $00002023 ,p  ' i.sw         ,p
  $fe00707f ,p  $00000033 ,p  ' i.add        ,p
  $fe00707f ,p  $02000033 ,p  ' i.mul        ,p
  $fe00707f ,p  $40000033 ,p  ' i.sub        ,p
  $fe00707f ,p  $00001033 ,p  ' i.sll        ,p
  $fe00707f ,p  $02001033 ,p  ' i.mulh       ,p
  $fe00707f ,p  $00002033 ,p  ' i.slt        ,p
  $fe00707f ,p  $02002033 ,p  ' i.mulhsu     ,p
  $fe00707f ,p  $00003033 ,p  ' i.sltu       ,p
  $fe00707f ,p  $02003033 ,p  ' i.mulhu      ,p
  $fe00707f ,p  $00004033 ,p  ' i.xor        ,p
  $fe00707f ,p  $02004033 ,p  ' i.div        ,p
  $fe00707f ,p  $00005033 ,p  ' i.srl        ,p
  $fe00707f ,p  $02005033 ,p  ' i.divu       ,p
  $fe00707f ,p  $40005033 ,p  ' i.sra        ,p
  $fe00707f ,p  $00006033 ,p  ' i.or         ,p
  $fe00707f ,p  $02006033 ,p  ' i.rem        ,p
  $fe00707f ,p  $00007033 ,p  ' i.and        ,p
  $fe00707f ,p  $02007033 ,p  ' i.remu       ,p
\ End  
  $00000000 ,p
End-Create

Createp rv32i2_table
  $0000007f ,p  $00000037 ,p  ' i.lui        ,p
  $0000707f ,p  $00000063 ,p  ' i.beq        ,p
  $0000707f ,p  $00001063 ,p  ' i.bne        ,p
  $0000707f ,p  $00004063 ,p  ' i.blt        ,p
  $0000707f ,p  $00005063 ,p  ' i.bge        ,p
  $0000707f ,p  $00006063 ,p  ' i.bltu       ,p
  $0000707f ,p  $00007063 ,p  ' i.bgeu       ,p
  $0000707f ,p  $00000067 ,p  ' i.jalr       ,p
  $0000007f ,p  $0000006f ,p  ' i.jal        ,p
  $ffffffff ,p  $00000073 ,p  ' i.ecall      ,p
  $ffffffff ,p  $00100073 ,p  ' i.ebreak     ,p
  $ffffffff ,p  $00200073 ,p  ' i.uret       ,p
  $ffffffff ,p  $10200073 ,p  ' i.sret       ,p
  $ffffffff ,p  $10500073 ,p  ' i.wfi        ,p
  $fe00707f ,p  $12000073 ,p  ' i.sfence.vma ,p
  $ffffffff ,p  $30200073 ,p  ' i.mret       ,p
  $00007fff ,p  $00001073 ,p  ' i.csrw       ,p
  $0000707f ,p  $00001073 ,p  ' i.csrrw      ,p
  $00007fff ,p  $00005073 ,p  ' i.csrwi      ,p
  $0000707f ,p  $00005073 ,p  ' i.csrrwi     ,p
  $00007fff ,p  $00002073 ,p  ' i.csrs       ,p
  $0000707f ,p  $00002073 ,p  ' i.csrrs      ,p
  $00007fff ,p  $00006073 ,p  ' i.csrsi      ,p
  $0000707f ,p  $00006073 ,p  ' i.csrrsi     ,p
  $00007fff ,p  $00003073 ,p  ' i.csrc       ,p
  $0000707f ,p  $00003073 ,p  ' i.csrrc      ,p
  $00007fff ,p  $00007073 ,p  ' i.csrci      ,p
  $0000707f ,p  $00007073 ,p  ' i.csrrci     ,p
\ End
  $00000000 ,p
End-Create

: sim32         ( addr -- addr2 0 / addr -1 )
  opcode @  rv32i1_table  findop
  IF 8 +  @p execute  0 exit THEN
  opcode @  rv32i2_table  findop
  IF 8 +  @p execute  0 exit THEN
  -1 ;

\ --------------------------------------------------------------
\ Disassembler for 16-bit opcode:
\ --------------------------------------------------------------
: uimm5_0_c     ( -- u ; imm = [12,4:0] => [5:0] )
  opcode @  0
  over  $1000 and  IF $00000020      or THEN \ bit 12    =>     5
  swap  #02 rshift    $0000001f and  or  ;   \ bit  6: 2 =>  4: 0

: imm5_0_c      ( -- n ; imm = [12,4:0] => [5:0] )
  opcode @  0
  over  $1000 and  IF $ffffffe0      or THEN \ bit 12    =>     5+Sign
  swap  #02 rshift    $0000001f and  or ;    \ bit  6: 2 =>  4: 0

: uimm7_2_ci(sp) ( -- addr ; imm = [3:2,12,6:4] => [7:2] )
  opcode @  0
  over  #04 lshift    $000000c0 and  or      \ bit  3: 2 =>  7: 6
  over  $1000 and  IF $00000040      or THEN \ bit    12 =>     5
  swap  #02 rshift    $0000001c and  or      \ bit  6: 4 =>  4: 2
  2 reg@  + ;

: imm9_4_ci     ( -- u ; imm = [12,4:3,5,2,6] => [9:4] )
  opcode @  0
  over  $1000 and  IF $fffffe00      or THEN  \ bit   12 =>     9+Sign
  over  #04 lshift    $00000180 and  or       \ bit 4: 3 =>  8: 7
  over  $0020 and  IF $00000040      or THEN  \ bit    5 =>     6
  over  $0004 and  IF $00000020      or THEN  \ bit    2 =>     5
  swap  $0040 and  IF $00000010      or THEN ; \ bit    6 =>     4

: imm17_12_ci   ( -- u ; imm = [12,6:2] => [17:12] )
  opcode @  0
  over  $1000 and  IF $fffe0000      or THEN \ bit 12    =>    17+Sign
  swap  #10 lshift    $0001f000 and  or ;    \ bit  6: 2 => 16:12

: addr8_1_cb    (  -- n ; imm = [12,6:5,2,11:10,4:3] => [8:1] )
  opcode @  0
  over  $1000 and  IF $ffffff00      or THEN \ bit 12    =>     8+Sign
  over  #01 lshift    $000000c0 and  or      \ bit  6: 5 =>  7: 6
  over  $0004 and  IF $00000020      or THEN \ bit  2    =>     5
  over  #07 rshift    $00000018 and  or      \ bit 11:10 =>  4: 3
  swap  #02 rshift    $00000006 and  or ;    \ bit  4: 3 =>  2: 1

: addr11_1_cb          ( -- n ; imm = [12,8,10:9,6,7,2,11,5:3] => [11:1] )
  opcode @  0
  over  $1000 and  IF $fffff800      or THEN \ bit 12    =>    11+Sign
  over  $0100 and  IF $00000400      or THEN \ bit  8    =>    10
  over  #01 rshift    $00000300 and  or      \ bit 10: 9 =>  9: 8
  over  $0040 and  IF $00000080      or THEN \ bit  6    =>     7
  over  $0080 and  IF $00000040      or THEN \ bit  7    =>     6
  over  $0004 and  IF $00000020      or THEN \ bit  2    =>     5
  over  $0800 and  IF $00000010      or THEN \ bit 11    =>     4
  swap  #02 rshift    $0000000e and  or ;    \ bit  5: 3 =>  3: 1

: uimm7_2(sp)_css  ( -- addr ; imm = [8:7,12:9] => [7:2] )
  opcode @  0
  over  #01 rshift    $000000c0 and  or      \ bit  8: 7 =>  7: 6
  swap  #07 rshift    $0000003c and  or      \ bit 12: 9 =>  5: 2
  2 reg@  + ;

: uimm9_2_ciw  ( -- ; imm = [10:7,12:11,5,6] => [9:2] )
  opcode @  0
  over  #01 rshift    $000003c0 and  or      \ bit 10: 7 =>  9: 6
  over  #07 rshift    $00000030 and  or      \ bit 12:11 =>  5: 4
  over  $0020 and  IF $00000008      or THEN \ bit  5    =>     3
  swap  $0040 and  IF $00000004      or THEN ; \ bit  6    =>     2

: uimm6_2(rs')_c  ( -- addr ; uimm = [5,12:10,6] => [6:2] )
  opcode @  0
  over  $0020 and  IF $00000040      or THEN \ bit  5    =>     6
  over  #07 rshift    $00000078 and  or      \ bit 12:10 =>  5: 3
  swap  $0040 and  IF $00000004      or THEN \ bit  6    =>     2
  rd'@ + ;

\ execute 16-bit opcode
: c.unimp       -1 endflag ! ;
: c.addi4spn    2 reg@ uimm9_2_ciw  +       rs2'!  2 + ;
: c.lw                 uimm6_2(rs')_c  @s   rs2'!  2 + ;
: c.sw          rs2'@  uimm6_2(rs')_c       !s     2 + ;
: c.addi        rd@    imm5_0_c     +       rd!    2 + ;
: c.jal         dup 2 +  1 reg!   addr11_1_cb  + ;
: c.li          imm5_0_c                    rd!    2 + ;
: c.addi16sp    rd@    imm9_4_ci    +       rd!    2 + ;
: c.lui         imm17_12_ci                 rd'!   2 + ;
: c.srli        rd'@   uimm5_0_c   rshift   rd'!   2 + ;
: c.srai        rd'@   uimm5_0_c   ashift   rd'!   2 + ;
: c.andi        rd'@   imm5_0_c    and      rd'!   2 + ;
: c.subi        rd'@   imm5_0_c    -        rd'!   2 + ;
: c.sub         rd'@   rs2'@       -        rd'!   2 + ;
: c.xor         rd'@   rs2'@       xor      rd'!   2 + ;
: c.or          rd'@   rs2'@       or       rd'!   2 + ;
: c.and         rd'@   rs2'@       and      rd'!   2 + ;
: c.j           addr11_1_cb  + ;
: c.beqz        rd'@  IF 2 ELSE addr8_1_cb THEN + ;
: c.bnez        rd'@  IF addr8_1_cb ELSE 2 THEN + ;
: c.slli        rd@    uimm5_0_c   lshift   rd!    2 + ;
: c.lwsp        uimm7_2_ci(sp) @s           rd!    2 + ;
: c.jr          drop    rd@ ;
: c.mv          rs2_c@                      rd!    2 + ;
: c.ebreak      mepc !  #03 cells ;
: c.jalr        rd@     swap  rd! ;
: c.add         rd@     rs2_c@       +      rd!    2 + ;
: c.swsp        rs2_c@  uimm7_2(sp)_css     !s     2 + ;

Createp rv32c_table
\ mask         opcode       mnemonics        operands
\ bit 16=1 if rd<>0; bit 17=1 if rs2<>0
  $0000ffff ,p  $00000000 ,p  ' c.unimp      ,p
  $0000e003 ,p  $00000000 ,p  ' c.addi4spn   ,p
  $0000e003 ,p  $00004000 ,p  ' c.lw         ,p
  $0000e003 ,p  $0000c000 ,p  ' c.sw         ,p
  $0000ffff ,p  $00000001 ,p  ' nop          ,p
  $0001e003 ,p  $00010001 ,p  ' c.addi       ,p
  $0000e003 ,p  $00002001 ,p  ' c.jal        ,p
  $0001e003 ,p  $00014001 ,p  ' c.li         ,p
  $0001ef83 ,p  $00016101 ,p  ' c.addi16sp   ,p
  $0001e003 ,p  $00016001 ,p  ' c.lui        ,p
  $0000ec03 ,p  $00008001 ,p  ' c.srli       ,p
  $0000ec03 ,p  $00008401 ,p  ' c.srai       ,p
  $0000ec03 ,p  $00008801 ,p  ' c.andi       ,p
  $0000fc63 ,p  $00008c01 ,p  ' c.sub        ,p
  $0000fc63 ,p  $00008c21 ,p  ' c.xor        ,p
  $0000fc63 ,p  $00008c41 ,p  ' c.or         ,p
  $0000fc63 ,p  $00008c61 ,p  ' c.and        ,p
  $0000e003 ,p  $0000a001 ,p  ' c.j          ,p
  $0000e003 ,p  $0000c001 ,p  ' c.beqz       ,p
  $0000e003 ,p  $0000e001 ,p  ' c.bnez       ,p
  $0001e003 ,p  $00010002 ,p  ' c.slli       ,p
  $0001e003 ,p  $00014002 ,p  ' c.lwsp       ,p
  $0001f07f ,p  $00018002 ,p  ' c.jr         ,p
  $0001f003 ,p  $00018002 ,p  ' c.mv         ,p
  $0000ffff ,p  $00009002 ,p  ' c.ebreak     ,p
  $0001f07f ,p  $00019002 ,p  ' c.jalr       ,p
  $0003f003 ,p  $00039002 ,p  ' c.add        ,p
  $0000e003 ,p  $0000c002 ,p  ' c.swsp       ,p
\ End  
  $00000000 ,p
End-Create

: sim16         ( addr -- addr+2 )
  opcode @  $ffff and                   \ only 16 bit
        dup $0f80 or  IF $10000 or THEN \ rd<>0  => bit 16
        dup $007c or  IF $20000 or THEN \ rs2<>0 => bit 17
  opcode !
  opcode @  rv32c_table  findop
  IF   8 +  @p execute  0 exit THEN
  -1 ;

\ --------------------------------------------------------------
\ Disassembler for all opcode:
\ --------------------------------------------------------------
Forth Definitions  Simulator also  Forth

: siminit       ( -- ) \ Reset regs and pc
  regs  #regs cells  erase  0 mepc !
  $00000000  $20000 FOR  $ff over tc!  1+  NEXT  drop \ Flash (default $FF)
  $20000000  $08000 FOR  $00 over tc!  1+  NEXT  drop \ RAM (default $00)
  $80 uart_state !  0 uart_data ! ;

: step          ( -- ) \ endflag: 0=ok; -1=unimpl
  opcounter @  1 +  opcounter !      \ Count of steps
  base @  >r  hex
  regs @  dup (@s   dup opcode !
  dup $ffff and  $ffff =             \ special case $ffff
  IF   -1
  ELSE 3 and  3 =   IF sim32 ELSE sim16 THEN
  THEN endflag !  regs !  r>  base ! ;

: d           ( -- ) \ Display opcode and register
  regs @  tdis drop  regs. ;

: dis         ( addr -- ) \ Liste Befehl
  tdis  drop  at-xy? drop ?dup IF #54 swap - ELSE 5 THEN spaces ;
: disn        ( addr n -- ) \ Disassembler
  tdisn drop ;

: stop?       ( -- f ) \ ESC liefert -1 sonst UART
  key?  dup 0=exit drop                      \ keine Taste => 0
  key  dup #27 =  IF drop -1 exit THEN              \ ESC => -1
  uart_data !  uart_state dup @ $20 or swap !   0 ; \ KEY => -1

: s           ( -- ) \ Ein Step
  -1 info !   regs @  dis   step ;
: steps       ( n -- ) \ n Steps
  BEGIN s  1-  dup 0=  stop? or  endflag @ or UNTIL drop ;

: (run_pc       ( PC -- ) \ Laufen bis Stop oder Breakpoint ?
  >r
  BEGIN info @  2  and IF regs @  dis THEN step
    endflag @   IF cr ." Error"      cr  rdrop exit THEN
    regs @ r@ = IF cr ." Breakpoint" cr  rdrop exit THEN
    stop?       IF cr ." Stopped"    cr  rdrop exit THEN
  AGAIN ;
: drun        ( -- ) \ Run mit Anzeige bis Stop
  -1 info !  -1 (run_pc ;
: run         ( -- ) \ Run ohne Anzeigen bis Stop
  0  info !  -1 (run_pc ;
: d_pc        ( PC -- ) \ Debug bis Breakpoint
  -1 info !  (run_pc ;
: r_pc        ( PC -- ) \ Run bis Breakpoint ?
  0  info !  (run_pc ;

Variable ds \ debug-Stack
: debug       ( -- ) \ Debug with info
  -1 info !
  depth  ds !
  BEGIN regs @  dis
    key upc  dup #27 = IF drop exit THEN \ ESC = Break
    CASE [char] C OF ." > " key  dup emit  uart_data !
                     uart_state  dup @  $20 or  swap !
                     step  0            ENDOF           \ Key
         $0d      OF regs @  dup tc@ 3 and 3 = IF 4 + ELSE 2+ THEN  >r
                     0 info !
                     cr  ." Run to $"  base @ hex r@ 8 0u.r base ! 
                     BEGIN step   endflag @   stop?  or
                           regs @  r@  =  or UNTIL
                     -1 info !
                     rdrop 0            ENDOF \ Over
                     step  0            swap  ENDCASE  \ Step
    endflag @ or  ?exit                           \ Abbruch ?
    depth  ds @  -           \ Test des Datenstacks
  UNTIL ."  STACK !!!" ;

: read        ( Filename -- )
  bl word  count  r/w bin  fopen >r
  r@ fsize@ drop
  >r  seg_f 2@  r>  r@  fread drop
  r> fclose ;

: help        ( -- ) \ Help Screen
  cr ." siminit ( -- )              Reset RISC-V (not memory)"
  cr ." read    ( file ; -- )       Read file"
  cr ." dis     ( addr -- addr2)    Disassemble one line"
  cr ." ndis    ( addr n -- addr2 ) Disassemble n lines"
  cr ." d       ( -- )              Show next command"
  cr ." s       ( -- )              One step"
  cr ." steps   ( n -- )            n steps"
  cr ." debug   ( -- )              Single step debug (cr = skip)"
  cr ."    CR  : Jump over (execute call without debug)"
  cr ."    C   : Input one character"
  cr ."    ESC : Stop debugger (also for r and r_pc)"
  cr ." run     ( -- )              Run till ESC or error"
  cr ." r_pc    ( pc -- )           Run till breakpoint"
  cr ." d_run   ( -- )              Trace
  cr ." d_pc    ( pc -- )           Trace till breakpoint
  cr ." x       ( -- )              Exit simulator"
  cr ;

: sim         ( -- ) ( Start Simulator )
  page
  ."     GD32F103-Simulator"      cr
  ." (c) 2021 Klaus Kohl-Schoepe" cr
  ." (use help for command list)" cr cr
  mem_alloc  siminit  hex ;

: x           ( -- ) ( Programm beenden )
  mem_free  bye ;

onlyforth
