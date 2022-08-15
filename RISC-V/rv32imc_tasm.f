\ --------------------------------------------------------------
\ Assembler for RISC-V (RV32IMC)
\ (c) Klaus Kohl-Schoepe
\ Licence: LGPLv3 - see https://www.gnu.org/licenses/lgpl-3.0.de.html
\ --------------------------------------------------------------
\ Comment:
\ - Supports register r0...r31 (or zero and sp)
\ - Separate memory for up to 16 local labels
\ - Check stack depth and parameter
\   ready = reset label and all flags for code
\   reset = only reset flags between mnemonics
\   check = resolve label (e.g. at End-Code)
\   ssp+  = incremenent saved stack deep
\ - FORTH like assembler style:
\   Code test
\     x3 , x4 , x5 add,
\     x3 ,( #4 x4 lb,
\     x3 , $300 , $1f csrrw,
\     x3 , x4 , 1$ beqz,
\     1$:
\     x1 c.jr,  
\   End-Code 
\ - requires 32-Bit Forth - testet on mcForth (using Createp/End-Create/p,/$,p/@p)
\ - !!! <= change this if you use a system which don't align code
\
\ Logbook:
\ - 13.08.21 KKS: Assembler started
\ - 17.08.21 KKS: First revision finished
\ - 02.09.21 KKS: Changed to TASM-Version
\ --------------------------------------------------------------

onlyforth

[UNDEFINED] TAssembler   Vocabulary TAssembler [THEN]
TAssembler Definitions

\ --------------------------------------------------------------
\ Tools:
\ --------------------------------------------------------------
\ Variables
Variable ssp    \ saved stack deep
Variable <arg>  \ amount of parameters

\ Variables and tables for locals and modes
Variable <$>    \ Flag fÅor local variable
Create $tab   ( -- addr ) \ table with 16 entries ($:)
  #16 cells allot End-Create
Create $links ( -- addr ) \ table with 32 flags, Opcodes and addresses
  3 #32 * 1+ cells allot End-Create
Variable addrmodes ( -- addr ) \ 3 * adress modes
Variable pars  ( -- addr ) \ 3 * parameter
  2 cells allot

\ --------------------------------------------------------------
\ Reset of parameters
: reset         ( -- ) \ reset stack depth and arguments
  depth ssp !   \ save stack depth
  0 <arg> !     \ no arguments
  0 addrmodes ! \ reset adress modes
  0 <$> ! ;     \ reset flags foÅr lokal

: -reset?       ( ... f -- f ) \ Reset if f<>0
  dup IF >r reset r> THEN ;

: (reset        ( -- ; test, then clear variable )
  <$> @  -reset? ?abort" Label not used "
  reset ;

: ready         ( -- ) \ reset all flags without error message
  reset                    \ alle variables for the next command
  $tab   #16 cells erase   \ no locals
  $links #65 cells erase ; \ no locals used

\ --------------------------------------------------------------
\ $: $ = local labels
: $:            ( -- ) \ Reference for local address
  Createp dup c,p  cell+              \ save offset and increment
  Doesp>  c@p $tab +
          dup >r @  -reset? ?abort" Label still used "
          tprg@  r>  ! ;             \ write adress

0   $:  1$:  $:  2$:  $:  3$:  $:  4$:
    $:  5$:  $:  6$:  $:  7$:  $:  8$:
    $:  9$:  $: 10$:  $: 11$:  $: 12$:
    $: 13$:  $: 14$:  $: 15$:  $: 16$:  drop

: $             ( -- ; -- addr | n ) \ Use local address
  Createp dup c,p  cell+              \ save offset and increment
  Doesp> c@p   dup $tab +  @          \ Adresse oder Referenz nutzen
         ?dup IF nip ELSE -1 <$> ! THEN ; \ use address or reference

0   $  1$    $  2$    $  3$    $  4$
    $  5$    $  6$    $  7$    $  8$
    $  9$    $ 10$    $ 11$    $ 12$
    $ 13$    $ 14$    $ 15$    $ 16$    drop

\ --------------------------------------------------------------
\ parameter (and register) definition
\ Definition of addrmodes
\ %10987654321098765432109876543210: for the last parameter
\ %0000000xxxxx00000xxxxx00000xxxxx => x=register 0-31    ($01f/07c00/01f00000)
\        |         |         |      => register is x0     ($020/08000/02000000)
\       |         |         |       => register is x2     ($040/10000/04000000)
\      |         |         |        => register is x8-x15 ($080/20000/08000000)
\     |         |         |         => register is x0-x31 ($100/40000/10000000)
\    |         |         |          => immediate/address  ($200/80000/20000000)
\ Bit 10-19  : for the next parameter
\ Bit 20-29  : for the first parameter
\ Bit 30     : last parameter is label
\ Bit 31     : last parameter is imm(reg)
\ 0x000003ff = 3rd (mask $fffffc00)
\ 0x000ffc00 = 2nd (mask $fff00000)
\ 0x3ff00000 = 1nd (mask $fff003ff)

: r1@           ( -- reg )
  addrmodes @              $1f and ;

: r2@           ( -- reg )
  addrmodes @  #10 rshift  $1f and ;

: r3@           ( -- reg )
  addrmodes @  #20 rshift  $1f and ;

: reg:          ( name ; flag -- ) \ define register
  Createp w,p              \ save flag
  Doesp> addrmodes @  $1ff and \ still used ?
         ?abort" second register not allowed "
         w@p  addrmodes @  or  addrmodes ! ;

  $120 reg: x0      $101 reg: x1
  $142 reg: x2      $103 reg: x3
  $104 reg: x4      $105 reg: x5
  $106 reg: x6      $107 reg: x7
  $188 reg: x8      $189 reg: x9
  $18a reg: x10     $18b reg: x11
  $18c reg: x12     $18d reg: x13
  $18e reg: x14     $18e reg: x15
  $110 reg: x16     $111 reg: x17
  $112 reg: x18     $113 reg: x19
  $114 reg: x20     $115 reg: x21
  $116 reg: x22     $117 reg: x23
  $118 reg: x24     $119 reg: x25
  $11a reg: x26     $11b reg: x27
  $11c reg: x28     $11d reg: x29
  $11e reg: x30     $11f reg: x31
  ' x0  Alias zero  ' x1  Alias lr
  ' x2  Alias frp   ' x2  Alias sp
  ' x8  Alias fsp   ' x9  Alias fip
  ' x10 Alias tos   ' x11 Alias cfa
  ' x11 Alias cfc

\ --------------------------------------------------------------
\ test parameter and shift it
: testpars      ( ??? -- ) \ combine adress modes
  depth  ssp @  -   \ stack depth
  dup 1 u>  -reset? ?abort" too much parameter "  \ >1 => Error
  IF   pars !       \ save stack to pars
       addrmodes @  $200 or  addrmodes !
  THEN ;

: ?nolabel?     ( -- ) \ no labels allowed
  <$> @  -reset?  ?abort" labels only for jump" ;

: shiftpars     ( -- ) \ Mode << 10 and par >> par+4 >> par+8
  ?nolabel?
  addrmodes @  $3ff and  0=   -reset? ?abort" no parameter "
  addrmodes @  0<             -reset? ?abort" imm(reg) here not allowed"
  addrmodes @  $fff00000 and  -reset? ?abort" maximal 3 parameters"
  addrmodes @  #10 lshift  addrmodes !
  pars  dup >r @   0    r@ !   r> cell+
        dup >r @   swap r@ !   r> cell+  ! ;

: ?range?          ( flag -- ) \ flag=1 if allowed
  0=  -reset? ?abort" immediate range ?"   ?nolabel? ;
: ?aligned?        ( flag -- ) \ flag=1 if aligned
  0=  -reset? ?abort" not aligned (2/4/16) ?"   ?nolabel? ;
: ?uimm5?       (  -- ) \ uimm5
  pars       @            $00000020  u<      ?range? ;
: ?uimm6?       (  -- ) \ uimm5
  pars       @            $00000040  u<      ?range? ;
: ?imm6?        ( -- ) \ imm6
  pars       @  $fffffe0  $00000020  within  ?range? ;
: ?uimm7-2?      ( -- ) \ uimm5<<2
  pars       @            $00000080  u<      ?range?
  pars       @  $03 and  0=                  ?aligned? ;
: ?imm7-2?      ( -- ) \ imm5<<2
  pars       @  $fffffc0  $00000040  within  ?range?
  pars       @  $03 and  0=                  ?aligned? ;
: ?uimm9-2?      ( -- ) \ imm7<<2
  pars       @            $00000200  u<      ?range?
  pars       @  $03 and  0=                  ?aligned? ;
: ?imm9-4?      ( -- ) \ imm5<<4
  pars       @  $fffff00  $00000100  within  ?range?
  pars       @  $0f and  0=                  ?aligned? ;
: ?imm12?       ( imm12 -- )
  pars       @  $ffff800  $00000800  within  ?range? ;
: ?uimm12-2?    ( uimm12 -- )
  pars cell+ @            $00001000  u<      ?range? ;
: ?imm8o?       ( imm8o -- ) \ offset word aligned
  pars       @  $fffff00  $00000100  within  ?range?
  pars       @  1 and  0=                    ?aligned? ;
: ?imm11o?      ( imm11o -- ) \ offset word aligned
  pars       @  $ffff800  $00000800  within  ?range?
  pars       @  1 and  0=                    ?aligned? ;
: ?imm12o?      ( imm12o -- ) \ offset word aligned
  pars       @  $ffff000  $00001000  within  ?range?
  pars       @  1 and  0=                    ?aligned? ;
: ?uimm20?      ( imm20o -- )
  pars       @            $000fffff  u<      ?range? ;
: ?imm20o?      ( imm20o -- ) \ offset word aligned
  pars       @  $ff80000  $0007ffff  within  ?range?
  pars       @  1 and  0=                    ?aligned? ;

: ?nopar?           ( -- ) \ no parameter allowed
  testpars   addrmodes @
  -reset?  ?abort" no parameter expected" ;

: ?reg?             ( mask val -- ) \ Check parameter
  >r  >r  testpars
          addrmodes @  <$> @ IF $40000000 or THEN
      r>  and 
  r>  -   -reset?  ?abort" wrong parameter(s) " ;

\ --------------------------------------------------------------
\ Opcode test and compilation
\ --------------------------------------------------------------
: ,             ( ??? -- ) \ test then shift
  testpars  shiftpars ;

: ,(            ( ??? -- ) \ test then shift - set flag for imm(reg)
  ,  addrmodes @  $80000000 or  addrmodes ! ;

\ --------------------------------------------------------------
\ tools for generating 32 bit opcode
: +rd           ( opcode reg -- opcode2 ) \ bit [11: 7]
  #07 lshift  or ;

: +rs1          ( opcode reg -- opcode2 ) \ bit [19:15]
  #15 lshift  or ;

: +rs2          ( opcode reg -- opcode2 ) \ bit [24:20]
  #20 lshift  or ;

: +uimm5<<20    ( opcode imm -- opcode2 ) \ bit [31:20]
  $01f and  #20 lshift  or ;

: +imm7|5       ( opcode imm -- opcode2 ) \ bit [31:25]/bit[11:7]
  tuck  $fe0 and  #20 lshift  or  swap $1f and  #07 lshift  or ;

: +imm12<<20    ( opcode imm -- opcode2 ) \ bit [31:20]
  $fff and  #20 lshift  or ;

: +imm8o        ( opcode imm -- opcode2 ) \ bit 8|6:5|2|11:10|4:3
  >r r@ $0100 and  IF $00001000 or THEN \ bit [    8] => [   12]
     r@ $00c0 and  #01 rshift   or      \ bit [ 7: 6] => [ 6: 5]
     r@ $0020 and  #03 rshift   or      \ bit [    5] => [    2]
     r@ $0018 and  #07 lshift   or      \ bit [ 4: 3] => [11:10]
  r>    $0006 and  #02 lshift   or ;    \ bit [ 2: 1] => [ 4: 3]

: +imm11o       ( opcode imm -- opcode2 ) \ bit 11|4|9:8|10|6|7|3:1|5
  >r r@ $0800 and  #01 lshift   or      \ bit [   11] => [   12]
     r@ $0400 and  #02 rshift   or      \ bit [   10] => [    8]
     r@ $0300 and  #01 lshift   or      \ bit [ 9: 8] => [10: 9]
     r@ $0080 and  #01 rshift   or      \ bit [    7] => [    6]
     r@ $0040 and  #01 lshift   or      \ bit [    6] => [    7]
     r@ $0020 and  #03 rshift   or      \ bit [    5] => [    2]
     r@ $0010 and  #07 lshift   or      \ bit [    4] => [   11]
  r>    $000e and  #02 lshift   or ;    \ bit [ 3: 1] => [ 5: 3]

: +imm12o       ( opcode imm -- opcode2 ) \ bit 12|10:5|4:1|11
  >r r@ $1000 and  IF $80000000 or THEN \ bit [   12] => [   31]
     r@ $0800 and  IF $00000080 or THEN \ bit [   11] => [    7]
     r@ $07e0 and  #20 lshift   or      \ bit [10: 5] => [30:25]
  r>    $001e and  #07 lshift   or ;    \ bit [ 4: 1] => [11: 8]

: +uimm5<<15     ( opcode imm -- opcode2 ) \ bit [19:15]
  $01f and  #15 lshift  or ;

: +uimm20<<12   ( opcode uimm -- opcode2 ) \ bit [31:12]
  $fffff and  #12 lshift  or ;

: +imm20o       ( opcode imm -- opcode2 ) \ bit [31:12]
  >r r@ $100000 and  IF $80000000 or THEN
     r@ $000800 and  IF $00100000 or THEN
     r@ $0007ff and  #20 lshift   or
  r>    $0ff000 and               or ;
\ --------------------------------------------------------------
\ tools for generating 16 bit opcode
: +c.rd         ( opcode reg -- opcode2 ) \ bit [11: 7]
  #07 lshift  or ;

: +c.rd'        ( opcode reg -- opcode2 ) \ bit [ 9: 7]
  8 -  #07 lshift  or ;

: +c.rs2        ( opcode reg -- opcode2 ) \ bit [ 6: 2]
       #02 lshift  or ;

: +c.rs2'       ( opcode reg -- opcode2 ) \ bit [ 4: 2]
  8 -  #02 lshift  or ;

\ --------------------------------------------------------------
\ Compile labels
: (li$,          ( from imm32 reg -- ) \ reg , hi lui,  reg , reg addi,
  rot >r  >r
  dup $fffff000 and  over $800 and IF $1000 + THEN  r@ +rd  $37 or
  swap     $fff and  #20 lshift  r@ +rd  r> +rs1            $13 or
  r@ cell+ t!p   r> t!p ;

: (imm12$,       ( from to opcode -- ) \ SB-Type
  >r  over >r  swap -  pars !  ?imm12o?
  r>       r>  pars @ +imm12o  swap t!p ;

: (imm20$,       ( from to opcode -- ) \ UJ-Type
  >r  over >r  swap -  pars !  ?imm20o?
  r>       r>  pars @  +imm20o  swap t!p ;

: (imm8$c,       ( from to opcode -- ) \ CB-Type
  >r  over >r  swap -  pars !  ?imm8o?
  r>       r>  pars @ +imm8o  swap tw!p ;

: (imm11$c,      ( from to opcode -- ) \ CJ-Type
  >r  over >r  swap -  pars !  ?imm11o?
  r>       r>  pars @ +imm11o  swap tw!p ;

: offset,        ( from to opcode/reg typ -- )
  CASE 0 OF (li$,                             ENDOF \ LI, (to=imm32)
       1 OF (imm12$,                          ENDOF \ SB-Type
       2 OF (imm20$,                          ENDOF \ UJ-Type
       3 OF (imm8$c,                          ENDOF \ CB-Type
       4 OF (imm11$c,                         ENDOF \ CJ-Type
       2drop 2drop reset abort" unknown label type"
  ENDCASE ;

\ ?$, = compile label
: ?$,           ( to opcode/reg typ -- )
  <$> @ IF   $links @ #31 > -reset? ?abort" Too much labels "
             $links @  dup 1+  $links !   \ increment counter
             3 * 1 + cells  $links +      \ address at $links
             tprg@  over 2 cells +  !          \ save adress
             over 3 < IF 4 ELSE 2 THEN tallotp \ reserve space
             over 0=  IF 4 tallotp THEN      \ + space for LI,
             tuck c!                                   \ Type
             tuck cell+ !                        \ Opcode/reg
             1 + c!                                      \ to
             0 <$> !                            \ delete flag
        ELSE 2>r tprg@ swap  2r>    \ -- from to opcode type
             dup 3 < IF 4 ELSE 2 THEN tallotp     \ freihalten
             dup 0=  IF 4 tallotp THEN     \ li, => lui, addi,
             offset,
        THEN ;

\ check = resolve all labels
  : check       ( -- ) ( test $tab )
   $links  dup @ 3 * 1 + cells +   $links cell+
   ?DO i 1+ c@  $tab +  @                   \ Zieladresse holen
       dup 0=  -reset? ?abort" Unresolved local label"
       i 2 cells + @  swap                             \ from
       i cell+ @                                     \ opcode
       i c@                                            \ type
       offset,                 \ save opcode with offset/addr
   3 cells +LOOP ;

\ --------------------------------------------------------------
\ RV32C-Opcodes
: NoPars:       ( opc -- ; -- ) \ no parameter
  Createp ,p
  Doesp>  >r  ?nopar?  r> @p  t,p  reset ;

  $0000100f     NoPars:         fence.i,
  $0ff0000f     NoPars:         fence,
  $00000073     NoPars:         ecall,
  $00100073     NoPars:         ebreak,
  $00200073     NoPars:         uret,
  $10200073     NoPars:         sret,
  $30200073     NoPars:         mret,
  $10500073     NoPars:         wfi,

: R-Type:       ( opcode -- ; -- ) \ rd , rs1 , rs2
  Createp ,p
  Doesp>  >r  $f00c0300 $10040100 ?reg? 
          r> @p  r3@ +rd  r2@ +rs1  r1@  +rs2  t,p   reset ;

  $00000033     R-Type:         add,
  $40000033     R-Type:         sub,
  $00001033     R-Type:         sll,
  $00002033     R-Type:         slt,
  $00003033     R-Type:         sltu,
  $00004033     R-Type:         xor,
  $00005033     R-Type:         srl,
  $40005033     R-Type:         sra,
  $00006033     R-Type:         or,
  $00007033     R-Type:         and,

  $02000033     R-Type:         mul,
  $02001033     R-Type:         mulh,
  $02002033     R-Type:         mulhsu,
  $02003033     R-Type:         mulhu,
  $02004033     R-Type:         div,
  $02005033     R-Type:         divu,
  $02006033     R-Type:         rem,
  $02007033     R-Type:         remu,

: I-Type1:      ( opcode -- ; -- ) \ rd ,( imm12 rs1
  Createp ,p
  Doesp>  >r  $fffc0300 $80040300 ?reg?   ?imm12?
          r> @p  r2@ +rd  r1@ +rs1  pars @ +imm12<<20  t,p   reset ;

  $00000003     I-Type1:        lb,
  $00001003     I-Type1:        lh,
  $00002003     I-Type1:        lw,
  $00004003     I-Type1:        lbu,
  $00005003     I-Type1:        lhu,

: I-Type2:      ( opcode -- ; -- ) \ rd , rs1 , imm12
  Createp ,p
  Doesp>  >r  $f00c0300 $10040200 ?reg?   ?imm12?
          r> @p  r3@ +rd  r2@ +rs1  pars @ +imm12<<20  t,p   reset ;

  $00000013     I-Type2:        addi,
  $00002013     I-Type2:        slti,
  $00003013     I-Type2:        sltiu,
  $00004013     I-Type2:        xori,
  $00006013     I-Type2:        ori,
  $00007013     I-Type2:        andi,
  $00000067     I-Type2:        jalr,

: I-Type3:      ( opcode -- ; -- ) \ rd , rs1 , imm5
  Createp ,p
  Doesp>  >r  $f00c0300 $10040200 ?reg?   ?uimm5?
          r> @p  r3@ +rd  r2@ +rs1  pars @ +uimm5<<20  t,p   reset ;

  $00001013     I-Type3:        slli,
  $00005013     I-Type3:        srli,
  $40005013     I-Type3:        srai,

: S-Type:       ( opcode -- ; -- ) \ rs2 ,( imm12 rs1
  Createp ,p
  Doesp>  >r  $fffc0300 $80040300 ?reg?   ?imm12?
          r> @p  r2@ +rs2  r1@ +rs1  pars @ +imm7|5  t,p   reset ;

  $00000023     S-Type:         sb,
  $00001023     S-Type:         sh,
  $00002023     S-Type:         sw,

: SB-Type:      ( opcode -- ; -- ) \ rs1 , rs2 , addr12/label
  Createp ,p
  Doesp>  @p  r3@ +rs1  r2@ +rs2  
          >r  $b00c0300 $10040200 ?reg?
              pars @  r>  1  ?$,   reset ;

  $00000063     SB-Type:        beq,
  $00001063     SB-Type:        bne,
  $00004063     SB-Type:        blt,
  $00005063     SB-Type:        bge,
  $00006063     SB-Type:        bltu,
  $00007063     SB-Type:        bgeu,

: U-Type:       ( opcode -- ; -- ) \ rd , imm20
  Createp ,p
  Doesp>  >r  $fffc0300 $00040200 ?reg?   ?uimm20?
          r> @p  r2@ +rd  pars @ +uimm20<<12  t,p   reset ;

  $00000017    U-Type:          auipc,
  $00000037    U-Type:          lui,

: UJ-Type:      ( opcode -- ; -- ) \ rd , imm20o
  Createp ,p
  Doesp>  >r  $bffc0300 $00040200 ?reg?
          pars @   r> @p  r2@ +rd  2  ?$,   reset ;

  $0000006f    UJ-Type:         jal,

: sfence.vma,   ( -- ) \ rs1 , rs2
  $fffc0300 $00040100 ?reg?
  $120000f3  r2@ +rs1  r1@ +rs2  t,p  reset ;

: CSR-Type:     ( opcode -- ; -- ) \ rd , reg , rs1
  Createp ,p
  Doesp>  >r  $f00c0300 $10080100 ?reg?   ?imm12?
          r> @p  r3@ +rd  r1@ +rs1
          pars cell+ @ +imm12<<20  t,p   reset ;

  $00001073     CSR-Type:       csrrw,
  $00002073     CSR-Type:       csrrs,
  $00003073     CSR-Type:       csrrc,

: CSRI-Type:    ( opcode -- ; -- ) \ rd , reg , uimm5
  Createp ,p
  Doesp>  >r  $f00c0300 $10080200 ?reg?   ?uimm12-2?  ?uimm5?
          r> @p  r3@ +rd  pars @ +uimm5<<15
          pars cell+ @ +imm12<<20  t,p   reset ;

  $00005073     CSRI-Type:      csrrwi,
  $00006073     CSRI-Type:      csrrsi,
  $00007073     CSRI-Type:      csrrci,

: li,           ( -- ) \ rd , imm32 ($ allowed)
  $bffc0300 $00040200 ?reg?
  pars @  r2@  0  ?$,   reset ;

\ --------------------------------------------------------------
\ RV32C
: c.NoPars:   ( opc -- ; -- )
  Createp ,p
  Doesp>  >r  ?nopar?  r> @p  tw,p  reset ;

  $0001         c.NoPars:       c.nop,
  $9002         c.NoPars:       c.ebreak,

\ C-Type = all other c.xxx
: C-Type:      ( mask val opcode exec -- ; -- )
  Createp ,p  ,p  swap ,p  ,p
  Doesp>  dup >r  2 cells +  dup @p  swap cell+ @p  ?reg?
          r@ cell+ @p  r> @p  execute ;

\ CR-Type
: +c.mv/jr,     ( opcode --) \ rd<>0 , rs2<>0
  r2@ +c.rd  r1@ +c.rs2
  tw,p  reset ;
: +c.jr/jalr,   ( opcode -- ) \ rs<>0 (rs2=0)
  r1@ +c.rd
  tw,p  reset ;

  $fffc8320 $00040100 $8002 ' +c.mv/jr,    C-Type: c.mv,       \ rd<>0 , rs2<>0
  $ffffff20 $00000100 $8002 ' +c.jr/jalr,  C-Type: c.jr,       \ rs<>0 (rs2=0)
  $fffc8300 $00040100 $9002 ' +c.mv/jr,    C-Type: c.add,      \ rd , rs2<>0
  $ffffff20 $00000100 $9002 ' +c.jr/jalr,  C-Type: c.jalr,     \ rs<>0 (rs2=0)

\ CI-Type
: +c.li/slli,   ( opcode -- ) \ imm5 or uimm5
  r2@ +c.rd
  pars @  tuck  $020 and  #07 lshift  or \ bit [    5] => [   12]
          swap  $01f and  #02 lshift  or \ bit [ 4: 0] => [ 6: 2]
  tw,p  reset ;
: +c.li,        ( opcode -- ) \ rd<>0 , imm5
  >r  ?imm6?    r>  +c.li/slli, ;
: +c.slli,      ( opcode -- ) \ rd<>0 , uimm5
  >r  ?uimm6?   r>  +c.li/slli, ;
: +c.addi16,    ( opcode -- ) \ rd=2 , imm4-9
  >r  ?imm9-4?  r>  r2@ +c.rd
  pars @  tuck  $200 and  #03 lshift  or \ bit [    9] => [   12]
          over  $180 and  #04 rshift  or \ bit [ 8: 7] => [ 4: 3]
          over  $040 and  #01 rshift  or \ bit [    6] => [    5]
          over  $020 and  #03 rshift  or \ bit [    5] => [    2]
          swap  $010 and  #02 lshift  or \ bit [    4] => [    6]
  tw,p  reset ;
: +c.lwsp,      ( opcode -- ) \ rd<>0 , uimm7-2
  >r  ?uimm7-2?  r>  r2@  +c.rd
  pars @  tuck  $0c0 and  #04 rshift  or \ bit [ 7: 6] => [ 3: 2]
          over  $020 and  #07 lshift  or \ bit [    5] => [   12]
          swap  $01c and  #02 lshift  or \ bit [ 4: 2] => [ 6: 4]
  tw,p  reset ;

  $fffc8300 $00040200 $4001 ' +c.li,       C-Type: c.li,       \ rd<>0 , imm5
  $fffd8300 $00040200 $6001 ' +c.li,       C-Type: c.lui,      \ rd<>0|2 , imm5
  $fffd0300 $00050200 $6001 ' +c.addi16,   C-Type: c.addi16sp, \ rd=2 , imm9-4
  $fffc8300 $00040200 $0001 ' +c.li,       C-Type: c.addi,     \ rd<>0 , imm5
  $fffc8300 $00040200 $0002 ' +c.slli,     C-Type: c.slli,     \ rd<>0 , uimm5
  $fffc8340 $80040340 $4002 ' +c.lwsp,     C-Type: c.lwsp,     \ rd<>0 ,( uimm7-2 x2

\ CS/CL-Type ( load/store )
: +c.sw/lw,     ( opcode -- ) \ rs2' ,( imm6-2 rs'
  >r  ?imm7-2?  r>  r2@ +c.rs2'  r1@ +c.rd'
  pars @  tuck  $040 and  #01 rshift  or \ bit [    6] => [    5]
          over  $038 and  #07 lshift  or \ bit [ 5: 3] => [12:10]
          swap  $004 and  #04 lshift  or \ bit [    2] => [    6]
  tw,p  reset ;

  $fffe8380 $80060380 $c000 ' +c.sw/lw,    C-Type: c.sw,       \ rs2' ,( imm7-2 rs'
  $fffe8380 $80060380 $4000 ' +c.sw/lw,    C-Type: c.lw,       \ rs2' ,( imm7-2 rs'

\ CS-Type
: +c.rd'/rs',     ( opcode --)
  r2@ +c.rd'  r1@ +c.rs2'
  tw,p  reset ;

  $fffe0380 $00060180 $8c01 ' +c.rd'/rs',  C-Type: c.sub,      \ rs2' , rs'
  $fffe0380 $00060180 $8c21 ' +c.rd'/rs',  C-Type: c.xor,      \ rs2' , rs'
  $fffe0380 $00060180 $8c41 ' +c.rd'/rs',  C-Type: c.or,       \ rs2' , rs'
  $fffe0380 $00060180 $8c61 ' +c.rd'/rs',  C-Type: c.and,      \ rs2' , rs'

\ CSS-Type
: c.swsp,       ( uimm 7-2 -- ) \ rs2 ,( uimm7-2 x2
  $fffc0340 $80040340 ?reg?  ?uimm7-2?
  $c002  r2@ +c.rs2
  pars @  tuck  $0c0 and  #01 lshift  or \ bit [ 7: 6] => [ 8: 7]
          swap  $01c and  #07 lshift  or \ bit [ 5: 2] => [11: 9]
  tw,p  reset ;

\ CIW-Type
: c.addi4spn,   ( uimm 7-2 -- ) \ rs2' , sp , uimm9-2
  $f80d0300 $18050200 ?reg?  ?uimm9-2?
  $0000  r2@ +c.rs2
  pars @  tuck  $3c0 and  #01 lshift  or \ bit [ 9: 6] => [10: 7]
          over  $030 and  #07 lshift  or \ bit [ 5: 4] => [12:11]
          over  $008 and  #02 lshift  or \ bit [    3] => [    5]
          swap  $004 and  #04 lshift  or \ bit [    2] => [    6]
  tw,p  reset ;

\ CB-Jump
: CB-Type:      ( opcode -- ; -- ) \ rs' , addr8/label
  Createp ,p
  Doesp>  >r  $b00e0300 $00060200 ?reg?
          pars @  r> @p  r2@ +c.rd'  3  ?$,   reset ;

  $0000c001     CB-Type:        c.beqz,
  $0000e001     CB-Type:        c.bnez,

: CBI-Type:     ( opcode -- ; -- ) \ rs' , (u)imm5
  Createp ,p
  Doesp>  >r  $f00e0300 $00060200 ?reg?  ?imm6?
          r>  @p  r2@ +c.rd'
              pars @  tuck  $020 and  #07 lshift  or \ bit [    5] => [   12]
                      swap  $01f and  #02 lshift  or \ bit [ 4: 0] => [ 6: 2]
          tw,p  reset ;

  $00008801     CBI-Type:       c.andi,
  $00008001     CBI-Type:       c.srli,
  $00008401     CBI-Type:       c.srai,

: CJ-Type:      ( opcode -- ; -- ) \ addr12/label
  Createp ,p
  Doesp>  >r  $bffff300 $00000200 ?reg?
          pars @  r> @p  4  ?$,   reset ;

  $0000a001     CJ-Type:        c.j,
  $00002001     CJ-Type:        c.jal,

\ --------------------------------------------------------------
\ Forth vocabulary part of assembler (direct threaded code)
Forth Definitions

i: ;Code        ( 0 -- ; -- ) \ Code in threaded Forth
  dup 0 ?pairs drop             \ Test for :
  postpone [                    \ kein link und cfa
\ there cell+ t,p  there cell+ t,p  \ link/cfa if indirect threaded
  [ TAssembler ] ready [ Forth ] also TAssembler ;

: Code          ( name ; -- -1 ; -- ) \ actual no CFA
  Header  -1
\ tprg@ 1 and IF 1 tallotp THEN  \ !!! for test align address
\ here cell+ ,p \ link / cfa if indirect threaded
  [ also TAssembler ] ready [ previous ] also TAssembler ;

: Proc          ( name ; -1 -- ; -- addr ) \ only address
  Createp  -1
  tprg@ 1 and IF 1 tallotp THEN  \ !!! for test align address
  [ also TAssembler ] ready [ previous ]  also TAssembler ;

: End-Proc      ( -1 -- ; -- )  \ End of Proc or Code
  dup -1 ?pairs drop            \ check for Code
  [ TAssembler ] check [ Forth ]  previous  End-Create ;
' End-Proc    Alias End-Code

onlyforth
