\ -------------------------------------------------------------
\ New editor for mcForth (Windows-DOS)
\ -------------------------------------------------------------
\ File    : mcFEd.scr
\ Author  : Klaus Kohl-Schoepe
\ Version : 1.0 - 05.06.2023
\ Licence : MIT Licence
\ -------------------------------------------------------------
\ Remark:
\  Handles sequencial files with LF or CR/LF
\  Handles block files with different sizes
\  Use actual open files with ed (line) or v (error)
\  Prepared for using a second file (not yet used)
\  The file will be completely load to memory
\  Each line on sequencial file ends with LF or CR/LF 
\  Expect 32-bit addresses or offsets and <2GByte free memory
\  All key definitions are actual for Windows 11 Terminal
\
\  This screen editor is adapable to different screen sizes
\  - position: #soffx, #soffy (starting at 1 and 3 because of frame)
\  - size    : #lenx, #leny (default 64 and 16 - minimum 4 and 1)
\  - step    : #cstepx, #cstepy (screen movement step default 4 and 1)
\  Block files are normally 1024 bytes (value #c/s)
\ -------------------------------------------------------------
\ Logbook:
\  05.04.23 KKS: Start with block editor files
\  07.04.23 KKS: Load file integrated
\  01.05.23 KKS: Cursor movement realized
\  06.05.23 KKS: Marking realized
\  02.06.23 KKS: Add some functions and tests
\  05.06.23 KKS: Add search/replace
\  06.06.23 KKS: Automatic recognition of SCR or seq. files - remove Tabs
\  26.10.23 KKS: MIT Licence and remove debug parts
\ -------------------------------------------------------------

  +|                            \ Hide all editor words

\  Forth definitions
\  Vocabulary Editor             \ Editor Vocabulary
\  Editor definitions

\ -------------------------------------------------------------
\ Screen informations (can be partly modified)
\ -------------------------------------------------------------
  #01   Value    #soffx         \ Frame needs first character left
  #03   Value    #soffy         \ Top 3 Lines are comment
  #16   Value    #leny          \ Visible lines (16 lines)
  #64   Value    #lenx          \ Visible chars (64 chars)
  #04   Constant #cstepx        \ Move screen (default 4 chars)
  #01   Constant #cstepy        \ Move screen (default 1 line)
  #02   Constant #tabs          \ Tabulator (default 2 characters)

  Variable smax                 \ Last screen number
  #16   Value    #l/s           \ 16 Zeilen/Screen
  #64   Value    #c/l           \ 64 Zeichen/Zeile
  #1024 value    #c/s           \ 1024 character used (ignore rest e.g. 25*40+24)

\ Additional flags
  Variable disp?                \ Display-Flag: 1=char, 2=line, 4=frame
  : disp?!      ( f -- ) \ Set modify flag for this screen
    disp? @  or  disp? ! ;

\ Display colors
  $07 Constant #c_frame         \ Frame color
  $07 Constant #c_text          \ Normal text and frame color
  $03 Constant #c_number        \ Number color
  $07 Constant #c_free          \ Color after text

\ Display characters
  $cd Constant #f_h     \ Horizontal linie
  $ba Constant #f_v     \ Vertical linie

  $c9 Constant #f_tl    \ Top left
  $cb Constant #f_tc    \ Top center
  $bb Constant #f_tr    \ Top right
  $cc Constant #f_ml    \ Mid left
  $ce Constant #f_mc    \ Mid center
  $b9 Constant #f_mr    \ Mid right
  $c8 Constant #f_bl    \ Bottom left
  $ca Constant #f_bc    \ Bottom center
  $bc Constant #f_br    \ Bottom right

  $20 Constant #f_free  \ Character after text $5f = _
  $b2 Constant #f_<bl   \ Replacement for character < $20

\ -------------------------------------------------------------
\ File buffer handling
\ -------------------------------------------------------------
\ Use complete free memory from "here $100 +" to "here unused + $100 -"
\ - Load complete files to memory
\  + Line and character buffer at end of memory
\  + Check for CR/LF or LF line end and expect n * #c/s screen size
\  + Calculate SV-Parameter depending of line/screen number or error position
\ - Save complete file (maybe reduce size)
\ - a second file split the size of free memory

\ Loaded Image Variables
  0 Value par                   \ Pointer to parameter field

  : SV:  Create dup ,  cell+  Does> @  par + ; \ Field items
  0
    \ Don't change position of sv_fid and sv_eof used by lbcb_free ?
    SV:    sv_fid               \ File ID
    SV:    sv_eaddr             \ End of file in memory
    SV:    sv_baddr             \ File start in memory
    SV:    sv_seq?              \ Flag for sequencial files: 0=BLK; 1=lf; 2=cr/lf
    SV:    sv_mod?              \ File changed ?
    SV:    sv_sy                \ Display start line (0...)
    SV:    sv_y                 \ Actual line number (0...)
    SV:    sv_yaddr             \ Address of current line
    SV:    sv_ymax              \ Lines in file (1...)
    SV:    sv_sx                \ Display start column (0...)
    SV:    sv_x                 \ Actual column number (0...)
    SV:    sv_xmax              \ Character in this line (without CR/LF)
  Constant #sv_size             \ Save file parameter

  Create spar  #sv_size cells allot End-Create \ First parameter field
         spar  #sv_size cells  erase
  Create fpar  #sv_size cells allot End-Create \ Second parameter field
         fpar  #sv_size cells  erase

  spar to par                   \ select spar as parameter field

\ -------------------------------------------------------------
\ Line and character buffer
\ -------------------------------------------------------------
\ Editor can copy or move lines or character to this buffer
\ It will be handled as a stack: last in - first out
\ For a line also the size of the line will be saved
\ Line buffer:
  Variable lb_b                 \ Start of line buffer
  Variable lb_n                 \ Saved lines
\ character buffer:
  Variable cb_b                 \ Begin of  character buffer
  Variable cb_n                 \ Saved characters

  : lbcbinit    ( -- ) \ Initialize buffer for line and chars
    here  unused +  $100 -   dup cb_b !  lb_b !
    0 cb_n !   0 lb_n ! ;

\ Check available free spaces
  : lbcb_free?  ( -- n ) \ memory for line or character ?
    cb_b @                      \ Begin of character buffer
    fpar @ IF fpar ELSE spar THEN cell+ @  -  ; \ - End of file 

  : ed_free?    ( -- n ) \ memory for line or character ?
    fpar @ 0=   par @  fpar  =  or
    IF   lbcb_free?             \ only one file or fpar used
    ELSE fpar 2 cells + @   spar cells @  - \ two files
    THEN ;

\ -------------------------------------------------------------
\ Manage file buffer
\ -------------------------------------------------------------
  : #firstline  ( -- line ) \ First line of display
    sv_seq? @ IF 0 ELSE sv_y @  #l/s / #l/s * THEN ;

  : #lastline   ( -- line ) \ Last line of display
    sv_seq? @ IF sv_ymax @ ELSE sv_y @  #l/s / 1+ #l/s * THEN 1- ;

  : curaddr     ( addr -- ) \ Actual cursor address
    sv_yaddr @  sv_x @  + ;

  : >nextline   ( addr -- addr2 ) \ Begin of next line (after LF)
    sv_seq? @
    IF    BEGIN dup sv_eaddr @  =  ?exit  \ End of file
                count $0a = UNTIL         \ or after LF
    ELSE  #c/l +  sv_eaddr @  umin
    THEN ;

  : y>addrlen   ( line -- addr len ) \ Line with CR/LF
    dup  sv_ymax @  u< 0= IF drop  sv_eaddr @  0 exit THEN
    sv_seq? @
    IF   sv_baddr @  swap
      ?FOR BEGIN count $0a = UNTIL NEXT
      dup  BEGIN count $0a = UNTIL over -
    ELSE 0  #l/s  um/mod  #c/s *  swap #c/l *  +   sv_baddr @  +  #c/l
    THEN ;

  : -crlf       ( len -- len2 ) \ remove CR/LF size
    sv_seq? @  -  0 max ;

  : +crlf       ( len -- len2 ) \ add CR/LF size
    sv_seq? @  + ;

  : xy>addrlen  ( x y -- addr len )
    swap >r  y>addrlen          \ Begin of line
    dup -crlf  r@  < IF rdrop  exit THEN \ if x > max: whole line
    r@ -  swap r> +  swap ;     \ Go to x

  : offset>xy   ( offset 0 | line -1 -- x y ) \ X and Y from error address
    IF   0  swap                \ go to begin of line
    ELSE sv_seq? @
      IF   sv_baddr @  tuck  +  \ -- addr curaddr
        sv_eaddr @  over  u< IF drop  sv_eaddr @ THEN \ > End ?
        0 >r                    \ Line 0
        BEGIN over over - \ End of file ?
        WHILE >r  dup BEGIN count $0a = UNTIL \ Next line
          dup  r@  u<  \ < curaddr ?
          IF   nip  r>  r> 1+ >r \ Line + 1 ; start with next line
          ELSE drop  r> swap -  r> exit \ -- offset line.s
          THEN
        REPEAT drop drop  r>  0
      ELSE 0  #c/s  um/mod  #l/s *  >r
           0  #c/l  um/mod   r>  +
      THEN
    THEN
    dup 0< IF drop  drop  0  0 THEN    \ y >= 0 ?
    sv_ymax @ 1-  over  < IF drop  drop  0  sv_ymax @ 1- THEN ; \ > Limit ?

  : sv_xmax?!      ( -- ) \ save actual line size
    sv_y @  y>addrlen  -crlf  sv_xmax !   sv_yaddr !  \ Set address of line
    sv_seq? @
    IF   sv_yaddr @  dup  >nextline  swap -  -crlf  0 max
    ELSE #c/l
    THEN sv_xmax ! ;

\ -------------------------------------------------------------
\ Display frame - set cursor
\ -------------------------------------------------------------
\ Show frame
  : >>scr       ( x y -- ) \ Move cursor to screen position
    swap #soffx +  swap #soffy +  at-xy ;

  : nemit       ( char n -- ) \ n times the character
    ?FOR dup emit NEXT drop ;
  : .lc         ( -- ) \ Print saved lines and characters
    #31 -2 >>scr  cb_n @  3 .r
    #25 -2 >>scr  lb_n @  3 .r ;

\ Print filename and ask user
  : .filename
    -1 -3  >>scr  #f_tl emit  #f_h  #lenx nemit  #f_tr emit
    2  -3  >>scr
    fcb @ ?dup IF count   #lenx 2 -  umin  type THEN ;

  : "y/n?       ( addr len -- f ) \ Ask for Y/N
    0 -1 >>scr  type    key upc [char] Y =  \ Display text - y/n?
    0 -1 >>scr  #f_h #lenx nemit  #f_v emit ;

  : "key        ( addr len -- char ) \ Ask for key
    0 -1 >>scr  type    key     \ Display text and wait for key
    0 -1 >>scr  #f_h #lenx nemit  #f_v emit ;

\ Display frame and cursor information
  : frame       ( -- ) \ draw frame
    page   0 0 at-xy   #c_frame color!  \ blue on white
    -1 -3    >>scr  #f_tl emit  #f_h  #lenx nemit  #f_tr emit
    -1 -2    >>scr  #f_v  emit  bl    #lenx nemit  #f_v  emit
    0  -2    >>scr   sv_seq? @
    IF    ." y=_______  x=_______  (L:___/C:___) ( )"
    ELSE  ." Screen=___ y=__ x=__  (L:___/C:___) ( )" THEN
    -1 -1    >>scr  #f_ml emit  #f_h  #lenx nemit  #f_mr emit
    #leny 0  DO
      -1 i     >>scr  #f_v  emit  bl    #lenx nemit  #f_v  emit
    LOOP
    -1 #leny >>scr  #f_bl emit  #f_h  #lenx nemit  #f_br emit ;

  : cursor_check   ( -- ) \ Check and modify screen offset
    sv_xmax?!                   \ Get line size
    \ Correct sy if sy < y
    sv_y @  sv_sy @  u<
    IF sv_y @                   \ Actual line
       sv_seq? @ 0= IF #l/s / #l/s * THEN \ or top on screen
       sv_sy !   4 disp?!
    THEN
    \ Correct sy if  #leny-1 < y-sy  to  y-#leny+1  or  0
    #leny 1-   sv_y @  sv_sy @  -   u<
    IF sv_y    @  #leny -  1+   0 max  sv_sy !  4 disp?! THEN
    \ Correct sy if  ymax < sy+#leny  to  ymax-#leny  or  0  
    sv_ymax @   sv_sy @  #leny  +   u<
    IF sv_ymax @  #leny -  0 max  sv_sy !  4 disp?! THEN
    \ Stay at same screen and check for end of line
    sv_seq? @  0=
    IF   #firstline  sv_sy @  over  u<  
         IF sv_sy !  4 disp?! ELSE drop THEN
         sv_xmax @  1-          \ x maximum is last char on screens files
    ELSE sv_xmax @              \ or after last char on seq. files
    THEN dup  sv_x @  u< IF sv_x ! ELSE drop THEN
    \ Correct sx if  sx < x
    sv_x  @  sv_sx @   u< IF sv_x @  sv_sx !  4 disp?! THEN
    \ Correct sx if  #lenx-1 < x-sx  to  x-#lenx+1  or  0
    #lenx 1-   sv_x @  sv_sx @  -   u<
    IF sv_x @  #lenx -  1+   0 max   sv_sx !   4 disp?! THEN ;

  : sv_setup       ( x y -- ) \ Setup actual sv_... parameter;
    sv_y !   sv_x !   4 disp?!   cursor_check ; \ Save x and y

  : cursor      ( -- ) \ Print cursor position and set it
    #c_frame color!
    #37        -2  >>scr  sv_mod? @ IF [char] * ELSE bl THEN emit \ Modified ?
    sv_seq? @
    IF   2   -2 >>scr  sv_y @           1+  7 .r
         #13 -2 >>scr  sv_x @           1+  7 .r
    ELSE 7   -2 >>scr  sv_y @  #l/s /       3 .r
         #13 -2 >>scr  sv_y @  #l/s mod 1+  2 .r
         #18 -2 >>scr  sv_x @           1+  2 .r
    THEN 
    sv_x @  sv_sx @  -   sv_y @  sv_sy @  -   >>scr ;

\ -------------------------------------------------------------
\ Display one line up to complete screen with marking
\ -------------------------------------------------------------
\ Marking Flags:
\  0: Start: check for space (=> 1 or 2)
\  1: mcol till maddr, then check word (=>2, 3, 5)
\  2: mcol till maddr, then check spaces (=>1)
\  3: mcol till end of line
\  4: mncol till maddr, then check spaces (=> 5)
\  5: space till maddr, then word in mncol (=> 2)
\  6: mcol till maddr, then (=>7)
\  7: mcol till char, maddr=addr+1 (=>2)
  Variable mflag               \ Marking flag
  Variable maddr               \ Color till this address
  Variable mcol                \ Actual color
  Variable mncol               \ Next color
  Variable mchar               \ Check for this character

  : mcol!       ( color -- ) \ set color
    dup mcol !   color! ;

  Createp mwords \ Marking words array: String, next state, col1, col2/character
  \ Colors: 0=black, 2=green, 4=red, 5=violett, 6=yellow, 7=white, 8=grey
  \ String                State   Color     NColor    Char
    s" \"           $,p   3 c,p   $02 c,p   $00 c,p   $0a c,p
    s" \\"          $,p   3 c,p   $02 c,p   $00 c,p   $0a c,p
    s" ("           $,p   6 c,p   $02 c,p   $00 c,p   $29 c,p
    s" [Defined]"   $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" [Undefined]" $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" [IF]"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" [ELSE]"      $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" [THEN]"      $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" DO"          $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" LOOP"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" IF"          $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" ELSE"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" THEN"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" BEGIN"       $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" WHILE"       $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" REPEAT"      $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" UNTIL"       $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" ?FOR"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" FOR"         $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" NEXT"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" CASE"        $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" OF"          $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" ENDOF"       $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" ENDCASE"     $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" :"           $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" ;"           $,p   2 c,p   $06 c,p   $00 c,p   $00 c,p
    s" exit"        $,p   2 c,p   $06 c,p   $00 c,p   $00 c,p
    s" ?exit"       $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" 0=exit"      $,p   2 c,p   $04 c,p   $00 c,p   $00 c,p
    s" abort"       $,p   2 c,p   $06 c,p   $00 c,p   $00 c,p
    2 c,p   char s  c,p   $22  c,p    0 c,p \ Patch for s" s"" $,p
                          6 c,p   $03 c,p   $03 c,p   $22 c,p
    2 c,p   char p  c,p   $22  c,p    0 c,p \ Patch for s" p"" $,p
                          6 c,p   $03 c,p   $03 c,p   $22 c,p
    2 c,p   char .  c,p   $22  c,p    0 c,p \ Patch for s" ."" $,p
                          6 c,p   $03 c,p   $03 c,p   $22 c,p
    s" Variable"    $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" 2Variable"   $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Constant"    $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" 2Constant"   $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Value"       $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Create"      $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Createp"     $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Does>"       $,p   4 c,p   $06 c,p   $0f c,p   $00 c,p
    s" Doesp>"      $,p   4 c,p   $06 c,p   $0f c,p   $00 c,p
    s" End-Create"  $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Alias"       $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" Include"     $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" postpone"    $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" [char]"      $,p   4 c,p   $04 c,p   $0f c,p   $00 c,p
    s" false"       $,p   2 c,p   $03 c,p   $00 c,p   $00 c,p
    s" true"        $,p   2 c,p   $03 c,p   $00 c,p   $00 c,p
    0 c,p
   End-Create

  : compstr     ( addr1 len1 caddr -- addr1 len1 0 | -1 ) \ f=0 if <>
    >r  dup  r@ c@p  - IF rdrop  0 exit THEN \ Empty string
    over  r> countp              \ ( addr1 addr2 len )
    FOR countp upc >r  swap count upc  r> -
        IF rdrop  drop drop  0 exit THEN swap
    NEXT drop drop  drop drop  -1 ;

  : checkword   ( addr len -- addr len )
    over over  bl scan   drop  dup maddr !  \ skip next word
    >r   over  r> over -  ?dup \ found ?
    IF   mwords  >r
      BEGIN  r@ c@p
      WHILE  r@   compstr
        IF r>  countp +  1+  \ move to flags
           countp mflag !   countp mcol!
           countp mncol !   c@p    mchar !  exit THEN
        r>  countp +  5 +  >r
      REPEAT rdrop   2 mflag !   $>number? \ Check for number
      ?dup 0= IF drop  #c_text mcol!  exit THEN
      1+ IF drop THEN drop  #c_number mcol!  exit
    THEN drop  #c_text mcol!   3 mflag ! ;
 
  : checkspace  ( addr len -- addr len 0|-1 ) \ -1 if no space
    >r   dup r@ bl skip  r@ -   \ Spaces ?
    IF   maddr !   7 mcol!  1 mflag !   0 \ Skip spaces
    ELSE drop                          -1 \ Scan word
    THEN r>  swap ;

  : checkcolor   ( addr len -- addr len ) \ change color, set flags
    mflag @
    CASE 0 OF checkspace IF checkword THEN ENDOF
         1 OF over     maddr @  -  ?exit \ Skip till maddr
              checkword ENDOF \ next is checkword
         2 OF over     maddr @  -  ?exit \ Skip till maddr
              checkspace drop ENDOF \ next is checkspace
         3 OF exit ENDOF \ color till end of line
         4 OF over     maddr @  -  ?exit \ Skip till maddr
              checkspace drop   5 mflag ! ENDOF \ next word in mncol
         5 OF over     maddr @  -  ?exit \ Skip till maddr
              over over  bl scan  drop  maddr ! \ Next word in mncol
              mncol @  mcol!   2 mflag ! ENDOF
         6 OF over     maddr @  -  ?exit \ Skip till maddr
              7 mflag ! ENDOF \ next is 7
         7 OF over c@  mchar @  -  ?exit \ Skip till mchar
               over 1+  maddr !   2 mflag !  ENDOF \ next in same color
     ENDCASE ;

  : .line       ( y -- ) \ Display one line
    >r  0  r@  >>scr            \ set cursor to begin of line
    sv_sy @ r> +  y>addrlen  -crlf \ line address and size
    0 mflag !   over maddr !    \ restart color selection
    sv_sx @                     \ Check color till start of screen
    ?FOR dup IF checkcolor  swap 1+ swap 1- THEN NEXT
    #lenx                       \ Fill screen
    ?FOR  dup                   \ till end of line
      IF   checkcolor  >r  count  dup bl u< IF drop #f_<bl THEN emit  r> 1-
      ELSE #c_free mcol!  #f_free emit
      THEN
    NEXT drop drop ;

  : lcdrestore  ( -- ) \ Restore display
    cursor_check
    disp? @   disp? off   #c_frame color!
    dup 4 and IF drop  #leny  0
                       DO i .line LOOP         cursor exit THEN
    dup 2 and IF drop  #leny  sv_y @ sv_sy @ -
                       DO i .line LOOP         cursor exit THEN
        1 and IF       sv_y @ sv_sy @ -
                            .line                          THEN
    cursor ;

  : lcdinit     ( -- ) \ Display screen and set cursor
    #c_frame color!   frame  .filename  .lc  lcdrestore ;

\ -------------------------------------------------------------
\ Initialize variables, load file into memory, start editor
\ -------------------------------------------------------------
  : file_check  ( -- sv_seq? ) \ Remove Tab - check for file type
    \ Replace all char < $0a with space
    sv_baddr @  sv_eaddr @ over -
    ?FOR count $0a u< IF bl over 1- c!  sv_mod? on THEN NEXT
    drop
    \ Check for CR/LF or only LF
    0  sv_baddr @  sv_eaddr @ over - \ -- flag addr len
    ?FOR count dup $0d = IF drop drop drop  2  rdrop  exit THEN
                   $0a = IF nip             1  swap        THEN
    NEXT drop  ?dup ?exit       \ only LF found
    sv_eaddr @  sv_baddr @  -  dup 0=  swap #c/s mod  or
    IF 2 ELSE 0 THEN ;          \ SCR if not empty file and size n * #c/s

  : scr_check   ( -- ) \ Check for size (n * #c/s)
    sv_eaddr @  sv_baddr @  -   #c/s /  #l/s *  sv_ymax ! ;

  : lf_check    ( sv_seq? -- ) \ Count lines and correct end of file
    0 sv_ymax !                 \ Reset line numbers
    \ State: 0/4=begin of line, 1/5=char, 6=CR
    2 = IF 4 ELSE 0 THEN >r  \ Setup state - 0 for LF files
    sv_baddr @                  \ Start address
    BEGIN
      dup  sv_eaddr @  -        \ if not end of file
    WHILE count                 \ get next character
      CASE $0d OF r@ 4 =  r@ 5 = or \ CR expected => state=6
                  IF rdrop  6 >r ELSE 2drop 2drop abort" CR not allowed" THEN
               ENDOF
           $0a OF r@ 4 =  r@ 5 = or \ LF expected: increment lines, state=0 or 4
                  IF 2drop 2drop abort" Expect CR before LF" ELSE 1 sv_ymax +!  r>  4 and  >r THEN
               ENDOF
                  r@ 6 = \ LF not expected => state=1 or 5
                  IF 2drop 2drop abort" Expect LF after CR" ELSE r>  1 or  >r THEN
      ENDCASE
    REPEAT drop
    r@  3 and 0=  IF rdrop  exit THEN  \ Empty line ?
    r>  5 =       IF $0d sv_eaddr @ c!  1 sv_eaddr +! THEN \ add CR
                     $0a sv_eaddr @ c!  1 sv_eaddr +!      \ add LF
    1 sv_ymax +!   sv_mod? on ; \ increment line and set modified flag

  : loadfile    ( -- ) \ Load file and check CR/LF or size
    sv_fid @  fsize@  >r  dup ed_free? u>  r> or  ?abort" File too big"  ( -- size )
    0.  0  sv_fid @  fpos!      \ To start of file
    >r  sv_baddr @  dup r@ +  sv_eaddr !  \ New end, then load file
        >x  r@  sv_fid @  fread  r> - ?abort" File read not completed"
    file_check  dup sv_seq? !  \ check for screen or seq. files
    dup IF dup lf_check ELSE scr_check THEN \ Check for CR/LF and count lines
    \ Set type of file and adapt editor screen size (error in Windows 11)
    \ >r  at-max  swap 2 - swap 5 -   \ whole screen - e.g. #77 #22 )
    \ r> 0= IF #l/s umin  swap #c/l umin  swap THEN \ Or only maximal SCR size
    IF  #80 #25 ELSE #c/l #l/s THEN \ Actual fix
    to #leny   to #lenx ;

  : savefile    ( -- ) \ Save file
    0.  0  sv_fid @  fpos!      \ Go to start
    sv_baddr @  sv_eaddr @  over -  >r \ address and size
    >x  r@  sv_fid @  fwrite    \ Write the file
    r> 0  sv_fid @  fsize!      \ And set new size
    0  sv_mod? ! ;              \ Reset modified flag

\ -------------------------------------------------------------
\ Insert or overwrite text
\ -------------------------------------------------------------
  : addchars?   ( n -- f ) \ f=-1, if insert/overwrite possible
    sv_seq? @
    IF   ed_free?  u< IF true ELSE false THEN \ Seq. File: check memory
    ELSE #c/l 1+  sv_yaddr @ #c/l -trailing nip  -  u< \ free chars
    THEN ;

  : addline?    ( len -- f ) \ f=1 if insert possible
    sv_seq? @
    IF   ed_free?  u>           \ Seq. File: not enough space ?
    ELSE drop  #lastline y>addrlen  -trailing nip \ SCR: last line not empty ?
    THEN  IF false ELSE true THEN ;

\ -------------------------------------------------------------
\ Cursor movement
\ -------------------------------------------------------------
  : lsstart?    ( line - f ) \ Check if start of screen/file
    sv_seq? @ 0= IF #l/s mod THEN 0= ; 

  : lsend?      ( line - f ) \ Check if end of screen/file
    sv_seq? @
    IF   sv_ymax @                \ File end
    ELSE dup  #l/s /  1+  #l/s *  \ Screen end
    THEN 1-  = ;

  : curup       ( -- ) \ Cursor one line up
    sv_y @  dup lsstart? \ if not first line (per screen)
    IF drop ELSE 1-  sv_y ! THEN ;

  : curdown     ( -- ) \ Cursor one line down
    sv_y @   dup lsend?
    IF drop ELSE 1+  sv_y ! THEN ;

  : curleft     ( -- ) \ Cursor left
    sv_x @  ?dup IF 1-  sv_x ! THEN ;

  : curright    ( -- ) \ Cursor right
    sv_x @  1+                  \ new cursor, line size
    dup  sv_xmax @ sv_seq? @ IF 1+ THEN  < \ Seq. File allow + 1
    IF sv_x ! ELSE drop THEN ;

  : curbln      ( -- ) \ Cursor to begin of the line
    sv_x @ 0=   0 sv_x !        \ Go to first character
    IF  sv_sy @  sv_y ! THEN ;  \ Or first line 

  : cureln      ( -- ) \ Cursor to end of the line
    sv_xmax @                   \ Seq. file: go to end of line
    sv_seq? @ 0=                \ Screen file: go to end of last word
    IF >r  sv_yaddr @  r@ -trailing nip  r> 1- umin THEN
    sv_x ! ;

  : curnw       ( -- ) \ Cursor to next word
    sv_yaddr @  sv_xmax @  sv_x @  /string \ rest of line
    bl scan  bl skip  0= IF drop exit THEN
    sv_yaddr @  -  sv_x ! ;

  : curbw       ( -- ) \ Cursor to last word
    sv_yaddr @  sv_x @  dup 0= IF 2drop exit THEN \ begin of line ?
    BEGIN 2dup + 1- c@  bl  =  over 0<>  and WHILE 1- REPEAT
    BEGIN 2dup + 1- c@  bl  <>  over 0<>  and WHILE 1- REPEAT
    sv_x ! ;

  : scr1        ( -- ) \ First sceen
    0 sv_x !   0 sv_y !   4 disp?! ;

  : scr-        ( -- ) \ Previous screen or #leny up
    sv_y @  ?dup 0=exit  \ only if y > 0
    sv_seq? @ IF #leny - ELSE #l/s - THEN
    0 max  sv_y !  4 disp?! ;

  : scr+        ( -- ) \ Next screen or #leny down
    sv_seq? @
    IF   sv_y  @  #leny +
    ELSE sv_sy @  #l/s +  sv_sy !   sv_y  @  #l/s  +
    THEN sv_ymax @ 1-  min   sv_y !  4 disp?! ;

  : lastscr     ( -- ) \ Last screen or line
    sv_ymax @  1-      \ Last line
    sv_seq? @ 0=  IF #l/s /  #l/s * THEN \ Or last screen
    0 swap   sv_setup ; \ Begin of line

  : gotoln      ( -- ) \ Goto line or screen
    sv_seq? @
    IF   0 -1 >>scr   ."  Goto Line   : ..... "
    ELSE 0 -1 >>scr   ."  Goto Screen : ..... "
    THEN #14 -1 >>scr  here 5 accept
           0 -1 >>scr   #f_h  #22 nemit
    >r  0.  here  r> >number  drop drop drop
    sv_seq? @  0= IF #l/s * THEN  sv_ymax @  umin  sv_y ! ;

\ -------------------------------------------------------------
\ Insert/delete chars
\ -------------------------------------------------------------
  : ins$        ( addr len -- ) \ Add string at actual position
    curaddr >r   sv_seq? @ 
    IF   r@   over r@ +  sv_eaddr @ r@ -  cmove> \ Move rest of line
         dup  sv_eaddr +! \ increment file size
         dup  sv_xmax +! \ increment line size
    ELSE r@   over dup >r over +   sv_xmax @  sv_x @ - r> -  cmove> \ Move rest of line
    THEN r> swap  cmove   sv_mod? on   1 disp?! ; \ Insert string
    
  : charins?    ( char -- f ) \ f=0 if error
    1 addchars? 0= IF drop  false exit THEN \ Ignore character
    here c!   here  1  ins$ \ Insert character
    sv_seq? @
    IF   1 sv_x +!              \ Set cursor after character
    ELSE sv_x @  sv_xmax @ 1-  u<  \ End of line ?
      IF   1 sv_x +!            \ Next char position ?
      ELSE sv_y @  #lastline  - \ Not last line ?
        IF 1 sv_y +!   0 sv_x !   4 disp?! THEN \ Goto begin of next line
      THEN
    THEN true ;

  : del$        ( len -- ) \ Delete n chars in this line
    curaddr >r   sv_seq? @
    IF   r@ over +   sv_eaddr @  over -  r> swap  cmove \ Remove
         sv_eaddr -!            \ Correct size
    ELSE r>  swap  dup >r  sv_xmax @ sv_x @ - r> -  \ -- cur len rlen
         over >r  >r \ -- cur len ; R: -- len rlen
         over +  over  r@  cmove \ move rest down
         r> +  r>  blank \ add spaces
    THEN sv_mod? on   1 disp?! ;
    
  : chardel     ( -- ) \ Delete character on cursor position
    sv_x @  sv_xmax @  =  \ Seq. file: cursor at end
    IF   sv_y @  sv_ymax @ 1-  = ?exit \ Last line not erasable
         0 +crlf   1 sv_ymax -!  2 disp?! \ Delete CR/LF line end
    ELSE 1
    THEN del$ ;

  Create $crlf  $0d c, $0a c, End-Create \ must be in RAM
  : newln$      ( -- addr len ) \ CR/LF or only LF
    sv_seq? @   $crlf 2+ over -  swap ;

  : curcr       ( -- ) \ Cursor to begin next line or insert CR/LF 
    sv_seq? @                   \ Insert at seq. file ?
    IF   newln$  dup >r  addchars? 0= IF rdrop drop  exit THEN \ Memory full
         curaddr  sv_eaddr @ over -  over r@ + swap  cmove> \ Move rest
         curaddr  r@  cmove     \ Insert CR/LF
         r>  sv_eaddr +!   1 sv_ymax +! \ Increment size/lines
         0 sv_x !   1 sv_y +!   \ Begin next line
         sv_mod? on   4 disp?!  \ Modified, redraw all
    ELSE sv_y @ 1+   dup  sv_ymax @  u< \ last screen line ?
      IF sv_y !  0 sv_x ! ELSE drop THEN
    THEN ;

  : <del        ( -- ) \ Delete char left from cursor
    sv_x @
    IF   curleft  chardel  1 disp?! 
    ELSE sv_seq? @ 0<>  sv_y @  and \ Seq. File and not line 1
      IF  sv_y @  1-  y>addrlen nip  -crlf >r \ Save x position of line before
          curaddr  sv_eaddr @ over -  over sv_seq? @ - swap  cmove
          sv_seq? @  sv_eaddr -!  1 sv_ymax -! \ Reduce size
          1 sv_y -!   r> sv_x !   sv_mod? on   2 disp?!
      THEN
    THEN ;

  : charins     ( char -- ) \ Char to screen (insert/overwrite)
    charins?  drop ;

  : blins       ( -- ) \ Insert blank
    bl charins? drop ;

  : curtab      ( -- ) \ Cursor to next tab (insert spaces)
    sv_x @  #tabs /  1+  #tabs *  sv_x @ -  ?FOR blins NEXT ;

  : lnins?      ( addr len -- f ) \ Insert lines - f=false if error
    dup addline? 0= IF drop drop  false exit THEN
    >r   sv_seq? @
    IF   r@ sv_eaddr +!   1 sv_ymax +! \ increment size/lines
         sv_yaddr @   dup r@ +   sv_eaddr @  over -
    ELSE sv_yaddr @   dup r@ +   #lastline 1+ y>addrlen drop over -
    THEN cmove>   sv_yaddr @   r>   cmove 
    sv_mod? on   2 disp?!   true ;

  : lndel       ( -- ) \ Delete actual line
    sv_ymax @  1  =  ?exit \ last line not removeable
    sv_y @  y>addrlen  sv_seq? @  \ -- addr len
    IF   >r  r@ sv_eaddr -!  1 sv_ymax -! \ decrement size/lines
            dup r> +  swap  sv_eaddr @                over -  cmove
         sv_y @  sv_ymax @  =  IF 1 sv_y -! THEN \ Cursor up if last line
    ELSE >r dup r> +  swap  #lastline y>addrlen drop  over -  cmove
         #lastline y>addrlen blank \ Erase last line
    THEN sv_mod? on   2 disp?! ;

  : lninsbl     ( -- ) \ Insert blank line (move old down)
    sv_seq? @  dup addline?  0= IF drop exit THEN \ Insert possible ?
    IF   newln$ >r  \ Insert CRLF or LF: -- addr ; R: -- len
         sv_yaddr @  dup r@ +  sv_eaddr @ sv_yaddr @ -  cmove>
         sv_yaddr @  r@  cmove
         r> sv_eaddr +!   1 sv_ymax +!   0 sv_x !
    ELSE sv_yaddr @  dup #c/l +  #lastline 1+ y>addrlen drop over -  cmove>
         sv_yaddr @  #c/l  blank
    THEN sv_mod? on   2 disp?! ;

  : lnsplit     ( -- ) \ Split: Move rest to next line
    sv_seq? @ IF curcr exit THEN \ Insert in seq. File possible
    sv_y @  #lastline  =  ?exit  \ Split on last line not possible
    #c/l  addline?  0=exit       \ Last line not empty
    sv_yaddr @ #c/l +  dup >r  dup #c/l +  #lastline y>addrlen drop  r> - cmove>
    sv_yaddr @ #c/l +  #c/l  blank  \ Erase next line
    sv_yaddr @ dup >r  sv_x @ +  r> #c/l +  #c/l sv_x @ -  cmove \ Copy part
    curaddr  #c/l sv_x @ -  blank \ Erase rest of line
    1 sv_y +!   0 sv_x !   4 disp?! ;

  : lnjoin      ( -- ) \ Join: Append next line
    sv_seq? @  ?exit \ Not supported at seq. file
    sv_y @  #lastline  =  ?exit \ No additional line
    sv_yaddr @ #c/l +  #c/l  bl skip  -trailing \ Only text
    dup  #c/l sv_x @ -  u> IF 2drop exit THEN \ Not enough space
    curaddr  dup  #c/l sv_x @ -  blank \ Delete rest of line
    swap cmove \ Move next line to end of line
    sv_yaddr @ #c/l +  dup #c/l + swap  #lastline y>addrlen drop over -  cmove
    #lastline y>addrlen  blank \ Erase last line
    2 disp?! ;

\ -------------------------------------------------------------
\ Character/line buffer handling
\ -------------------------------------------------------------
  : c+          ( char -- f ) \ Save char - f=0 if error
    ed_free?
    IF   cb_b @  1-  dup cb_b !  c!  1 cb_n +!  .lc  true
    ELSE drop  false
    THEN ;

  : c?          ( -- char tf | ff ) \ get char and -1 or 0 if empty
    cb_n @ IF cb_b @  c@  true ELSE false THEN ;

  : c-          ( -- ) \ Remove char from buffer
    1 cb_n -!  1 cb_b +!  .lc ;

  : l+          ( addr len -- f ) \ Save line - f=0 if error
    >r   ed_free?  r@ cell+  u<
    IF   rdrop  drop  false     \ not enough space
    ELSE cb_b @  dup r@ cell+ -  cb_n @  cmove \ space for line
         1 lb_n +!   lb_b @  r@ cell+ -  dup lb_b ! \ new count/addr
         r@ over !  cell+  r> cmove \ save line
         .lc  true
    THEN ;

  : l?          ( -- addr len tf | ff ) \ Get line and -1 or 0 if empty
    lb_n @ IF lb_b @  dup cell+  swap @  true ELSE false THEN ;

  : l-          ( -- ) \ Remove line from buffer
    lb_b @  dup  @ cell+  dup >r  +  lb_b ! \ Size
    cb_b @  dup r@ +  dup cb_b !   r>  cmove> \ move to new start
    1 lb_n -!  .lc ;

\ Move/copy lines to buffer and bring it back to file
  : lnsave      ( -- ) \ Save one line to buffer
    sv_yaddr @  sv_xmax @ +crlf  l+ IF lndel THEN ;
  : lncopy      ( -- ) \ Copy actual line to buffer
    sv_yaddr @  sv_xmax @ +crlf  L+ IF curdown THEN ;
  : lnget       ( -- ) \ Insert line if possible
    l? IF lnins? IF l- THEN THEN ;

  : charsave    ( -- ) \ Save actual char to buffer
    curaddr c@ c+ IF chardel THEN ;
  : charcopy    ( -- ) \ Copy actual char to buffer
    curaddr c@ c+ IF curright THEN ;
  : charget     ( -- ) \ Insert char from buffer
    c? IF charins? IF c-  -1 sv_x +! THEN THEN ;

\ -------------------------------------------------------------
\ Insert or remove screens
\ -------------------------------------------------------------
  : scrins      ( -- ) \ Insert Screen before actual screen
    sv_seq? @  ?exit            \ only for screen files
    ed_free?  #c/s  u< ?exit    \ enough space ?
    s"  Insert Screen (y/n) ? " "y/n?  0=exit
    #firstline  dup >r  y>addrlen drop >r \ R: -- line addr 
    r@  dup #c/s +  sv_eaddr @ r@ -  cmove>  \ move rest to end
    r>  #c/s ?FOR bl over c! 1+ NEXT  drop \ fill screen with space
    #c/s sv_eaddr +!   #l/s sv_ymax +! \ correct size/lines
    r>  dup sv_y !  sv_sy !   0 sv_x ! \ to begin of screen
    sv_mod? on   4 disp?! ;     \ redraw display

  : scrdel      ( -- ) \ Delete actual screen
    sv_seq? @  ?exit            \ only for screen files
    sv_ymax @  #l/s  u> 0=exit  \ only 1 screen left
    s"  Delete screen (y/n) ? " "y/n?  0=exit
    #firstline  dup >r  y>addrlen drop \ -- addr ; R: -- line
    dup #c/s +  tuck >r  sv_eaddr @ r> -  cmove \ delete screen
    #c/s sv_eaddr -!   #l/s sv_ymax -! \ correct size/lines
    r>   dup  sv_ymax @  u< 0= IF #l/s - THEN \ last screen: go back 1 screen
    dup sv_y !  sv_sy !   0 sv_x ! \ to begin of screen
    sv_mod? on   4 disp?! ;     \ redraw display

\ -------------------------------------------------------------
\ Convert word to upper or lower case
\ -------------------------------------------------------------
  : upper       ( addr len -- ) \ String in upper case
    BEGIN dup WHILE >r  dup c@  upc  over c!  1+  r> 1- REPEAT
    2drop ;

  : lowc        ( char1 -- char2 ) \ Lower case
    dup $41 $5b within IF $20 + exit THEN       \  A-Z => a-z
    dup $9a =          IF drop $81 exit THEN    \  ->
    dup $8e =          IF drop $84 exit THEN    \  ->
    dup $99 =          IF drop $94 THEN ;       \  ->

  : lower       ( addr len -- )
    BEGIN dup WHILE >r  dup c@  lowc  over c!  1+  r> 1- REPEAT
    2drop ;

\ wcaps  w-caps
  : wcaps       ( -- ) \ Next word in upper case
    curaddr  sv_xmax @  sv_x @ -
    bl skip  2dup  bl scan  nip  -  upper
    sv_mod? on   1 disp?!   curnw ; 

  : w-caps      ( -- ) \ Next word in lower case
    curaddr  sv_xmax @  sv_x @ -
    bl skip  2dup  bl scan  nip  -  lower
    sv_mod? on   1 disp?!   curnw ; 

\ -------------------------------------------------------------
\ Search and Replace
\ -------------------------------------------------------------
  Variable s/r          \ Flag: -1 = replace or 0 = search
  Variable dirflag      \ Direction: -1 = up; 0 = down 
  Create f$ #26 allot End-Create  f$ #26 erase
  Create r$ #26 allot End-Create  r$ #26 erase

  : find$@      ( -- )
      0 -1 >>scr   ."  Find    : ......................... "
    #11 -1 >>scr   f$ char+  #25  accept   f$ c!
      0 -1 >>scr   ."  Replace : ......................... "
    #11 -1 >>scr   r$ char+  #25  accept   dup r$ c!
    IF true ELSE false THEN s/r !
      0 -1 >>scr   ."  Direction (Enter = upward) ?        "
    key  #13  =  IF -1 ELSE 0 THEN dirflag !
      0 -1 >>scr   #f_h  #47 nemit ;

  : $=?         ( csa addr -- csa addr false | offset. true ) \ Compare strings
    >r  dup count  r@ swap  \ -- csa addr2 addr len; R: -- addr
    ?FOR count upc >r  swap count upc  r> - \ Compare char
      IF drop drop  rdrop  r> false exit THEN \ not the same
    NEXT drop drop  drop  r> sv_baddr @ - 0  true ; \ Identical

  : search      ( csa dir -- 0 | offset. -1 ) \ search up or down
    curaddr  over IF 1+ ELSE 1- THEN  swap
    IF   \ Search upward:
       sv_eaddr @  over -       \ -- csa addr len
       ?FOR $=?  IF rdrop true exit THEN  char+ NEXT
    ELSE \ Seard downward
       sv_baddr @  over  swap - \ -- csa addr len
       ?FOR $=?  IF rdrop true exit THEN  1 chars - NEXT
    THEN drop drop  false ;

  : sfind        ( -- ) \ Find next string
    f$  dirflag @  search
    IF offset>xy   sv_setup   lcdrestore THEN ;

  : replace?        ( -- f ) ( 0:Break,1:Ignore,2...:Repace )
   0 -1 >>scr ."   (R)eplace, (B)reak or (A)ll  "
   cursor  key upc
   0 -1 >>scr   #f_h  #31 nemit  cursor
   dup [char] R =  IF drop  2 exit THEN
   dup [char] B =  IF drop  0 exit THEN
       [char] A =  IF -1 ELSE 1 THEN ;

  : $replace    ( n -- n | exit sreplace ) \ Replace string at positon
    f$ c@  r$ c@  -  dup 0< \ Replace string greater than old string
    IF  0 swap -  addchars?  0= \ Insert possible ?
      IF drop  drop rdrop  exit THEN \ Leave also sreplace
    ELSE drop
    THEN 
    f$ c@  del$ \ Delete string
    r$ count  ins$  r$ c@ sv_x +! ;

  : sreplace     ( -- ) \ Replace string
   0 BEGIN f$  dirflag @   search
     WHILE offset>xy   sv_setup   lcdrestore
           dup 0= IF drop replace? THEN     \ 0: ask for action
           dup 0= IF drop  exit THEN    \ again 0: abort search
           1- dup IF $replace 1- THEN            \ <>0: replace
           key? IF key drop  drop exit THEN \ Abbruch bei Taste
     REPEAT drop ;

  : firstfind   ( -- ) \ First find/replace ask for string
   find$@  s/r @ IF sreplace ELSE sfind THEN ;

  : nextfind    ( -- ) \ All other find/replace (^L)
           s/r @ IF sreplace ELSE sfind THEN ;

\ -------------------------------------------------------------
\ Get ID Code
\ -------------------------------------------------------------
\ ID-String used for end of line
  Create idstring 0 c, #19 chars allot  End-Create \ 15+4 chars

  : addid        ( -- ) \ Add ID at end of line
    idstring c@  ?dup 0=exit    \ No ID
    addchars? 0=exit            \ No insert possible
    idstring count  ins$        \ Insert ID
    idstring c@  sv_x  +!  1 disp?!  sv_mod? on ; 

  : date@       ( -- d m y wd ) \ Actual date
    $0204 [ also HW ] syscall [ previous ] throw ;

  : getid         ( -- ) ( ID vorgeben )
    4 disp?!
    0 -1 >>scr   ."  ID : .... "
    6 -1 >>scr  idstring #13 chars + 4 accept
    0 -1 >>scr   #f_h  #12 nemit
    dup IF #15 +  idstring over 1+ chars +  hld  !
         bl hold  [char] ) hold  bl hold
         idstring #13 chars +  hld !   [char] / hold
         base @   decimal   date@ drop
          0  # # 2drop  [char] . hold
          0  # # 2drop  [char] . hold
          0  # # 2drop  bl       hold  [char] ( hold  bl hold
         base !
    THEN idstring  c! ;

\ -------------------------------------------------------------
\ Key definitions
\ -------------------------------------------------------------
BEGIN-CREATE
\ General function keys
  $001b Constant #k-esc         \ ESC
  $000d Constant #k-cr          \ Return
  $000a Constant #k-^cr         \ Split line
  $0008 Constant #k-bs          \ Backspace
  $007f Constant #k-^bs         \ CTRL+Backspace
  $53e0 Constant #k-del         \ DEL
  $52e0 Constant #k-ins         \ INS

\ Keys (Cursor)                                 ( 30.03.14/KK )
  $48e0 Constant #k-up          \ Cursor up
  $0005 Constant #k-^e          \ CTRL + E
  $8de0 Constant #k-^up         \ CTRL + Cursor up
  $50e0 Constant #k-down        \ Cursor down
  $0018 Constant #k-^x          \ CTRL + X
  $91e0 Constant #k-^down       \ CTRL + Cursor down
  $4be0 Constant #k-left        \ Cursor left
  $0013 Constant #k-^s          \ CTRL + S
  $0001 Constant #k-^a          \ CTRL + A
  $73e0 Constant #k-^left       \ CTRL + Cursor left
  $4de0 Constant #k-right       \ Cursor right
  $0004 Constant #k-^d          \ CTRL + D
  $0006 Constant #k-^f          \ CTRL + F
  $74e0 Constant #k-^right      \ CTRL + Cursor right

\ Keys (Screen)                                 ( 17.05.15/KK )
  $47e0 Constant #k-pos1        \ POS1
  $77e0 Constant #k-^pos1       \ CTRL + POS1
  $4fe0 Constant #k-end         \ END
  $75e0 Constant #k-^end        \ CTRL + END
  $49e0 Constant #k-pgup        \ Page-Up
  $0012 Constant #K-^R
  $86e0 Constant #k-^pgup       \ CTRL + Page-Up
  $51e0 Constant #k-pgdown      \ Page-Down
  $0003 Constant #K-^C
  $76e0 Constant #k-^pgdown     \ CTRL + Page-Down
  $0007 Constant #k-^g          \ CTRL + G

\ Keys (CTRL+?)
  $0009 Constant #k-tab         \ Tab
\  $9400 Constant #k-^tab        \ CTRL + Tab (not usable with Windows 11)
  $0015 Constant #k-^u          \ CTRL + U: Upper Case
  $0014 Constant #k-^t          \ CTRL + T: Lower Case
  $000c Constant #k-^l          \ CTRL + L: Next Search/Replace
\  $0016 Constant #k-^v          \ CTRL + V (is insert on Windows 11)
  $000e Constant #k-^n          \ CTRL + N: New line
  $0019 Constant #k-^y          \ CTRL + Y: Delete line
  $93e0 Constant #k-^del        \ CTRL + DEL: Delete Screen
  $92e0 Constant #k-^ins        \ CTRL + INS: New Screen

\ Keys (F1-F12)
  $3b00 Constant #k-f1          \ Save character
  $3c00 Constant #k-f2          \ Copy character
  $3d00 Constant #k-f3          \ Insert character
  $3e00 Constant #k-f4          \ Swap file
  $3f00 Constant #k-f5          \ Save line
  $4000 Constant #k-f6          \ Copy line
  $4100 Constant #k-f7          \ Insert line
  $4200 Constant #k-f8          \ Toggle insert/overwrite
  $4300 Constant #k-f9          \ Find
  $4400 Constant #k-f10         \ Get user ID
\ $85e0 Constant #k-f11
\ $86e0 constant #k-f12         \ is CTRL + Page Up on Windows 11
END-CREATE

\ -------------------------------------------------------------
\ Keyboard functions
\ -------------------------------------------------------------
\ Key:          |    Function:       |  \ Comment
Createp eseq    ( -- addr ) \ Table with key and cfa
  #k-up         ,p   ' curup         ,p \ Up
  #k-^e         ,p   ' curup         ,p
  #k-down       ,p   ' curdown       ,p \ Down
  #k-^x         ,p   ' curdown       ,p
  #k-left       ,p   ' curleft       ,p \ Left
  #k-^s         ,p   ' curleft       ,p
  #k-right      ,p   ' curright      ,p \ Right
  #k-^d         ,p   ' curright      ,p
  #k-^a         ,p   ' curbw         ,p \ Last word
  #k-^f         ,p   ' firstfind     ,p \ Find (instead curnw)
  #k-^left      ,p   ' curbln        ,p \ Begin of line
  #k-pos1       ,p   ' curbln        ,p \ Begin of line
  #k-^right     ,p   ' cureln        ,p \ End of line
  #k-end        ,p   ' cureln        ,p \ End of line
  #k-cr         ,p   ' curcr         ,p \ Begin of next line
  #k-pgup       ,p   ' scr-          ,p \ Previous screen
  #k-^r         ,p   ' scr-          ,p \ Previous screen
  #k-pgdown     ,p   ' scr+          ,p \ Next screen
  #k-^c         ,p   ' scr+          ,p \ Next screen
  #k-^pos1      ,p   ' scr1          ,p \ Pos1
  #k-^pgup      ,p   ' scr1          ,p \ First screen
  #k-^pgdown    ,p   ' lastscr       ,p \ Last screen
  #k-^end       ,p   ' lastscr       ,p \ Pos1
  #k-^g         ,p   ' gotoln        ,p \ Goto line screen
  #k-tab        ,p   ' curtab        ,p \ Tab
  #k-^n         ,p   ' lninsbl       ,p \ Insert blank Line
  #k-^y         ,p   ' lndel         ,p \ Delete Line
  #k-bs         ,p   ' <del          ,p \ Backspace
  #k-^cr        ,p   ' lnsplit       ,p \ Split to next line
  #k-^bs        ,p   ' lnjoin        ,p \ Append next line
  #k-del        ,p   ' chardel       ,p \ DEL
  #k-ins        ,p   ' blins         ,p \ INS
  #k-^del       ,p   ' scrdel        ,p \ DEL Screen
  #k-^ins       ,p   ' scrins        ,p \ INS Screen
  #k-^u         ,p   ' wcaps         ,p \ Word in upper case
  #k-^t         ,p   ' w-caps        ,p \ Word in lower case
  #k-^l         ,p   ' nextfind      ,p \ Next Search/Replace
  #k-f1         ,p   ' charsave      ,p \ Char to buffer
  #k-f2         ,p   ' charcopy      ,p \ Copy char
  #k-f3         ,p   ' charget       ,p \ Insert char
\  #k-f4         ,p   ' fswap         ,p \ Alternative file
  #k-f5         ,p   ' lnsave        ,p \ Line to buffer
  #k-f6         ,p   ' lncopy        ,p \ Copy line
  #k-f7         ,p   ' lnget         ,p \ Insert line
  #k-f8         ,p   ' addid         ,p \ Insert ID
  #k-f9         ,p   ' firstfind     ,p \ Find/Replace string
  #k-f10        ,p   ' getid         ,p \ get user id
  0             ,p                     \ End of eseq
End-Create

  : sequencer   ( key -- ) \ Evaluate key
    eseq                        \ Table
    BEGIN
      dup @p                     \ End of table ?
    WHILE
      over over @p =             \ Key found ?
      IF nip cell+ @p  execute exit THEN
      cell+  cell+              \ next entry
    REPEAT
    drop drop ;

  : escfnc?     ( -- f ) \ f=-1 if exit
    sv_mod? @ 0=  ?dup ?exit \ nothing to save
    s"  S = Save / X = Save and Quit / Q = Quit without Save "  "key
    upc
    CASE [char] S OF savefile              0   ENDOF
         [char] X OF savefile              -1  ENDOF
         [char] Q OF                       -1  ENDOF
         0  swap                                   \ do nothing
    ENDCASE ;

\ -------------------------------------------------------------
\ Editor main routine                           ( 20.05.15/KK )
\ -------------------------------------------------------------
\ Get key with ESC functions
  : edkey      ( -- key ) \ 0 and $e0 used by special keys
    key   dup 0=  over $e0 = or
    IF key $100 *  + THEN ;

  : ed_setup    ( offset. 0 fid | line/screen -1 fid -- ) \ Init all
    spar to par                 \ Set spar as actual file information
    0 fpar !                    \ Ignore fpar information (no second file)
    sv_fid !                    \ save FID
    \ Set file pointer in memory and 0 for offset and actual line
    here $100 +  dup sv_baddr !  dup sv_eaddr !   sv_yaddr !
    0  dup sv_mod? !  dup sv_ymax !
       dup sv_sy !  dup sv_y !  dup sv_sx !  sv_x !
    lbcbinit                    \ Init character/line buffer
    loadfile                    \ Load and check file
    \ Use offset to calculate possition (actual ignored)
    IF   sv_seq? @ 0= IF #l/s * ELSE 1- THEN
         0 max   sv_ymax @  min   0 swap \ Line or Screen start
    ELSE offset>xy              \ Or error position
    THEN sv_setup ;
\    sv.  key $1b = IF quit THEN ; \ !!! Debug !!!

  : edit       ( offset. fid | line/screen -1 fid -- ) \ Editor and special functions
    ed_setup                    \ prepare memory and load file
    0 idstring c!               \ Normally without ID
    color@ >r  base @ >r  decimal
    lcdinit
    BEGIN edkey  dup $20 $7f within  over $80 $100 within  or
        IF    charins
        ELSE  dup #k-esc =
            IF   drop  escfnc?
                IF r> base !  r> color!
                  #lenx  #leny  >>scr  cr  exit THEN
            ELSE sequencer
            THEN
        THEN
        lcdrestore
    AGAIN ;

  -|                            \ Next commands visible

\ -------------------------------------------------------------
\ Editor calls in mcForth (use open file from FID)
\ -------------------------------------------------------------
\  also  Forth definitions

  : l           ( screen/line -- ) \ Edit screen
    -1  fid @  edit ;

  : v           ( -- ) \ only after errors
    source-id  3 cells +  @   \ Source type 1 or 2
    3 1 within ?abort" Error not in screen or seq. file"
    source-id  cell+  2@  1. d-  0 \ Position
    fid @  edit ;

  OnlyForth   clearh            \ Remove hidden words

