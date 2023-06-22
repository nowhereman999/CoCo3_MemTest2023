* Version 0.1
*       - Stack Blast Test: Blasts bytes 0 to 255 in each 8k block then moves on
* Version 0.2
*       - Added a routine to detect the GIME '86 or '87 and show it on screen
*       - Newer Stack Blast Test: Blasts bytes 0 to all 8k blocks then goes to 1, 2, 3, etc.
*         This has the benefit of going through 2 Megs of RAM with each value more quickly
* Version 0.3 (Version 1.0)
*       - Walking Ones Test: Tests all address bits in all memory banks by using a walking ones address pattern.
*       - Moving Inversions Test: This test is the most basic and tests the ability of the memory chips to read and write data correctly.
*         The test writes a series of patterns to the memory and then reads them back to ensure that the data has been correctly stored.
*       - Random Patterns Test: This test writes random patterns to the memory and then reads them back to ensure that the data has been
*         correctly stored. It is designed to detect any issues with the RAM's ability to handle random patterns of data.
*       - Surround Read Disturb:  Starts by creating a background of all 0s. Then, each cell in turn becomes the test cell. The test cell is
*         complemented and the eight physically adjacent cells are repeatedly read. After a number of iterations the test cell is read to
*         determine if it has been affected by the read of its neighbors. The operation is then repeated for a background of 1s. The intent is
*         to find disturbances caused by adjacent cell operations.
*       - Bit Fade Test: This test writes a pattern to the memory and then repeatedly reads it back, gradually reducing the strength of
*         the pattern until it becomes almost imperceptible. This test is designed to detect any "bit fade" issues, where the voltage levels
*         in the memory chips can gradually decay over time, causing errors.
*
* Version 0.4 (Version 1.01)
*       - Just some cosmetic changes, made the background darker and the text white, so it's easier to read
*
* Version 0.5 (Version 1.20)
*       - Fixed detection of 6809/6309 on a 128k CoCo, as it would erase the flag when clearing the screen.  Moved the flag to a better place in RAM
*       - Fixed detection code which year the GIME chip was made.  Now does 30 vsyncs after the FIRQ countdown timer is started and gets the count
*         From this value if it's less than 13891 it's an 86 GIME, if it's higher than it's an 87 GIME.
*
* Version 0.6 (Version 1.21)
*       - Doug Masten tweaked the label for the Hitachi 6309, it was previously showing Motorola 6309
*
* Version 1.21
*
* Written by Glen Hewlett, have fun with the code :)
* Assembled with LWASM


SpeedTest 	EQU     1  * 1 = show the border while drawing dots on screen then turn off when done, shows CPU usage
RGB_Colours     EQU     1  * 1 = Use RGB colour palette, otherwise we use composite colours, composite in 80 columns looks terrible

* Include CoCo 3 standard hardware pointers
        INCLUDE ./includes/CoCo3_Start.asm

* Include some generic Macros:
* Logical,Arithmetic,Rotate D - LSRD1,2,3,4 - ASLD1,2,3,4 - ROLD1,2,3,4 - INC/DEC 16bit registers (INCX,DECY,...)
        INCLUDE ./includes/Macros.asm

* Keep track of CPU cycles
* opt c  - enable cycle counts: [8]
* opt cd - enable detailed cycle counts breaking down addressing modes: [5+3]
* opt ct - show a running subtotal of cycles
* opt cc - clear the running subtotal

    opt   cd  * - enable detailed cycle counts breaking down addressing modes: [5+3]
    opt   ct  * - show a running subtotal of cycles
    opt   cc  * - clear the running subtotal
* Coco 3 Memory test

* Memroy test program starts here, first we detect what type of GIME then jump to the rest of the memory testing
;DirectPage      EQU     $0E

;        SETDP   DirectPage
        ORG     $0000

ScreenStart:                            * Location for the text screen in memory
FIRQ_GimeTest:
        LDA     FIRQENR         * Re enable the FIRQ
        INC     FIRQ_Count+1
        BEQ     >
        RTI
!       INC     FIRQ_Count
        RTI

GimeTest:
        ORCC    #%01010000      * Disable the interrupts
        CLRA
        STA     $FF40           * Turn off drive motor
        STA     High_Speed_Mode * Put the CoCo 3 in high speed mode
;        LDA     #DirectPage
;        TFR     A,DP

* Let's detect the CPU type:
        LDX     #$0100          * X = $0100
        TFR     X,A             * If it's 6809 then A will equal $00, if it's a 6309 then A will now equal $01
        STA     _Flag6309+1        * 6309 Flag will be $01 if it's a 6309 and a $00 if it's a 6809
        BEQ     >               * If it's a zero then it's a 6809, go draw it
        FCB     $11,$3D,%00000000 * put 6309 in 6809 compatible mode
!

* Setup and enable the FIRQ
        LDA     #$7E                    * Write the JMP instruction if it's possible to use Direct Page for the sample playback then use $0E = direct page JMP location, and 1 byte for address
        LDX     #FIRQ_GimeTest          * Enable FIRQ0 - Play no sounds
        STA     FIRQ_Jump_position
        STX     FIRQ_Start_Address      * FIRQ now set to playback no sound, this will be changed when an sound is played

        LDA     #%11111100              *
        STA     INIT0_Register0         * CoCo 3 Mode, MMU Enabled, GIME IRQ Enabled, GIME FIRQ Enabled, Vector RAM at FEXX enabled, Standard SCS Normal, ROM Map 16k Int, 16k Ext
        LDA     #%00100000              * Make sure the Timer is using the faster 279.365 nanosecond timer source
        STA     INIT1_Register1
        LDA     #%00000100              * Alphanumeric,,Decenders enabled, color, 60hz, 011 = 8 lines per char row, 100 = 9 lines per char row
        STA     Video_Mode_Register
        LDA     #%00010000              * $08
        STA     IRQENR                  * Enable only the Vertical Border Sync (VBORD) Interrupt

        LDD     #$0000
        STD     FIRQ_Count              * number of Timer counts completed, start at zero
        STD     FIRQ_Count+1
        LDA     #%00100000              * $20
        STA     FIRQENR                 * Enable only TIMER FIRQ Interrupt

* Start FIRQ & IRQ
* Wait until a vsync occurs so the timing starts right after
        TST     $FF02
!       TST     $FF03
        BPL     <
        LDD     #$0080                  * This is the value for the countdown of the Timer
        STD     $FF94                   * Set countdown Timer        
*                 EFHINZOC
        ANDCC 	#%10111111      * Enable FIRQ interrupt
        LDA     FIRQENR         * Re-enable the FIRQ
        LDB     #30             * Number of vsyncs to do
VSYNC   TST     $FF02
!       TST     $FF03
        BPL     <
        DECB
        BNE     VSYNC
        ORCC    #$50            * Disable the interrupts
        LDD     FIRQ_Count
;        STD     $1FFE
;        STD     $400
;        BRA     *

        CLR     FlagGIME87      * Clear 87 GIME flag
        CMPD    #13891          * If it's lower than this value
        BLO     >               * then it's an '86 GIME leave the flag as zero
        INC     FlagGIME87      * Set 87 GIME flag to 1
!
        JMP     MemTest

        ORG     $0E60
ProgramStart:
FIRQ_Count      FDB     $0000
FlagGIME87      FCB     $00     * Equals 1 if it's a '87 GIME, otherwise it's a zero if it's an '86 GIME
ScreenPOS       FDB     $0000
ColorTable:
        IF RGB_Colours
* Colour Info        xxRGBrgb
Black   EQU  $00
        FCB   %00000000         * Black
LightBlue  EQU  $01
        FCB   %00011011         * LightBlue
Blue    EQU  $02
        FCB   %00000001         * Blue
Green   EQU  $03
        FCB   %00010111         * LightGreen
Red     EQU  $04
        FCB   %00100111         * LightRed
Yellow  EQU  $05
        FCB   %00110111         * Yellow
LightGrey  EQU  $06
        FCB   %00111000         * LightGrey
White   EQU  $07
        FCB   %00111111         * White
        ELSE
Black   EQU  $00
        FCB   %00000000         * Black
LightBlue  EQU  $01
        FCB   %00011011         * LightBlue
Blue    EQU  $02
        FCB   %00011100         * Blue
Green   EQU  $03
        FCB   %00010001         * LightGreen
Red     EQU  $04
        FCB   %00010110         * LightRed
Yellow  EQU  $05
        FCB   %00110100         * Yellow
White  EQU  $06
        FCB   %00100000         * White
White   EQU  $07
        FCB   %00110000         * White
        ENDIF
* Text Attributes
Blink         EQU   %10000000
NoBlink       EQU   %00000000
Underline     EQU   %01000000
NoUnderline   EQU   %00000000
Foreground    EQU   %00001000   * Foreground (actual text colour) multiplier

TextPalette   RMB   16          * Save the palette for 80 column Text mode
HexTable      FCC     '0123456789ABCDEF'        * Table of ASCII values for converting to number to Hex
* Show on screen which block is being tested
ShowTestBlock:
        LDX     #$320           * Bottom half of text screen
        LDB     FirstBlock      * Get block value in B
        ANDB    #%11110000
        LDA     #10
        MUL
        LEAX    D,X
        LDB     FirstBlock
        ANDB    #%00001111      * Need the value between 0 and 15
        STB     ADD1+1
        ASLB                    * space between each entry on a line
ADD1:
        ADDB    #$FF
        ASLB                    * Each character takes two bytes (value,attribute)
        ADDB    #24             * Start at the correct position on this line
        ABX
        STX     CursorPos       * Save the screen position
* Test 8k blks moved to $4000-$5FFF
        LDA     Pattern
* Show A in Hex at X
ShowAHexAtX:
        LDU     #HexTable
        TFR     A,B
        LSRA
        LSRA
        LSRA
        LSRA
        LDA     A,U
        ANDB    #%00001111
        LDB     B,U
        STA     ,X
        STB     2,X
        RTS

* Draw pattern in A onscreen
PrintPatternA:
        PSHS    D,X,Y,U
        LDX     #MessageText    * X points to the text message to show on screen
!       LDB     #'0'            * B = ASCII value of zero
        ROLA                    * rotate the bits left and move bit 7 to the carry flag
        ADCB    #$00            * B=B + the value of the carry
        STB     ,X+             * save B and move X to the next byte
        CMPX    #MessageText+8  * Have we done all 8 bits yet?
        BNE     <               * If not keep looping
* Print Pattern type message 
* Pattern: position is 43,4
        LDX     #ScreenStart+10*2+2*160 * Screen location start + x co-ordinate*2 (bytes per character) + y co-ordinate*160 (160 bytes per row)
        JSR     PrintTextatX            * Print the text message below
MessageText:
        FCN     '00000000'              * Bit pattern, Null terminated
        PULS    D,X,Y,U,PC

* Print text on 80 coloumn screen at location ScreenStart
* text location, attributes and text are located at the returning Program Counter (on the stack) location
* Do a Print@ on the 80 column screen
Printat80:
        LDX     #ScreenStart
        LDU     ,S++    * U now points at the data, fix the Stack
        LDB     ,U+     * get the X co-ordinate
        LSLB            * B = B * 2 (two bytes per text entry, value byte & attribute byte)
        ABX             
        LDA     ,U+     * get the Y co-ordinate     
        LDB     #160    * Each row is 160 bytes
        MUL        
        LEAX    D,X     * X now has the starting address on screen
GetAttribute:
        LDB     ,U+     * Get the text attribute in B
!
        LDA     ,U+     * A=character from the message, add 1 to the pointer
        BEQ     >       * If A=0 then we reached the end of the text, exit loop
        CMPA    #$FF    * If change attribute flag is found then
        BEQ     GetAttribute    * Go get the new attribute value
        STD     ,X++    * Write A & B attribute byte to the text screen @ X then add 2 to pointer
        BRA     <       * Keep looping
!
        JMP     ,U      * Return

PrintTextatX:
        LDU     ,S++    * U now points at the data, fix the Stack
        LDB     #NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
!       LDA     ,U+     * A=character from the message, add 1 to the pointer
        BEQ     >       * If A=0 then we reached the end of the text, exit loop
        STD     ,X++    * Write A & B attribute byte to the text screen @ X then add 2 to pointer
        BRA     <       * Keep looping
!       JMP     ,U      * Return



* Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
Copy00to01:
        LDA     #$01        
        STA     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        LDX     #$0000          * Copy this program to blk $01
        LEAU    $4000,X
!       LDD     ,X++
        STD     ,U++
        CMPX    #ProgramEnd
        BLS     <
        LDA     #$01
        STA     MMU_Reg_Bank0_0 * EQU $FFA0   * Page $0000-$1FFF  Bank #0       * We are now executing code in bank $01
        RTS


* Copy Block $01 to $00 we just tested Block $00 and move page 0 to block $00
Copy01to00:
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        LDX     #$0000          * Copy this program to blk $01
        LEAU    $4000,X
!       LDD     ,X++
        STD     ,U++
        CMPX    #ProgramEnd
        BLS     <
        CLR     MMU_Reg_Bank0_0 * EQU $FFA0   * Page $0000-$1FFF  Bank #0       * We are now executing code in bank $00
        RTS

* Display the test Number and description
* Test # position is 13,1
DisplayTestInfo:
;        LDA     TestNumber
;        ADDA    #'0'
;        STA     UTNA
;        BSR     Printat80               * Do a Print@ on the 80 column screen
;        FCB     13,1                    * Where to start printing the text on screen
;        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
;UTNA:      
;        FCB     '8'                     * Place holder
;        FCB     $00                     * Termination value

* Print testing method message to match the TestNumber
* Testing description is position is 10,3
        LDX     #ScreenStart+10*2+3*160 * Screen location start + x co-ordinate*2 (bytes per character) + y co-ordinate*160 (160 bytes per row)
        LDA     TestNumber
        DECA
        BNE     >
* Test 1 - Stack Blasting values 0 to 255 in each byte
        JSR     PrintTextatX            * Print the text message below
        FCN     'Stack Blasting values 0 to 255 in each byte'   * Null terminated
        RTS
!       DECA
        BNE     >
* Test 2 - Walking Ones Test
        JSR     PrintTextatX            * Print the text message below
        FCN     'Walking Ones Test                          '   * Null terminated
        RTS
!       DECA
        BNE     >
* Test 3 - Moving Inversions Test
        JSR     PrintTextatX            * Print the text message below
        FCN     'Moving Inversions Test'   * Null terminated
        RTS
!       DECA
        BNE     >
* Test 4 - Random Patterns Test
        JSR     PrintTextatX            * Print the text message below
        FCN     'Random Patterns Test  '   * Null terminated
        RTS
!       DECA
        BNE     >
* Test 5 - Surround Read Disturb
        JSR     PrintTextatX            * Print the text message below
        FCN     'Surround Read Disturb'   * Null terminated
        RTS
!       DECA
        BNE     >
* Test 6 - Bit Fade Test
        JSR     PrintTextatX            * Print the text message below
        FCN     'Bit Fade Test - Read each byte 256 times'   * Null terminated
        RTS
* If we get here we have completed all the tests, start again
!
* Lets increment the cycle all tests counter
        INC     CycleCounter+5  * ones
        LDA     CycleCounter+5
        CMPA    #10
        BNE     ShowCycles
        CLRA
        STA     CycleCounter+5
        INC     CycleCounter+4  * tens
        LDA     CycleCounter+4
        CMPA    #10
        BNE     ShowCycles
        CLRA
        STA     CycleCounter+4
        INC     CycleCounter+3  * hundreds
        LDA     CycleCounter+3
        CMPA    #10
        BNE     ShowCycles
        CLRA
        STA     CycleCounter+3
        INC     CycleCounter+2  * thousands
        LDA     CycleCounter+2
        CMPA    #10
        BNE     ShowCycles
        CLRA
        STA     CycleCounter+2
        INC     CycleCounter+1  * ten thousands
        LDA     CycleCounter+1
        CMPA    #10
        BNE     ShowCycles
        CLRA
        STA     CycleCounter+1
        INC     CycleCounter    * Hundred thousands
        LDA     CycleCounter
        CMPA    #10
        BNE     ShowCycles
        CLRA
        STA     CycleCounter
ShowCycles:
        LDX     #ScreenStart+160+26*2     * Starting location of number of cycles on screen
        LDU     #CycleCounter
!       CMPU    #CycleCounter+6
        BEQ     >
        LDA     ,U+
        BEQ     <
!       ADDA    #'0'
        STA     ,X+
        LDA     #NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        STA     ,X+
        LDA     ,U+
        CMPU    #CycleCounter+7
        BLO     <

        LDA     #1
        STA     TestNumber
        LBRA    DisplayTestInfo         * Keep doing the tests

* We get here after the interrupts have happened and the GIME type has been figured out
MemTest:
        ORCC    #$50            * No Interrupts
        LDS     #StackSpace     * Stack pointer is at the end of unused page (page 1)

* Copy Block $38 to Block $00 and then move it to Page 0 so we can test block $00 first
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        LDX     #$0000          * Copy this program to blk $00
        LEAU    $4000,X
!       LDD     ,X++
        STD     ,U++
        CMPX    #ProgramEnd
        BLS     <
        CLR     MMU_Reg_Bank0_0 * EQU $FFA0   * Page $0000-$1FFF  Bank #0       * We are now executing code in bank $00

* Let' figure out CoCo 3 text mode behavior
        LDB     #8
        LDX     #ColorTable
        LDU     #Palette_Start
        LDY     #TextPalette
!       LDA     ,X+
        STA     8,U                     * Foreground (actual text) colour
        STA     ,U+                     * Backgroundground colour
        STA     8,Y                     * Foreground (actual text) colour Text Palette Buffer
        STA     ,Y+                     * Backgroundground colour colour Text Palette Buffer
        DECB
        BNE     <

        LDA     #%01011100              *
        STA     INIT0_Register0         * CoCo 3 Mode, MMU Enabled, GIME IRQ Disabled, GIME FIRQ Enabled, Vector RAM at FEXX enabled, Standard SCS Normal, ROM Map 16k Int, 16k Ext

* Super Extended BASIC uses these values for $FF98,$FF9A,...
* FCB $03,$15,$12,$00,$00,$D8
* FCB $00,$00

        LDA     #%00000100              * Alphanumeric (text) mode,unused,Extra Descender Disabled,Colour,60hz, 8 lines per row
        STA     Video_Mode_Register     * $FF98
* Bits 4 & 2 seem to control the text mode characters per row (0x0 = 32, 0x1 = 40, 1x0 = 64, 1x1 = 80 characters per row) bit 3 is ignored
        LDA     #%00010101              * Unused,0 Lines per field, HRES210 - Horizonal res= 101 = 80 characters, 01=4 colors 4 pixels per plane
        STA     Vid_Res_Reg             * $FF99
        LDA     #%00000001              * Blue
        STA     Border_Register         * $FF9A border colour for the text or graphics screen
        CLR     $FF9B                   * Not used (Disto 2 Meg Upgrade bank)
        CLR     $FF9C                   * VideoScroll register

* Calculate value is $3F * $2000 = $7E000 / 8 = $FC00 in our 512k of RAM = $E000 for screen start
* Point video screen veiwer to $00000 (Hi-Res page start)
* 80 Character Text screen starting address is where the variable ScreenStart is located ($0000 to $FDFF)

TextScreenMem   EQU     ScreenStart/8   * Calculate where in RAM the 80 coloumn text screen will be shown
TextScreenBlock EQU     ScreenStart/8192

        LDD     #TextScreenMem
        STD     VidStart
        CLR     Hor_Offset_Reg          * Don't use a Horizontal offset

* The code above only needs to be run once at startup
* To save memory and give the stack lots of space
* we will set the stack here
StackSpace:

* Clear the screen
        LDX     #ScreenStart
        LDD     #' '*$100+NoBlink+NoUnderline+Blue+Foreground*Blue    * Blue Blank
!       STD     ,X++
        CMPX    #ScreenStart+160*22
        BNE     <
* Write to screen MemTest2023+ v1.01
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,0                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Green+Foreground*Black    * Attribute byte = Green background with Black text
        FCC     '       MemTest2023'      * Message
        FCB     $FF                     * Indicate attribute change
        FCB     NoBlink+NoUnderline+Green+Foreground*Red    * Attribute byte = Green background with Red text
        FCC     '+'                     * Message
        FCB     $FF                     * Indicate attribute change
        FCB     NoBlink+NoUnderline+Green+Foreground*Black    * Attribute byte = Blue background with White text
        FCN     ' v1.20        '        * Message to write on screen
_Flag6309:
        LDA     #$FF            * 6309 Flag will be $01 if it's a 6309 and a $00 if it's a 6809
        BEQ     Show6809        * If it's a zero then it's a 6809, go draw it
* We have a 6309 CPU
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,0                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with LightGrey text
        FCN     '|Hitachi 6309     1.79 MHz'    * Message to write on screen, FCN terminates string with a zero
        FCB     $11,$3D,%00000001       * put 6309 in native mode (speed up the tests a little more)
        BRA     >
Show6809:
* We have a 6809 CPU
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,0                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '|Motorola 6809    1.79 MHz'    * Message to write on screen, FCN terminates string with a zero
!
* Let's display the type of GIME that is in this CoCo 3
        LDA     FlagGIME87      * If it's an '87 GIME this will be a 1 otherwise it will be a zero if it's and '86 GIME
        BEQ     GIME86          * Branch if zero we have an 86 GIME
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,1                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '|1987 GIME Detected'    * Message to write on screen, FCN terminates string with a zero
        BRA     >
GIME86:
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,1                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '|1986 GIME Detected'    * Message to write on screen, FCN terminates string with a zero
!
* Figure out how much RAM this CoCo 3 has
* 128k uses RAM blocks from $00 to $0F (mirrored every 128k, where $10 = $00, $20 = $00, $30 = $00, ...)
* 512k uses RAM blocks from $00 to $3F (mirrored every 512k, where $40 = $00, $80 = $00, $C0 = $00)
* 2 Meg uses RAM blocks from $00 to $FF (no mirroring)
        LDB     #$10
        STB     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $10
        CLR     MemCheckByte+$4000      * Make value at block 2, byte a zero, just so our 128k mode detection will work all the time

        LDB     #$40
        STB     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $40
        CLR     MemCheckByte+$4000      * Make value at block 2, byte zero a zero
* Test for 128k
        LDB     #$10
        STB     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $10
        STB     MemCheckByte+$4000      * Make value at block 2, byte a zero, just so our 128k mode detection will work all the time
        CLR     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        LDA     MemCheckByte+$4000
        CMPA    #$10
        LBEQ    Test128k                * If this bytes has been changed to $10 then we have 128k
* Make block $40 a $40
        LDB     #$40
        STB     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $40
        STB     MemCheckByte+$4000      * Make value at block 2, byte zero a $40
* Let's see if it changed
        CLR     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        LDA     MemCheckByte+$4000
        CMPA    #$40
        BEQ     Test512k        * If this block is still zero then we have 512k
* If we get here then we have 2 Megs of RAM
Test2Megs:
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,2                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '|Memory  : 2048k'    * Message to write on screen, FCN terminates string with a zero
        LDB     #$FF                    * 2 Meg uses RAM blocks from $00 to $FF
        BRA     >
Test512k:
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,2                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '|Memory  :  512k'    * Message to write on screen, FCN terminates string with a zero
        LDB     #$3F                    * 512k uses RAM blocks from $00 to $3F
        BRA     >
Test128k:
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     33,2                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '|Memory  :  128k'    * Message to write on screen, FCN terminates string with a zero
        LDB     #$0F                    * 128k uses RAM blocks from $00 to $0F
!
        CLRA
        STD     FirstBlock              * Save the First Block as zero and Last Block to be tested (value in B)

;        JSR     Printat80               * Do a Print@ on the 80 column screen
;        FCB     0,1                     * Where to start printing the text on screen
;        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
;        FCN     'Test Number:'          * Message to write on screen, FCN terminates string with a zero

        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     0,1             * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Cycles through all tests: 0'

        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,2                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Pattern:'              * Message to write on screen, FCN terminates string with a zero

        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,3                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Testing:'              * Message to write on screen, FCN terminates string with a zero

* Let's draw memory map
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     4,4                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '8K'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     2,5                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Blocks'

        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,4                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,5                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 128k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,6                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 256k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,7                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 384k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,8                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Test 1 -    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 512k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,9                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Test 2 -    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 640k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,10                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Test 3 -    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 768k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,11                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Test 4 -    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 896k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,12                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Test 5 -    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1024k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     0,13                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'Test 6 -    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1152k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,14                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1280k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,15                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1408k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,16                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1536k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,17                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1664k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,18                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1792k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,19                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 1920k'
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     12,20                    * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '.. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. - 2048k'
!
        CLR     TestNumber              * Start with the first test
NextTest:
        INC     TestNumber              
        JSR     DisplayTestInfo * Display the test Number and description
        LDA     TestNumber
        DECA
        LBEQ    DoTest1         * Test 1 - Stack Blasting values 0 to 255 in each byte
        DECA
        LBEQ    DoTest2         * Test 2 - Walking Ones Test
        DECA
        LBEQ    DoTest3         * Test 3 - Moving Inversions Test
        DECA
        LBEQ    DoTest4         * Test 4 - Random Patterns Test
        DECA
        LBEQ    DoTest5         * Test 5 - Surround Read Disturb
        DECA
        LBEQ    DoTest6         * Test 6 - Bit Fade Test
        BRA     <               * Should never get here but restart the tests if it does

* Test 2 - Walking Ones Test
DoTest2:
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     9,9                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '??'
        CLR     FirstBlock      * Start at the first block again
        JSR     Copy00to01      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
* Test bank $00
* Now we can test page 2 like normal
        LDD     #TextScreenMem+$2000/8  * Point at block $01
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        JSR     WalkingOnesTest * Go test the RAM at $4000 to $5FFF
        JSR     Copy01to00      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
        LDD     #TextScreenMem  * Point at block $00
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
* Now test Blocks $01 to last block
!       INC     FirstBlock      * Move to the next mem block to test
        LDA     FirstBlock
        STA     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        JSR     WalkingOnesTest * Go test the RAM at $4000 to $5FFF
        LDA     FirstBlock
        CMPA    LastBlock       * Did we just test the last block?
        BNE     <
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,9             * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'OK'
        JMP     NextTest        * Go do the next test


* Test 3 - Moving Inversions Test
DoTest3:         
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,10            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '??'
        CLR     FirstBlock      * Start at the first block again
        JSR     Copy00to01      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
* Test bank $00
* Now we can test page 2 like normal
        LDD     #TextScreenMem+$2000/8  * Point at block $01
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        JSR     MovingInversions * Go test the RAM at $4000 to $5FFF
        JSR     Copy01to00      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
        LDD     #TextScreenMem  * Point at block $00
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
* Now test Blocks $01 to last block
!       INC     FirstBlock      * Move to the next mem block to test
        LDA     FirstBlock
        STA     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        JSR     MovingInversions * Go test the RAM at $4000 to $5FFF
        LDA     FirstBlock
        CMPA    LastBlock       * Did we just test the last block?
        BNE     <
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,10            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'OK'
        JMP     NextTest        * Go do the next test


* Test 4 - Random Patterns Test
DoTest4:      
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,11            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '??'
        CLR     FirstBlock      * Start at the first block again
        JSR     Copy00to01      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
* Test bank $00
* Now we can test page 2 like normal
        LDD     #TextScreenMem+$2000/8  * Point at block $01
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        JSR     RandomPatterns  * Go test the RAM at $4000 to $5FFF
        JSR     Copy01to00      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
        LDD     #TextScreenMem  * Point at block $00
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
* Now test Blocks $01 to last block
!       INC     FirstBlock      * Move to the next mem block to test
        LDA     FirstBlock
        STA     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        JSR     RandomPatterns  * Go test the RAM at $4000 to $5FFF
        LDA     FirstBlock
        CMPA    LastBlock       * Did we just test the last block?
        BNE     <
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,11            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'OK'
        JMP     NextTest        * Go do the next test

* Test 5 - Surround Read Disturb
DoTest5:         
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,12            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '??'
        CLR     FirstBlock      * Start at the first block again
        JSR     Copy00to01      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
* Test bank $00
* Now we can test page 2 like normal
        LDD     #TextScreenMem+$2000/8  * Point at block $01
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart                * Update screen viewer
        CLR     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        JSR     SurroundReadDisturb     * Go test the RAM at $4000 to $5FFF
        JSR     Copy01to00              * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
        LDD     #TextScreenMem          * Point at block $00
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart                 * Update screen viewer
* Now test Blocks $01 to last block
!       INC     FirstBlock              * Move to the next mem block to test
        LDA     FirstBlock
        STA     MMU_Reg_Bank0_2         * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        JSR     SurroundReadDisturb     * Go test the RAM at $4000 to $5FFF
        LDA     FirstBlock
        CMPA    LastBlock               * Did we just test the last block?
        BNE     <
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,12            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'OK'
        JMP     NextTest        * Go do the next test

* Test 6 - Bit Fade Test
DoTest6:    
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,13            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '??'
        CLR     FirstBlock      * Start at the first block again
        JSR     Copy00to01      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
* Test bank $00
* Now we can test page 2 like normal
        LDD     #TextScreenMem+$2000/8  * Point at block $01
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        JSR     BitFadeTest     * Go test the RAM at $4000 to $5FFF
        JSR     Copy01to00      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
        LDD     #TextScreenMem  * Point at block $00
; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <
        STD     VidStart        * Update screen viewer
* Now test Blocks $01 to last block
!       INC     FirstBlock      * Move to the next mem block to test
        LDA     FirstBlock
        STA     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        JSR     BitFadeTest     * Go test the RAM at $4000 to $5FFF
        LDA     FirstBlock
        CMPA    LastBlock       * Did we just test the last block?
        BNE     <
        JSR     Printat80       * Do a Print@ on the 80 column screen
        FCB     9,13            * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'OK'
        JMP     NextTest        * Go do the next test

* Test 1 - Stack Blasting values 0 to 255 in each byte
* Start testing all the blocks with the value $00, then all the blocks with $01, and so on
DoTest1:
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     9,8                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     '??'
        CLR     Pattern                 * Start with $00
        BRA     FirstTestBlock
DoneTest1:
        LDA     Pattern
        CMPA    #$FF
        BNE     >
        JSR     Printat80               * Do a Print@ on the 80 column screen
        FCB     9,8                     * Where to start printing the text on screen
        FCB     NoBlink+NoUnderline+Blue+Foreground*White    * Attribute byte = Blue background with White text
        FCN     'OK'
        JMP     NextTest                * Go do the next test

!       INC     Pattern
FirstTestBlock:
        LDA     Pattern
        JSR     PrintPatternA   * Draw pattern in A onscreen
        CLR     FirstBlock      * Start at the first block again
        JSR     Copy00to01      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
* Test bank $00
* Now we can test page 2 like normal
        LDD     #TextScreenMem+$2000/8  * Point at block $01

; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <

        STD     VidStart        * Update screen viewer
        CLR     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2, point at block $00
        LDX     #DoneTest_00A   * Setup the return address
        STX     FixReturn1+1    * Self mod the return address jump
        JMP     BlockTest1      * Go test the RAM at $4000 to $5FFF with the value in Pattern
DoneTest_00A:
        JSR     Copy01to00      * Copy Block $00 to $01 so we can test Block $00 and move page 0 to block $01
DoneCopy1to0_0:
        LDD     #TextScreenMem  * Point at block $00

; VSYNC
        TST     $FF02
!       TST     $FF03
        BPL    <

        STD     VidStart        * Update screen viewer

        LDX     #CompareLastBlock
        STX     FixReturn1+1     * Self mod the return address jump
BlockTestLoop:
;        LDX     CursorPos
;        LDA     #'O'
;        STA     ,X
;        LDA     #'K'
;        STA     2,X
        INC     FirstBlock      * Move to the next mem block to test
        LDA     FirstBlock
        STA     MMU_Reg_Bank0_2 * EQU $FFA2   * Page $4000-$5FFF  Bank #2
        LDA     Pattern         * Get the byte value to test with
        JMP     BlockTest1      * Go test the block in $4000-$5FFF
CompareLastBlock:
        LDA     FirstBlock
        CMPA    LastBlock       * Did we just test the last block?
        BNE     BlockTestLoop   * If not keep testing
* If we get here then all the RAM is good :)
        JMP     DoneTest1       * Return
BlockTest1:
        JSR     ShowTestBlock
        LDA     Pattern
        STS     RestoreStack+2  * Save the current Stack pointer
;TestWithA:
        TFR     A,B
        LEAX    TestD+2+5,PCR
        STD     -5,X            * Self modify value to test below
        STD     ,X              * Self modify value to test below
        STD     6,X             * Self modify value to test below
        STD     12,X            * Self modify value to test below

* Fill RAM block $4000 to $5FFF with the value in A
        JSR     BlastBlockA
        LDS     #$4000
* We get here once the block has been written to
* Let's test it
!       PULS    D,X,Y,U         * Read 8 bytes
TestD:
        CMPD    #$FFFF          * Self modified from above        
        BNE     BadD
TestX:
        CMPX    #$FFFF          * Self modified from above        
        BNE     BadX
TestY:
        CMPY    #$FFFF          * Self modified from above        
        BNE     BadY
TestU:
        CMPU    #$FFFF          * Self modified from above        
        BNE     BadU
        CMPS    #$6000
        BNE     <
RestoreStack:
        LDS     #$FFFF
* If we get here then the block just tested good
FixReturn1:
        JMP     $FFFF           * Return address will be self modified

* If we get here then there was an error in RAM!
* Bad RAM at address S - 8
BadD:
        LEAX    -8,S
        BRA     >
* Bad RAM at address S - 6
BadX:
        LEAX    -6,S
        BRA     >
* Bad RAM at address S - 4
BadY:
        LEAX    -4,S
        BRA     >
* Bad RAM at address S - 2
BadU:
        LEAX    -2,S
* X now has the memory location with the bad value (we don't know if it's X or X+1 for the actual bad byte)
!
Here:
        LDA     TestD-Here-1,PC
        CMPA    ,X              * Is it the MSB that is bad?
        BNE     BadRAMatX       * If so then X is pointing to it already
        LEAX    1,X             * Otherwise it's the next byte that must be bad
* X now points to the bad byte in RAM
BadRAMatX:
        LEAX    -$4000,X        * Make the value between $0000 and $1FFF in this block that is bad
        PSHS    X               * Save memory location in the block that is bad
        LDA     #'X'
        LDB     #Blink+NoUnderline+Red+Foreground*Black    * Attribute byte = Red background with Black text
        LDU     CursorPos       * get Block location on screen
        STD     ,U
        STD     2,U 
        LDX     #ScreenStart+10*2+3*160 * Screen location start + x co-ordinate*2 (bytes per character) + y co-ordinate*160 (160 bytes per row)
        JSR     PrintTextatX            * Print the text message below
        FCN     'Bad RAM found at Block $xx, byte $xxxx in that block'   * Null terminated
        LDX     #ScreenStart+34*2+3*160 * Screen location start + x co-ordinate*2 (bytes per character) + y co-ordinate*160 (160 bytes per row)
        LDA     FirstBlock              * Get the bad block, being tested
        JSR     ShowAHexAtX             * Show the Hex value of A at location on screen in X
        LDA     ,S+                     * get the MSB
        LDX     #ScreenStart+44*2+3*160 * Screen location start + x co-ordinate*2 (bytes per character) + y co-ordinate*160 (160 bytes per row)
        JSR     ShowAHexAtX             * Show the Hex value of A at location on screen in X
        LDA     ,S+                     * get the LSB and fix the stack
        LDX     #ScreenStart+46*2+3*160 * Screen location start + x co-ordinate*2 (bytes per character) + y co-ordinate*160 (160 bytes per row)
        JSR     ShowAHexAtX             * Show the Hex value of A at location on screen in X

        BRA     *               * Loop forever, until we figure out how to show the user

* Blast value in A in page 2
BlastBlockA:
        TFR     A,B
        TFR     A,DP
        TFR     D,X
        LEAY    ,X
        LEAU    ,X
        STS     BlastBlockA_GetS+2
        LDS     #$6000
!       PSHS    D,DP,X,Y,U      * 9 Bytes
        PSHS    D,DP,X,Y,U      * 18 Bytes
        PSHS    D,DP,X,Y,U      * 27 Bytes
        PSHS    D,DP,X,Y,U      * 36 Bytes
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      * 72 Bytes
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      * 144 Bytes
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,DP,X,Y,U      
        PSHS    D,X             * 256 Bytes
        CMPS    #$4000
        BNE     <
BlastBlockA_GetS:
        LDS     #$FFFF
        RTS

* Do the walking ones test:
* Clear all RAM, then set bit 0 of each byte, then set bit 1 and so on.
* Do the compliment:
* Set all RAM, clear bit 0 of each byte, then set bit 1 and so on.
WalkingOnesTest:
        CLRA                    * Fill block with $00
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        LDA     #%00000001      * Set bit 0
Loop1@:
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDA     Pattern
        LDX     #$4000
Loop2@:
        STA     ,X
        LDB     ,X+
        CMPB    Pattern
        BNE     FoundBadRAM2
        CMPX    #$6000
        BNE     Loop2@
        LSLA
        BCC     Loop1@

        LDA     #$FF            * Fill block with $FF
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        LDA     #%11111101      * When we add 1 below it will Clear bit 0
Loop1@:
        ADDA    #$01
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDA     Pattern
        LDX     #$4000
Loop2@:
        STA     ,X
        LDB     ,X+
        CMPB    Pattern
        BNE     FoundBadRAM2
        CMPX    #$6000
        BNE     Loop2@
        LSLA
        BCS     Loop1@
        RTS
FoundBadRAM2:
        LEAX    -1,X
        JMP     BadRAMatX


* Clear all RAM, then set bit 0 of each byte, then set bit 1 and so on.
* Write the byte, then invert it, then invert it again back to original
MovingInversions:
        CLRA                    * Fill block with $00
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        LDA     #%00000001      * Set bit 0
Loop1@:
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDA     Pattern
        LDX     #$4000
Loop2@:
        STA     ,X
        COM     ,X
        COM     ,X
        LDB     ,X+
        CMPB    Pattern
        BNE     FoundBadRAM2
        CMPX    #$6000
        BNE     Loop2@
        LSLA
        BCC     Loop1@

        LDA     #$FF            * Fill block with $FF
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        LDA     #%11111101      * When we add 1 below it will Clear bit 0
Loop1@:
        ADDA    #$01
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDA     Pattern
        LDX     #$4000
Loop2@:
        STA     ,X
        COM     ,X
        COM     ,X
        LDB     ,X+
        CMPB    Pattern
        BNE     FoundBadRAM2
        CMPX    #$6000
        BNE     Loop2@
        LSLA
        BCS     Loop1@
        RTS

* Set all RAM, then fill a block with random values
* Read back random values to see if they match
RandomPatterns:
        CLRA                    * Fill block with $00
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
;        LDD     #$53A7
;        STD     SEED
;        LDA     #$92
;        STA     LSEED
        LDB     #32
RandomPatLoop:
        BSR     RandomA         * Get random # in A
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        PSHS    B
        JSR     ShowTestBlock
        PULS    B
        LDA     Pattern
        LDX     #$4000
!       STA     ,X+             * Fill with random byte
        CMPX    #$6000
        BNE     <

        LDX     #$4000
!       LDA     ,X+             * Check for random byte
        CMPA    Pattern
        LBNE    FoundBadRAM2
        CMPX    #$6000
        BNE     <
        DECB
        BNE     RandomPatLoop
        RTS

*
*RANDOM NUMBER GENERATOR
*
RandomA:
       PSHS     B
       LDB      SEED
       LDA      #$03
       MUL
       ADDB     #17
       LDA      LSEED
       LSRA
       LSRA
       LSRA
       EORA     LSEED
       LSRA
       ROR      HSEED
       ROR      LSEED
       ADDB     LSEED
       ADCB     HSEED
       STB      SEED
       LDA      SEED
       PULS     B,PC
*RANDOM VARIABLES
SEED   FCB      $53
HSEED  FCB      $A7
LSEED  FCB      $92     

* Surround Read Disturb
* - Clear all RAM, then each cell in turn becomes the test cell. The test cell is complemented and the previous byte
* and the next byte are repeatedly read 32 times then the test cell is read to determine if it has been affected by
* the reading of its neighbour's. The operation is then repeated for a background of 1s. The intent is to find
* disturbances caused by adjacent cell operations.
SurroundReadDisturb:
        CLRA                    * Fill block with $00
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDA     Pattern
        LDX     #$4000
Loop1@  COM     ,X
        LDB     #32
!       LDA     -1,X            * Read adjacent bytes B times
        LDA     1,X
        DECB
        BNE     <
        LDA     ,X+             * Is our byte still the same?
        COMA
        CMPA    Pattern
        LBNE    FoundBadRAM2
        CMPX    #$6000
        BNE     Loop1@

        LDA     #$FF            * Fill block with $FF
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDA     Pattern
        LDX     #$4000
Loop1@  COM     ,X
        LDB     #32
!       LDA     -1,X            * Read adjacent bytes B times
        LDA     1,X
        DECB
        BNE     <
        LDA     ,X+             * Is our byte still the same?
        COMA
        CMPA    Pattern
        LBNE    FoundBadRAM2
        CMPX    #$6000
        BNE     Loop1@
        RTS

* Bit Fade Test: This test writes a pattern to the memory and then repeatedly reads it back, gradually reducing the strength of
*         the pattern until it becomes almost imperceptible. This test is designed to detect any "bit fade" issues, where the voltage levels
*         in the memory chips can gradually decay over time, causing errors.
BitFadeTest:
        LDA     #$FF            * Fill block with $FF
        JSR     BlastBlockA     * Blast memory block at $4000-$5FFF with the value of A
        STA     Pattern         * Save it for later
        JSR     PrintPatternA   * Draw pattern in A on screen, keeps all registers
        JSR     ShowTestBlock
        LDU     CursorPos       * Get the screen position for the Block
        LDA     #'?'
        STA     ,U
        STA     2,U     * Show ??
        LDA     Pattern
        LDX     #$4000
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400
        BNE     Loop1@
        LDA     #'.'
        STA     ,U
        LDA     #'?'
        STA     2,U     * Show .?

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400
        BNE     Loop1@
        LDA     #'o'
        STA     ,U
        LDA     #'?'
        STA     2,U     * Show o?

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400+$0400
        BNE     Loop1@
        LDA     #'O'
        STA     ,U
        LDA     #'?'
        STA     2,U     * Show O?

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400+$0400+$0400
        BNE     Loop1@
        LDA     #'.'
        STA     2,U     * Show O.

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400+$0400+$0400+$0400
        BNE     Loop1@
        LDA     #'o'
        STA     2,U     * Show Oo

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400+$0400+$0400+$0400+$0400
        BNE     Loop1@
        LDA     #'O'
        STA     2,U     * Show OO

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400+$0400+$0400+$0400+$0400+$0400
        BNE     Loop1@
        LDA     #'K'
        STA     2,U     * Show OK

        LDA     Pattern
Loop1@  STA     ,X
        CLRB             * Read each byte 256 times
!       LDA     ,X
        CMPA    #$FF            * Are all the bits still set?
        LBNE    FoundBadRAM2    
        DECB
        BNE     <        
        LEAX    1,X
        CMPX    #$4000+$0400+$0400+$0400+$0400+$0400+$0400+$0400+$0400
        BNE     Loop1@

        RTS

CursorPos       FDB     $500    * Position to show which memory block is being tested
FirstBlock      FCB     $00     * First Block to be tested
LastBlock       FCB     $00     * Last Block to be tested
MemCheckByte    FCB     $00     * This byte is used to test the amount of memory the CoCo 3 has
Pattern         FCB     $00     * Byte value to test with
TestNumber      FCB     $00     * The number referring to the type of test being performed
CycleCounter    FDB     $0000,$0000,$0000   * Cycle through all the tests counter
Author          FCC     'Written By: Glen Hewlett, May 11th, 2023'
ProgramEnd:
;        END     MemTest
        END     GimeTest