        PUBLIC  __iar_program_start
        EXTERN  __vector_table

        SECTION .text:CODE:REORDER(2)
        
        ;; Keep vector table even if it's not referenced
        REQUIRE __vector_table
        
        THUMB

; System Control definitions
SYSCTL_BASE             EQU     0x400FE000
SYSCTL_RCGCGPIO         EQU     0x0608
SYSCTL_PRGPIO		EQU     0x0A08
SYSCTL_RCGCUART         EQU     0x0618
SYSCTL_PRUART           EQU     0x0A18
; System Control bit definitions
PORTA_BIT               EQU     000000000000001b ; bit  0 = Port A
PORTF_BIT               EQU     000000000100000b ; bit  5 = Port F
PORTJ_BIT               EQU     000000100000000b ; bit  8 = Port J
PORTN_BIT               EQU     001000000000000b ; bit 12 = Port N
UART0_BIT               EQU     00000001b        ; bit  0 = UART 0

; NVIC definitions
NVIC_BASE               EQU     0xE000E000
NVIC_EN1                EQU     0x0104
VIC_DIS1                EQU     0x0184
NVIC_PEND1              EQU     0x0204
NVIC_UNPEND1            EQU     0x0284
NVIC_ACTIVE1            EQU     0x0304
NVIC_PRI12              EQU     0x0430

; GPIO Port definitions
GPIO_PORTA_BASE         EQU     0x40058000
GPIO_PORTF_BASE    	EQU     0x4005D000
GPIO_PORTJ_BASE    	EQU     0x40060000
GPIO_PORTN_BASE    	EQU     0x40064000
GPIO_DIR                EQU     0x0400
GPIO_IS                 EQU     0x0404
GPIO_IBE                EQU     0x0408
GPIO_IEV                EQU     0x040C
GPIO_IM                 EQU     0x0410
GPIO_RIS                EQU     0x0414
GPIO_MIS                EQU     0x0418
GPIO_ICR                EQU     0x041C
GPIO_AFSEL              EQU     0x0420
GPIO_PUR                EQU     0x0510
GPIO_DEN                EQU     0x051C
GPIO_PCTL               EQU     0x052C

; UART definitions
UART_PORT0_BASE         EQU     0x4000C000
UART_FR                 EQU     0x0018
UART_IBRD               EQU     0x0024
UART_FBRD               EQU     0x0028
UART_LCRH               EQU     0x002C
UART_CTL                EQU     0x0030
UART_CC                 EQU     0x0FC8
;UART bit definitions
TXFE_BIT                EQU     10000000b ; TX FIFO full
RXFF_BIT                EQU     01000000b ; RX FIFO empty
BUSY_BIT                EQU     00001000b ; Busy

; --------------**************************************------------- ;
; --------------* CALCUMAX Assembly based calculator *------------- ;
; --------------**************************************------------- ;

__iar_program_start
        
main:   
        ;This part of the code is made as so all the ports as well as the UART
        ;are properly configured to execute the operations needed------------;

        MOV R2, #(UART0_BIT)
	BL UART_enable ; Toggles clock on UARTs port 0

        MOV R2, #(PORTA_BIT)
	BL GPIO_enable ; Toggles clock on GPIOA port 0
        
	LDR R0, =GPIO_PORTA_BASE
        MOV R1, #00000011b ; Using bits 0 and 1 as special bits
        BL GPIO_special

	MOV R1, #0xFF ; special functions' mask on port A (bits 1 and 0)
        MOV R2, #0x11  ; Chooses RX and TX in port A (UART)
        BL GPIO_select

	LDR R0, =UART_PORT0_BASE
        BL UART_config ; configures the UART peripheric
        
        ; In this program, we will be working with the polling strategy, -----; 
        ; which gives us an echo: everything that the user types will also be-;
        ; printed in the terminal simulator.----------------------------------;
        
        ;R3: The first operand of the mathematical operation -----------------;
        ;R4: The second operand ----------------------------------------------;
        ;R5: Identifies if we're handling the first (0) or second (1) operand-;
        ;R6: Identifies with which decimal part of the number we're dealing --;
        ;with (1, 2, 3 or 4) to multiply it by 10, 100 or 1000 to be soon ----;
        ;after added to form the operand -------------------------------------;
        ;R7: Identifies which operation to be executed (1 for mult, 2 for add ;
        ;, 3 for sub and 4 to divide)-----------------------------------------;
        MOV R5, #0 
        MOV R3, #0
        MOV R4, #0
        MOV R6, #0x1
        MOV R7, #1
                     
loop:
wrx:    LDR R2, [R0, #UART_FR] ; UART STATUS
        TST R2, #RXFF_BIT ; Is it full?
        BEQ wrx ; If full, go back

        LDR R1, [R0] ; Receive data from UART RX typed by the user

        
wtx:    LDR R2, [R0, #UART_FR] ; UART STATUS
        TST R2, #TXFE_BIT ; Is the transmitter empty?
        BEQ wtx ; If empty, go back 
        STR R1, [R0] ; Transmits to the UART TX the data supposed to be printed out
          
        BL handle_input
       
        SUB R1, #0x30 ; Transforms the ASCII number in HEX
        
        PUSH {R1}
        
        BL test_after_input                         
          
        ADD R6, #0x1  ;;MUDAR ISSO!!!
        B loop


; --------------------------------SUBROUTINES--------------------------------;

;form_number_first/second = Sequentially build the operands 1 and 2 ---------;
;Stack = Contains the numeric hex value of the operand ----------------------;
;Destroys R10, R11 ----------------------------------------------------------;
form_number_first:
        MOV R10, #0x10
        MOV R11, #0x10
        
        POP {R1}
        ADD R3, R1
        
        POP {R1}
        MUL R1, R10
        ADD R3, R1
        
        MUL R10, R11
        
        POP {R1}
        MUL R1, R10
        ADD R3, R1
        
        MUL R10, R11
        
        POP {R1}
        MUL R1, R10
        ADD R3, R1
        
        BX LR        
form_number_second:
        MOV R10, #0x10
        
        POP {R1}
        ADD R4, R1
        
        POP {R1}
        MUL R1, R10
        ADD R4, R1
        
        MUL R10, R10
        
        POP {R1}
        MUL R1, R10
        ADD R4, R1
        
        MUL R10, R10
        
        POP {R1}
        MUL R1, R10
        ADD R4, R1
        
        BX LR
;--------------------------------------------------------------------------;

;test_after_input = Checks if we are gathering the last digit of the number;
;R6 = In which power of 10 are we operating? ------------------------------;
;R5 = Is it the first or second operand? ----------------------------------;
test_after_input:
            B handle_end_num
exit_henum:
        BX LR
        
handle_end_num:

        CMP R6, #0x4; Indicates decimal 4 (end of number since they're supposed to have 4 digits max) 
        IT NE
          BNE exit_henum
        
        CMP R5, #0
        IT EQ
          BEQ form_number_first
          
        CMP R5, #0x1  
        IT EQ
          BEQ form_number_second
        
        BX LR        

;--------------------------------------------------------------------------;

;handle_input = Handles the input values accordingly ----------------------;
;R1 = The input value -----------------------------------------------------;
;R7 = Represents the operation the user chose -----------------------------;
;R6 = In which power of 10 are we operating? ------------------------------;
;R5 = Is it the first or second operand? ----------------------------------;
handle_input:    
            B handle_mult
exit_hmul:  B handle_add
exit_hadd:  B handle_sub
exit_hsub:  B handle_div
exit_hdiv:  B handle_eq
exit_heq:   
            
            BX LR



handle_eq:        
        CMP R1, #0x3B; Equals ASCII symbol
        IT NE
          BNE exit_heq
        ORN R5, #0
        MOV R6, #1
        MOV R7, #0x1
        
        BL form_number_second
        
        CMP R7, #0x1; Tests for multiplication
        IT EQ
          MULEQ R3, R4
          
        CMP R7, #0x2; Tests for addition
        IT EQ
          ADDEQ R3, R4
        
        CMP R7, #0x3; Tests for subtraction
        IT EQ
          SUBEQ R3, R4
        
        ;;CMP R7, #0x4; Tests for division
        ;;IT EQ
          ;;DIVEQ R3, R4
        
        B exit_heq
handle_mult:

        CMP R1, #0x2A ; Multiplication ASCII symbol
        IT NE
          BNE exit_hmul
        ORN R5, #0
        MOV R6, #0x1
        MOV R7, #0x1
        B exit_hmul
handle_add:

        CMP R1, #0x2B ; Addition ASCII symbol
        IT NE
          BNE exit_hadd
        ORN R5, #0
        MOV R6, #0x1
        MOV R7, #0x2
        B exit_hadd       
handle_sub:

        CMP R1, #0x2D ; Subtraction ASCII symbol
        IT NE
          BNE exit_hsub
        ORN R5, #0
        MOV R6, #0x1
        MOV R7, #0x3
        B exit_hsub
handle_div:

        CMP R1, #0x2F ; Division ASCII symbol
        IT NE
          BNE exit_hdiv
        ORN R5, #0
        MOV R6, #0x1
        MOV R7, #0x4
        B exit_hdiv
;----------
; UART_enable: Toggles clock for UART
; R2 = Bit standard used
; Destroys: R0 and R1
UART_enable:
        LDR R0, =SYSCTL_BASE
	LDR R1, [R0, #SYSCTL_RCGCUART]
	ORR R1, R2 
	STR R1, [R0, #SYSCTL_RCGCUART]

waitu	LDR R1, [R0, #SYSCTL_PRUART]
	TEQ R1, R2 ; Is the clock up?
	BNE waitu

        BX LR
        
; UART_config: configures the desired UART
; R0 = Base address of the desired UART
; Destroys R1
UART_config:
        LDR R1, [R0, #UART_CTL]
        BIC R1, #0x01 ; Disables UART
        STR R1, [R0, #UART_CTL]

        ; clock = 16MHz, baud rate = 14400 bps
        MOV R1, #69
        STR R1, [R0, #UART_IBRD]
        MOV R1, #28
        STR R1, [R0, #UART_FBRD]
        
        ; 7 bits, 1 stop, even parity, FIFOs disabled, no interrupts
        MOV R1, #0x46
        STR R1, [R0, #UART_LCRH]
        
        ; clock source = system clock
        MOV R1, #0x00
        STR R1, [R0, #UART_CC]
        
        LDR R1, [R0, #UART_CTL]
        ORR R1, #0x01 ; Enables UART
        STR R1, [R0, #UART_CTL]

        BX LR


; GPIO_special: Toggles special functions in UART
; R0 = Base address of the port
; R1 = Bit standard to be activated with special functions
; Destroys R2
GPIO_special:
	LDR R2, [R0, #GPIO_AFSEL]
	ORR R2, R1 ; Configures special bits
	STR R2, [R0, #GPIO_AFSEL]

	LDR R2, [R0, #GPIO_DEN]
	ORR R2, R1 ; Activates digital function
	STR R2, [R0, #GPIO_DEN]

        BX LR

; GPIO_select: Selects special functions on the GPIO port
; R0 = Base address of the port
; R1 = Target's bit mask
; R2 = Bit standard to be activated with special functions
; Destroys R3
GPIO_select:
	LDR R3, [R0, #GPIO_PCTL]
        BIC R3, R1
	ORR R3, R2 ; Selects special bits
	STR R3, [R0, #GPIO_PCTL]

        BX LR
;----------

; GPIO_enable: Toggles clock for the selected port
; R2 = Bit standard of the port
; Destroys R0 and R1
GPIO_enable:
        LDR R0, =SYSCTL_BASE
	LDR R1, [R0, #SYSCTL_RCGCGPIO]
	ORR R1, R2 ; Activates selected ports
	STR R1, [R0, #SYSCTL_RCGCGPIO]

waitg	LDR R1, [R0, #SYSCTL_PRGPIO]
	TEQ R1, R2 ; Is the clock up?
	BNE waitg

        BX LR

; GPIO_digital_output: Activates digital outputs in the selected port
; R0 = Port's base address
; R1 = output bits standard
; Destroys R2
GPIO_digital_output:
	LDR R2, [R0, #GPIO_DIR]
	ORR R2, R1 ; Configures output bits
	STR R2, [R0, #GPIO_DIR]

	LDR R2, [R0, #GPIO_DEN]
	ORR R2, R1 ; Activates digital function
	STR R2, [R0, #GPIO_DEN]

        BX LR

; GPIO_write: Writes in the desired digital outputs
; R0 = Base address
; R1 = Bit mask to be accessed
; R2 = Bits to be written
GPIO_write:
        STR R2, [R0, R1, LSL #2] ; Write bits with access mask
        BX LR

; GPIO_digital_input: Activates digital inputs
; R0 = Base address of the target port
; R1 = Bits to be activated as digital inputs
; Destroys R2
GPIO_digital_input:
	LDR R2, [R0, #GPIO_DIR]
	BIC R2, R1 ; Configures input bits
	STR R2, [R0, #GPIO_DIR]

	LDR R2, [R0, #GPIO_DEN]
	ORR R2, R1 ; Activates digital function
	STR R2, [R0, #GPIO_DEN]

	LDR R2, [R0, #GPIO_PUR]
	ORR R2, R1 ; Activates pull-up resistors
	STR R2, [R0, #GPIO_PUR]

        BX LR

; GPIO_read: Read the desired GPIO port
; R0 = Port's base address
; R1 = Bit mask to be accessed
; R2 = Read bits
GPIO_read:
        LDR R2, [R0, R1, LSL #2] ; Read bits with access mask
        BX LR

; SW_delay: Software delay 
; R0 = Delay's value
; Destroys R0
SW_delay:
        CBZ R0, out_delay
        SUB R0, R0, #1
        B SW_delay        
out_delay:
        BX LR

; LED_write: Writes a binary value in LEDs D1 to D4
; R0 = Value to be written in LEDs (bit 3 to bit 0)
; Destroys R1, R2, R3 and R4
LED_write:
        AND R3, R0, #0010b
        LSR R3, R3, #1
        AND R4, R0, #0001b
        ORR R3, R3, R4, LSL #1 ; D1 and D2 LEDs 
        LDR R1, =GPIO_PORTN_BASE
        MOV R2, #000000011b ; PN1|PN0 mask 
        STR R3, [R1, R2, LSL #2]

        AND R3, R0, #1000b
        LSR R3, R3, #3
        AND R4, R0, #0100b
        ORR R3, R3, R4, LSL #2 ; D3 and D4 LEDs
        LDR R1, =GPIO_PORTF_BASE
        MOV R2, #00010001b ; PF4|PF0 mask
        STR R3, [R1, R2, LSL #2]
        
        BX LR

; Button_read: Reads SW1 and SW2 buttons
; R0 = Read value
; Destroys R1, R2, R3 and R4
Button_read:
        LDR R1, =GPIO_PORTJ_BASE
        MOV R2, #00000011b ; PJ1|PJ0 mask
        LDR R0, [R1, R2, LSL #2]
        
dbc:    MOV R3, #50 ; debounce number
again:  CBZ R3, last
        LDR R4, [R1, R2, LSL #2]
        CMP R0, R4
        MOV R0, R4
        ITE EQ
          SUBEQ R3, R3, #1
          BNE dbc
        B again
last:
        BX LR

; Button_int_conf: Configures SW0 button's interruptions
; Destroys R0, R1 and R2
Button_int_conf:
        MOV R2, #00000001b ; PJO bit
        LDR R1, =GPIO_PORTJ_BASE
        
        LDR R0, [R1, #GPIO_IM]
        BIC R0, R0, R2 ; disable interruptions
        STR R0, [R1, #GPIO_IM]
        
        LDR R0, [R1, #GPIO_IS]
        BIC R0, R0, R2 ; transition interruption
        STR R0, [R1, #GPIO_IS]
        
        LDR R0, [R1, #GPIO_IBE]
        BIC R0, R0, R2 ; only one transition
        STR R0, [R1, #GPIO_IBE]
        
        LDR R0, [R1, #GPIO_IEV]
        BIC R0, R0, R2 ; ladder transition
        STR R0, [R1, #GPIO_IEV]
        
        LDR R0, [R1, #GPIO_ICR]
        ORR R0, R0, R2 ; Cleans 
        STR R0, [R1, #GPIO_ICR]
        
        LDR R0, [R1, #GPIO_IM]
        ORR R0, R0, R2 ; Activates interruptions in GPIO J
        STR R0, [R1, #GPIO_IM]

        MOV R2, #0xE0000000 ; lower priority for IRQ51
        LDR R1, =NVIC_BASE
        
        LDR R0, [R1, #NVIC_PRI12]
        ORR R0, R0, R2 ; define IRQ51's priority at NVIC
        STR R0, [R1, #NVIC_PRI12]

        MOV R2, #10000000000000000000b ; bit 19 = IRQ51
        MOV R0, R2 ; Cleans IRQ51 at NVIC
        STR R0, [R1, #NVIC_UNPEND1]

        LDR R0, [R1, #NVIC_EN1]
        ORR R0, R0, R2 ; activates IRQ51 at NVIC
        STR R0, [R1, #NVIC_EN1]
        
        BX LR

; Button1_int_enable: Configures SW1 button's interruptions
; Destroys R0, R1 e R2
Button1_int_enable:
        MOV R2, #00000001b ; PJ0 bit
        LDR R1, =GPIO_PORTJ_BASE
        
        LDR R0, [R1, #GPIO_IM]
        ORR R0, R0, R2 ; Activates interruptions
        STR R0, [R1, #GPIO_IM]

        BX LR

; Button1_int_disable: Disables SW1 button's interruptions
; Destroys R0, R1 e R2
Button1_int_disable:
        MOV R2, #00000001b ; PJ0 bit
        LDR R1, =GPIO_PORTJ_BASE
        
        LDR R0, [R1, #GPIO_IM]
        BIC R0, R0, R2 ; disables interruptions
        STR R0, [R1, #GPIO_IM]

        BX LR

; Button1_int_clear: Cleans to-be-made interruptions
; Destroys R0 e R1
Button1_int_clear:
        MOV R0, #00000001b ; cleans bit 0
        LDR R1, =GPIO_PORTJ_BASE
        STR R0, [R1, #GPIO_ICR]

        BX LR

        END
