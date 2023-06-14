;######################################
; FCEFyN
; Trabajo Final Electrónica Digital II
; @Autores: Felipe Montero Bruni
;	    Franco Saporito
; @Fecha:   junio 2023
;######################################
	    
;######################################
; PIC16f887
; D0        -> LCD5110 RST
; D1        -> LCD5110 CE
; D2        -> LCD5110 DC
; D3        -> LCD5110 DIN
; D4	    -> LCD5110 CLK
; D5	    -> LCD5110 LED
;######################################

	    LIST        P = 16F887
	    include     <p16f887.inc>
    
; CONFIG1
; __config 0xFFF2
 __CONFIG _CONFIG1, _FOSC_HS & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _CPD_OFF & _BOREN_ON & _IESO_ON & _FCMEN_ON & _LVP_ON
; CONFIG2
; __config 0xFFFF
 __CONFIG _CONFIG2, _BOR4V_BOR40V & _WRT_OFF
 
;######################################
; Asignación de pines conectados 
; a lcd 5110
;######################################
CE          EQU        1
RST         EQU        0
DC_         EQU        2
DIN         EQU        3
CLK         EQU        4
LED         EQU        5

;######################################
; Dirección de variables  utilizadas
; para display lcd 5110
;######################################
            CBLOCK     0x20
BYTE
MODE
CNT
INDEX
CHAR
XPOS
YPOS
TMP

;######################################
; Dirección de variables  utilizadas
; para guardar valores BCD y ASCII
;######################################
UNI
DEC
CEN
MIL

;######################################
; Dirección de variables  utilizadas
; como contador de TMR0 y TMR1
;######################################
TMR0_CNT
TMR1_CNT

;######################################
; Dirección y valores de  variables
; utilizadas en retardo por software
;######################################
C1
C2
C3
	    ENDC
	    
XVAL        EQU        0x10
YVAL        EQU        0x50
ZVAL        EQU        0xFF	    

;######################################
; Dirección de variables para guardar 
; contexto durante interrupción
;######################################
W_TEMP      EQU        0x70
STATUS_TEMP EQU        0x71

;######################################
; Dirección de variables  utilizadas
; para guardar valores de ADC
;######################################
	    CBLOCK     0x72
ADCL
ADCH
;######################################
; Dirección de variables
; utilizadas en rutina multiplicación
;######################################
MUL1
MUL2
RESL
RESH
	    ENDC

;######################################
; Vector de Inicio de programa
;######################################
	    ORG        0x00
	    goto       MAIN
	    
	    ORG        0x04            ; Vector de Interrupción
	    goto       INT_R
	    
	    ORG        0x05
;######################################
; Rutina de servicio de interrupciones
;######################################
INT_R
	    movwf      W_TEMP
	    swapf      STATUS, w
	    movwf      STATUS_TEMP
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    btfsc      INTCON, T0IF
	    call       TMR0_INT
	    btfsc      PIR1, TMR1IF
	    call       TMR1_INT
	    btfsc      PIR1, ADIF
	    call       ADC_INT
	    ;btfsc      INTCON, INTF
	    ;call       RB0_INT
	    swapf      STATUS_TEMP, w
	    movwf      STATUS
	    swapf      W_TEMP, f
	    swapf      W_TEMP, w
	    retfie

;######################################
; Rutina de interrupción de TMR0
;######################################
TMR0_INT
	    decfsz     TMR0_CNT
	    goto       T0_INT_END
	    call       MAIN_SCREEN        ; Refresca vista en display
	    movlw      0x0A               ; Carga 10
	    movwf      TMR0_CNT           ; ... en cuenta de TMR0
T0_INT_END  movlw      0x3C               ; Configura T de TMR0
	    movwf      TMR0               ; ... en 50 ms
	    bcf        INTCON, T0IF       ; Baja TMR0 flag
	    return
	    
;######################################
; Rutina de interrupción de TMR1
;######################################
TMR1_INT
	    decfsz     TMR1_CNT            ; Decrementa el contador
	    goto       T1_INT_END          ; ... si pasó 1 seg,
	    btfsc      PORTC, 4            ; ... togglea el led
	    goto       LED_OFF
	    bsf        PORTC, 4
	    movlw      0x02
	    movwf      TMR1_CNT
	    goto       T1_INT_END
LED_OFF	    bcf        PORTC, 4
	    movlw      0x02
	    movwf      TMR1_CNT
T1_INT_END  clrf       TMR1L               ; Reinicia el TMR1
	    clrf       TMR1H
	    bcf        PIR1, TMR1IF        ; Baja TMR1 flag
	    return	    

;######################################
; Rutina de interrupción de ADC
;######################################
ADC_INT
	    bsf        STATUS, RP0
	    movf       ADRESL, w            ; Guarda byte L
	    movwf      ADCL                 ; ... en ADCL
	    bcf        STATUS, RP0
	    movf       ADRESH, w            ; Guarda byte H
	    movwf      ADCH                 ; ... en ADCH
	    bcf        PIR1, ADIF           ; Baja ADC flag
	    bsf        ADCON0, GO           ; inicia nueva conversión
	    return
	       
;######################################
; Rutina de multiplicación 8x8 -> 16.
; Multiplica MUL1 y MUL2.
; Guarda resultado en RESL y RESH
;######################################
MULT
            clrf       RESL          ; Limpia RESL
	    clrf       RESH          ; Limpia RESH
	    movf       MUL1, w       ; Mueve MUL1 a w
MULT_LOOP   addwf      RESL, f       ; Suma MUL1 (w) a RESL, MUL2 veces
	    btfsc      STATUS, 0     ; Si hay overflow en la suma
	    incf       RESH, f       ; ... incrementa RESH
	    decfsz     MUL2, f       ; Decrementa MUL2, si no es 0
	    goto       MULT_LOOP     ; Vuelve a repetir
	    return                   ; Si es 0, terminó la multiplicación

;######################################
; Rutina para convertir valor de ADC
; a BCD. Guarda resultado en
; UNI, DEC, CEN y MIL
;######################################
TO_BCD
	    clrf       DEC        ; Inicio registros DEC = 0,
	    clrf       CEN        ; ... CEN = 0
	    clrf       MIL        ; ... y MIL = 0
	    movf       ADCL, w    ; Cargo ADCL 
	    movwf      UNI        ; ... en UNI
INI_BCD	    movlw      0x0A       ; resto 10
	    subwf      UNI, w     ; ... a UNI
	    btfsc      STATUS, 0  ; Si UNI > 10                    
	    goto       CONTINUE   ; Continuo
	    movf       ADCH, f    ; Si UNI < 10, reviso ADCH
	    btfsc      STATUS, 2  ; Si es 0
	    return                ; return, BCD == UNI
	    decf       ADCH, f    ; Si no, decremento ADCH
CONTINUE    movlw      0x0A
	    subwf      UNI, w
	    movwf      UNI        ; else,
	    incf       DEC, f     ; ... DEC++
	    movlw      0x0A       ; sub
	    subwf      DEC, w     ; ... DEC - 10
	    btfss      STATUS, 0  ; if DEC < 0
	    goto       INI_BCD    ; goto next UNI
	    incf       CEN, f     ; else CEN++
	    clrf       DEC        ;
	    
	    movlw      0x0A       ; sub
	    subwf      CEN, w     ; ... DEC - 10
	    btfss      STATUS, 0  ; if DEC < 0
	    goto       INI_BCD    ; goto next UNI
	    incf       MIL, f     ; else CEN++
	    clrf       CEN        ;
	    
	    goto       INI_BCD    ; start over

;######################################
; Rutina para convertir valor de ADC
; BCD en ASCII. Guarda resultado en
; UNI, DEC, CEN y MIL
;######################################
TO_ASCII
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      0x30
	    addwf      UNI, f
	    addwf      DEC, f
	    addwf      CEN, f
	    addwf      MIL, f
	    return
	    
;######################################
; Rutina de retardo por software
;######################################
RETARDO 
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      ZVAL            ; ti
            movwf      C3              ; ti
CUENTA3     movlw      YVAL            ; ti*z
	    movwf      C2              ; ti*z            
CUENTA2     movlw      XVAL            ; ((ti*y               )*z
	    movwf      C1              ; ((ti*y               )*z
CUENTA1	    decfsz     C1,1            ; ((ti*(x-1) + 2*ti )*y)*z
	    goto       CUENTA1         ; ((2*ti*(x-1)      )*y)*z
	    decfsz     C2,1            ; (ti*(y-1) + 2*ti     )*z
	    goto       CUENTA2         ; (2*ti*(y-1)          )*z
	    decfsz     C3,1            ; ti*(z-1) + 2*ti
	    goto       CUENTA3         ; 2*ti*(z-1)
	    return

;######################################
; Rutina para escribir en PCD8544, 
; driver del display lcd 5110.
; Envía el byte almacenado en W.
; MODE controla si es un comando al
; PCD8544 o escritura en el display
;######################################
WRITE_DISP
	    bcf        STATUS, RP1
	    bcf        STATUS, RP1
	    movwf      BYTE
	    bcf        PORTD, CE    ; CE low
	    btfss      MODE, 0      ; Si MODE == 0x00
	    goto       CMD_MODE     ; ... envia un comando al dispplay
DATA_MODE   bsf        PORTD, DC_   ; ... si MODE == 0x01 manda un dato
	    goto       SHIFTOUT     ;
CMD_MODE    bcf        PORTD, DC_
SHIFTOUT    movlw      0x08         ; Inicio contador
	    movwf      INDEX        ; ... en 8
WRT_LOOP    btfss      BYTE, 7      ; Si MSB de BYTE es 1
	    goto       SEND_0  ;
SEND_1	    bsf        PORTD, DIN   ; ... manda un 1
	    goto       TOGGLE_CLK
SEND_0	    bcf        PORTD, DIN   ; ... si no, manda un 0
TOGGLE_CLK  bsf        PORTD, CLK   ; CLK high
	    bcf        PORTD, CLK   ; CLK low
	    rlf        BYTE, f      ; Shift byte a la izquierda
	    decfsz     INDEX, f     ; Decremento el contador
	    goto       WRT_LOOP     ; ... si es 0, salgo del shiftout loop
	    bsf        PORTD, CE    ; CE high
	    return

;######################################
; Rutina para posicionar el cursor
; del display lcd 5110
;######################################
SET_CURSOR
	    clrf       MODE
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      0x80          ; Genera byte de posición
	    addwf      XPOS, w       ; ... de columna
	    call       WRITE_DISP    ; Envía posicion
	    movlw      0x40          ; Genera byte de posición
	    addwf      YPOS, w       ; ... de fila
	    call       WRITE_DISP    ; Envía posicion
	    return

;######################################
; Rutina para limpiar el contenido
; del display lcd 5110
;######################################
CLEAR_DISP
	    clrf       MODE
	    clrf       XPOS           ; columna 0
	    clrf       YPOS           ; fila 0
	    call       SET_CURSOR
	    bsf        MODE, 0        ; Modo escritura
	    movlw      0xFF
	    movwf      CNT    
CLR_LOOP1   movlw      0x00           ; Limpia las primeras 255 posiciones
	    call       WRITE_DISP
	    decfsz     CNT, f
	    goto       CLR_LOOP1
	    movlw      0xF8
	    movwf      CNT
CLR_LOOP2   movlw      0x00           ; Limpia las 249 restantes. 255+249 = 504
	    call       WRITE_DISP
	    decfsz     CNT, f
	    goto       CLR_LOOP2
	    return

;######################################
; Rutina de inicialización del
; Display lcd 5110
;######################################
INIT_DISP
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    bcf        PORTD, CE       ; CE low
	    bcf        PORTD, RST      ; Pulso de reset
	    call       RETARDO         ; Delay de un segundo
	    bsf        PORTD, RST      ; Fin reset
	    clrf       MODE
	    movlw      0x21            ; Set de instrucciones extendido
	    call       WRITE_DISP
	    movlw      0xB2            ; Vop
	    call       WRITE_DISP
	    movlw      0x20            ; Set de instrucciones normal
	    call       WRITE_DISP
	    movlw      0x0C            ; Modo Normal
	    call       WRITE_DISP
	    call       CLEAR_DISP      ; Limpia RAM
	    return

;######################################
; Rutina para escribir un caracter
; ASCII en display lcd 5110
;######################################
WRITE_CHAR
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    bsf        MODE, 0
	    movwf      CHAR
	    movlw      0x20
	    subwf      CHAR, f
	    movf       CHAR, w
	    movwf      MUL1
	    movlw      0x05
	    movwf      MUL2
	    call       MULT
	    clrf       CNT
CHAR_LOOP   movlw      0x08
	    movwf      PCLATH
	    movf       RESL, w
	    addwf      CNT, w
	    call       TABLE_CALL
            call       WRITE_DISP
	    incf       CNT, f
	    movlw      0x05
	    subwf      CNT, w
	    btfss      STATUS, 2
	    goto       CHAR_LOOP
	    return

;######################################
; Pantalla principal.
;######################################
MAIN_SCREEN
	    movlw      0x0F
	    movwf      XPOS
	    clrf       YPOS
	    call       SET_CURSOR
	    movlw      0x44           ; D
	    call       WRITE_CHAR
	    movlw      0x69           ; i
	    call       WRITE_CHAR
	    movlw      0x67           ; g
	    call       WRITE_CHAR
	    movlw      0x69           ; i
	    call       WRITE_CHAR
	    movlw      0x74           ; t
	    call       WRITE_CHAR
	    movlw      0x61           ; a
	    call       WRITE_CHAR
	    movlw      0x6C           ; l
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    movlw      0x49           ; I
	    call       WRITE_CHAR
	    movlw      0x49           ; I
	    call       WRITE_CHAR
	    
	    movlw      0x01
	    movwf      XPOS
	    movlw      0x02
	    movwf      YPOS
	    call       SET_CURSOR
	    movlw      0x41           ; A
	    call       WRITE_CHAR
	    movlw      0x44           ; D
	    call       WRITE_CHAR
	    movlw      0x43           ; C
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    movlw      0x3D           ; =
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    
	    call       TO_BCD
	    call       TO_ASCII
	    
	    movf       MIL, w
	    call       WRITE_CHAR
	    movf       CEN, w
	    call       WRITE_CHAR
	    movf       DEC, w
	    call       WRITE_CHAR
	    movf       UNI, w
	    call       WRITE_CHAR
	    
	    
	    movlw      0x01
	    movwf      XPOS
	    movlw      0x03
	    movwf      YPOS
	    call       SET_CURSOR
	    movlw      0x56           ; V
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    movlw      0x3D           ; =
	    call       WRITE_CHAR
	    movlw      0x20           ; 
	    call       WRITE_CHAR
	    movlw      0x30           ; 0
	    call       WRITE_CHAR
	    
	    return
	    
;######################################
; Inicialización de clock.
;######################################
INIT_CLOCK
	    bsf        STATUS, RP0
	    movlw      0x61
	    movwf      OSCCON
	    return
	    
;######################################
; Inicialización de puertos.
;######################################
INIT_PORTS
	    bsf        STATUS, RP0
	    movlw      0x01
	    movwf      TRISA           ; A0 input, resto output
	    clrf       TRISC           ; Puerto C output
	    clrf       TRISD           ; Puerto D output
	    clrf       TRISE           ; Puerto E output
	    bsf        STATUS, RP1
	    movlw      0x01
	    movwf      ANSEL           ; A0 analog input
	    clrf       ANSELH
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    clrf       PORTA           ; Puerto A en bajo
	    clrf       PORTC           ; Puerto C en bajo
	    movlw      0xFF
	    movwf      PORTD           ; Puerto D en alto
	    clrf       PORTE           ; Puerto E en bajo
	    return

;######################################
; Inicialización de ADC.
;######################################
INIT_ADC
	    bsf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      0x80             ; ADC justificado a la derecha,
	    movwf      ADCON1           ; ... Vdd y Vss referencia
	    bcf        STATUS, RP0
	    movlw      0xC0
	    movwf      ADCON0           ; clock interno
	    bsf        ADCON0, ADON     ; ADC enable
	    call       RETARDO          ; Delay de inicialización
	    bsf        ADCON0, GO       ; Start ADC
	    return

;######################################
; Inicialización de TMR0.
;######################################
INIT_TMR0
	    bsf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      0x07
	    addwf      OPTION_REG, f     ; PS a TMR0, PS = 256
	    bcf        STATUS, RP0
	    movlw      0x0A              ; Carga 10
	    movwf      TMR0_CNT          ; ... en contador de TMR0
	    movlw      0x3C
	    movwf      TMR0              ; TMR0 = 3C, T = 50 ms
	    return

;######################################
; Inicialización de TMR1.
;######################################
INIT_TMR1
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      0x02
	    movwf      TMR1_CNT
	    clrf       TMR1L
	    clrf       TMR1H
	    movlw      0x31
	    movwf      T1CON            ; PS = 8, TMR1ON
	    return
	    
;######################################
; Inicialización de interrupciones.
;######################################
INIT_INT
	    bsf        STATUS, RP0
	    bcf        STATUS, RP1
	    movlw      0x41
	    movwf      PIE1              ; ADIE y TMR1IE
	    movlw      0xE8
	    movwf      INTCON            ; GIE, PEIE, T0IE, RBIE
	    return
	    
;######################################
; Rutina principal.
; Programa comienza aquí.
;######################################
MAIN
	    call       INIT_CLOCK
	    call       INIT_PORTS
	    call       INIT_DISP
	    call       INIT_ADC
	    call       INIT_TMR0
	    call       INIT_TMR1
	    call       INIT_INT
	    
	    bcf        STATUS, RP0
	    bcf        STATUS, RP1
	    bsf        PORTC, 4
            nop
	    goto       $

	    
	    ORG        0x0800             ; Inicio de Página 1
;######################################
; Rutina de llamado de tabla ASCII.
; Maneja cambio de página  y cambio 
; de PCLATH en llamada.
;######################################
TABLE_CALL
	    movwf      TMP
	    movlw      0xEB
	    subwf      TMP, w
	    btfsc      STATUS, C
	    goto       TABLE_H
	    btfsc      RESH, 0
	    goto       TABLE_H
	    movf       TMP, w
	    call       ASCII_TABLE_L
	    movwf      TMP
	    goto       END_CALL
TABLE_H	    movwf      TMP
	    movlw      0x09
	    movwf      PCLATH
	    movf       TMP, w
	    goto       CLL_TABLE_H
END_CALL    movlw      0x00
	    movwf      PCLATH
	    movf       TMP, w
	    return
	    
;######################################
; LUT de caracteres ASCII
; Parte inferior.
;######################################
ASCII_TABLE_L                      
	    addwf      PCL, f
	    retlw      0x00       ; 0x20, space
	    retlw      0x00
	    retlw      0x00
	    retlw      0x00
	    retlw      0x00
	    
	    retlw      0x00       ; 0x21, !
	    retlw      0x00
	    retlw      0x5F
	    retlw      0x00
	    retlw      0x00 	    
	    
	    retlw      0x00       ; 0x22, "
	    retlw      0x07
	    retlw      0x00
	    retlw      0x07
	    retlw      0x00 	    	    
	    
	    retlw      0x14       ; 0x23, #
	    retlw      0x7F
	    retlw      0x14
	    retlw      0x7F
	    retlw      0x14 	    	    
	    
	    retlw      0x24       ; 0x24, $
	    retlw      0x2A
	    retlw      0x7F
	    retlw      0x2A
	    retlw      0x12	    
	    
	    retlw      0x23       ; 0x25, %
	    retlw      0x12
	    retlw      0x08
	    retlw      0x64
	    retlw      0x62
	    
	    retlw      0x36       ; 0x26, &
	    retlw      0x49
	    retlw      0x55
	    retlw      0x22
	    retlw      0x50
	    
	    retlw      0x00       ; 0x27, '
	    retlw      0x05
	    retlw      0x03
	    retlw      0x00
	    retlw      0x00    	    
	    
	    retlw      0x00       ; 0x28, (
	    retlw      0x1C
	    retlw      0x22
	    retlw      0x41
	    retlw      0x00	    
	    
	    retlw      0x00       ; 0x29, )
	    retlw      0x41
	    retlw      0x22
	    retlw      0x1C
	    retlw      0x00	    
	    
	    retlw      0x14       ; 0x2A, *
	    retlw      0x08
	    retlw      0x3E
	    retlw      0x08
	    retlw      0x14	    
	    
	    retlw      0x08       ; 0x2B, +
	    retlw      0x08
	    retlw      0x3E
	    retlw      0x08
	    retlw      0x08	    
	    
	    retlw      0x00       ; 0x2C, ,
	    retlw      0x50
	    retlw      0x30
	    retlw      0x00
	    retlw      0x00
	    
	    retlw      0x08       ; 0x2D, -
	    retlw      0x08
	    retlw      0x08
	    retlw      0x08
	    retlw      0x08	        
	    
	    retlw      0x00       ; 0x2E, .
	    retlw      0x60
	    retlw      0x60
	    retlw      0x00
	    retlw      0x00
	    
	    retlw      0x20       ; 0x2F, /
	    retlw      0x10
	    retlw      0x08
	    retlw      0x04
	    retlw      0x02
	    
	    retlw      0x3E       ; 0x30, 0
	    retlw      0x51
	    retlw      0x49
	    retlw      0x45
	    retlw      0x3E    
	    
	    retlw      0x00       ; 0x31, 1
	    retlw      0x42
	    retlw      0x7F
	    retlw      0x40
	    retlw      0x00  
	    
	    retlw      0x42       ; 0x32, 2
	    retlw      0x61
	    retlw      0x51
	    retlw      0x49
	    retlw      0x46	    
	    
	    retlw      0x21       ; 0x33, 3
	    retlw      0x41
	    retlw      0x45
	    retlw      0x4B
	    retlw      0x31   
	    
	    retlw      0x18       ; 0x34, 4
	    retlw      0x14
	    retlw      0x12
	    retlw      0x7F
	    retlw      0x10     
	    
	    retlw      0x27       ; 0x35, 5
	    retlw      0x45
	    retlw      0x45
	    retlw      0x45
	    retlw      0x39 
	    
	    retlw      0x3C       ; 0x36, 6
	    retlw      0x4A
	    retlw      0x49
	    retlw      0x49
	    retlw      0x30     
	    
	    retlw      0x01       ; 0x37, 7
	    retlw      0x71
	    retlw      0x09
	    retlw      0x05
	    retlw      0x03 
	    
	    retlw      0x36       ; 0x38, 8
	    retlw      0x49
	    retlw      0x49
	    retlw      0x49
	    retlw      0x36
	    
	    retlw      0x06       ; 0x39, 9
	    retlw      0x49
	    retlw      0x49
	    retlw      0x29
	    retlw      0x1E
	    
	    retlw      0x00       ; 0x3A, :
	    retlw      0x36
	    retlw      0x36
	    retlw      0x00
	    retlw      0x00 
	    
	    retlw      0x00       ; 0x3B, ;
	    retlw      0x56
	    retlw      0x36
	    retlw      0x00
	    retlw      0x00 
	    
	    retlw      0x08       ; 0x3C, <
	    retlw      0x14
	    retlw      0x22
	    retlw      0x41
	    retlw      0x00
	    
	    retlw      0x14       ; 0x3D, =
	    retlw      0x14
	    retlw      0x14
	    retlw      0x14
	    retlw      0x14
	    
	    retlw      0x00       ; 0x3E, >
	    retlw      0x41
	    retlw      0x22
	    retlw      0x14
	    retlw      0x08 
	    
	    retlw      0x02       ; 0x3F, ?
	    retlw      0x01
	    retlw      0x51
	    retlw      0x09
	    retlw      0x06 
	    
	    retlw      0x32       ; 0x40, @
	    retlw      0x49
	    retlw      0x59
	    retlw      0x51
	    retlw      0x3E 
	    
	    retlw      0x7E       ; 0x41, A
	    retlw      0x11
	    retlw      0x11
	    retlw      0x11
	    retlw      0x7E
	    
	    retlw      0x7F       ; 0x42, B
	    retlw      0x49
	    retlw      0x49
	    retlw      0x49
	    retlw      0x36
	    
	    retlw      0x3E       ; 0x43, C
	    retlw      0x41
	    retlw      0x41
	    retlw      0x41
	    retlw      0x22 
	    
	    retlw      0x7F       ; 0x44, D
	    retlw      0x41
	    retlw      0x41
	    retlw      0x22
	    retlw      0x1C
	    
	    retlw      0x7F       ; 0x45, E
	    retlw      0x49
	    retlw      0x49
	    retlw      0x49
	    retlw      0x41
	    
	    retlw      0x7F       ; 0x46, F
	    retlw      0x09
	    retlw      0x09
	    retlw      0x09
	    retlw      0x01
	    
	    retlw      0x3E       ; 0x47, G
	    retlw      0x41
	    retlw      0x49
	    retlw      0x49
	    retlw      0x7A
	    
	    retlw      0x7F       ; 0x48, H
	    retlw      0x08
	    retlw      0x08
	    retlw      0x08
	    retlw      0x7F
	    
	    retlw      0x00       ; 0x49, I
            retlw      0x41
            retlw      0x7F
            retlw      0x41
            retlw      0x00

            retlw      0x20       ; 0x4A, J
            retlw      0x40
            retlw      0x41
            retlw      0x3F
            retlw      0x01

            retlw      0x7F       ; 0x4B, K
            retlw      0x08
            retlw      0x14
            retlw      0x22
            retlw      0x41

            retlw      0x7F       ; 0x4C, L
            retlw      0x40
            retlw      0x40
            retlw      0x40
            retlw      0x40

            retlw      0x7F       ; 0x4D, M
            retlw      0x02
            retlw      0x0C
            retlw      0x02
            retlw      0x7F

            retlw      0x7F       ; 0x4E, N
            retlw      0x04
            retlw      0x08
            retlw      0x10
            retlw      0x7F

	    ORG        0x0900
CLL_TABLE_H movf       TMP, w
	    call       ASCII_TABLE_H
	    movwf      TMP
	    movlw      0x08
	    movwf      PCLATH
	    goto       END_CALL
	    
;######################################
; LUT de caracteres ASCII
; Parte superior.
;######################################
ASCII_TABLE_H
	    addwf      PCL, f
	    retlw      0x3E       ; 0x4F, O
            retlw      0x41
            retlw      0x41
            retlw      0x41
            retlw      0x3E
	    
	    retlw      0x7F       ; 0x50, P
            retlw      0x09
            retlw      0x09
            retlw      0x09
            retlw      0x06
            
	    retlw      0x3E       ; 0x51, Q
            retlw      0x41
            retlw      0x51
            retlw      0x21
            retlw      0x5E

            retlw      0x7F       ; 0x52, R
            retlw      0x09
            retlw      0x19
            retlw      0x29
            retlw      0x46

            retlw      0x46       ; 0x53, S
            retlw      0x49
            retlw      0x49
            retlw      0x49
            retlw      0x31

            retlw      0x01       ; 0x54, T
            retlw      0x01
            retlw      0x7F
            retlw      0x01
            retlw      0x01

            retlw      0x3F       ; 0x55, U
            retlw      0x40
            retlw      0x40
            retlw      0x40
            retlw      0x3F

            retlw      0x1F       ; 0x56, V
            retlw      0x20
            retlw      0x40
            retlw      0x20
            retlw      0x1F

            retlw      0x3F       ; 0x57, W
            retlw      0x40
            retlw      0x38
            retlw      0x40
            retlw      0x3F

            retlw      0x63       ; 0x58, X
            retlw      0x14
            retlw      0x08
            retlw      0x14
            retlw      0x63

            retlw      0x07       ; 0x59, Y
            retlw      0x08
            retlw      0x70
            retlw      0x08
            retlw      0x07

            retlw      0x61       ; 0x5A, Z
            retlw      0x51
            retlw      0x49
            retlw      0x45
            retlw      0x43

            retlw      0x00       ; 0x5B, [
            retlw      0x7F
            retlw      0x41
            retlw      0x41
            retlw      0x00

            retlw      0x55       ; 0x5C, backslash
            retlw      0x2A
            retlw      0x55
            retlw      0x2A
            retlw      0x55

            retlw      0x00       ; 0x5D, ]
            retlw      0x41
            retlw      0x41
            retlw      0x7F
            retlw      0x00

            retlw      0x04       ; 0x5E, ^
            retlw      0x02
            retlw      0x01
            retlw      0x02
            retlw      0x04

            retlw      0x40       ; 0x5F, _
            retlw      0x40
            retlw      0x40
            retlw      0x40
            retlw      0x40

            retlw      0x00       ; 0x60, `
            retlw      0x01
            retlw      0x02
            retlw      0x04
            retlw      0x00

            retlw      0x20       ; 0x61, a
            retlw      0x54
            retlw      0x54
            retlw      0x54
            retlw      0x78

            retlw      0x7F       ; 0x62, b
            retlw      0x48
            retlw      0x44
            retlw      0x44
            retlw      0x38

            retlw      0x38       ; 0x63, c
            retlw      0x44
            retlw      0x44
            retlw      0x44
            retlw      0x20

            retlw      0x38       ; 0x64, d
            retlw      0x44
            retlw      0x44
            retlw      0x48
            retlw      0x7F

            retlw      0x38       ; 0x65, e
            retlw      0x54
            retlw      0x54
            retlw      0x54
            retlw      0x18

            retlw      0x08       ; 0x66, f
            retlw      0x7E
            retlw      0x09
            retlw      0x01
            retlw      0x02

            retlw      0x0C       ; 0x67, g
            retlw      0x52
            retlw      0x52
            retlw      0x52
            retlw      0x3E

            retlw      0x7F       ; 0x68, h
            retlw      0x08
            retlw      0x04
            retlw      0x04
            retlw      0x78

            retlw      0x00       ; 0x69, i
            retlw      0x44
            retlw      0x7D
            retlw      0x40
            retlw      0x00
	    
            retlw      0x20       ; 0x6A, j
            retlw      0x40
            retlw      0x44
            retlw      0x3D
            retlw      0x00

            retlw      0x7F       ; 0x6B, k
            retlw      0x10
            retlw      0x28
            retlw      0x44
            retlw      0x00

            retlw      0x00       ; 0x6C, l
            retlw      0x41
            retlw      0x7F
            retlw      0x40
            retlw      0x00

            retlw      0x7C       ; 0x6D, m
            retlw      0x04
            retlw      0x18
            retlw      0x04
            retlw      0x78

            retlw      0x7C       ; 0x6E, n
            retlw      0x08
            retlw      0x04
            retlw      0x04
            retlw      0x78

            retlw      0x38       ; 0x6F, o
            retlw      0x44
            retlw      0x44
            retlw      0x44
            retlw      0x38
	    
	    retlw      0x7C       ; 0x70, p
	    retlw      0x14
	    retlw      0x14
	    retlw      0x14
	    retlw      0x08
	    
            retlw      0x08       ; 0x71, q
            retlw      0x14
            retlw      0x14
            retlw      0x18
            retlw      0x7C

            retlw      0x7C       ; 0x72, r
            retlw      0x08
            retlw      0x04
            retlw      0x04
            retlw      0x08

            retlw      0x48       ; 0x73, s
            retlw      0x54
            retlw      0x54
            retlw      0x54
            retlw      0x20

            retlw      0x04       ; 0x74, t
            retlw      0x3F
            retlw      0x44
            retlw      0x40
            retlw      0x20

            retlw      0x3C       ; 0x75, u
            retlw      0x40
            retlw      0x40
            retlw      0x20
            retlw      0x7C

            retlw      0x1C       ; 0x76, v
            retlw      0x20
            retlw      0x40
            retlw      0x20
            retlw      0x1C

            retlw      0x3C       ; 0x77, w
            retlw      0x40
            retlw      0x30
            retlw      0x40
            retlw      0x3C

            retlw      0x44       ; 0x78, x
            retlw      0x28
            retlw      0x10
            retlw      0x28
            retlw      0x44
	    
            retlw      0x0C       ; 0x79, y
            retlw      0x50
            retlw      0x50
            retlw      0x50
            retlw      0x3C

            retlw      0x44       ; 0x7A, z
            retlw      0x64
            retlw      0x54
            retlw      0x4C
            retlw      0x44

            retlw      0x00       ; 0x7B, {
            retlw      0x08
            retlw      0x36
            retlw      0x41
            retlw      0x00

            retlw      0x00       ; 0x7C, |
            retlw      0x00
            retlw      0x7F
            retlw      0x00
            retlw      0x00

            retlw      0x00       ; 0x7D, }
            retlw      0x41
            retlw      0x36
            retlw      0x08
            retlw      0x00

            retlw      0x10       ; 0x7E, ~
            retlw      0x08
            retlw      0x08
            retlw      0x10
            retlw      0x08

            retlw      0x78       ; 0x7F, DEL
            retlw      0x46
            retlw      0x41
            retlw      0x46
            retlw      0x78

	    END