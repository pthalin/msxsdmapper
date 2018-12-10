; Projeto MSX SD Mapper

; Copyright (c) 2014 Fabio Belavenuto
; Enhanced by FRS

; This documentation describes Open Hardware and is licensed under the CERN OHL v. 1.1.
; You may redistribute and modify this documentation under the terms of the
; CERN OHL v.1.1. (http://ohwr.org/cernohl). This documentation is distributed
; WITHOUT ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING OF MERCHANTABILITY,
; SATISFACTORY QUALITY AND FITNESS FOR A PARTICULAR PURPOSE.
; Please see the CERN OHL v.1.1 for applicable conditions

; Technical info:
; 7B00h~7EFFh	: SPI data transfer window (read/write)
; 7F00h		: Interface status and card select register (read/write)
;	<read>
;	If no SD card is selected:
;	    b7-b2 : always 0
;	    b1 : SW1 status (Driver selection)
;	    b0 : SW0 status. 0=RAM disabled, 1=RAM enabled
;	If any SD card is selected:
;	    b7-b3 : always 0
;	    b2 : 1=Write protecton enabled for SD card-slot selected
;	    b1 : 0=SD card present in the selected card-slot
;	    b0 : 1=SD Card on slot selected changed since last read
;	<write>
;	    b0 : SD card slot-0 chip-select (1=selected)
;	    b1 : SD card slot-1 chip-select (1=selected)

; 7F02h		: 8-bit timer (97.65625 KHz frequency, 10.24uS resolution) (read/write)
; When a value is written, the timer will decrease it until it reaches zero

	output	"driver.bin"

;-----------------------------------------------------------------------------
;
; Driver configuration constants
;

; DEFINE HASMEGARAM	; Driver for an SD-Mapper with MegaRAM
; DEFINE TURBOINIT	; Disable for the interfaces with the CPLD firmware turbo bug 
; DEFINE DEBUG		; Set for debugging output

;Driver type:
;   0 for drive-based
;   1 for device-based

DRV_TYPE	equ	1

;Hot-plug devices support (device-based drivers only):
;   0 for no hot-plug support
;   1 for hot-plug support

DRV_HOTPLUG	equ	1


;Driver version

VER_MAIN	equ	1
VER_SEC		equ	0
VER_REV		equ	10

;-----------------------------------------------------------------------------
; SPI addresses. Check the Technical info above for the bit contents

SPIDATA		= $7B00
SPICTRL		= $7FF0
SPISTATUS	= $7FF0
TIMERREG	= $7FF1

; Interface status flags
IF_RAM		= 0		; 1=Interface RAM is enabled
IF_DRVER	= 1		; RAM mode: 0=MegaRAM, 1=MemoryMapper
IF_M_RAM	= (1 shl IF_RAM)		; bitmask for IF_RAM
IF_M_DRVER	= (1 shl IF_DRVER)		; bitmask for IF_DRVER

; card slot status flags
SD_DSKCHG	= 0		; SD card changed since last status check
SD_PRESENT	= 1		; SD card present
SD_WRTPROT	= 2		; SD card is write protected
SD_M_DSKCHG	= (1 shl SD_DSKCHG)		; bitmask for SD_DSKCHG
SD_M_PRESENT	= (1 shl SD_PRESENT)		; bitmask for SD_PRESENT
SD_M_WRTPROT	= (1 shl SD_WRTPROT)		; bitmask for SD_WRTPROT

; SPI commands: 
CMD0	= 0  | $40
CMD1	= 1  | $40
CMD8	= 8  | $40
CMD9	= 9  | $40
CMD10	= 10 | $40
CMD12	= 12 | $40
CMD16	= 16 | $40
CMD17	= 17 | $40
CMD18	= 18 | $40
CMD24	= 24 | $40
CMD25	= 25 | $40
CMD55	= 55 | $40
CMD58	= 58 | $40
ACMD23	= 23 | $40
ACMD41	= 41 | $40

; Work area variables 
 STRUCT WRKAREA
BCSD 		ds 16	; Card Specific Data
BCID1		ds 16	; Card-ID of card1
BCID2		ds 16	; Card-ID of card2
NUMSD		db 	; Currently selected card: 1 or 2 
CARDFLAGS	db 	; Flags that indicate card-change or card error 
NUMBLOCKS	db 	; Number of blocks in multi-block operations 
BLOCKS1		ds 3	; 3 bytes. Size of card1, in blocks.
BLOCKS2		ds 3	; 3 bytes. Size of card2, in blocks.
TEMP		db	; Temporary data
TRLDIR		ds 8	; R800 data transfer helper 
 ENDS


;-----------------------------------------------------------------------------
;
; Standard BIOS and work area entries
CALSLT	= $001C		; Call routine in any slot
CALLF	= $0030		; Call routine in any slot
INITXT	= $006C		; Inicializa SCREEN0
CHSNS	= $009C		; Sense keyboard buffer for character
CHGET	= $009F		; Get character from keyboard buffer
CHPUT	= $00A2		; A=char
CLS	= $00C3		; Chamar com A=0
ERAFNK	= $00CC		; Erase function key display
SNSMAT	= $0141		; Read row of keyboard matrix
KILBUF	= $0156		; Clear keyboard buffer
EXTROM	= $015F
CHGCPU	= $0180		; Change the turbo mode
GETCPU	= $0183		; Get the turbo mode

; subROM functions
SDFSCR	= $0185
REDCLK	= $01F5


; System variables
MSXVER	= $002D
LINL40	= $F3AE		; Width
LINLEN	= $F3B0
INTFLG	= $FC9B
SCRMOD	= $FCAF
EXPTBL	 =$FCC1


;-----------------------------------------------------------------------------


	org		$4000

	ds		256, $FF		; 256 dummy bytes

DRV_START:

;-----------------------------------------------------------------------------
;
; Miscellaneous constants
;

;This is a 2 byte buffer to store the address of code to be executed.
;It is used by some of the kernel page 0 routines.

CODE_ADD:	equ	0F84Ch


;-----------------------------------------------------------------------------
;
; Error codes for DEV_RW
;

ENCOMP	equ	0FFh
EWRERR	equ	0FEh
EDISK	equ	0FDh
ENRDY	equ	0FCh
EDATA	equ	0FAh
ERNF	equ	0F9h
EWPROT	equ	0F8h
EUFORM	equ	0F7h
ESEEK	equ	0F3h
EIFORM	equ	0F0h
EIDEVL	equ	0B5h
EIPARM	equ	08Bh

;-----------------------------------------------------------------------------
;
; Macros
;

 MACRO BYTE2STR value

 IF value > 99
	db	((value / 100) % 10)+$30
 ENDIF
 IF value > 9
	db	((value / 10) % 10)+$30
 ENDIF
	db	(value % 10)+$30

 ENDM


;-----------------------------------------------------------------------------
;
; Routines and information available on kernel page 0
;

;* Get in A the current slot for page 1. Corrupts F.
;  Must be called by using CALBNK to bank 0:
;    xor a
;    ld ix,GSLOT1
;    call CALBNK

GSLOT1	equ	402Dh


;* This routine reads a byte from another bank.
;  Must be called by using CALBNK to the desired bank,
;  passing the address to be read in HL:
;    ld a,<bank number>
;    ld hl,<byte address>
;    ld ix,RDBANK
;    call CALBNK

RDBANK	equ	403Ch


;* This routine temporarily switches kernel main bank
;  (usually bank 0, but will be 3 when running in MSX-DOS 1 mode),
;  then invokes the routine whose address is at (CODE_ADD).
;  It is necessary to use this routine to invoke CALBAS
;  (so that kernel bank is correct in case of BASIC error)
;  and to invoke DOS functions via F37Dh hook.
;
;  Input:  Address of code to invoke in (CODE_ADD).
;          AF, BC, DE, HL, IX, IY passed to the called routine.
;  Output: AF, BC, DE, HL, IX, IY returned from the called routine.

CALLB0	equ	403Fh


;* Call a routine in another bank.
;  Must be used if the driver spawns across more than one bank.
;
;  Input:  A = bank number
;          IX = routine address
;          AF' = AF for the routine
;          HL' = Ix for the routine
;          BC, DE, HL, IY = input for the routine
;  Output: AF, BC, DE, HL, IX, IY returned from the called routine.

CALBNK	equ	4042h


;* Get in IX the address of the SLTWRK entry for the slot passed in A,
;  which will in turn contain a pointer to the allocated page 3
;  work area for that slot (0 if no work area was allocated).
;  If A=0, then it uses the slot currently switched in page 1.
;  Returns A=current slot for page 1, if A=0 was passed.
;  Corrupts F.
;  Must be called by using CALBNK to bank 0:
;    ld a,<slot number> (xor a for current page 1 slot)
;    ex af,af'
;    xor a
;    ld ix,GWORK
;    call CALBNK

GWORK	equ	4045h


;* This address contains one byte that tells how many banks
;  form the Nextor kernel (or alternatively, the first bank
;  number of the driver).

K_SIZE	equ	40FEh


;* This address contains one byte with the current bank number.

CUR_BANK	equ	40FFh


;-----------------------------------------------------------------------------
;
; Built-in format choice strings
;

NULL_MSG  equ     781Fh	;Null string (disk can't be formatted)
SING_DBL  equ     7820h ;"1-Single side / 2-Double side"


;-----------------------------------------------------------------------------
;
; Driver signature
;
	db	"NEXTOR_DRIVER",0


;-----------------------------------------------------------------------------
;
; Driver flags:
;    bit 0: 0 for drive-based, 1 for device-based
;    bit 1: 1 for hot-plug devices supported (device-based drivers only)
;    bit 2: 1 if the driver implements the DRV_CONFIG routine

	db 1+2*DRV_HOTPLUG+4

;-----------------------------------------------------------------------------
;
; Reserved byte
;
	db	0

;-----------------------------------------------------------------------------
;
; Driver name
;
; It will be shown in the FDISK interface selection menu

DRV_NAME:
	db	"FBLabs SDHC"
	ds	32-($-DRV_NAME)," "


;-----------------------------------------------------------------------------
;
; Jump table for the driver public routines
;

	; These routines are mandatory for all drivers
        ; (but probably you need to implement only DRV_INIT)

	jp	DRV_TIMI
	jp	DRV_VERSION
	jp	DRV_INIT
	jp	DRV_BASSTAT
	jp	DRV_BASDEV
	jp	DRV_EXTBIO
	jp	DRV_DIRECT0
	jp	DRV_DIRECT1
	jp	DRV_DIRECT2
	jp	DRV_DIRECT3
	jp	DRV_DIRECT4
	jp	DRV_CONFIG

	ds	12

	; These routines are mandatory for device-based drivers

	jp	DEV_RW
	jp	DEV_INFO
	jp	DEV_STATUS
	jp	LUN_INFO


;=====
;=====  END of data that must be at fixed addresses
;=====


;-----------------------------------------------------------------------------
;
; Timer interrupt routine, it will be called on each timer interrupt
; (at 50 or 60Hz), but only if DRV_INIT returns Cy=1 on its first execution.

DRV_TIMI:
	ret

;-----------------------------------------------------------------------------
;
; Driver initialization routine, it is called twice:
;
; 1) First execution, for information gathering.
;    Input:
;      A = 0
;      B = number of available drives
;      HL = maximum size of allocatable work area in page 3
;    Output:
;      A = number of required drives (for drive-based driver only)
;      HL = size of required work area in page 3
;      Cy = 1 if DRV_TIMI must be hooked to the timer interrupt, 0 otherwise
;
; 2) Second execution, for work area and hardware initialization.
;    Input:
;      A = 1
;      B = number of allocated drives for this controller
;
;    The work area address can be obtained by using GWORK.
;
;    If first execution requests more work area than available,
;    second execution will not be done and DRV_TIMI will not be hooked
;    to the timer interrupt.
;
;    If first execution requests more drives than available,
;    as many drives as possible will be allocated, and the initialization
;    procedure will continue the normal way
;    (for drive-based drivers only. Device-based drivers always
;     get two allocated drives.)

DRV_INIT:
	or	a		; Is this the 1st call? 
	jr	nz,.call2
; 1st call:
	ld	hl,WRKAREA.TRLDIR ; size of work area needed for the Z80
	ld	a,(MSXVER)
	cp	3		; MSX Turbo-R?
	ccf
	ret	nc		; No, return with Cy off
	ld	hl,WRKAREA	; size of work area needed for the TR
	or	a		; Clear Cy
	ret


.call2:
; 2nd call: 
 IFDEF TURBOINIT
	ld	a,(CHGCPU)
	cp	#C3		; IS CHGCPU present?
	jr	nz,.call2ini
	call	GETCPU
	push	af		; Save the current CPU
	ld	a,#82
	call	CHGCPU		; Enable the turbo
.call2ini:
 ENDIF ; TURBOINIT
	call	MYSETSCR		; Set the screen mode
	call	pegaWorkArea		; IY=Work area pointer

;	; Clear the work area
	push	iy
	pop	hl
	ld	de,hl
	inc	de
	ld	bc,WRKAREA.TRLDIR
	ld	(hl),0
	ldir

	ld	de,strTitle		; prints the title 
	call	printString


.sdhcinit:	; FBLabs SDHC Interface initialization
	call	.printmode		; Print the switches configuration
	xor	a			; zera flags do cartao
	ld	(iy+WRKAREA.CARDFLAGS), a

	ld	a, 1			; detectar cartao 1
	call	.detecta
	ld	a, 2			; detectar cartao 2
	call	.detecta
	ld	bc, 0
	ld	e, 5

	call	INSTR800HLP		; Install R800 data copy on workarea

	call	INICHKSTOP		; Check if the STOP key was pressed

	ld	de, strCrLf
 
	call	printString
 IFDEF TURBOINIT
.drv_init_end:
	; ***Workaround for a bug in Nextor that causes it to freeze if
	; CTRL+STOP was pressed on boot
	ld	a,(INTFLG)
	cp	3		; Is CTRL+STOP still signaled?
	jr	nz,.restCPU	; no, skip
	xor	a
	ld	(INTFLG),a	; Clear CTRL+STOP otherwise Nextor will freeze

.restCPU:	; Restore the CPU if necessary
	ld	a,(CHGCPU)
	cp	#C3		; IS CHGCPU present?
	ret	nz
	pop	af
	or	#80
	jp	CHGCPU
 ELSE
	ret
 ENDIF ; TURBOINIT


;------- DRV_INIT aux routines ----------

.detecta:
	ld	(iy+WRKAREA.NUMSD), a	; Save the requested card slot
	ld	de, strCartao
	call	printString
	ld	c, (iy+WRKAREA.NUMSD)
	ld	a,c
	add	'0'
	call	CHPUT
	ld	a, ':'
	call	CHPUT
	ld	a, ' '
	call	CHPUT
	ld	a,c			; Get card slot#
;	cpl				; invert bits
;	and	3
	ld	(SPICTRL), a		; Select card slot
	ld	a, (SPISTATUS)		; get card slot status
	call	disableSDs
	and	SD_M_PRESENT		; Is there an card present?
	jr	z,.naoVazio		; Yes, skip
	ld	de, strVazio		; Empty SD card slot message
	jp	printString
;	jp	.marcaErro
.naoVazio:
	call	detectaCartao		; tem cartao no slot, inicializar e detectar
	jr	nc,.detectou
	call	disableSDs
	ld	de, strNaoIdentificado
	jp	printString
;.marcaErro:
;	jp	marcaErroCartao		; slot vazio ou erro de deteccao, marcar nas flags
.detectou:
	call	calculaCIDoffset	; calculamos em IX a posicao correta do offset CID dependendo do cartao atual
	ld	a,(ix+15)	; pegar byte SDV1 ou SDV2
	ld	de, strSDV1	; e imprimir
	or	a
	jr	z,.pula1
	ld	de, strSDV2
.pula1:
	call	printString
	ld	a,'('
	call	CHPUT
	ld	a,(ix)		; pegar byte do fabricante
	call	printDecToAscii	; Imprimir Manufacturer ID
	ld	a,')'
	call	CHPUT
	ld	a,' '
	call	CHPUT
	ld	a,(ix)		; pegar byte do fabricante
	call	pegaFabricante	; achar nome do fabricante
	ex	de,hl
	call	printString	; e imprimir
	ld	de,strCrLf
	jp	printString


.printmode:		; Print the two switches configuration
	xor	a			; 0=Interface status
	ld	(SPICTRL),a
	ld	a, (SPISTATUS)		; Check if the mapper/megaRAM is active
	and	IF_M_RAM		; Is the RAM enabled?
	ld	de,strMr_mp_desativada
 IFDEF HASMEGARAM
	jr	z,.print		; No, skip
	ld	a, (SPISTATUS)		; ativa, testar se eh mapper ou megaram
	and	IF_M_DRVER
	ld	de,strMapper
	jr	nz,.print
	ld	de, strMegaram		; Megaram ativa
.print:
	jp	printString
 ELSE
	call	z,printString		; Yes, print
	ld	a, (SPISTATUS)		; Get the MainBIOS/DevBIOS switch status
	and	IF_M_DRVER
	ld	de,strDrvMain
	jr	nz,.printdrv
	ld	de, strDrvDev
.printdrv:
	jp	printString
 ENDIF





;-----------------------------------------------------------------------------
;
; Obtain driver version
;
; Input:  -
; Output: A = Main version number
;         B = Secondary version number
;         C = Revision number

DRV_VERSION:
	ld	a, VER_MAIN
	ld	b, VER_SEC
	ld	c, VER_REV
	ret


;-----------------------------------------------------------------------------
;
; BASIC expanded statement ("CALL") handler.
; Works the expected way, except that if invoking CALBAS is needed,
; it must be done via the CALLB0 routine in kernel page 0.

DRV_BASSTAT:
	scf
	ret


;-----------------------------------------------------------------------------
;
; BASIC expanded device handler.
; Works the expected way, except that if invoking CALBAS is needed,
; it must be done via the CALLB0 routine in kernel page 0.

DRV_BASDEV:
	scf
	ret

;-----------------------------------------------------------------------------
;
; Extended BIOS hook.
; Works the expected way, except that it must return
; D'=1 if the old hook must be called, D'=0 otherwise.
; It is entered with D'=1.

DRV_EXTBIO:
	ret

;-----------------------------------------------------------------------------
;
; Direct calls entry points.
; Calls to addresses 7850h, 7853h, 7856h, 7859h and 785Ch
; in kernel banks 0 and 3 will be redirected
; to DIRECT0/1/2/3/4 respectively.
; Receives all register data from the caller except IX and AF'.

DRV_DIRECT0:
DRV_DIRECT1:
DRV_DIRECT2:
DRV_DIRECT3:
DRV_DIRECT4:
	ret


;-----------------------------------------------------------------------------
;
; Get driver configuration 
; (bit 2 of driver flags must be set if this routine is implemented)
;
; Input:
;   A = Configuration index
;   BC, DE, HL = Depends on the configuration
;
; Output:
;   A = 0: Ok
;       1: Configuration not available for the supplied index
;   BC, DE, HL = Depends on the configuration
;
; * Get number of drives at boot time (for device-based drivers only):
;   Input:
;     A = 1
;     B = 0 for DOS 2 mode, 1 for DOS 1 mode
;   Output:
;     B = number of drives
;
; * Get default configuration for drive
;   Input:
;     A = 2
;     B = 0 for DOS 2 mode, 1 for DOS 1 mode
;     C = Relative drive number at boot time (0~n)
;   Output:
;     B = Device index (1~7)
;     C = LUN index

DRV_CONFIG:
	or	a
	jr	z,.notavail
	cp	1
	jr	nz,.tryC2

	; Config-1: Get number of drives at boot time
	ld	b,2
	xor	a
	ret

.tryC2: cp	2
	jr	nz,.notavail

	; Config-2: Get default configuration for drive
	ld	b,c
	inc	b
	ld	c,1
	xor	a
	ret


.notavail:
	ld	a,1
	ret


;=====
;=====  BEGIN of DEVICE-BASED specific routines
;=====

;-----------------------------------------------------------------------------
;
; Read or write logical sectors from/to a logical unit
;
;Input:    Cy=0 to read, 1 to write
;          A = Device number, 1 to 7
;          B = Number of sectors to read or write
;          C = Logical unit number, 1 to 7
;          HL = Source or destination memory address for the transfer
;          DE = Address where the 4 byte sector number is stored.
;Output:   A = Error code (the same codes of MSX-DOS are used):
;              0: Ok
;              .IDEVL: Invalid device or LUN
;              .NRDY: Not ready
;              .DISK: General unknown disk error
;              .DATA: CRC error when reading
;              .RNF: Sector not found
;              .UFORM: Unformatted disk
;              .WPROT: Write protected media, or read-only logical unit
;              .WRERR: Write error
;              .NCOMP: Incompatible disk.
;              .SEEK: Seek error.
;          B = Number of sectors actually read (in case of error only)

DEV_RW:
	push	af
	cp	3		; somente 2 dispositivos
	jr	nc,.saicomerroidl
	dec	c		; somente 1 logical unit
	jr	nz,.saicomerroidl
	call	pegaWorkArea	; IY=Work area pointer
	ld	(iy+WRKAREA.NUMSD),a
	ld	(iy+WRKAREA.NUMBLOCKS),b	; save the number of blocks to transfer 
	call	setaSDAtual
	ld	a,(SPISTATUS)	; Get this card slot status
	and	SD_M_PRESENT	; Is there a card present?
	jr	z,.cardok	; Yes, skip
	pop	af
	ld	a, ENRDY	; Not ready
	ld	b, 0
	ret
.saicomerroidl:
	pop	af
	ld	a, EIDEVL	; error: Invalid device or LUN 
	ld	b, 0
	ret
.cardok:
;	exx
	push	de,hl

	call	SETLDIRHLPR	; hl'=Pointer to LDIR helper in RAM
	call	calculaCIDoffset	; ix=CID offset 
	ld	a,(ix+15)	; verificar se eh SDV1 ou SDV2
	ld	ixl,a		; ixl=SDcard version
	pop	hl,de
	pop	af		; a=Device number, f=read/write flag 
;	exx			; hl=Source/dest Address, de=Pointer to sect#
;	ld	ixh, b 		; ixh=Number of blocks to transfer
	jr	c,DEV_W	; Skip if it's a write operation 

DEV_R:
	ld	a, (de)		; 1. n. bloco
	push	af
	inc	de
	ld	a, (de)		; 2. n. bloco
	push	af
	inc	de
	ld	a, (de)		; 3. n. bloco
	ld	c, a
	inc	de
	ld	a, (de)		; 4. n. bloco
	inc	de
	ld	b, a
	pop	af
	ld	d, a
	pop	af		; HL = ponteiro destino
	ld	e, a		; BC DE = 32 bits numero do bloco
	call	LerBloco	; chamar rotina de leitura de dados
	ret	nc		; Return with A=0 if no error occurred
	call	marcaErroCartao		; ocorreu erro, marcar nas flags
	ld	a,(iy+WRKAREA.NUMBLOCKS) ; Get the number of requested blocks
	sub	ixh		; subtract the number of remaining blocks
	ld	b,a		; b=number of blocks written
	ld	a, EDISK	; General unknown disk error
	ret

DEV_W:
	; Test if the card is write protected
	ld	a,(SPISTATUS)	; Get this card slot status
	and	SD_M_WRTPROT	; Is the card write protected?
	jr	z,.ok
	ld	a, EWPROT	; disco protegido
	ld	b,0		; 0 blocks were written
	ret
.ok:
	ld	a, (de)		; 1. n. bloco
	push	af
	inc	de
	ld	a, (de)		; 2. n. bloco
	push	af
	inc	de
	ld	a, (de)		; 3. n. bloco
	inc	de
	ld	c, a
	ld	a, (de)		; 4. n. bloco
	inc	de
	ld	b, a
	pop	af
	ld	d, a
	pop	af		; HL = ponteiro destino
	ld	e, a		; BC DE = 32 bits numero do bloco
	call	GravarBloco	; chamar rotina de gravacao de dados
	ret	nc		; Return with A=0 if no error occurred

	call	marcaErroCartao		; ocorreu erro, marcar nas flags
	ld	a,(iy+WRKAREA.NUMBLOCKS) ; Get the number of requested blocks
	sub	ixh		; subtract the number of remaining blocks
	ld	b,a		; b=number of blocks written
	ld	a,EWRERR	; Write error
	ret

;-----------------------------------------------------------------------------
;
; Device information gathering
;
;Input:   A = Device index, 1 to 7
;         B = Information to return:
;             0: Basic information
;             1: Manufacturer name string
;             2: Device name string
;             3: Serial number string
;         HL = Pointer to a buffer in RAM
;Output:  A = Error code:
;             0: Ok
;             1: Device not available or invalid device index
;             2: Information not available, or invalid information index
;         When basic information is requested,
;         buffer filled with the following information:
;
;+0 (1): Numer of logical units, from 1 to 7. 1 if the device has no logical
;        units (which is functionally equivalent to having only one).
;+1 (1): Device flags, always zero in Beta 2.
;
; The strings must be printable ASCII string (ASCII codes 32 to 126),
; left justified and padded with spaces. All the strings are optional,
; if not available, an error must be returned.
; If a string is provided by the device in binary format, it must be reported
; as an hexadecimal, upper-cased string, preceded by the prefix "0x".
; The maximum length for a string is 64 characters;
; if the string is actually longer, the leftmost 64 characters
; should be provided.
;
; In the case of the serial number string, the same rules for the strings
; apply, except that it must be provided right-justified,
; and if it is too long, the rightmost characters must be
; provided, not the leftmost.

DEV_INFO:
	cp	3		; somente 2 dispositivos
	jr	c,.devok
	ld	a,1		; invalid device index
	ret
.devok:
	call	pegaWorkArea	; IY=Work area pointer
	ld	(iy+WRKAREA.NUMSD),a
	call	setaSDAtual
	inc	b
	djnz	.naoBasic

; Basic information:
	ld	(hl),1		; 1 logical unit somente
	inc	hl
	xor	a
	ld	(hl),a		; reservado, deve ser 0
	ret			; retorna com A=0 (OK)

.naoBasic:
	push	hl,bc,af
	call	DEV_STATUS	; We need to do this because SD cards are removable devices, not just
				; removable media 
	or	a		; Is this device available?
	jr	nz,.getinfo	; Yes, get this device info
	pop	af,bc,hl	; Flush the stack
.noinfo:
	ld	a,2		; Quit with "Info not available" status
	ret

.getinfo:
	pop	af
	call	calculaCIDoffset	; calculamos em IX a posicao correta do offset CID dependendo do cartao atual
	pop	bc,hl
	djnz	.naoManuf
; Manufacturer Name:
	push	hl		; salva ponteiro do buffer
	ld	b, 64		; preenche buffer com espaco
	ld	a, ' '
.loop1:
	ld	(hl), a
	inc	hl
	djnz	.loop1
	pop	hl		; hl=pointer to Nextor buffer 
	ld	(hl),'('	; Place "(xx) " on the buffer
	inc	hl
	ld	a, (ix)		; byte do fabricante
	call	DecToAscii
	ld	(hl),')'
	inc	hl
	ld	(hl),' '
	inc	hl
	ld	a, (ix)		; byte do fabricante
	ex	de,hl
	call	pegaFabricante	; pegar nome do fabricante em HL
	ldir			; e colocar no buffer
	xor	a		; Return with A=0 (Ok)
	ret

.naoManuf:
	djnz	.naoProduct
; Product Name:
	push	hl		; guarda HL que aponta para buffer do Nextor
	push	ix
	pop	hl		; joga IX para HL
	ld	de,3		; adiciona offset do productname em HL
	add	hl, de
	pop	de		; recupera buffer do Nextor em DE
	ld	bc, 5		; 5 caracteres
	ldir			; copia nome do produto
	ex	de,hl		; troca DE com HL, agora HL aponta para Buffer do nextor atualizado
	ld	b, 59		; Coloca espaco no restante do buffer
	ld	a, ' '
.loop2:
	ld	(hl), a
	inc	hl
	djnz	.loop2
	xor	a		; Return with A=0 (Ok)
	ret

.naoProduct:
	djnz	.noinfo
; Serial Number:
	ld	(hl),'0'	; Coloca prefixo "0x"
	inc	hl
	ld	(hl), 'x'
	inc	hl
	push	hl		; guarda HL que aponta para buffer do Nextor
	push	ix
	pop	hl		; joga IX para HL
	ld	de, 9		; adiciona offset do productname em HL
	add	hl, de
	pop	de		; recupera buffer do nextor em DE
	ld	b, 4		; 4 bytes do serial
.loop3:
	ld	a,(hl)
	call	HexToAscii	; converter HEXA para ASCII
	inc	hl
	djnz	.loop3
	ld	b, 54		; Coloca espaco no restante
	ld	a, ' '
.loop4:
	ld	(de), a
	inc	de
	djnz	.loop4
	xor	a		; Return with A=0 (Ok)
	ret

;-----------------------------------------------------------------------------
;
; Obtain device status
;
;Input:   A = Device index, 1 to 7
;         B = Logical unit number, 1 to 7
;             0 to return the status of the device itself.
;Output:  A = Status for the specified logical unit,
;             or for the whole device if 0 was specified:
;                0: The device or logical unit is not available, or the
;                   device or logical unit number supplied is invalid.
;                1: The device or logical unit is available and has not
;                   changed since the last status request.
;                2: The device or logical unit is available and has changed
;                   since the last status request
;                   (for devices, the device has been unplugged and a
;                    different device has been plugged which has been
;                    assigned the same device index; for logical units,
;                    the media has been changed).
;                3: The device or logical unit is available, but it is not
;                   possible to determine whether it has been changed
;                   or not since the last status request.
;
; Devices not supporting hot-plugging must always return status value 1.
; Non removable logical units may return values 0 and 1.
;
; The returned status is always relative to the previous invokation of
; DEV_STATUS itself. Please read the Driver Developer Guide for more info.

DEV_STATUS:
	ld	c,a		; c=Device number
	cp	3		; 2 dispositivos somente
	jr	nc,.nodev
	ld	a,b
	or	a		; Device itself status?
	jr	z,.devok	; I'm fine, thanks
	dec	a		; Only LUN=1 are allowed
	jr	nz,.nodev
 IFDEF DEBUG
	push	af
	ld	a,'S'		; DEV_STATUS debug ID
	call	PRTCHAR
	pop	af
	push	af
	add	'0'		; Device number
	call	PRTCHAR
	pop	af
 ENDIF
	push	bc
	ld	a,c
	call	pegaWorkArea	; IY=Work area pointer
	pop	bc
	ld	(iy+WRKAREA.NUMSD),c	; salva numero do device atual (1 ou 2)
	ld	a,c
	ld	(SPICTRL), a	; selects SD
	ld	a, (SPISTATUS)	; Get this card-slot status
	call	disableSDs
	bit	SD_PRESENT,a	; Any card here?
	jr	nz,.saicomerro	; Report that there's no card here
	and	SD_M_DSKCHG	; Has the card changed since last time?
	jr	nz,.detcard	; Yes, force a detection
.tsterror:
	ld	a, (iy+WRKAREA.CARDFLAGS)	; testar bit de erro do cartao nas flags
	and	c		; Test against the card #
	jr	z,.semMudanca	; cartao nao marcado com erro, pula
.detcard:
	call	detectaCartao	; erro na deteccao do cartao, tentar re-detectar
	jr	c,.cartaoComErro	; nao conseguimos detectar, sai com erro
	ld	a, (iy+WRKAREA.NUMSD)		; conseguimos detectar, tira erro nas flags
	cpl			; inverte bits para fazer o AND
	ld	c, a
	ld	a, (iy+WRKAREA.CARDFLAGS)
	and	c		; clear the error bit 
	ld	(iy+WRKAREA.CARDFLAGS), a
.comMudanca:
 IFDEF DEBUG
	ld	a,'2'
	call	PRTCHAR
 ENDIF
	ld	a, 2		; informa ao Nextor que cartao esta OK e mudou
	ret
.semMudanca:
 IFDEF DEBUG
	ld	a,'1'
	call	PRTCHAR
 ENDIF
	ld	a, 1		; informa ao Nextor que cartao esta OK e nao mudou
	ret
.cartaoComErro:
	call	marcaErroCartao	; marcar erro do cartao nas flags
 IFDEF DEBUG
	ld	a,'E'
	call	PRTCHAR
	xor	a
	ret
 ENDIF
.nodev:
 IFDEF DEBUG
	ld	a,'s'
	call	PRTCHAR
	xor	a
	ret
 ENDIF
.saicomerro:
 IFDEF DEBUG
	ld	a,'0'
	call	PRTCHAR
 ENDIF
	xor	a		; this device has an error 
	ret
.devok:
	ld	a,3
	ret


;-----------------------------------------------------------------------------
;
; Obtain logical unit information
;
;Input:   A  = Device index, 1 to 7
;         B  = Logical unit number, 1 to 7
;         HL = Pointer to buffer in RAM.
;Output:  A = 0: Ok, buffer filled with information.
;             1: Error, device or logical unit not available,
;                or device index or logical unit number invalid.
;         On success, buffer filled with the following information:
;
;+0 (1): Medium type:
;        0: Block device
;        1: CD or DVD reader or recorder
;        2-254: Unused. Additional codes may be defined in the future.
;        255: Other
;+1 (2): Sector size, 0 if this information does not apply or is
;        not available.
;+3 (4): Total number of available sectors.
;        0 if this information does not apply or is not available.
;+7 (1): Flags:
;        bit 0: 1 if the medium is removable.
;        bit 1: 1 if the medium is read only. A medium that can dinamically
;               be write protected or write enabled is not considered
;               to be read-only.
;        bit 2: 1 if the LUN is a floppy disk drive.
;+8 (2): Number of cylinders
;+10 (1): Number of heads
;+11 (1): Number of sectors per track
;
; Number of cylinders, heads and sectors apply to hard disks only.
; For other types of device, these fields must be zero.

LUN_INFO:
	cp	3		; somente 2 dispositivo
	jr	nc,.saicomerro
	dec	b		; somente 1 logical unit
	jr	nz,.saicomerro

 IFDEF DEBUG
	push	af
	ld	a,'L'		; LUN_INFO debug ID
	call	PRTCHAR
	pop	af
	push	af
	add	'0'		; Print the device number
	call	PRTCHAR
	pop	af
 ENDIF

	push	af,hl
	inc	b		; b=LUN=1
	call	DEV_STATUS	; Is there a card present? 
	pop	hl
	or	a
	jr	nz,.ok		; device is available
	pop	af
.nomedia:
 IFDEF DEBUG
	push	af
	ld	a,'N'
	call	PRTCHAR
	pop	af
 ENDIF
	xor	a
	ld	b,7
.nmloop:
	ld	(hl),a		; 0=block device
	inc	hl
	djnz	.nmloop		; Fill the rest with "0=information not available"
	jr	.wflagsnCHS	; Fill the rest of the info about the device

.ok:
 IFDEF DEBUG
	ld	a,'P'
	call	PRTCHAR
 ENDIF

	pop	af
	push	hl
	call	calculaBLOCOSoffset	; calcular em IX e HL o offset correto do buffer que armazena total de blocos
	pop	hl		; do cartao dependendo do cartao atual solicitado
	xor	a
	ld	(hl),a		; 0 = block device 
	inc	hl
	ld	(hl),a		; Block size lsb 
	inc	hl
	ld	(hl),2		; Block size msb = 512
	inc	hl
	ld	a, (ix)		; copia numero de blocos total
	ld	(hl), a
	inc	hl
	ld	a, (ix+1)
	ld	(hl), a
	inc	hl
	ld	a, (ix+2)
	ld	(hl), a
	inc	hl
	ld	(hl), 0		; cartoes SD tem total de blocos em 24 bits, mas o Nextor pede numero de
	inc	hl 		; 32 bits, entao coloca 0 no MSB

.wflagsnCHS:
	ld	(hl),1		; flags: dispositivo R/W removivel
	inc	hl
	xor	a		; CHS = 0
	ld	(hl), a
	inc	hl
	ld	(hl), a
	inc	hl
	ld	(hl), a
	ret			; Return with A-0: Ok, filled the buffer

.saicomerro:
	ld	a, 1		; Return with A=1: Error
	ret


;=====
;=====  END of DEVICE-BASED specific routines
;=====

;------------------------------------------------
; Rotinas auxiliares
;------------------------------------------------

;------------------------------------------------
; Pedir ao Nextor o ponteiro de dados de trabalho
; na RAM e colocar em HL e IY
; Output: 
; IY = WorkArea pointer
; Modifies: IX
;------------------------------------------------
pegaWorkArea:
	push	af,hl
	xor	a		; Pegar endereco da area de trabalho
	ex	af,af'
	xor	a
	ld	ix, GWORK
	call	CALBNK
	ld	l,(ix)		; em HL tem o ponteiro da nossa area da RAM
	ld	h,(ix+1)
	push	hl
	pop	iy		; em IY temos o mesmo ponteiro
	pop	hl,af
	ret

;------------------------------------------------
; Marcar bit de erro nas flags
; Destroi AF, C
;------------------------------------------------
marcaErroCartao:
	ld	c, (iy+WRKAREA.NUMSD)		; cartao atual (1 ou 2)
	ld	a, (iy+WRKAREA.CARDFLAGS)	; marcar erro
	or	c
	ld	(iy+WRKAREA.CARDFLAGS), a
	ret

;------------------------------------------------
; Testar se cartao atual esta protegido contra
; gravacao, A=0 se protegido
; Destroi AF, C
;------------------------------------------------
testaWP:
	ld	a, (iy+WRKAREA.NUMSD)	; cartao atual (1 ou 2)
	ld	(SPICTRL), a
	ld	a, (SPISTATUS)	; testar se cartao esta protegido
;	call	disableSDs
	and	$04
	ret			; se A for 0 cartao esta protegido

;------------------------------------------------
; Calcula offset do buffer na RAM em HL e IX para
; os dados do CID dependendo do cartao atual
; Destroi AF, DE, HL e IX
;------------------------------------------------
calculaCIDoffset:
	push	iy		; copiamos IY para HL
	pop	hl
	ld	d, 0
	ld	e, WRKAREA.BCID1	; DE aponta para buffer BCID1
	ld	a, (iy+WRKAREA.NUMSD)	; vamos fazer IX apontar para o buffer correto
	dec	a		; dependendo do cartao: BCID1 ou BCID2
	jr	z,.c1
	ld	e, WRKAREA.BCID2	; DE aponta para buffer BCID2
.c1:
	add	hl, de		; HL aponta para buffer correto
	push	hl		
	pop	ix		; vamos colocar HL em IX
	ret

;------------------------------------------------
; Calcula offset do buffer na RAM para os dados
; do total de blocos dependendo do cartao atual
; Offset fica em HL e IX
; Destroi AF, DE, HL e IX
;------------------------------------------------
calculaBLOCOSoffset:
	push	iy		; copiamos IY para HL
	pop	hl
	ld	d, 0
	ld	e, WRKAREA.BLOCKS1	; DE aponta para buffer BLOCKS1
	ld	a, (iy+WRKAREA.NUMSD)	; Vamos fazer IX apontar para o buffer correto
	dec	a		; dependendo do cartao: BLOCKS1 ou BLOCKS2
	jr	z,.c1
	ld	e, WRKAREA.BLOCKS2	; DE aponta para buffer BLOCKS2
.c1:
	add	hl, de		; HL aponta para buffer correto
	push	hl		
	pop	ix		; Vamos colocar HL em IX
	ret


;------------------------------------------------
; Minhas funcoes para cartao SD
;------------------------------------------------

;------------------------------------------------
; Processo de inicializacao e deteccao do cartao.
; Detecta se cartao responde, qual versao (SDV1
; ou SDV2), faz a leitura do CSD e CID e calcula
; o numero de blocos do cartao, colocando o CID
; e total de blocos no buffer correto dependendo
; do cartao 1 ou 2.
; Retorna erro no carry. Se for 0 indica deteccao
; com sucesso.
; Destroi todos os registradores
;------------------------------------------------
detectaCartao:
	call	iniciaSD	; manda pulsos de clock e comandos iniciais
	ret	c			; retorna se erro
	call	testaSDCV2	; tenta inicializar um cartao SDV2
	ret	c
	push	iy		; colocar em HL buffer da RAM de trabalho
	pop	hl		; CSD esta no offset 0, nao precisamos somar
	ld	a, CMD9		; ler CSD
	call	lerBlocoCxD
	ret	c
	call	calculaCIDoffset	; calculamos em IX e HL a posicao correta do offset CID dependendo do cartao atual
	ld	a, CMD10	; ler CID
	call	lerBlocoCxD
	ret	c
	ld	a, CMD58	; ler OCR
	ld	de, 0
	call	SD_SEND_CMD_2_ARGS_GET_R3	; enviar comando e receber resposta tipo R3
	ret	c
	ld	a, b		; testa bit CCS do OCR que informa se cartao eh SDV1 ou SDV2
	and	$40
	ld	(ix+15), a	; salva informacao da versao do SD (V1 ou V2) no byte 15 do CID
	call	z,mudarTamanhoBlocoPara512	; se bit CCS do OCR for 1, eh cartao SDV2 (Block address - SDHC ou SDXD)
	ret	c		; e nao precisamos mudar tamanho do bloco para 512
	call	disableSDs
				; agora vamos calcular o total de blocos dependendo dos dados do CSD
	call	calculaBLOCOSoffset	; calcular em IX e HL o offset correto do buffer que armazena total de blocos
	push	iy		; copiamos IY para HL
	pop	hl
	ld	d, 0
	ld	e, WRKAREA.BCSD+5
	add	hl, de		; HL aponta para buffer BCSD+5
	ld	a, (iy+WRKAREA.BCSD)
	and	$C0		; testa versao do registro CSD
	jr	z,.calculaCSD1
	cp	$40
	jr	z,.calculaCSD2
	scf			; versao do registro CSD nao reconhecida, informa erro na deteccao
	ret

; -----------------------------------
; Registro CSD versao 1, calcular da
; maneira correta para a versao 1
; -----------------------------------
.calculaCSD1:
	ld	a, (hl)
	and	$0F		; isola READ_BL_LEN
	push	af
	inc	hl
	ld	a, (hl)		; 2 primeiros bits de C_SIZE
	and	3
	ld	d, a
	inc	hl
	ld	e, (hl)		; 8 bits de C_SIZE (DE contem os primeiros 10 bits de C_SIZE)
	inc	hl
	ld	a, (hl)
	and	$C0		; 2 ultimos bits de C_SIZE
	add	a, a		; rotaciona a esquerda
	rl	e		; rotaciona para DE
	rl	d
	add	a, a		; mais uma rotacao
	rl	e		; rotaciona para DE
	rl	d
	inc	de		; agora DE contem todos os 12 bits de C_SIZE, incrementa 1
	inc	hl
	ld	a, (hl)		; proximo byte
	and	3		; 2 bits de C_SIZE_MUL
	ld	b, a		; B contem os 2 bits de C_SIZE_MUL
	inc	hl						
	ld	a, (hl)		; proximo byte
	and	$80		; 1 bit de C_SIZE_MUL
	add	a, a		; rotaciona para esquerda jogando no carry
	rl	b		; rotaciona para B
	inc	b		; agora B contem os 3 bits de C_SIZE_MUL
	inc	b		; faz B = C_SIZE_MUL + 2
	pop	af		; volta em A o READ_BL_LEN
	add	a, b		; A = READ_BL_LEN + (C_SIZE_MUL+2)
	ld	bc, 0
	call	.eleva2
	ld	e, d		; aqui temos 32 bits (BC DE) com o tamanho do cartao
	ld	d, c		; ignoramos os 8 ultimos bits em E, fazemos BC DE => 0B CD (divide por 256)
	ld	c, b
	ld	b, 0
	srl	c		; rotacionamos a direita o C, carry = LSB (divide por 2)
	rr	d		; rotacionamos D e E
	rr	e		; no final BC DE contem tamanho do cartao / 512 = numero de blocos
.salvaBlocos:
	ld	(ix+2), c	; colocar no buffer BLOCOS correto a quantidade de blocos
	ld	(ix+1), d	; que o cartao (1 ou 2) tem
	ld	(ix), e
	xor	a		; limpa carry
	ret

.eleva2:			; aqui temos: A = (READ_BL_LEN + (C_SIZE_MUL+2))
				; BC = 0
				; DE = C_SIZE
	sla	e		; rotacionamos C_SIZE por 'A' vezes
	rl	d
	rl	c
	rl	b
	dec	a		; subtraimos 1
	jr	nz,.eleva2
	ret			; em BC DE temos o tamanho do cartao (bytes) em 32 bits

; -----------------------------------
; Registro CSD versao 2, calcular da
; maneira correta para a versao 2
; -----------------------------------
.calculaCSD2:
	inc	hl		; HL ja aponta para BCSD+5, fazer HL apontar para BCSD+7
	inc	hl
	ld	a, (hl)
	and	$3F
	ld	c, a
	inc	hl
	ld	d, (hl)
	inc	hl
	ld	e, (hl)
	call	.inc32		; soma 1
	call	.desloca32	; multiplica por 512
	call	.rotaciona24	; multiplica por 2
	jp		.salvaBlocos

.inc32:
	inc	e
	ret	nz
	inc	d
	ret	nz
	inc	c
	ret	nz
	inc	b
	ret

.desloca32:
	ld	b, c
	ld	c, d
	ld	d, e
	ld	e, 0
.rotaciona24:
	sla	d
	rl	c
	rl	b
	ret

; ------------------------------------------------
; Setar o tamanho do bloco para 512 se o cartao
; for SDV1
; ------------------------------------------------
mudarTamanhoBlocoPara512:
	ld	a, CMD16
	ld	bc, 0
	ld	de, 512
	jp	SD_SEND_CMD_GET_ERROR

; ------------------------------------------------
; Tenta inicializar um cartao SDV2, se houver erro
; o cartao deve ser SDV1
; ------------------------------------------------
testaSDCV2:
	ld	a, CMD8
	ld	de, $1AA
	call	SD_SEND_CMD_2_ARGS_GET_R3
	ld	hl, SD_SEND_CMD1	; HL aponta para rotina correta
	jr	c,.pula		; cartao recusou CMD8, enviar comando CMD1
	ld	hl, SD_SEND_ACMD41	; cartao aceitou CMD8, enviar comando ACMD41
.pula:
	ld	bc, 20		; B = 0, C = 20: 5120 tentativas
.loop:
	push	bc
	call	.jumpHL		; chamar rotina correta em HL
	pop	bc
	ret	nc
	djnz	.loop
	dec	c
	jr	nz,.loop
	scf
	ret
.jumpHL:
	jp	(hl)		; chamar rotina correta em HL

; ------------------------------------------------
; Ler registro CID ou CSD, o comando vem em A
; ------------------------------------------------
lerBlocoCxD:
	call	SD_SEND_CMD_NO_ARGS
	ret	c
	call	WAIT_RESP_FE
	ret	c
	ex	de,hl

	; Check for a Z80 or R800
;	or 	a		; Clear Cy
	ld	a,20
	db	#ED,#F9		; mulub a,a
	ld	hl, SPIDATA
	jr	c,.r800		; Use LDIR for R800
	.16	ldi	; 16 vezes o opcode LDI
.end:	ld	a, (hl)
	ld	a, (hl)		; byte de resposta
	or	a
	ex	de,hl
;	jr	disableSDs
	ret

.r800:
	ld	bc,16
	ldir
	jr	.end

; ------------------------------------------------
; Algoritmo para inicializar um cartao SD
; Destroi AF, B, DE
; ------------------------------------------------
iniciaSD:
	call	disableSDs

	ld	b, 10		; enviar 80 pulsos de clock com cartao desabilitado
enviaClocksInicio:
	ld	a, $FF		; manter MOSI em 1
	ld	(SPIDATA), a
	djnz	enviaClocksInicio
	call	setaSDAtual	; ativar cartao atual
	ld	b, 8		; 8 tentativas para CMD0
SD_SEND_CMD0:
	ld	a, CMD0		; primeiro comando: CMD0
	ld	de, 0
	push	bc
	call	SD_SEND_CMD_2_ARGS_TEST_BUSY
	pop	bc
	ret	nc			; retorna se cartao respondeu ao CMD0
	djnz	SD_SEND_CMD0
	scf			; cartao nao respondeu ao CMD0, informar erro
	; fall throw

; ------------------------------------------------
; Desabilitar (de-selecionar) todos os cartoes
; Nao destroi registradores
; ------------------------------------------------
disableSDs:
	push	af
	xor	a
	ld	(SPICTRL), a
	pop	af
	ret

; ------------------------------------------------
; Enviar comando ACMD41
; ------------------------------------------------
SD_SEND_ACMD41:
	ld	a, CMD55
	call	SD_SEND_CMD_NO_ARGS
	ld	a, ACMD41
	ld	bc, $4000
	ld	d, c
	ld	e, c
	jr		SD_SEND_CMD_GET_ERROR

; ------------------------------------------------
; Enviar CMD1 para cartao. Carry indica erro
; Destroi AF, BC, DE
; ------------------------------------------------
SD_SEND_CMD1:
	ld	a, CMD1
SD_SEND_CMD_NO_ARGS:
	ld	bc, 0
	ld	d, b
	ld	e, c
SD_SEND_CMD_GET_ERROR:
	call	SD_SEND_CMD
	or	a
	ret	z			; se A=0 nao houve erro, retornar
	; fall throw

; ------------------------------------------------
; Informar erro
; Nao destroi registradores
; ------------------------------------------------
setaErro:
	scf
	jr		disableSDs

; ------------------------------------------------
; Enviar comando em A com 2 bytes de parametros
; em DE e testar retorno BUSY
; Retorna em A a resposta do cartao
; Destroi AF, BC
; ------------------------------------------------
SD_SEND_CMD_2_ARGS_TEST_BUSY:
	ld	bc, 0
	call	SD_SEND_CMD
	ld	b, a
	and	$FE		; testar bit 0 (flag BUSY)
	ld	a, b
	jr	nz,setaErro	; BUSY em 1, informar erro
	ret			; sem erros

; ------------------------------------------------
; Enviar comando em A com 2 bytes de parametros
; em DE e ler resposta do tipo R3 em BC DE
; Retorna em A a resposta do cartao
; Destroi AF, BC, DE, HL
; ------------------------------------------------
SD_SEND_CMD_2_ARGS_GET_R3:
	call	SD_SEND_CMD_2_ARGS_TEST_BUSY
	ret	c
	push	af
	call	WAIT_RESP_NO_FF
	ld	h, a
	call	WAIT_RESP_NO_FF
	ld	l, a
	call	WAIT_RESP_NO_FF
	ld	d, a
	call	WAIT_RESP_NO_FF
	ld	e, a
	ld	b, h
	ld	c, l
	pop	af
	ret

; ------------------------------------------------
; Enviar comando em A com 4 bytes de parametros
; em BC DE e enviar CRC correto se for CMD0 ou 
; CMD8 e aguardar processamento do cartao
; Output  : A=0 if there was no error
; Modifies:  AF, B, AF'
; ------------------------------------------------
SD_SEND_CMD:
	call	setaSDAtual
	ld	(SPIDATA), a
	push	af
	ld	a, b
	ld	(SPIDATA), a
	ld	a, c
	ld	(SPIDATA), a
	ld	a, d
	ld	(SPIDATA), a
	ld	a, e
	ld	(SPIDATA), a
	pop	af
	cp	CMD0
	ld	b, $95		; CRC para CMD0
	jr	z,enviaCRC
	cp	CMD8
	ld	b, $87		; CRC para CMD8
	jr	z,enviaCRC
	ld	b, $FF		; CRC dummy
enviaCRC:
	ld	a, b
	ld	(SPIDATA), a
	jr	WAIT_RESP_NO_FF

; ------------------------------------------------
; Esperar que resposta do cartao seja $FE
; Destroi AF, B
; ------------------------------------------------
WAIT_RESP_FE:
	ld	b, 10		; 10 tentativas
.loop:
	push	bc
	call	WAIT_RESP_NO_FF	; esperar resposta diferente de $FF
	pop	bc
	cp	$FE		; resposta é $FE ?
	ret	z		; sim, retornamos com carry=0
	djnz	.loop
	scf			; erro, carry=1
	ret

; ------------------------------------------------
; Esperar que resposta do cartao seja diferente
; de $FF
; Destroi AF, BC
; ------------------------------------------------
WAIT_RESP_NO_FF:
	ld	bc, 1		; 256 tentativas
.loop:
	ld	a, (SPIDATA)
	cp	$FF		; testa $FF
	ret	nz		; sai se nao for $FF
	djnz	.loop
	dec	c
	jr	nz,.loop
	ret

; ------------------------------------------------
; Esperar que resposta do cartao seja diferente
; de $00
; Destroi A, BC
; ------------------------------------------------
WAIT_RESP_NO_00:
	ld	bc, 128		; 32768 tentativas
.loop:
	ld	a, (SPIDATA)
	or	a
	ret	nz			; se resposta for <> $00, sai
	djnz	.loop
	dec	c
	jr	nz,.loop
	scf			; erro
	ret

; ------------------------------------------------
; Sets the requested card slot and check its status
; In case it detects that the card was changed, it will call the detection
; routine to re-dectect the card type
; Input: Target card slot
; Output: Cy = No SD card is present
;         A: 0=The same card is still present
;            1=The card was changed since the last check
; ------------------------------------------------
setaSDAtual:
	push	af
	ld	a, (SPIDATA)	; dummy read
	ld	a, (iy+WRKAREA.NUMSD)
	ld	(SPICTRL), a
	pop	af
	ret

 IFDEF BLA
	ld	a, (SPIDATA)	; dummy read
	ld	a, (iy+WRKAREA.NUMSD)
;	cpl			; invert bits
;	and	3
	ld	(SPICTRL), a
	;
	ld	a,(SPISTATUS)	; Get this card slot status
	bit	SD_PRESENT,a	; Is there a card present?
	scf
	ret	nz		; No, return with error

	; ***Workaround for the problem that Nextor doesn't call DEV_STATUS to
	; check if the media has changed before every disk operation, so we
	; need to always check if it changed.
	and	SD_M_DSKCHG	; Was the card changed since last checked?
	ret	z		; No, return
	push	bc,de,hl,ix
	call	detectaCartao	; Detect the new card
	pop	ix,hl,de,bc
	ret	c		; Error? Then return
	ld	a,1
	ret
 ENDIF ; BLA

; ------------------------------------------------
; Grava um bloco de 512 bytes no cartao
; HL = aponta para o inicio dos dados
; BC:DE = contem o numero do bloco (BCDE = 32 bits)
; IXH = Number of blocks
; IXL = SDcard version (0 or 1)
; Modifies:  AF, BC, DE, HL, IXL
; ------------------------------------------------
GravarBloco:
 IFDEF DEBUG
	push	af
	ld	a,'G'
	call	PRTCHAR
	pop	af
 ENDIF
;	ld	a, (ix+15)	; verificar se eh SDV1 ou SDV2
	ld	a,ixl
	or	a
	call	z,blocoParaByte		; se for SDV1 coverter blocos para bytes
;	call	setaSDAtual	; selecionar cartao atual
	ld	a, (iy+WRKAREA.NUMBLOCKS)	; Get the number of blocka 
	ld	ixh,a		; ixh=Number of blocks
	dec	a
	jp	z,.umBloco	; somente um bloco, gravar usando CMD24

; multiplos blocos
 IFDEF DEBUG
	push	af
	ld	a,'M'
	call	PRTCHAR
	pop	af
 ENDIF
	push	bc
	push	de
	ld	a, CMD55	; Multiplos blocos, mandar ACMD23 com total de blocos
	call	SD_SEND_CMD_NO_ARGS
	ld	a, ACMD23
	ld	bc, 0
	ld	d, c
	ld	e, (iy+WRKAREA.NUMBLOCKS)	; parametro = total de blocos a gravar
	call	SD_SEND_CMD_GET_ERROR
	pop	de
	pop	bc
	jp	c,terminaLeituraEscritaBloco	; erro no ACMD23
 IFDEF DEBUG
	call	PRTDOT
 ENDIF
	ld	a, CMD25	; comando CMD25 = write multiple blocks
	call	SD_SEND_CMD_GET_ERROR
	jp	c,terminaLeituraEscritaBloco	; erro
 IFDEF DEBUG
	call	PRTDOT
 ENDIF
.loop:
 IFDEF DEBUG
	call	PRTDASH
 ENDIF
	ld	a, $FC		; mandar $FC para indicar que os proximos dados sao
	ld	(SPIDATA),a	; dados para gravacao

	ld	de,SPIDATA
	call	RUN_HLPR

	ld	(SPIDATA),de
;	ld	a,$FF		; envia dummy CRC
;	ld	(SPIDATA),a	; Can't be done with ld (SPIDATA),de. It's too fast
;	ld	(SPIDATA),a
	call	WAIT_RESP_NO_FF	; esperar cartao
	and	$1F		; testa bits erro
	cp	5
	scf
	jp	nz,terminaLeituraEscritaBloco	; resposta errada, informar erro
	call	WAIT_RESP_NO_00	; esperar cartao
	jp	c,terminaLeituraEscritaBloco
	dec	ixh		; Next block
	jp	nz,.loop

	ld	a, (SPIDATA)	; acabou os blocos, fazer 2 dummy reads
	ld	a, (SPIDATA)
	ld	a, $FD		; enviar $FD para informar ao cartao que acabou os dados
	ld	(SPIDATA),a
	ld	a,(SPIDATA)	; 2 dummy reads
	ld	a,(SPIDATA)
	call	WAIT_RESP_NO_00	; esperar cartao
	jp	.fim		; CMD25 concluido, sair informando nenhum erro

.umBloco:
 IFDEF DEBUG
	push	af
	ld	a,'S'
	call	PRTCHAR
	pop	af
 ENDIF
	ld	a, CMD24	; gravar somente um bloco com comando CMD24 = Write Single Block
	call	SD_SEND_CMD_GET_ERROR
	jp	c,terminaLeituraEscritaBloco	; erro

 IFDEF DEBUG
	call	PRTDOT
 ENDIF
	ld	a, $FE		; mandar $FE para indicar que vamos mandar dados para gravacao
	ld	(SPIDATA),a

	ld	de,SPIDATA
	call	RUN_HLPR

	ld	(SPIDATA),de
;	ld	a,$FF		; envia dummy CRC
;	ld	(SPIDATA),a	; Can't be done with ld (SPIDATA),de. It's too fast
;	ld	(SPIDATA),a

	call	WAIT_RESP_NO_FF	; esperar cartao
	and	$1F		; testa bits erro
	cp	5
	scf
	jp	nz,terminaLeituraEscritaBloco	; resposta errada, informar erro
.esp:
	call	WAIT_RESP_NO_FF	; esperar cartao
	or	a
	jr	z,.esp
.fim:
	xor	a		; zera carry e informa nenhum erro
terminaLeituraEscritaBloco:
;	push	af
	call	disableSDs	; desabilitar todos os cartoes
;	pop	af
	ret


; ------------------------------------------------
; Ler um bloco de 512 bytes do cartao
; HL =  aponta para o inicio dos dados
; BC:DE = contem o numero do bloco (BCDE = 32 bits)
; IXH = Number of blocks
; IXL = SDcard version (0 or 1)
; Destroi AF, BC, DE, HL, IXL
; ------------------------------------------------
LerBloco:
;	ld	a, (ix+15)	; verificar se eh SDV1 ou SDV2
	ld	a,ixl
	or	a
	call	z,blocoParaByte	; se for SDV1 coverter blocos para bytes
;	call	setaSDAtual

	ld	a, (iy+WRKAREA.NUMBLOCKS)	; Get the number of blocka 
	ld	ixh,a		; ixh=Number of blocks
	dec	a
	jp	z,.umBloco	; only one block

; multiplos blocos
	ld	a, CMD18	; ler multiplos blocos com CMD18 = Read Multiple Blocks
	call	SD_SEND_CMD_GET_ERROR
	jp	c,terminaLeituraEscritaBloco
	ex	de,hl		; de=Destination address
.loop:
	call	WAIT_RESP_FE
	jp	c,terminaLeituraEscritaBloco

	ld	hl, SPIDATA
	call	RUN_HLPR

	ld	hl, (SPIDATA)	; descarta CRC
;	ld	a, (SPIDATA)
;	ld	a, (SPIDATA)
	dec	ixh
	jp	nz,.loop
	ld	a, CMD12	; acabou os blocos, mandar CMD12 para cancelar leitura
	call	SD_SEND_CMD_NO_ARGS
	jp	.fim

.umBloco:
	ld	a, CMD17	; ler somente um bloco com CMD17 = Read Single Block
	call	SD_SEND_CMD_GET_ERROR
	jp	c,terminaLeituraEscritaBloco

	call	WAIT_RESP_FE
	jp	c,terminaLeituraEscritaBloco
	ex	de,hl

	ld	hl, SPIDATA
	call	RUN_HLPR

	ld	hl, (SPIDATA)	; descarta CRC
;	ld	a, (SPIDATA)
;	ld	a, (SPIDATA)
.fim:
	xor	a		; zera carry para informar leitura sem erros
	jp	terminaLeituraEscritaBloco


; ------------------------------------------------
; Converte blocos para bytes. Na pratica faz
; BC DE = (BC DE) * 512
; ------------------------------------------------
blocoParaByte:
	ld	b, c
	ld	c, d
	ld	d, e
	ld	e, 0
	sla	d
	rl	c
	rl	b
	ret

; ------------------------------------------------
; Funcoes utilitarias
; ------------------------------------------------


; ------------------------------------------------
; Imprime string na tela apontada por DE
; Destroi todos os registradores
; ------------------------------------------------
printString:
	ld	a, (de)
	or	a
	ret	z
	call	CHPUT
	inc	de
	jr	printString


; ------------------------------------------------
; Converte o byte em A para string em decimal no
; buffer apontado por hl
; Destroi AF, BC, HL, DE
; Output: hl=pointer to the end of the buffer
; ------------------------------------------------
DecToAscii:
	ex	de,hl		; de=pointer to buffer
	ld	h, 0
	ld	l, a		; copiar A para HL
	ld	(iy+WRKAREA.TEMP),1	; flag para indicar que devemos cortar os zeros a esquerda
	ld	bc, -100	; centenas
	call	.num1
	ld	c, -10		; dezenas
	call	.num1
	ld	(iy+WRKAREA.TEMP),2	; unidade deve exibir 0 se for zero e nao corta-lo
	ld	c, -1		; unidades
	call	.num1
	ex	de,hl
	ret

.num1:
	ld	a, '0'-1
.num2:
	inc	a		; contar o valor em ascii de '0' a '9'
	add	hl, bc		; somar com negativo
	jr	c,.num2		; ainda nao zeramos
	sbc	hl, bc		; retoma valor original
	dec	(iy+WRKAREA.TEMP)	; se flag do corte do zero indicar para nao cortar, pula
	jr	nz,.naozero
	cp	'0'		; devemos cortar os zeros a esquerda. Eh zero?
	jr	nz,.naozero
	inc	(iy+WRKAREA.TEMP)	; se for zero, nao salvamos e voltamos a flag
	ret
.naozero:
	ld	(de), a		; eh zero ou eh outro numero, salvar
	inc	de		; incrementa ponteiro de destino
	ret

; ------------------------------------------------
; Converte o byte em A para string em hexa no
; buffer apontado por DE
; Destroi AF, C, DE
; ------------------------------------------------
HexToAscii:
	ld	c, a
	rra
	rra
	rra
	rra
	call	.conv
	ld  	a, c
.conv:
	and	$0F
	add	a, $90
	daa
	adc	a, $40
	daa
	ld	(de), a
	inc	de
	ret

; ------------------------------------------------
; Converte o byte em A para string em decimal e
; imprime na tela
; Destroi AF, BC, HL, DE
; ------------------------------------------------
printDecToAscii:
	ld	h, 0
	ld	l, a		; copiar A para HL
	ld	b, 1		; flag para indicar que devemos cortar os zeros a esquerda
	ld	de, -100	; centenas
	call	.num1
	ld	e, -10		; dezenas
	call	.num1
	ld	b, 2		; unidade deve exibir 0 se for zero e nao corta-lo
	ld	e, -1		; unidades
.num1:
	ld	a, '0'-1
.num2:
	inc	a		; contar o valor em ascii de '0' a '9'
	add	hl, de		; somar com negativo
	jr	c,.num2		; ainda nao zeramos
	sbc	hl, de		; retoma valor original
	djnz	.naozero	; se flag do corte do zero indicar para nao cortar, pula
	cp	'0'		; devemos cortar os zeros a esquerda. Eh zero?
	jr	nz,.naozero
	inc	b		; se for zero, nao imprimimos e voltamos a flag
	ret
.naozero:
;	push	hl		; nao eh zero ou eh outro numero, imprimir
;	push	bc
;	call	CHPUT
;	pop	bc
;	pop	hl
;	ret
	jp	CHPUT

; ------------------------------------------------
; Procura pelo nome do fabricante em uma tabela.
; A contem o byte do fabricante
; Devolve HL apontando para o buffer do fabricante
; e BC com o comprimento do texto
; Destroi AF, BC, HL
; ------------------------------------------------
pegaFabricante:
	ld	c, a
	ld	hl, tblFabricantes

.loop:
	ld	a, (hl)
	inc	hl
	cp	c
	jr	z,.achado
	or	a
	jr	z,.achado
	push	bc
	call	.achado
	add	hl, bc
	inc	hl
	pop	bc
	jr	.loop

.achado:
	ld	c, 0
	push	hl
	xor	a
.loop2:
	inc	c
	inc	hl
	cp	(hl)
	jr	nz,.loop2
	pop	hl
	ld	b, 0
	ret

tblFabricantes:
	db	1
	db	"Panasonic",0
	db	2
	db	"Toshiba",0
	db	3
	db	"SanDisk",0
	db	4
	db	"SMI-S",0
	db	6
	db	"Renesas",0
	db	17
	db	"Dane-Elec",0
	db	19
	db	"KingMax",0
	db	21
	db	"Samsung",0
	db	24
	db	"Infineon",0
	db	26
	db	"PQI",0
	db	27
	db	"Sony",0
	db	28
	db	"Transcend",0
	db	29
	db	"A-DATA",0
	db	31
	db	"SiliconPower",0
	db	39
	db	"Verbatim",0
	db	65
	db	"OKI",0
	db	115
	db	"SilverHT",0
	db	137
	db	"L.Data",0
	db	0
	db	"Generico",0


; ------------------------------------------------
; Restore screen parameters on MSX>=2 if they're
; not set yet
; ------------------------------------------------
MYSETSCR:
	ld	a,(MSXVER)
	or	a			; MSX1?
	jr	nz,.notMSX1		; No, skip
.MSX1:
	ld	a,(SCRMOD)
	or	a			; SCREEN0 already?
	ret	z			; Yes, quit
	jp	INITXT			; set screen0

.notMSX1:
	ld	c,$23			; Block-2, R#3
	ld 	ix,REDCLK
	call	EXTROM
	and	1
	ld	b,a
	ld	a,(SCRMOD)
	cp	b
	jr	nz,.restore
	inc	c
	ld 	ix,REDCLK
	call	EXTROM
	ld	b,a
	inc	c
	ld 	ix,REDCLK
	call	EXTROM
	add	a,a
	add	a,a
	add	a,a
	add	a,a
	or	b
	ld	b,a
	ld	a,(LINLEN)
	cp	b
	ret	z
.restore:
	xor	a		; Don't displat the function keys
	ld	ix,SDFSCR
	jp	EXTROM

; ------------------------------------------------
; Check if the STOP key was signaled on DRV_INIT
; ------------------------------------------------
INICHKSTOP:
	ld	a,(INTFLG)
	cp	4			; Was STOP pressed?
	ret	nz			; No, quit as fast as possible 

	; Handle STOP to pause and read messages, and ask for the copyright info
	ld	de,strBootpaused
	call	printString
.wait1:	ld	a,7
	call	SNSMAT
	and	$10			; Is STOP still pressed?
	jr	z,.wait1		; Wait for STOP to be released
	xor	a
	ld	(INTFLG),a		; Clear STOP flag
	ld	b,0			; b=inhibit 'i' key flag
.wait2: call	CHSNS
	call	nz,.chkikey		; Wait until a key is pressed
	ld	a,(INTFLG)
	cp	4			; Was STOP pressed?
	jr	nz,.wait2		; No, return
	xor	a
	ld	(INTFLG),a		; Clear STOP flag
	call	KILBUF
	ld	b,30			; Since the user is trying pause the
.wait3:	halt				; boot messages, this gives him enough
					; time to react and pause the next
					; driver
	ld	a,(INTFLG)
	cp	4			; Was STOP pressed?
	ret	z			; quit so the next driver can process it
	djnz	.wait3			; The user will have the impression
					; that he has a perfect timing.   ;)
	ret

.chkikey:
	bit	0,b			; Was the copyright message shown?
	ret	nz			; Yes, return
	call	CHGET
	cp	'i'
	jr	z,.showcopyright
	cp	'I'
	ret	nz
.showcopyright:
	inc	b			; Inhibit further presses of the i key 
	ld	de,strCopyright
	jp	printString



; ------------------------------------------------
; Install the R800 data transfer helper routine on WorkArea 
; ------------------------------------------------
INSTR800HLP:
	ld	a,(MSXVER)
	cp	3		; MSX Turbo-R?
	ret	c		; No, return
	call	GTR800LDIR
	exx
	ex	de,hl
	ld	hl,R800DATHLP
	ld	bc,R800DATHLP.end-R800DATHLP
	ldir
	ret

; ------------------------------------------------
; R800 optimized data transfer routine, copied to the WorkArea
; ------------------------------------------------
R800DATHLP:
	exx
	ld	bc,512
	ldir
	ret
.end:

; ------------------------------------------------
; Z80 optimized data transfer routine, kept in ROM 
; ------------------------------------------------
LDI512:	; Z80 optimized 512 byte transfer
	exx
	.512	ldi
	ret

; ------------------------------------------------
; Jumps to a helper routine, usually in RAM
; Input: HL': Address of the target routine
; ------------------------------------------------
RUN_HLPR:
	exx
	jp	(hl)

; ------------------------------------------------
; Setup the arbitrary block size LDIR helper to be used
; Input   : none
; Output  : HL': Address of the block transfer routine to be used 
; Modifies: AF, DE', HL'
; ------------------------------------------------
SETLDIRHLPR:
	exx
	; Check for a Z80 or R800
	xor	a		; Clear Cy
	dec	a		; A=#FF
	db	#ED,#F9		; mulub a,a
	jr	c,.useLDIR	; Always use LDIR in RAM for the R800

	ld	hl,LDI512
	exx
	ret

.useLDIR:
	exx
	; ***Continues on GTR800LDIR

; ------------------------------------------------
; Obtain the pointer to the R800 data transfer helper routine
; Input   : IY=Pointer to the WorkArea
; Output  : HL=pointer to R800 data transfer helper routine 
; Modifies: DE
;	    - Does an exx at the end
; ------------------------------------------------
GTR800LDIR:
	push	iy
	pop	hl			; hl=WorkArea
	ld	de,WRKAREA.TRLDIR
	add	hl,de
	exx
	ret


; ------------------------------------------------
; Debugging routines
; ------------------------------------------------
 IFDEF DEBUG
PRTCHAR:
	push	ix,iy
	ld	ix,CHPUT
	ld	iy,(EXPTBL-1)
	call	CALSLT
	pop	iy,ix
	ret
PRTDOT:
	push	af
	ld	a,'.'
	call	PRTCHAR
	pop	af
	ret
PRTDASH:
	push	af
	ld	a,'-'
	call	PRTCHAR
	pop	af
	ret
 ENDIF



; ==========================================================================
strTitle:
	db	13,"FBLabs SDHC driver v",27,'J'
	BYTE2STR VER_MAIN
	db	'.'
	BYTE2STR VER_SEC
	db	'.'
	BYTE2STR VER_REV
	db	13,10,0

;		 |-------------39 chars----------------|
strBootpaused:
	db	"Paused. Press <i> for the copyright info",13,10,0

strCopyright:
	db	"(c) 2014 Fabio Belavenuto",13,10
	db	"(c) 2017 FRS",13,10
	db	"Licenced under CERN OHL v1.1",13,10
	db	"http://ohwr.org/cernohl",13,10
	db	"PCB designed by Luciano Sturaro",13,10
	; will use the CR+LF+EOS bellow 
strCrLf:
	db	13,10,0
strCartao:
	db	"- Slot ",0
strVazio:
	db	"Empty",13,10,0
strNaoIdentificado:
	db	"Unknown!",13,10,0
;		 |-------------39 chars----------------|
strMr_mp_desativada:
	db	"- Slot expander & Mem Mapper disabled",13,10,0
 IFDEF HASMEGARAM
strMapper:
	db	"- Slot expander & Mem Mapper enabled",13,10,0
strMegaram:
	db	"- Slot expander & MegaRAM enabled",13,10,0
 ELSE
strDrvMain:
	db	"- Main driver selected",13,10,0
strDrvDev:
	db	"- Development driver selected",13,10,0
 ENDIF
strSDV1:
	db	"SDV1, ",0
strSDV2:
	db	"SDV2, ",0

;-----------------------------------------------------------------------------
;
; End of the driver code

DRV_END:

;	ds	3ED0h-(DRV_END-DRV_START), $FF
	ds	$7B00-$, #FF

