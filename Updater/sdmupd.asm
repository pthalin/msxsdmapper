; Projeto MSX SD Mapper

; Copyright (c) 2014
; Fabio Belavenuto
;
; Baseado no c�digo FL2 vers�o 2.2 de 29-12-2002  (c) Ramones 2002
;
;
; This documentation describes Open Hardware and is licensed under the CERN OHL v. 1.1.
; You may redistribute and modify this documentation under the terms of the
; CERN OHL v.1.1. (http://ohwr.org/cernohl). This documentation is distributed
; WITHOUT ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING OF MERCHANTABILITY,
; SATISFACTORY QUALITY AND FITNESS FOR A PARTICULAR PURPOSE.
; Please see the CERN OHL v.1.1 for applicable conditions


	output	"sdmupd.com"

	include	"bios.inc"
	include	"bdos.inc"


	org		COM_START_ADDR

; *** CONSTANTS ***
ALG_BYTE	= 1
ALG_PAGE	= 2
CHGBANK		= $7000
BUFFER1		= $1000
BUFFER2		= $C000

begin:

; **** MAIN PROGRAM ****

init:
	; Actual parameters saved
	ld		(savesp), sp				; Stack Pointer saved
	ld		a, (CSRSW)					; Cursor Saved
	ld		(savecursor), a
	call	cursorOff         			; Turn cursor off (no matter about its state)

	ld		de, strTitulo
	call	print						; Print logo

	call	checkDOS					; Dos version checked
	call	checkSystem					; MSX Type checked
	call	checkParams					; Check and Save parameters

	ld		a, (options)
	bit		1, a						; only list chips?
	jp nz,	showList					; yes, jump

	call	checkFlash     				; Search FLASH ROM

	ld		a, (options)
	bit		0, a						; only erasing flash?
	jp nz,	eraseFlash					; yes, jump to erase it

	call	checkFile					; Checks if file-argument exists and your size
	call	eraseFlash					; Erase flash.
	call	loadFile 					; Load file in flash

	jp		exitok						; No. Exit Program


; *** CHECKING ROUTINES ***

; -------------------
; checkFlash
; Searches FLASH ROM
; ------------------
checkFlash:
	ld		de, strProcuraFlash
	call	print						; Prints searching message
	ld		a, $FF
	ld		(thisslt), a				; Inits SIGSLOT Routine
.loop:
	di
	call	sigslot						; Calls the next slot (first one if first time)
	cp		$FF							; Is it the last slot?
	jr z,	.naoachado					; Yes. FLASH was not found

	push	af
	ld		h, $40						; It is not the last slot. Placed it in page 1
	call	ENASLT
	pop		af
	ld		h, $80						; Placed it in page 2 too
	call	ENASLT

	call	checkDeviceID				; Searching flash by executing its ID_CHECK command
	jr c,	.loop						; Not found in this slot, continue with next one

	ld		a, (thisslt)				; FLASH WAS FOUND
	ld		(flashslt), a				; Slot saved
	push	af 							; For printing the message of slot / subslot
	and		3
	add		a, '0'						; ASCII conversion of the Slot
	ld		(strAchado.slot), a
	pop		af
	rrca
	rrca
	and		3
	add		a, '0'
	ld		(strAchado.subslot), a		; ASCII conversion of the Subslot.

	ld		a, (RAMAD1)
	ld		h, $40
	call	ENASLT						; Restoring page 1 (Memory again)
	ld		a, (RAMAD2)
	ld		h, $80
	call	ENASLT						; Restoring page 2 (Memory again)

	ld		de, strAchado
	call	print						; Printing message with info about slot / subslot
	ld		de, (flashManPoint)			; recuperamos string do fabricante
	call	print
	ld		de, (flashProdPoint)		; recuperamos string do produto
	call	print						; e imprimimos
	ld		de, strCrLf
	jp		print

.naoachado:								; FLASH NOT FOUND
	ld		a, (RAMAD1)
	ld		h, $40
	call	ENASLT						; Memory placed
	ld		a, (RAMAD2)
	ld		h, $80
	call	ENASLT						; Memory placed
	ld		de, strNaoAchado
	jp		printErro					; Error message, exiting program

; ------------------------
; checkDeviceID
; Check Flash Manufacturer and Device ID
; Z = 1 if Flash found
;
; -------------------------
checkDeviceID:
	di
	ld		a, $F0						; Reset
	call	flashSendCommand
	ld		a, $90						; Software ID Entry
	call	flashSendCommand
	ld		a, ($8000)					; Read Manufacturer ID
	ld		(flashIdMan), a
	ld		a, ($8001)					; Read Product ID
	ld		(flashIdProd), a
	call	flashIdent					; Try to identify flash
	ld		a, (flashAlg)
	cp		0
	scf									; Carry = 1 - error
	jr z,	.sair
.ok:
	xor		a							; Carry = 0 - OK
.sair:
	push	af
	ld		a, $F0						; Reset
	call	flashSendCommand
	pop		af
	ei
	ret

; ----------------------
; CHECK DOS
; check operating system
; ----------------------
checkDOS:
	ld		c, _DOSVER
	call	callBdos					; Send DOSVER command to dos
	ld		a, b
	ld		(dos), a					; Save dos version
	ret

; ----------------
; checkSystem
; check MSX
; ----------------
checkSystem:
	ld		a, (EXPTBL)
	ld		hl, BASVER
	call	RDSLT						; Check byte $2D (MSX version)
	ld		(system), a
	cp		3							; Turbo R?
	ret	c								; no, return
	ld		iy, (EXPTBL - 1)			; is MSX Turbo R
	ld		ix, $183
	call	CALSTL
	or		$80
	ld		(savecpu), a				; save actual CPU mode
	ld		a, $80						; and set Z80 for compatibility

systemSetCPU:
	ld		iy, (EXPTBL - 1)
	ld		ix, $180
	call	CALSTL
	ret

; --------------------
; checkParams
; Check line params
; --------------------
checkParams:
	ld		hl, CMD_LENGTH
	ld		a, (hl)
	or		a							; Parameters?
	ld		de, strHelp
	jp z,	printErro					; no parameters. Show Help and exit

	call	checkOptions				; Check parameter options
	call	checkFileName				; Check file name

	ld		a, (dos)
	cp		2
	jp nc,	fillName					; If DOS2 then fill only the name
	ld		hl, fileNameDOS2			; DOS1. Extract filename
	ld		de, fileNameDOS1
.loop0:
	ld		a,(hl)
	cp		'.'
	jr z,	.loop2
	ld		(de), a
	inc		de
.loop1:
	inc		hl
	djnz	.loop0
	jp		fillName
.loop2:
	ld		de, filenDOS1Ext
	ld		b, 4
	jr		.loop1

; -------------------
; checkFileName
; Extract the name of
; the parameters in
; DOS2 format
; ------------------
checkFileName:
	ld		a, (CMD_LENGTH)
	cp		1
	ld		b, a
	jr z,	.p1
	dec		b
.p1:
	ld		hl, CMD_LINE
	ld		de, fileNameDOS2
	ld		c, 0
.loop0:
	ld		a, (hl)
	or		a
	jr z,	.p2
	cp		'/'
	jp z,	.p3
	cp		' '
	jp z,	.p2
	cp		13
	jr z,	.p2
	ld		(de),a
	inc		de
	inc		c
.p2:
	dec		hl
.p3:
	inc		hl
	inc		hl
	djnz	.loop0
	ld		b, c
	ld		a, c
	or		a
	ret	nz
	ld		b, 1
	ret

; --------------------------------------
; checkOptions
; OPTIONS :
;			0 - erase flash only	/e
;			1 - show list of chips	/l
;
; ---------------------------------------
checkOptions:
	push	bc
	xor		a
	ld		(options), a
	ld		a, (CMD_LENGTH)
	ld		b, a
	ld		hl, CMD_LINE
.loop0:
	ld		a, (hl)
	cp		'/'
	call	z, .ch1
.p1:
	inc		hl
	djnz	.loop0
	pop		bc

.ch1:
	inc		hl
	ld		a, (hl)
	or		000100000b
	ld		de, strHelp
	cp		'?'
	jp z,	printErro
	cp		'h'
	jp z,	printErro

	ld		c, 1
	cp		'e'
	jr z,	.achado
	sla		c
	cp		'l'
	jr z,	.achado
	ret
.achado:
	ld		a, (options)
	or		c
	ld		(options), a
	ret

; ---------------------
; CHECK FILE
; CHECK IF FILE EXIST
; ---------------------
checkFile:
	ld		de, strOpenFile
	call	print						; Open text
	ld		a, (dos)
	cp		2
	jp nc,	.dos2						; DOS2 mode

	call	makeFCB						; DOS1. Make FCB and Open command.
	ld		hl, fileNameDOS1
	call	buildFCB
	call	open						; Open. If file doesn't exist then error and exit.
	jp		.p1

.dos2:									; DOS2 Open
	ld		de, fileNameDOS2
	xor		a
	ld		b, 0
	ld		c, _OPEN					; Open file DOS2
	call	callBdos
	ld		de, strErroAbrirArq
	jp nz,	printErro					; if Z = 0 then file not found. Error and Exit
	ld		a, b
	ld		(fileHandle), a				; Save FILE HANDLE
	push	bc							; For check size in DOS 2, use SEEK Command.
	ld		a, SEEK_END
	ld		de, 0
	ld		hl, 0
	ld		c, _SEEK					; Seek
	call	callBdos
	ld		(sizefile), hl
	ld		(sizefile+2), de
	pop		bc
	xor		a							; And Now Return Seek to POS 0 (SEEK_SET)
	ld		de, 0
	ld		hl, 0
	ld		c, _SEEK
	call	callBdos
.p1:
	ld		de, strTamanhoErrado		; Test filesize. 128K = 00 00 02
	ld		a, (sizefile)
	or		a
	jp nz,	printErro
	ld		a, (sizefile+1)
	or		a
	jp nz,	printErro
	ld		a, (sizefile+2)
	cp		2
	jp nz,	printErro
	ret

; *** PROGRAM ROUTINES ***

; ----------------------------
; LOADFILE
; Load File into Flash
; Load ALL FILE into Flash ROM
; ----------------------------
loadFile:
	ld		de, strWriting
	call	print						; Show Write Text
	ld		b, 8						; 8 blocks of 16K = 128K
.loop:
	push	bc
	call	fillBuffers					; fill buffers with $FF
	call	load16K						; and load one 16 K page to buffers
	di
	call	writeFlash					; now write this 16 K to FLASH
	ei
	ld		de, strErroAoGravarFlash
	jp nz,	printErro					; oops! Error writing bytes. Show Error and exit.

	ld		de, strDot
	call	print						; show '*' for 16 K page loaded
	pop		bc
	ld		a, (actualpage)
	inc		a
	ld		(actualpage), a				; and inc page to next loop
	djnz	.loop						; next loop

	call	closeFile					; end for load. close File.
	ld		de, strCrLf
	call	print
	ld		de, strUpdateCompleto		; and print Success Text.
	jp		print


; --------------------
; FILLBUFFERs
; Fill buffers with $FF
; ---------------------
fillBuffers:
	ld		hl, BUFFER1
	ld		de, BUFFER1+1
	ld		bc, $2FFF
	ld		(hl), $FF
	ldir								; fill
	ld		hl, BUFFER2
	ld		de, BUFFER2+1
	ld		bc, $0FFF
	ld		(hl), $FF
	ldir								; fill
	ret

; ------------------
; LOAD16K
; Load 16K to buffers
; from file open
; ------------------
load16K:
	; this code load 16 KB from file
	; to buffers
	ei
	ld		de, BUFFER1					; buffer pointer
	ld		hl, $3000					; length
	ld		a, (dos)
	cp		2
	jr nc,	.dos2_1						; if DOS 2 make <> load
	push	hl							; DOS 1 LOAD.
	call	setDTA						; set DTA
	pop		hl
	call	readMax						; and read 8 KB or max if file < 8K or file size
	call	readFile					; is not mult. of 16. And READ.
	jr		.next
.dos2_1:								; DOS 2 LOAD
	ld		a, (fileHandle)
	ld		b, a
	ld		c, _READ					; DOS 2 COMMAND Read
	call	callBdosCE
.next:
	ld		de, BUFFER2					; buffer pointer
	ld		hl, $1000					; length
	ld		a, (dos)
	cp		2
	jr nc,	.dos2_2						; if DOS 2 make <> load
	push	hl							; DOS 1 LOAD.
	call	setDTA						; set DTA
	pop		hl
	call	readMax						; and read 8 KB or max if file < 8K or file size
	jp		readFile					; is not mult. of 16. And READ.
.dos2_2:								; DOS 2 LOAD
	ld		a, (fileHandle)
	ld		b, a
	ld		c, _READ					; DOS 2 COMMAND Read
	jp		callBdosCE

; -----------------
; FILLNAME
; Search params for
; extract fileName
; -----------------
fillName:
	ld		hl, fileNameDOS2 + 63
	ld		b, 64						; max params. End to init loop
.loop:
	ld		a,(hl)
	or		a
	jr z,	.p1							; 0 dec
	cp		':'							; end
	jr z,	.p2
	cp		'\\'						; params
	jr z,	.p2
.p1:
	dec		hl
	djnz	.loop
.p2:
	inc		hl
	ld		de, strOpenFile.filename		; loop for fill fileNameShow
.loop2:
	ld		a, (hl)
	or		a
	ret z
	ld		(de),a
	inc		hl
	inc		de
	jr		.loop2

; -------------------------------------------------------
; SIGSLOT
; Returns in A the next slot every time it is called.
; For initializing purposes, thisslt has to be #FF.
; If no more slots, it returns A=#FF.
; this code is programmed by Nestor Soriano aka Konamiman
; --------------------------------------------------------
sigslot:
	ld		a, (thisslt)				; Returns the next slot, starting by
	cp		$FF							; slot 0. Returns #FF when there are not more slots
	jr nz,	.p1							; Modifies AF, BC, HL.
	ld		a, (EXPTBL)
	and		%10000000
	ld		(thisslt), a
	ret
.p1:
	ld		a, (thisslt)
	cp		%10001111
	jr z,	.nomaslt
	cp		%00000011
	jr z,	.nomaslt
	bit		7, a
	jr nz,	.sltexp
.p2:
	and		%00000011
	inc		a
	ld		c, a
	ld		b, 0
	ld		hl, EXPTBL
	add		hl, bc
	ld		a, (hl)
	and		%10000000
	or		c
	ld		(thisslt), a
	ret
.sltexp:
	ld		c, a
	and		%00001100
	cp		%00001100
	ld		a, c
	jr z,	.p2
	add		a, %00000100
	ld		(thisslt), a
	ret
.nomaslt:
	ld		a, $FF
	ret

; *** FLASH ROUTINES ***

; ----------------------
; flashSendCommand
; send command in A to 
; flash mapped in page 2
; Preserve flags
; ----------------------
flashSendCommand:
	push	hl
	push	af
	ld		a, $01
	ld		(CHGBANK), a				; Selects bank 1
	ld		hl, $9555					; Write $AA to flash absolute address $5555
	ld		(hl), $AA
	ld		a, $00
	ld		(CHGBANK), a				; Selects bank 0
	ld		hl, $AAAA					; Write $55 to flash absolute address $2AAA
	ld		(hl), $55
	ld		a, $01
	ld		(CHGBANK), a				; Selects bank 1
	ld		hl, $9555					; Write command to flash absolute address $5555
	pop		af
	ld		(hl), a
	pop		hl
	ret

; ----------------------
; WRITEFLASH
; Write 16 KB into Flash
; NZ = Error
; ----------------------
writeFlash:
	ld		a, (flashslt)
	ld		h, $40
	call	ENASLT						; flash to frame 1
	ld		a, (flashslt)
	ld		h, $80
	call	ENASLT						; flash to frame 2
	ld		hl, BUFFER1					; buffer pointer
	ld		de, $8000					; flash pointer
	ld		bc, $3000					; 12K
.loop1:
	ld		a, $A0						; Flash Write Byte Command
	call	flashSendCommand
	ld		a, (actualpage)
	ld		(CHGBANK), a				; select 16K bank in frame 2
	call	writeByte					; program byte
	jr nz,	.exit						; ERROR.
	inc		hl							; Ok. Next byte.
	inc		de
	dec		bc
	ld		a, b
	or		c
	jp nz,	.loop1						; All programmed? Return for making next block.

	ld		hl, BUFFER2					; buffer pointer
	ld		bc, $1000					; 4K
.loop2:
	ld		a, $A0						; Flash Write Byte Command
	call	flashSendCommand
	ld		a, (actualpage)
	ld		(CHGBANK), a				; select 16K bank in frame 2
	call	writeByte					; program byte
	jr nz,	.exit						; ERROR.
	inc		hl							; Ok. Next byte.
	inc		de
	dec		bc
	ld		a, b
	or		c
	jp nz,	.loop2						; All programmed? Return for making next block.
.exit:
	push	af
	ld		a, (RAMAD1)
	ld		h, $40
	call	ENASLT						; Mem to PAGE 1
	ld		a, (RAMAD2)
	ld		h, $80
	call	ENASLT						; Mem to PAGE 2
	pop		af
	ret

; program byte and check 30 times
writeByte:
	push	hl
	push	de
	push	bc
	ld		c, 30
.loop2:									; write byte loop
	ld		a, (hl)
	ld		(de), a
.loop3:
	nop									; little delay
	nop
	dec		c
	ld		a, (de)
	xor		(hl)
	jr z,	.fim						; ok programmed
	ld		a, c
	or		a
	jr nz,	.loop3
	inc		a							; oops! Error
.fim:
	pop		bc
	pop		de
	pop		hl
	ret

; -----------------------
; ERASEFLASH
; Sector Erase command
; or CHIP Erase Command
; -----------------------
eraseFlash:
	ld		de, strEraseFlash
	call	print						; Erase Text Show.
	ld		a, (flashslt)
	ld		h, $40
	call	ENASLT						; set flash in page 1
	ld		a, (flashslt)
	ld		h, $80
	call	ENASLT						; and page 2
	di
	ld		a, $80						; Erase all command
	call	flashSendCommand
	ld		a, $10
	call	flashSendCommand
	ld		hl, $8000
	ld		a, (hl)
	ld		(togglebit), a
.loop:									; test finish by Toggle Bit
	ld		a, (togglebit)
	cp		(hl)
	ld		a, (hl)
	ld		(togglebit), a
	jr nz,	.loop
	ld		a, $F0						; end command. Reset Command.
	call	flashSendCommand
	ei
	ld		a, (RAMAD1)
	ld		h, $40
	call	ENASLT						; set mem to page 1
	ld		a, (RAMAD2)
	ld		h, $80
	call	ENASLT						; set mem to page 2
	ld		de, strOk
	call	print
	ld		a, (options)
	bit		0, a						; check if /e command.
	ret	z								; No. Return to Main program
	jp		exitok						; Yes. Exit.


; *** GENERIC SYSTEM ROUTINES ***

; -------------
; CURSOR ON OFF
; -------------
cursorOn:
	ld		a, 1

cursorSet:
	ld		(CSRSW), a
	ret

cursorOff:
	xor		a
	jr		cursorSet


; ----------------
; cmpHLcomDE
; Compares HL and DE
; BIOS RST 020H clone
; -----------------
cmpHLcomDE:
	ld		a, h
	sub		d
	ret		nz
	ld		a, l
	sub		e
	ret

; ---------------
; BDOSCE
; bdos with error
; Call to BDOS and
; check error
; ----------------
callBdosCE:
	call	BDOS
	or		a
	jp nz,	error
	ret

; -----------------
; BDOS
; bdos without error
; ------------------
callBdos:
	call	BDOS
	or		a
	ret

; -----------------------------------------------
; ERROR
; print error
; call printErro and set DE pointer to error text
; -----------------------------------------------
error:
	ld		de, strErro
printErro:
	ld		c, _STROUT
	call	callBdos

; ---------------
; EXITOK
; Exit OK
; ---------------
exitok:
	call	closeFile					; close file (open or not)
	ld		a, (RAMAD1)
	ld		h, $40
	call	ENASLT						; set mem to page 1
	ld		a, (RAMAD2)
	ld		h, $80
	call	ENASLT						; set mem to page 2
	ld		a, (system)
	cp		3							; Turbo R?
	jr c,	exit						; No. Go To Exit
	ld		a, (savecpu)				; Yes. Restore CPU Mode
	call	systemSetCPU

; ---------------
; EXIT
; program exit
; ---------------
exit:
	ld		a,(savecursor)				; Restore Cursor Value
	call	cursorSet
	ld		sp, (savesp)				; And Restore SP
	ld		c, _TERM0					; Program Terminate BDOS Command.
	jp		callBdos					; Go and Exit. WOW!

; *** BDOS ROUTINES ***

; -------------------
; PRINT
; DE : Pointer to Text
; --------------------
print:
	ld		c, _STROUT
	jp		callBdos

; ----------------
; makeFCB
; Prepare NEW FCB
; ---------------
makeFCB:
	ld		hl, FCB
	ld		de, FCB+1
	ld		(hl), 0
	ld		bc, 37
	ldir
	ret

; -------------
; buildFCB
; HL = Pointer to namefile string
; ----------------
buildFCB:
	ld		de, fileName
	ld		bc, 11
	ldir
	ret

; ---------------
; OPEN FCB FILE
; ---------------
open:
	ld		de, FCB
	ld		c, _FOPEN
	call	callBdos
	ld		de, strErroAbrirArq
	jp nz,	printErro
	ld		ix, FCB
	ld		a, 1
	ld		(ix+14), a
	xor		a
	ld		(ix+15), a
	ld		(ix+33), 0
	ld		(ix+34), 0
	ld		(ix+35), 0
	ld		(ix+36), 0
	ret

; --------------
; SEEK
; DEHL = Pointer
; --------------
seek:
	ld		ix, FCB
	ld		(ix+33), l
	ld		(ix+34), h
	ld		(ix+35), e
	ld		(ix+36), d
	ret

; ------------
; setDTA
; DE = Buffer
; ------------
setDTA:
	ld		c, _SETDTA
	jp		callBdos

; -------------------------------------------------------
; READ
; HL Bytes to read from opened FCB, with the pointer ready
; -------------------------------------------------------
readFile:
	ld		de, FCB
	ld		c, _RDBLK
	jp		callBdosCE

; ----------------
; closeFile
; close FCB FILE
; close File. Check DOS variable for DOS2 and DOS1 close
; ----------------
closeFile:
	ld		a, (dos)
	cp		2
	jr nc,	.dos2
	ld		de, FCB
	ld		c, _FCLOSE
	jp		callBdos
.dos2:
	ld		a, (fileHandle)
	ld		b, a
	ld		c, _CLOSE
	jp		callBdos

; -------------------------
; READ MAX
; HL : Size to read
; Returns HL max size
 ; this code is for DOS1 Compatibility
 ; In DOS2 if HL parameter (Read Size) > Size File or Bytes to read
 ; Do not Return ERROR
 ; But DOS1 RETURN ERROR
 ; The Load16K Code USES HL = 16384 bytes ALWAYS.
; -------------------------
readMax:
	push	af
	push	de
	push	hl
	xor		a
	ld		hl, (sizefile+2)
	ld		de, (sizeread+2)
	sbc		hl, de
	ld		(sizefiletmp+2), hl
	ld		hl, (sizefile)
	ld		de, (sizeread)
	sbc		hl, de
	ld		(sizefiletmp), hl
	pop		hl
	push	hl
	ld		de, (sizefiletmp)
	call	cmpHLcomDE
	jr nc,	.readmax0
	pop		hl
.readmaxend:
	pop		de
	pop		af
	ret
.readmax0:
	ld		de, (sizefiletmp+2)
	ld		hl, 0
	call	cmpHLcomDE
	pop		hl
	jr nz,	.readmaxend
.readmax1:
	ld		hl, (sizefiletmp)
	jr		.readmaxend

; ----------------------
; ShowList
; Exibe a lista de flashs
; suportadas
; ----------------------
showList:
	ld		de, strListaCab
	call	print
	ld		hl, tblFlash				; HL points to table start
.loop:
	ld		a, (hl)
	cp		0							; no more entries?
	jp z,	exit
	ld		(flashIdMan), a				; save manufacturer ID
	inc		hl
	ld		a, (hl)						; get product ID
	ld		(flashIdProd), a			; save it
	inc		hl
	ld		de, (hl)					; Get string pointer of manufacturer
	ld		(flashManPoint), de			; save it
	inc		hl
	inc		hl
	ld		de, (hl)					; Get string pointer of product
	ld		(flashProdPoint), de		; save it
	inc		hl
	inc		hl
	ld		a, (hl)						; Get algorythm
	ld		(flashAlg), a
	inc		hl
	push	hl
	ld		de, (flashManPoint)
	call	print
	ld		de, (flashProdPoint)
	call	print
	ld		de, strCrLf
	call	print
	pop		hl
	jr		.loop
	ret

; ----------------------
; FlashIdent
; Identifies Flash
; ----------------------
flashIdent:
	push	hl
	push	bc
	push	de
	ld		hl, tblFlash				; HL points to table start
	ld		a, (flashIdMan)
	ld		b, a						; Manufacturer ID in B
	ld		d, 0
.loop:
	ld		a, (hl)
	cp		0							; no more entries?
	jr z,	.naoId						; nop, unsucessfull!
	cp		b							; compares manufacturer ID
	jr z,	.idp						; Ok, detects product ID
	ld		e, 7
	add		hl, de						; no matches, next entry
	jr		.loop
.idp:
	inc		hl							; compares product ID
	ld		a, (flashIdProd)
	cp		(hl)						; equal?
	jr z,	.ok							; Yes, sucessful
	ld		e, 6
	add		hl, de						; no matches, next entry
	jr		.loop
.ok:
	inc		hl
	ld		de, (hl)
	ld		(flashManPoint), de
	inc		hl
	inc		hl
	ld		de, (hl)
	ld		(flashProdPoint), de
	inc		hl
	inc		hl
	ld		a, (hl)
	ld		(flashAlg), a
.naoId:
	pop		de
	pop		bc
	pop		hl
	ret

; *** TEXTS ***

strTitulo:
	.db		"SD Mapper flash programmer utility"
	.db		13, 10
	.db		"(c) 2014 by FBLabs"
	; fall throw

strCrLf:
	.db		13, 10, '$'

strHelp:
	.db		13, 10
	.db		"Usage:", 13, 10
	.db		"     sdmupd /opts <filename.ext>", 13, 10
	.db		"Example: sdmupd DRIVER.ROM", 13, 10
	.db		"         sdmupd /e", 13, 10
	.db		13, 10
	.db		"Options:", 13, 10
	.db		"     /h : Show this help.", 13, 10
	.db		"     /l : Show list of supported chips.", 13, 10
	.db		"     /e : Only erase flash and exit.", 13, 10
	.db		'$'

strProcuraFlash:
	.db		"Searching SD Mapper in system ...", 13, 10
	.db		'$'

strNaoAchado:
	.db		"Oops! SD Mapper not Found!!", 13, 10
	.db		'$'

strAchado:
	.db		"Found in slot "
.slot:
	.db		'0'
	.db		" subslot "
.subslot:
	.db		'0:', 13, 10
	.db		'$'

strTraco:
	.db		" - $"

strOk:
	.db		" OK!", 13, 10
	.db		'$'

strErro:
	.db		13, 10
	.db		"ERROR (BDOS)!!!!", 13, 10
	.db		'$'

strErroAbrirArq:
	.db		13, 10
	.db		"ERROR: Problems opening file ...", 13, 10
	.db		'$'

strTamanhoErrado:
	.db		13, 10
	.db		"ERROR: File size must be 128KB"
	.db		13, 10, '$'

strOpenFile:
	.db		13, 10
	.db		"Open file : "
.filename:
	.db		"                "
	.db		13, 10, '$'

strEraseFlash:
	.db		13, 10
	.db		"Erasing Flash "
	.db		'$'

strDot:
	.db		'*$'

strWriting:
	.dw 	13, 10
	.db		"Writing: "
	.db 	'$'

strErroAoGravarFlash:
	.db		13, 10
	.db		"ERROR: Writing Flash ...", 13, 10
	.db		'$'

strUpdateCompleto:
	.db		13, 10
	.db		"Flash programmed succesfully.", 13, 10
	.db		'$'

strListaCab:
	.db		13, 10
	.db		"List of supported flash chips:", 13, 10
	.db		"------------------------------", 13, 10
	.db		'$'

; *** TABLES ***
; AT49F002    = $1F $07		alg byte
; AT49F002T   = $1F $08		alg byte
; AT49(H)F010 = $1F $17		alg byte
; AT29C010A   = $1F $D5		alg 128-page
; AM29F010    = $01 $20		alg byte
; SST29EE010  = $BF $07		alg 128-page
; SST39SF020  = $BF $B6		alg byte
; W49F002U/N  = $DA $0B		alg byte
; W49F002B    = $DA $25		alg byte
; W39F010     = $DA $A1		alg byte
; 

tblFlash:
	.db		$1F, $07
		.dw strAtmel
		.dw strAt49f002
		.db ALG_BYTE
	.db		$1F, $08
		.dw strAtmel
		.dw	strAt49F002t
		.db ALG_BYTE
	.db		$1F, $17
		.dw strAtmel
		.dw	strAt49f010
		.db ALG_BYTE
	.db		$1F, $D5
		.dw strAtmel
		.dw strAt29c010a
		.db ALG_PAGE
	.db		$01, $20
		.dw strAMD
		.dw	strAm29F010
		.db	ALG_BYTE
	.db		$BF, $07
		.dw	strSST
		.dw	strSst29ee010
		.db	ALG_PAGE
	.db		$BF, $B6
		.dw	strSST
		.dw	strSst39sf020
		.db	ALG_BYTE
	.db		$DA, $0B
		.dw	strWinb
		.dw	strW49f002un
		.db	ALG_BYTE
	.db		$DA, $25
		.dw	strWinb
		.dw	strW49f002b
		.db	ALG_BYTE
	.db		$DA, $A1
		.dw	strWinb
		.dw	strW39f010
		.db	ALG_BYTE
	.db		0

strAtmel:
	.db		"Atmel $"
strAMD:
	.db		"AMD $"
strSST:
	.db		"SST $"
strWinb:
	.db		"Winbond $"
strAt49f002:
	.db		"AT49F002$"
strAt49F002t:
	.db		"AT49F002T$"
strAt49f010:
	.db		"AT49F010$"
strAt29c010a:
	.db		"AT29C010A$"
strAm29F010:
	.db		"AM29F010$"
strSst29ee010:
	.db		"SST29EE010$"
strSst39sf020:
	.db		"SST39SF020$"
strW49f002un:
	.db		"W49F002U/N$"
strW49f002b:
	.db		"W49F002B$"
strW39f010:
	.db		"W39F010$"

; *** VARIABLES ***

savesp:			.dw	0					; stack pointer
savecpu:		.db	0					; cpu mode in Turbo R
savecursor:		.db	0					; cursor
dos:			.db	0					; dos version
system:			.db	0					; msx version
options:		.db	0					; options variable 1
pages:			.db	0					; 16 Kb Pages
actualpage:		.db	0					; Temporal page for load
thisslt:		.db	0FFh				; sigslot flag
flashslt:		.db	0					; slot for flash
flashIdMan:		.db 0					; Flash Manufacturer ID
flashIdProd:	.db 0					; Flash Product ID
flashAlg		.db 0					; Flash algorithm
flashManPoint:	.dw 0					; Flash manufacturer string pointer
flashProdPoint:	.dw	0					; Flash product string pointer
;flashHalf:		.db	0					; What flash half is writing
togglebit:		.db 0					; tmp variable for Flash Toggle Bit test
sizefiletmp:	.ds	4					; tmp variable for READMAX Code
fileHandle:		.db	0					; DOS 2 File Handle
fileNameDOS2:	.ds	64					; Tmp for fileName
fileNameDOS1:	.db "        "			; DOS1 fileName for DOS1 Code
filenDOS1Ext:	.db	"   "
				.ds	4

; *** FCB DOS 1 ***
FCB:
unidad:		.db	0
fileName:	.ds	8
extname:	.ds	3
			.dw	0
registro:	.dw	0
sizefile:	.ds	4
			.ds	13
sizeread:
			.ds	4
			.db	0
