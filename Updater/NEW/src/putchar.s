;
;Copyright (c) 2017 FBLabs
;
;This program is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

	.module putchar
	.optsdcc -mz80

	.area _CODE

CALSLT	= 0x001C
CHPUT	= 0x00A2
EXPTBL	= 0xFCC1				; slotids

;-------------------------------------------------------------------------------
; void putchar(char c)
;-------------------------------------------------------------------------------
_putchar::
	ld		iy, #0
	add		iy, sp
	ld		a, 2(iy)
	ld		ix, #CHPUT
	ld		iy, (EXPTBL-1)
	jp		CALSLT
