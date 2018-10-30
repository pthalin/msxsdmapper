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

	.module getchar
	.optsdcc -mz80

; Constants
BDOS		= 0x0005				; BDOS entry point

_INNOE		= 0x08					; CONSOLE INPUT WITHOUT ECHO


	.area _CODE

;-------------------------------------------------------------------------------
; char getchar(void)
;-------------------------------------------------------------------------------
_getchar::
	ld		c, #_INNOE
	call	BDOS
	ret
