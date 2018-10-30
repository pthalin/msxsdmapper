/*
Copyright (c) 2017 FBLabs

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "types.h"
#include "conio.h"
#include "bios.h"

//------------------------------------------------------------------------------
void puts(char *s)
{
	while (*s != 0) {
		putchar(*s);
		s++;
	}
}

//------------------------------------------------------------------------------
static void puthex(int8_t nibbles, uint16_t v)
{
	int8_t i = nibbles - 1;
	while (i >= 0) {
		uint16_t aux = (v >> (i << 2)) & 0x000F;
		uint8_t n = aux & 0x000F;
		if (n > 9)
			putchar('A' + (n - 10));
		else
			putchar('0' + n);
		i--;
	}
}

//------------------------------------------------------------------------------
void puthex8(uint8_t v)
{
	puthex(2, (uint16_t) v);
}

//------------------------------------------------------------------------------
void puthex16(uint16_t v)
{
	puthex(4, v);
}

//------------------------------------------------------------------------------
static void putdec(uint16_t digits, uint16_t v)
{
	uint8_t fz = 0;
	while (digits > 0) {
		uint16_t aux = v / digits;
		uint8_t n = aux % 10;
		if (n != 0 || fz != 0) {
			putchar('0' + n);
			fz = 1;
		}
		digits /= 10;
	}
	if (fz == 0) {
		putchar('0');
	}
}

//------------------------------------------------------------------------------
void putdec8(uint8_t v)
{
	putdec(100, (uint16_t) v);
}

//------------------------------------------------------------------------------
void putdec16(uint16_t v)
{
	putdec(10000, v);
}
