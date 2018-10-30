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
#include "mem.h"

/******************************************************************************/
void memcpy(uint8_t *dest, uint8_t *src, uint16_t n)
{
	while (n > 0) {
		*dest = *src;
		dest++;
		src++;
		n--;
	}
}

/******************************************************************************/
void memset(uint8_t *s, uint8_t c, uint16_t n)
{
	while (n > 0) {
		*s = c;
		s++;
		n--;
	}
}

/******************************************************************************/
unsigned char memcmp(uint8_t *dest, uint8_t *src, uint16_t n)
{
	while (n > 0) {
		if (*dest != *src) {
			return 1;
		};
		dest++;
		src++;
		n--;
	}
	return 0;
}
