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

#ifndef  __MEM_H__
#define  __MEM_H__

#include "types.h"


#define peek(A) (*(volatile unsigned char*)(A))
#define poke(A,V) *(volatile unsigned char*)(A)=(V)
#define peek16(A) (*(volatile unsigned int*)(A))
#define poke16(A,V) *(volatile unsigned int*)(A)=(V)


extern void memcpy(uint8_t *dest, uint8_t *src, uint16_t n);
extern void memset(uint8_t *s, uint8_t c, uint16_t n);
extern unsigned char memcmp(uint8_t *dest, uint8_t *src, uint16_t n);

#endif  // __MEM_H__
