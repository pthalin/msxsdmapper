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

#ifndef  __CONIO_H__
#define  __CONIO_H__


#include "types.h"


extern void putchar(char c);
extern char getchar(void);
extern void puts(const char *s);
extern void puthex8(uint8_t v);
extern void puthex16(uint16_t v);
extern void putdec8(uint8_t v);
extern void putdec16(uint16_t v);


#endif  // __CONIO_H__
