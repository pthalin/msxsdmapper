
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* CRC7 table (for SD commands) */
static unsigned int sd_crc7_table[256];


/* Running CRC7 calculation for a byte. */
static unsigned int sd_crc7_byte(unsigned int crcval, unsigned int byte)
{
	return sd_crc7_table[(byte ^ (crcval << 1)) & 0xFFU];
}

void main()
{
	unsigned int crc, byt, bit, crcval, crc_byte;

	/* Generate CRC7 table */
	for (byt = 0; byt < 256; byt ++) {
		crc = byt;
		if ((crc & 0x80) != 0) {
			crc ^= 0x89;
		}
		for (bit = 1; bit < 8; bit ++) {
			crc <<= 1;
			if ((crc & 0x80) != 0) {
				crc ^= 0x89;
			}
		}
		sd_crc7_table[byt] = (crc & 0x7F);
	}

	crcval = 0x00;
	crcval = sd_crc7_byte(crcval, 0x40);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);
	crc_byte = (crcval << 1) | 0x01;
	printf("4000000000 -> CRC: %.2X\n", crc_byte);

	crcval = 0x00;
	crcval = sd_crc7_byte(crcval, 0x48);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x01);
	crcval = sd_crc7_byte(crcval, 0xAA);
	crc_byte = (crcval << 1) | 0x01;
	printf("48000001AA -> CRC: %.2X\n", crc_byte);

	crcval = 0x00;
	crcval = sd_crc7_byte(crcval, 0x77);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);
	crcval = sd_crc7_byte(crcval, 0x00);

	crc_byte = (crcval << 1) | 0x01;
	printf("7700000000 -> CRC: %.2X\n", crc_byte);
}
