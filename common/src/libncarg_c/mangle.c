/*
 * This is a small routine to "mangle" the bits of 2 32 bit quanttities 
 * together without overwriting any bits and thus preserving the 
 * uniqueness of the 2 original quantities compared to any two other
 * quantities that are fed through the same transformation.
 * The bit-reversal and byte interleaving is intended to semi-
 * randomize the result quantities with respect to each other. The goal is
 * to use the result quantities as keys for a binary tree. Randomizing
 * the comparisons helps to keep the tree balanced and therefore
 * efficient. This routine is currently used by CONPACKT ICAEDG.
 */

#include <ncarg/c.h>

static const unsigned char BitReverseTable256[256] =
{
#   define R2(n)     n,     n + 2*64,     n + 1*64,     n + 3*64
#   define R4(n) R2(n), R2(n + 2*16), R2(n + 1*16), R2(n + 3*16)
#   define R6(n) R4(n), R4(n + 2*4 ), R4(n + 1*4 ), R4(n + 3*4 )
        	R6(0), R6(2), R6(1), R6(3)
};

void NGCALLF(mangle,MANGLE)(void *i1, void *i2, void *iout)
{
        /* i1 and i2 are assumed to be pointers to 32 bit quantities */
	/* iout could be a pointer to a single 64 bit integer or 
	   an array of 2 32 bit integers or even an 8 character array */

        unsigned char * r = (unsigned char *) iout;
        unsigned char * p = (unsigned char *) i1;
        unsigned char * q = (unsigned char *) i2;

        r[7] = BitReverseTable256[p[0]];
        r[6] = BitReverseTable256[q[0]];
        r[5] = BitReverseTable256[p[1]];
        r[4] = BitReverseTable256[q[1]];
        r[3] = BitReverseTable256[p[2]];
        r[2] = BitReverseTable256[q[2]];
        r[1] = BitReverseTable256[p[3]];
        r[0] = BitReverseTable256[q[3]];

        return;
}
