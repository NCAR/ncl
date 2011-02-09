/*
 *	$Id: gsbytes.c,v 1.4 2008-07-27 12:23:45 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  2000                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR GRAPHICS V2.00 - UNIX Release              *
*                                                                      *
************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/c.h>
		
/*
 * Tools for storage/retrieval of arbitrary size bytes from 32 bit words
 * 
 * $Header: /home/brownrig/SVN/CVS/ncarg/common/src/libncarg_c/gsbytes.c,v 1.4 2008-07-27 12:23:45 haley Exp $
 * 
 * gbytes(p,u,q,b,s,n) gbyte (p,u,q,b) sbytes(p,u,q,b,s,n) sbyte (p,u,q,b)
 * 
 * q >= 0     number of bits to be skipped preceeding first byte in p 0 <    b <
 * sword  byte size s >= 0     number of bits to be skipped between bytes n
 * >= 0     number of bytes to be packed/unpacked
 * 
 * gbytes unpacks n b bit bytes from p into u, starting by skipping q bits in p,
 * then skipping s bits between bytes. gbyte unpacks one such byte. sbytes
 * packs n b bit bytes from u into p, starting by skipping q bits in p, then
 * skipping s bits between bytes. sbyte  packs one such byte.
 */

#define MASK	( (unsigned) 0xffffffff)
#define	SWORD	32		/* Word size in bits  */
#define G1BYTE(p,q,b) ((unsigned) p << q >> (SWORD - b))
/* right justify b bits, located q bits from left of p.  */

extern void NGCALLF(gbyte,GBYTE)(long *p, long *u, long *q, long *b);
extern void NGCALLF(sbyte,SBYTE)(long *p, long *u, long *q, long *b);

void NGCALLF(gbytes,GBYTES)(p, u, q, b, s, n)
	long           *p, *u, *q, *b, *s, *n;
{
	register long   i = 0, jp = 0;
	long            jq = *q;
	if (*n > 0) {
		while (1) {
  		        NGCALLF(gbyte,GBYTE)(p + jp, u + i, &jq, b);
			if (++i == *n)
				break;
			jq += *b + *s;
			jp += jq / SWORD;
			jq %= SWORD;
		}
	}
}


void NGCALLF(sbytes,SBYTES)(p, u, q, b, s, n)
	long           *p, *u, *q, *b, *s, *n;
{
	register long   i = 0, jp = 0;
	long            jq = *q;
	if (*n > 0) {
		while (1) {
			NGCALLF(sbyte,SBYTE)(p + jp, u + i, &jq, b);
			if (++i == *n)
				break;
			jq += *b + *s;
			jp += jq / SWORD;
			jq %= SWORD;
		}
	}
}


void NGCALLF(gbyte,GBYTE)(p, u, q, b)
	long           *p, *u, *q, *b;
{
	register long   j, jq = *q, jb = *b, lb, qb;

	if (jq < SWORD)
		j = 0;
	else {
		j = jq / SWORD;	/* number of words offset */
		jq %= SWORD;	/* odd bits of offset     */
	}

	if ((jq + jb) > SWORD) {
		qb = SWORD - jq;
		jb -= qb;
		lb = ((~(MASK << qb)) & (*(p + j))) << jb;
		jq = 0;
		j++;		/* increment to next word */
		*u = lb + (G1BYTE(*(p + j), jq, jb));
	} else
		*u = (G1BYTE(*(p + j), jq, jb));
}


void NGCALLF(sbyte,SBYTE)(p, u, q, b)
	long           *p, *u, *q, *b;
{
	register long  *t, jq = *q, jb = *b, qb;

	if (jq < SWORD)
		t = p;
	else {
		t = p + jq / SWORD;	/* number of words offset */
		jq %= SWORD;	/* odd bit offset         */
	}

	if ((jq + jb) > SWORD) {
		qb = SWORD - jq;
		jq = SWORD - jb;
		jb -= qb;
		*t = ((*t >> qb) << qb) + (G1BYTE(*u, jq, qb));
		jq = 0;
		t++;		/* point to next word */
	}
	/*
	 * *t = (*t & ~(MASK << (SWORD - jb) >> jq)) + ((~(MASK << jb) & *u)
	 * << SWORD-(jb+jq));
	 */
	*t = (*t & ~(MASK << (SWORD - jb) >> jq)) + (((unsigned) *u) <<
						     (SWORD - jb) >> jq);
}


void NGCALLF(g8bits,G8BITS)(p, u, k, n)
	long           *u, *k, *n;
	char           *p;

/*
 * Specialized routine for getting n 8 bit bytes, with no skipping between
 * bytes; k is the number of 8 bit bytes to skip at the beginning of p. Bits
 * are right justified in p (rightmost 8 bits of 32 bit variable).
 */

{
	register long   i, j = *k;
	for (i = 0; i < *n; i++)
		*(u + i) = *(p + j++);
}


void NGCALLF(s8bits,S8BITS)(p, u, k, n)
	char           *p, *u;
	long           *k, *n;

/*
 * Specialized routine for packing n 8 bit bytes, with no skipping between
 * bytes; k is the number of 8 bit bytes to skip at the beginning of p, while
 * jp keeps track of the position in u, which is being manipulated as a char,
 * even though it's a 32 bit array in the calling routine..
 */

{
	register long   jp = 3, i, j = *k;
	for (i = 0; i < *n; i++, jp += 4)
		*(p + j++) = *(u + jp);
}
