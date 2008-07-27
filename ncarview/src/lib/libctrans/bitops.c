/*
 *	$Id: bitops.c,v 1.10 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include	<stdio.h>
#include	<ncarg/c.h>
#include	"default.h"

/*
 *	bitops.c:
 *
 *		author 	John Clyne
 *			3/29/88
 *
 *		
 *		This file contains function that operate on the bit level
 */
			

/*ARGSUSED*/
/*
 *	GetInt:
 *		converts a buffer of chars to either a signed or unsigned
 *  		integer at specified precision.
 *	on entry
 *		bufptr : points to a buffer of char
 *		prec   : number of bits of precision(8,16,24,32);
 * 		signed : boolean indicating signed or unsigned encoding of int.
 *	on exit
 *		return = desired integer
 *
 *	note:
 *		GetInt performs destructive swaping if swap is true
 */
long GetInt(bufptr,prec,is_signed)
	unsigned char *bufptr;
	int 	prec;
	boolean is_signed;	/* indicates whether integer to 
				 * be returned is signed or unsigned
                         	 */
{


	boolean neg = FALSE; 	/*indicates whether an int is negative or not*/




	if (is_signed) neg = *bufptr >> (BYTESIZE - 1);/*determine if negative*/

	/* note: for 8,16 and 24 bit signed ints it is necessary to 
	 * extend the sign  bit for negative numbers. The code "~0L << prec" 
	 * performs this extension.
	 */
	if (prec == 8) 
	    return (neg ? (*bufptr | (~0L << prec)) : (*bufptr)); 

	if (prec == 16) {



		return(neg ? (((*bufptr << 8) |*(bufptr+1)) | (~0L << prec))
			:  (((unsigned) *bufptr << 8) | *(bufptr+1)));
	}


	if (prec == 24 ) 
		return(neg ? ((((unsigned) *bufptr << 16) | 
			((unsigned) *(bufptr+1) << 8) | 
			(unsigned) *(bufptr+2)) | 
			(~0L << prec))
			: 
			(((unsigned) *bufptr << 16) | 
			((unsigned) *(bufptr+1) << 8) | 
			*(bufptr+2)));

	if (prec == 32)  {

		return (((unsigned) *bufptr << 24) | 
			((unsigned) *(bufptr+1) << 16) | 
			((unsigned) *(bufptr+2) << 8) | 
			*(bufptr+3));

	}

	return (0L);
}


/*	GetReal:
 *
 *		getreal returns a converts a character string of bits
 *	representing a real number into a float at a specfied precision
 *
 *	on entry:
 *		bufptr : points to a string of bytes to be converted
 *		r_mode : boolean indicating fixed or floating point representation
 *		expon_prec : precision of exponent (number of bits)
 *		man_prec : precision of mantisa (number of bits)
 *
 *	on exit:
 *		return = the real number
 *
 *	Note: at present only fixed point representation is supported
 */

double GetReal(bufptr,r_mode,expon_prec,man_prec)
	unsigned char	*bufptr;
	boolean r_mode;			/*fixed/floating point mode indicator*/
	int	expon_prec,man_prec;	/*exponent and mantisa precision     */	
{
#define	SIGNOF(X)	((GETBITS(X,7,1)) ? -1 : 1)
	int 		whole_part;	
	unsigned int	fraction_part;

	if (r_mode) {	/* fixed point encoding	*/
		if (expon_prec == 16 && man_prec == 16) {

			fraction_part = GetInt(bufptr+2,16,FALSE);
			/*
			 * note: pre metafile version # 1 incorrectly
			 * encoded fixed point reals. In particular sign
			 * magnitude encoding was used to encode the "whole
			 * part" as opposed to 2's compliment and the fraction
			 * part was off by a factor of 2. Hence we need to
			 * check the metafile version number to determine
			 * the approriate encoding. 
			 */
			if (MFVERSION >= 1)  {
				whole_part = GetInt(bufptr, 16, TRUE);
				return (
					(double) (whole_part + 
					(fraction_part / POWER16))
				);
			} else {
			
				whole_part = Get_SI_int(bufptr, 16);
				return (
					(double)(whole_part + (SIGNOF(bufptr[0])
					* (fraction_part / (POWER15 - 1))))
				);
			}
		}

		if (expon_prec == 32 && man_prec == 32) {
			fraction_part = GetInt(bufptr+4,32,FALSE);
			if (MFVERSION >= 1)  {
				whole_part = GetInt(bufptr, 32, TRUE);
				return (
					(double) (whole_part + 
					(fraction_part / POWER32))
				);
			}
			else {
				whole_part = Get_SI_int(bufptr,32);
				return(
					(double)(whole_part + (SIGNOF(bufptr[0])
					* (fraction_part / (POWER31 - 1))))
				);
			}
		}

	} 
	else {

		/* The NCAR instruction set does not curently support 
		 * floating point  reals. If in the future the instruction 
		 * set is expanded to accept  floating points, the 
		 * translation code should be implemented here  in a similar 
		 * manner to that of fixed point. 
		 */
		if (expon_prec == 9 && man_prec == 23) {

		}

		else if (expon_prec == 12 && man_prec == 52) {

		}
	}

	return(0);	/* silly	*/
}


/*
 *	Get_SI_int:
 *		This routine is used to return a 16 or 32 bit signed int
 *	where the most signifigant bit gives the sign of the integer. This
 * 	routine is only used to decode fixed point reals in the CGM. All
 *	other encoding of signed int is in 2's complement. The stantard 
 *	describing the encoding of the fixed point is WRONG. It does NOT
 *	agree with how the encoding is actualy done
 *
 *	on entry:
 *		prec : is the desired precsion, 16 or 32
 *		bufptr: is buffer containing the bits
 *	on exit:
 *		return = the singed int
 */ 
int	Get_SI_int(bufptr,prec)
	int 	prec;
	unsigned char *bufptr;
{
	long	si;
	int	sign;
	
	sign = GETBITS(bufptr[0],7,1);

	if (prec == 16) {
		si = GETBITS(bufptr[1],14,15);
		return((sign == 1) ? (si * -1) : si);
	}

	if (prec == 32) {
		si = GETBITS(bufptr[3],30,31);
		return((sign == 1) ? (si * -1) : si);
	}

	return(0);	/* silly	*/
}


/*	insert
 *		inserts bits into a field of a buffer
 *
 *	on entry:
 *		bit_start : the possistion of the msb in the destination from
 *				the left
 *		bit_count : number of bits to insert
 *		src	  : the origin of the data
 *		src_start_bit : the possistion of the msb in src from the RIGHT
 * 
 *		on exit
 *		dst contains bit_count bits from src starting at bit_start
 */
insert(dst,bit_start,bit_count,src,src_start_bit)

char	*dst;
int	bit_start,
	bit_count;
long	src;
int	src_start_bit;
{

	int	i,
		temp,
		size,		/* size of char in bits		*/
		ind,		/* index into dst		*/
		len;		/* number of chars for dst	*/

	size = BYTESIZE * sizeof(char);

	/* number of iterations necessary*/
	len =  ((bit_count -1 ) / size ) + 1;	

	ind = (bit_start + bit_count - 1) / size;

	/* shift selected bits all the way to the right	*/
        src = (src >> (src_start_bit+1-bit_count)) &
        ~(~0L << bit_count); 

	/* shift bits to align with required destination	*/
	src = src << ((temp = ((bit_count + bit_start) % size)) ? size - temp : 0);


	/* move the bits	*/
	for(i=0; i<=len; i++) {
		*(dst + ind - i) |= src >> (i * size); 
	}
}

/*
 *	swapBytes
 *	[exported]
 *
 *		swap bytes, exchanging left most and right most bytes
 * on entry
 *	*cptr		: pointer to some bytes
 *	word_size	: size of word to be byte swapped
 *	num_words	: number of words to swap.
 */ 
swapBytes(cptr, word_size, num_words)
	char	*cptr;
	unsigned	word_size;
	unsigned	num_words;
{

	char	*left, *right;	/* pointers to left and right side of a word */
	char	c;		/* temp					*/
	int	i;

	for (i = 0; i < num_words; i++) {
		left = cptr;
		right = left + word_size - 1;
		while (left < right) {
			c = *left;
			*left++ = *right;
			*right-- = c;
		}
		cptr += word_size;
	}
}
