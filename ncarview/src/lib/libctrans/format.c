/*
 *	$Id: format.c,v 1.15 2008-07-27 03:18:43 haley Exp $
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
/*
 *	Author:	Tinsley Galyean (tag@boulder.colorado.edu)
 *
 *	Date:	Thu Mar 10 15:15:44 MST 1988
 *
 *	This file contains the functions need to format and encode the
 *	data before it is sent to the device.  These function once
 *	they have formated the data buffer it for output.
 *	
 *	You will notice that much code as been duplicated in the 
 *	different formating functions.  This is a produce of having
 *	to handle unusual circumstances that are unique to the
 *	particular data being formated and encoded.
 *
 *
 * rev 1.01 clyne 2/16/90	: fixed bug in binary encoding of colour 
 *				  intensities
 * rev 1.02 clyne 4/19/90	: rgb and hls intensities previously could not
 *				  no be specified out of order. e.g rbg.
 *				 
 */

#include 	<stdio.h>
#include 	<stdlib.h>
#include	<ncarg/c.h>
#include	"cgmc.h"
#include	"defines.h"
#include	"graphcap.h"
#include	"bitops.h"
#include	"format.h"

/*
 *	The different types of encoding
 */
#define	BINARY 	0
#define DECIMAL 1
#define HEX 	2	/* the only of these that is not currently supported */
#define OCT 	3
#define TEK 	4
#define FLOAT 	5

/*
 *	The fields in the formating array
 */
#define	BIT_START	0
#define	BIT_COUNT	1
#define DATA_TYPE	2
#define DATA_VALUE	3

/* 
 *	The max number of bytes need to format and encode any binary data.
 */	
#define	TEMPSIZE	30

/*
 *	Some static things calculated once by formatinit
 *	These values are the lengths of the formated output of the
 *	particular data if BINARY encoding is used
 */
static	int	coordoutsize = 0;   /*number bytes in a formated coord pair */
static	int	onecoordoutsize = 0;/*number bytes in a formated coord */
static	int	veccntoutsize = 0;  /*number bytes in a formated vector count */
static	int	indexoutsize = 0;   /*number bytes in a formated colour index */
static	int	widthoutsize = 0;   /*number bytes in a formated line width */


/*
 *	One time setup to calculate the values declared above.
 */
formatinit()
{
	int	i;

	/*
	 * 	calculate the number of bytes for a formated coord pair
	 *	and the number of bytes for one coord
	 */
	if (COORD_ENCODING == BINARY) {
	for(i=0;i<COORD_FORMAT_SIZE;i++) {

		if (COORD_FORMAT[DATA_TYPE][i] == 2)
			onecoordoutsize = coordoutsize;
		if (coordoutsize < 
		    (COORD_FORMAT[BIT_START][i] + COORD_FORMAT[BIT_COUNT][i]))
			coordoutsize = 
			COORD_FORMAT[BIT_START][i] + COORD_FORMAT[BIT_COUNT][i];
	}
	if (coordoutsize % BITS_PER_BYTE)
		coordoutsize /= BITS_PER_BYTE + 1;
	else
		coordoutsize /= BITS_PER_BYTE;
	if (onecoordoutsize % BITS_PER_BYTE)
		onecoordoutsize /= BITS_PER_BYTE + 1;
	else
		onecoordoutsize /= BITS_PER_BYTE;
	}
	/*
	 * 	calculate the number of bytes for a formated vector count
	 */
	if (VECTOR_COUNT_ENCODING == BINARY) {
	for(i=0;i<VECTOR_COUNT_FORMAT_SIZE;i++)
		if (veccntoutsize < (VECTOR_COUNT_FORMAT[BIT_START][i] + 
				     VECTOR_COUNT_FORMAT[BIT_COUNT][i]))
			veccntoutsize = VECTOR_COUNT_FORMAT[BIT_START][i] + 
					VECTOR_COUNT_FORMAT[BIT_COUNT][i];
	if (veccntoutsize % BITS_PER_BYTE)
		veccntoutsize /= BITS_PER_BYTE + 1;
	else
		veccntoutsize /= BITS_PER_BYTE;
	}

	/*
	 * 	calculate the number of bytes for a formated colour index
	 */
	if (COLOUR_INDEX_ENCODING == BINARY) {
	for(i=0;i<COLOUR_INDEX_FORMAT_SIZE;i++)
		if (indexoutsize < (COLOUR_INDEX_FORMAT[BIT_START][i] + 
				     COLOUR_INDEX_FORMAT[BIT_COUNT][i]))
			indexoutsize = COLOUR_INDEX_FORMAT[BIT_START][i] + 
					COLOUR_INDEX_FORMAT[BIT_COUNT][i];
	if (indexoutsize % BITS_PER_BYTE)
		indexoutsize /= BITS_PER_BYTE + 1;
	else
		indexoutsize /= BITS_PER_BYTE;
	}

	/*
	 * 	calculate the number of bytes for a formated line width
	 */
	if (LINE_WIDTH_ENCODING == BINARY) {
	for(i=0;i<LINE_WIDTH_FORMAT_SIZE;i++)
		if (widthoutsize < (LINE_WIDTH_FORMAT[BIT_START][i] + 
				     LINE_WIDTH_FORMAT[BIT_COUNT][i]))
			widthoutsize = LINE_WIDTH_FORMAT[BIT_START][i] + 
					LINE_WIDTH_FORMAT[BIT_COUNT][i];
	if (widthoutsize % BITS_PER_BYTE)
		widthoutsize /= BITS_PER_BYTE + 1;
	else
		widthoutsize /= BITS_PER_BYTE;
	}
}

/*
 *	Formating routines
 */

/* 
 *	Fills the string s with the Ascii decimal rep. of the number n.
 *	The return value is the number of chars required.
 */
itoa(s, n)
char *s;
long n;
{
	char *p, c;
	int	num;

	p = s;
	do {
		*p++ = n%10 + '0';
	} while (n /= 10);
	*p-- = 0;

	num = p - s + 1;
	for (; p > s; s++, p--) {
		c = *s; *s = *p; *p = c; }

	return (num);
}

/* 
 *	Fills the string s with the Ascii octal rep. of the number n.
 *	The return value is the number of chars required.
 */
itooct(s, n)
char *s;
long n;
{
	char *p, c;
	int	num;

	p = s;
	do {
		*p++ = n%8 + '0';
	} while (n /= 8);
	*p-- = 0;

	num = p - s + 1;
	for (; p > s; s++, p--) {
		c = *s; *s = *p; *p = c; }

	return (num);
}

/* 
 *	Fills the string s with the Ascii hex rep. of the number n.
 *	The return value is the number of chars required.
 *
 *	This routine has not been tested and is not used yet.
 */
itohex(s, n)
char *s;
long n;
{
	char *p, c;
	int	num;

	p = s;
	do {
		*p = n%16;
		*p = (*p > 9 ? *p + 'A' - 10 : *p + '0');
		p++;
	} while (n /= 16);
	*p-- = 0;

	num = p - s + 1;
	for (; p > s; s++, p--) {
		c = *s; *s = *p; *p = c; }

	return (num);
}

/*
 *	Fills the string s with the ascii rep of the floating point number
 *	and returns the number of char in the string
 */
#ifdef	__STDC__	/* Sun ANSI C compiler barfs on old style def.?	*/
int	ftoa(char *s, float f)
#else
int	ftoa(s, f)
	char	*s;
	float	f;
#endif
{

	(void) sprintf(s,"%.3f",f);
	return(strlen(s));
}

/*
 *  This function was taken with little change form the FORTRAN translator.
 *
 *  CONVERT TO AN ASCII TEKTRONIX INT VALUE
 *
 *  INPUT
 *	VALUE - INTEGER NUMBER TO BE CONVERTED TO ASCII TEKTRONIX INT
 *	returns the number of bytes (chars) to output.
 */
itotek(s, value)
char *s;
long value;
{
      long temp;

      /*
       *
       *   COMPUTE THE REQUESTED STRING
       *
       */
      temp = labs(value);
      s[0] = temp / 1024 + 64;
      s[1] = (temp/16) % 64 + 64;
      s[2] = temp % 16 + 32;
      if (value >=  0) s[2] = s[2] + 16;

      /*
       *
       *  DECIDE THE NUMBER OF CHARACTERS TO PRODUCE
       *
       */
      if (s[0] != 64)
		return (3);
      else if (s[1] != 64) {
		s[0] = s[1];
		s[1] = s[2];
		return (2);
      } else {
		s[0] = s[2];
		return(1);
      }
}


/*
 * 	Format and encode the coord pairs.
 *
 *	"number" can be a 1 or a 2 meaning
 *		1 - only the x value is formated and sent.
 *		2 - both the x and the y value are formated and sent.
 */
int	formatcoord(x,y,number)
long	x,y;			/* the x, y pair */
int	number;			/* the number of values to be sent */
{
	char	temp[TEMPSIZE];	/* temp buffer to work in */
	char	tmp;

	float	value;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	memset(temp,0,coordoutsize);

	dst = temp;

	for(i=0;i<COORD_FORMAT_SIZE;i++) {

		bitstart = COORD_FORMAT[BIT_START][i];
		bitcount = COORD_FORMAT[BIT_COUNT][i];
		datatype = COORD_FORMAT[DATA_TYPE][i];

		if (datatype == (number+1))
			break;

		if (datatype == -1) {
			tmp = COORD_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = COORD_FORMAT[DATA_VALUE][i];
			srcbitstart = COORD_FORMAT[BIT_COUNT][i]-1;
		} else if (datatype == 1) {
			src = x;
			srcbitstart = COORD_FORMAT[DATA_VALUE][i];
		} else if (datatype == 2) {
			src = y;
			srcbitstart = COORD_FORMAT[DATA_VALUE][i];
		} else {
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);
		}

		if (datatype > -1)
			switch (COORD_ENCODING) {
			case BINARY:
				insert(dst,bitstart,bitcount,src,srcbitstart);
				break;
			case DECIMAL:
				buffer(temp,itoa(temp,GETBITS(src,srcbitstart,bitcount)));
				break;
			case OCT:
				buffer(temp,itooct(temp,GETBITS(src,srcbitstart,bitcount)));
				break;
			case TEK:
				buffer(temp,itotek(temp,GETBITS(src,srcbitstart,bitcount)));
				break;
			case FLOAT:
				value = GETBITS(src,srcbitstart,bitcount);

				/*
				 * Normalize the value
				 */
				value = (value-COORD_INMIN) /
					(COORD_INMAX-COORD_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(COORD_OUTMAX- 
					       COORD_OUTMIN) +
					      COORD_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
				return (-1);
			}
	}

	if (COORD_ENCODING == BINARY)
		if (number == 1)
			buffer(temp,onecoordoutsize);
		else
			buffer(temp,coordoutsize);

	return (0);
}

/*
 * 	Format and encode A vector count.
 */
int	formatveccnt(count)
long	count;			/* the vector count pair */
{
	char	temp[TEMPSIZE];	/* temp buffer to work in */
	char	tmp;

	float	value;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	memset(temp,0,veccntoutsize);

	dst = temp;

	for(i=0;i<VECTOR_COUNT_FORMAT_SIZE;i++) {

		bitstart = VECTOR_COUNT_FORMAT[BIT_START][i];
		bitcount = VECTOR_COUNT_FORMAT[BIT_COUNT][i];
		datatype = VECTOR_COUNT_FORMAT[DATA_TYPE][i];

		if (datatype == -1) {
			tmp = VECTOR_COUNT_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = VECTOR_COUNT_FORMAT[DATA_VALUE][i];
			srcbitstart = VECTOR_COUNT_FORMAT[BIT_COUNT][i]-1;
		} else if (datatype == 1) {
			src = count;
			srcbitstart = VECTOR_COUNT_FORMAT[DATA_VALUE][i];
		} else {
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);
		}

		if (datatype > -1)
			switch (VECTOR_COUNT_ENCODING) {
			case BINARY:
				insert(dst,bitstart,bitcount,src,srcbitstart);
				break;
			case DECIMAL:
				buffer(temp,itoa(temp,GETBITS(src,srcbitstart,bitcount)));
				break;
			case OCT:
				buffer(temp,itooct(temp,GETBITS(src,srcbitstart,bitcount)));
				break;
			case TEK:
				buffer(temp,itotek(temp,GETBITS(src,srcbitstart,bitcount)));
				break;
			case FLOAT:
				value = GETBITS(src,srcbitstart,bitcount);

				/*
				 * Normalize the value
				 */
				value = (value-VECTOR_COUNT_INMIN) /
					(VECTOR_COUNT_INMAX-VECTOR_COUNT_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(VECTOR_COUNT_OUTMAX- 
					       VECTOR_COUNT_OUTMIN) +
					      VECTOR_COUNT_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
				return (-1);
			}
	}

	if (VECTOR_COUNT_ENCODING == BINARY)
		buffer(temp,veccntoutsize);

	return (0);
}

/*
 * 	Format and encode the a colour index.
 *
 *	The fillflag is a hack to assure that in the case of TEK encoding
 *	that the sign of the index set correctly.
 *
 *	As for the interface the fillflag should be TRUE when the colour
 *	index being encoded is Fill colour index, and FALSE otherwise.
 */
int	formatindex(index,fillflag)
long	index;			/* the colour index */
boolean	fillflag;		/* TRUE if the index is a fill colour */
{
	char	temp[TEMPSIZE];	/* temp buffer to work in */
	char	tmp;

	float	value;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	/*
	 * 	A Hack to send a negative fill colors to tek devices
	 */
	if (fillflag && COLOUR_INDEX_ENCODING == TEK)
		index = -index;

	memset(temp,0,indexoutsize);

	dst = temp;

	for(i=0;i<COLOUR_INDEX_FORMAT_SIZE;i++) {

		bitstart = COLOUR_INDEX_FORMAT[BIT_START][i];
		bitcount = COLOUR_INDEX_FORMAT[BIT_COUNT][i];
		datatype = COLOUR_INDEX_FORMAT[DATA_TYPE][i];

		if (datatype == -1) {
			tmp = COLOUR_INDEX_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = COLOUR_INDEX_FORMAT[DATA_VALUE][i];
			srcbitstart = COLOUR_INDEX_FORMAT[BIT_COUNT][i]-1;
		} else if (datatype == 1) {
			src = index;
			srcbitstart = COLOUR_INDEX_FORMAT[DATA_VALUE][i];
		} else {
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);
		}

		if (datatype > -1)
			switch (COLOUR_INDEX_ENCODING) {
			case BINARY:
				insert(dst,bitstart,bitcount,src,srcbitstart);
				break;
			case DECIMAL:
				buffer(temp,itoa(temp,GETBITS(src,
							      srcbitstart,
							      bitcount)));
				break;
			case OCT:
				buffer(temp,itooct(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case TEK:
				buffer(temp,itotek(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case FLOAT:
				value = GETBITS(src,srcbitstart,bitcount);

				/*
				 * Normalize the value
				 */
				value = (value-COLOUR_INDEX_INMIN) /
					(COLOUR_INDEX_INMAX-COLOUR_INDEX_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(COLOUR_INDEX_OUTMAX- 
					       COLOUR_INDEX_OUTMIN) +
					      COLOUR_INDEX_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
				return (-1);
			}
	}

	if (COLOUR_INDEX_ENCODING == BINARY)
		buffer(temp,indexoutsize);

	return (0);
}

/*
 * 	Format and encode the line width 
 */
int	formatwidth(width)
int	width;
{
	char	temp[TEMPSIZE];	/* temp buffer to work in */
	char	tmp;

	float	value;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	if (LINE_WIDTH_ENCODING == BINARY)
		memset(temp,0,widthoutsize);

	dst = temp;

	for(i=0;i<LINE_WIDTH_FORMAT_SIZE;i++) {

		bitstart = LINE_WIDTH_FORMAT[BIT_START][i];
		bitcount = LINE_WIDTH_FORMAT[BIT_COUNT][i];
		datatype = LINE_WIDTH_FORMAT[DATA_TYPE][i];

		if (datatype == -1) {
			tmp = LINE_WIDTH_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = LINE_WIDTH_FORMAT[DATA_VALUE][i];
			srcbitstart = LINE_WIDTH_FORMAT[BIT_COUNT][i]-1;
		} else if (datatype == 1) {
			src = width;
			srcbitstart = LINE_WIDTH_FORMAT[DATA_VALUE][i];
		} else {
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);
		}

		if (datatype > -1)
			switch (LINE_WIDTH_ENCODING) {
			case BINARY:
				insert(dst,bitstart,bitcount,src,srcbitstart);
				break;
			case DECIMAL:
				buffer(temp,itoa(temp,GETBITS(src,
							      srcbitstart,
							      bitcount)));
				break;
			case OCT:
				buffer(temp,itooct(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case TEK:
				buffer(temp,itotek(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case FLOAT:
				value = GETBITS(src,srcbitstart,bitcount);

				/*
				 * Normalize the value
				 */
				value = (value-LINE_WIDTH_INMIN) /
					(LINE_WIDTH_INMAX-LINE_WIDTH_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(LINE_WIDTH_OUTMAX- 
					       LINE_WIDTH_OUTMIN) +
					      LINE_WIDTH_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
				return (-1);
			}
	}

	if (LINE_WIDTH_ENCODING == BINARY)
		buffer(temp,widthoutsize);

	return (0);
}

/*
 * 	Format and encode the three valued colour intensity data.
 *
 *	data -- is an array of 3 values containing the values to be
 *	formatted and encoded before being sent to change the colour
 *	of a colour index.  These value will need to have been 
 *	already translated in to the appropriate colour model for the
 *	device being used.
 */
int	formatintensity(data, count)
long	data[3];		/* the three values to be sent	*/
int	count;			/* range of data		*/
{
	char	temp[30];	/* temp buffer to work in */
	char	tmp;

	float	value;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	int	index;		/* index to current data being encoded */

	index = 0;

	while (index < count) {

	if (MAP_INTENSITY_ENCODING == BINARY)
		memset(temp,0,indexoutsize);

	dst = temp;

	for(i=0;i<MAP_INTENSITY_FORMAT_SIZE;i++) {

		bitstart = MAP_INTENSITY_FORMAT[BIT_START][i];
		bitcount = MAP_INTENSITY_FORMAT[BIT_COUNT][i];
		datatype = MAP_INTENSITY_FORMAT[DATA_TYPE][i];

		if (datatype == -1) {
			tmp = MAP_INTENSITY_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = MAP_INTENSITY_FORMAT[DATA_VALUE][i];
			srcbitstart = MAP_INTENSITY_FORMAT[BIT_COUNT][i]-1;
		} else if ((datatype > 0) && (datatype  <= 3)){
			src = data[index++];
			srcbitstart = MAP_INTENSITY_FORMAT[DATA_VALUE][i];
		} else {
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);
		}

		if (datatype > -1)
			switch (MAP_INTENSITY_ENCODING) {
			case BINARY:
				insert(dst,bitstart,bitcount,src,srcbitstart);
				break;
			case DECIMAL:
				buffer(temp,itoa(temp,GETBITS(src,
							      srcbitstart,
							      bitcount)));
				break;
			case OCT:
				buffer(temp,itooct(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case TEK:
				buffer(temp,itotek(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case FLOAT:
				value = GETBITS(src,srcbitstart,bitcount);

				/*
				 * Normalize the value
				 */
				value = (value-MAP_INTENSITY_INMIN) /
				    (MAP_INTENSITY_INMAX-MAP_INTENSITY_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(MAP_INTENSITY_OUTMAX- 
					       MAP_INTENSITY_OUTMIN) +
					      MAP_INTENSITY_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
				return (-1);
			}
	}

	if (MAP_INTENSITY_ENCODING == BINARY)
		buffer(temp,indexoutsize);

	}

	return (0);
}
