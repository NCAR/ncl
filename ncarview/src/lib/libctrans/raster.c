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
 *	Date:	Thu Apr 20 15:15:44 MST 1988
 *
 *	This Module contains the function needed to produce the
 *	the cell array display by using the RASTER_HORIZONAL
 *	instructions.
 *
 *	11-13-89	Correctly implemented cell arrays in the raster()
 *			function - John Clyne
 */

#include	<stdio.h>
#include	<cterror.h>
#include	<ncarv.h>
#include	"cgmc.h"
#include	"defines.h"
#include	"graphcap.h"
#include	"ctrandef.h"
#include	"translate.h"

extern 	long	GetInt();
extern 	Ct_err	Instr_Dec();
extern 	Ct_err	formatcoord();



static	boolean	Runlength;	/* true if device wants run length encoded
				 * pixel map
				 */

/*
 * defines for the markers in the strings
 */
#define	VC 	-2	/* Vector Count */
#define	XC 	-3	/* X Coord */
#define	YC 	-4	/* Y Coord */
#define	XYC 	-5	/* X-Y Coord Pair */
#define RL	-7		/* run length encoding flag	*/


/*
 *	The different type of encoding
 */
#define	BINARY 	0
#define DECIMAL 1
#define HEX 	2
#define OCT 	3
#define TEK 	4
#define FLOAT 	5

/*
 *	the fields in the formating array
 */
#define	BIT_START	0
#define	BIT_COUNT	1
#define DATA_TYPE	2
#define DATA_VALUE	3

/*
 *	Some static things calculated once by formatinit
 *	These are will be the size of the output is BINARY encoding it used.
 */
static	int	veccntoutsize = 0;  /*bytes in a formated raster vector count */
static	int	dataoutsize = 0;   /*number bytes in a formated raster data */


/*
 *	One time setup to calculate the values declared above.
 */
rasterformatinit()
{
	int	i;

	/*
	 * 	calculate the number of bytes for a formated raster vector count
	 */
	for(i=0;i<RASTER_VECTOR_COUNT_FORMAT_SIZE;i++)
		if (veccntoutsize < (RASTER_VECTOR_COUNT_FORMAT[BIT_START][i] + 
				     RASTER_VECTOR_COUNT_FORMAT[BIT_COUNT][i]))
			veccntoutsize=RASTER_VECTOR_COUNT_FORMAT[BIT_START][i] +
				      RASTER_VECTOR_COUNT_FORMAT[BIT_COUNT][i];
	if (veccntoutsize % BITS_PER_BYTE)
		veccntoutsize /= BITS_PER_BYTE + 1;
	else
		veccntoutsize /= BITS_PER_BYTE;

	/*
	 * 	calculate the number of bytes for a formated raster data
	 */
	for(i=0;i<RASTER_DATA_FORMAT_SIZE;i++)
		if (dataoutsize < (RASTER_DATA_FORMAT[BIT_START][i] + 
				     RASTER_DATA_FORMAT[BIT_COUNT][i]))
			dataoutsize = RASTER_DATA_FORMAT[BIT_START][i] + 
					RASTER_DATA_FORMAT[BIT_COUNT][i];
	if (dataoutsize % BITS_PER_BYTE)
		dataoutsize /= BITS_PER_BYTE + 1;
	else
		dataoutsize /= BITS_PER_BYTE;
}

/*

/*
 *	Formating routines
 */



/*
 * 	Format and encode the raster vector count 
 */
Ct_err	formatrasterveccnt(count)
long	count;			/* the vector count */
{
	char	temp[30];	/* temp buffer to work in */
	char	tmp;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	float	value;

	bzero(temp,veccntoutsize);

	dst = temp;

	for(i=0;i<RASTER_VECTOR_COUNT_FORMAT_SIZE;i++) {

		bitstart = RASTER_VECTOR_COUNT_FORMAT[BIT_START][i];
		bitcount = RASTER_VECTOR_COUNT_FORMAT[BIT_COUNT][i];
		datatype = RASTER_VECTOR_COUNT_FORMAT[DATA_TYPE][i];

		if (datatype == -1) {
			tmp = RASTER_VECTOR_COUNT_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = RASTER_VECTOR_COUNT_FORMAT[DATA_VALUE][i];
			srcbitstart= RASTER_VECTOR_COUNT_FORMAT[BIT_COUNT][i]-1;
		} else if (datatype == 1) {
			src = count;
			srcbitstart = RASTER_VECTOR_COUNT_FORMAT[DATA_VALUE][i];
		} else {
			ct_error(NT_GFEE, "(data type out of range)");
			return (SICK);
		}

		if (datatype > -1)
			switch (RASTER_VECTOR_COUNT_ENCODING) {
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
				value = (value-RASTER_VECTOR_COUNT_INMIN) /
					(RASTER_VECTOR_COUNT_INMAX - RASTER_VECTOR_COUNT_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(RASTER_VECTOR_COUNT_OUTMAX- 
					       RASTER_VECTOR_COUNT_OUTMIN) +
					      RASTER_VECTOR_COUNT_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ct_error(NT_GFEE, "(unsupported encoding)");
				break;
			}
	}

	if (RASTER_VECTOR_COUNT_ENCODING == BINARY)
		buffer(temp,veccntoutsize);

	return (OK);
}

/*
 * 	Format and encode the data  -- the data is a colour index
 */
Ct_err	formatrasterdata(data,count)
Ctype	data;
int	count;
{
	char	temp[30];	/* temp buffer to work in */
	char	tmp;

	float	value;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	bzero(temp,dataoutsize);

	dst = temp;

	for(i=0;i<RASTER_DATA_FORMAT_SIZE;i++) {

		bitstart = RASTER_DATA_FORMAT[BIT_START][i];
		bitcount = RASTER_DATA_FORMAT[BIT_COUNT][i];
		datatype = RASTER_DATA_FORMAT[DATA_TYPE][i];

		if (datatype == -1) {
			tmp = RASTER_DATA_FORMAT[DATA_VALUE][i];
			buffer(&tmp,1);
		} else if (datatype == 0) {
			src = RASTER_DATA_FORMAT[DATA_VALUE][i];
			srcbitstart = RASTER_DATA_FORMAT[BIT_COUNT][i]-1;
		} else if (datatype == 1) {
			src = count;
			srcbitstart = RASTER_DATA_FORMAT[DATA_VALUE][i];
		} else if (datatype == 2) {
			src = data;
			srcbitstart = RASTER_DATA_FORMAT[DATA_VALUE][i];
		} else {
			ct_error(NT_GFEE, "(data type out of range)");
			return (SICK);
		}

		if (datatype > -1)
			switch (RASTER_DATA_ENCODING) {
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
				if (Runlength)
					insert(dst,bitstart,bitcount,
								src,
								srcbitstart);
				else
					buffer(temp,itotek(temp,GETBITS(src,
								srcbitstart,
								bitcount)));
				break;
			case FLOAT:
				value = GETBITS(src,srcbitstart,bitcount);

				/*
				 * Normalize the value
				 */
				value = (value-RASTER_DATA_INMIN) /
					(RASTER_DATA_INMAX-RASTER_DATA_INMIN);
				/*
				 * Scale in to new range
				 */
				value = value*(RASTER_DATA_OUTMAX- 
					       RASTER_DATA_OUTMIN) +
					      RASTER_DATA_OUTMIN;

				buffer(temp, ftoa(temp,value));
				break;
			default:
				ct_error(NT_GFEE, "(unsupported encoding)");
				break;
			}
	}

	if (RASTER_DATA_ENCODING == BINARY )
		buffer(temp,dataoutsize);

	/*	This is a hack to handle runlength encoding for
	 *	tektronics 41XX terminals
	 */

	if (Runlength)
		buffer(temp,itotek(temp, GetInt((unsigned char *) dst,
				(int) (dataoutsize * BITS_PER_BYTE),
				FALSE)));

	return (OK);
}

/*
 *	Displays the cell array using the RASTER_HORIZONAL instruction.
 *	The cell array must be rectangular and stored in packed index
 *	encoding. 
 *	
 */
Ct_err	raster(c)
CGMC	*c;
{
	long	deltax,deltay;

	long	px,py,rx,ry,qy;	/* cell array boundries coord pairs	*/
	int	fudge_x,
		fudge_y;	/* fudge factor added to cells when number
				 * of pixels is not a multiple of # cells
				 */ 
	int	fudge_x_ctr;	/* keep track of original fudge_x	*/
	int	i,j,k;

	int	index = 0;

	Ctype	*index_array;	/* an array of row colour indecies	*/
	long	nx,ny;		/* number of cells in x and y direction	*/
	long	snx,sny;	/* dimensions of cell array		*/

	Runlength = FALSE; 	/* default is packed encoding	*/
	
	nx = c->i[0];		/* number of cells in x direction	*/
	ny = c->i[1];		/* number of cells in y direction	*/

	index_array = (Ctype *) icMalloc ((unsigned) nx * sizeof (Ctype) + 1);  

	/*
	 * get boundry coordinates
	 */
	px = R_XConvert(c->p[0].x); py = R_YConvert(c->p[0].y);
	rx = R_XConvert(c->p[2].x); ry = R_YConvert(c->p[2].y);
	qy = R_YConvert(c->p[1].y);

	snx = labs(rx - px);
	sny = labs(ry - qy);
	
	/*
	 * base dimensions of a cell
	 */
	deltax = snx / nx;
	deltay = sny / ny;

	/*
	 * error in difference of cell array boundry and n[x,y] * delta[x,y]
	 */
	fudge_x_ctr = fudge_x = snx % nx;
	fudge_y = sny % ny;

	deltay++;	/* assume we need to fudge in y direction	*/

	/*
	 * for each row of cells
	 */
	for(i=0;i<ny;i++) {

		/*
		 * load an array of colour indecies for row i cells
		 */
		for (j = 0; j < nx; j++, index++) {

			/* make sure data available in cgmc     */
			if (index == c->Cnum && c->more) {
				if (Instr_Dec(c) != OK)
					return (pre_err);
				
				index = 0;
			}

			index_array[j] = c->c[index];

		}


		/* 
		 * see if done fudging
		 */
		if (!(fudge_y--))
			deltay--;

		/*
		 * foreach row of pixels per row of cells
		 */
		for (j = 0; j < abs((int) deltay); j++) {

			/*
			 * format the begin raster instruction
			 */
			for(k=0;k<RASTER_HOR_START_SIZE;k++)
				switch(RASTER_HOR_START[k]) {
				case (char)XYC:
					(void)formatcoord(px,py,2);
					break;
				case (char)XC:
					(void)formatcoord(px,(long)0,1);
					break;
				case (char)YC:
					(void)formatcoord(py,(long)0,1);
					break;
				case (char)VC:
					(void)formatrasterveccnt(nx);
					break;
				case (char)RL:
					Runlength = TRUE;
					break;
				default:
					buffer(&RASTER_HOR_START[k],1);
					break;
				}

			py++;	/* advance to next row of pixels	*/

			deltax++;	/* assume we need to fudge	*/
			fudge_x = fudge_x_ctr;

			/*
			 * foreach row of pixels per cell
			 */
			for (k = 0; k < nx; k++) {

                                /* see if done fudging row	*/
                                if (!(fudge_x--))
                                        deltax--;

				if (Runlength) {
					(void)formatrasterdata(index_array[k],
						(int)deltax);
				}
				else {
					ct_error(NT_GFEE,"");
				}
			}

			buffer(RASTER_HOR_TERM, RASTER_HOR_TERM_SIZE);
		}
	}

	cfree((char *) index_array);
	return(OK);
}
