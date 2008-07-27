/*
 *	$Id: gcaprast.c,v 1.21 2008-07-27 03:18:44 haley Exp $
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
#include	<stdlib.h>
#include	<errno.h>
#include	<ncarg/c.h>
#include	"cgmc.h"
#include	"defines.h"
#include	"graphcap.h"
#include	"ctrandef.h"
#include	"translate.h"
#include	"bitops.h"




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
 *	set_up_addressing
 *	[internal]
 *
 * on entry
 *	P,Q		: corners of cell array
 * on exit
 *	*step_y		: rasterize from top to bottom or vise versa
 *	*start_x	: starting X coordinate for raster instructions
 *	*start_y	: starting X coordinate for raster instructions
 */
static	void	set_up_addressing(P, Q,  step_y, start_x, start_y, swap)
	Ptype	P,Q;
	int	*step_y;
	long	*start_x,
		*start_y;
	boolean	*swap;	
{

	if (P.x < Q.x) {
		*start_x = P.x;
		*swap = FALSE;
	}
	else {
		*start_x = Q.x;
		*swap = TRUE;
	}

	*start_y = P.y;
	if (P.y < Q.y) {
		*step_y = 1;
	}
	else {
		*step_y = -1;
	}
}

static	void	swap_array(array, n)
	Ctype	*array;
	int	n;
{
	Ctype	*left, *right;
	Ctype	temp;

	left = &array[0];
	right = &array[n-1];

	while (left < right) {
		temp = *left;
		*left++ = *right;
		*right-- = temp;
	}
}

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
 *	Formating routines
 */



/*
 * 	Format and encode the raster vector count 
 */
int	formatrasterveccnt(count)
long	count;			/* the vector count */
{
	char	temp[30];	/* temp buffer to work in */
	char	tmp;

	register	char	*dst;
	register	long	src;
	register	int	bitstart, bitcount, srcbitstart, datatype;

	int	i;		/* loop variable */

	float	value;

	memset(temp,0,veccntoutsize);

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
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);

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
				ESPRINTF(E_UNKNOWN,("Invalid graphcap entry",NULL));
				return (-1);
			}
	}

	if (RASTER_VECTOR_COUNT_ENCODING == BINARY)
		buffer(temp,veccntoutsize);

	return (0);
}

/*
 * 	Format and encode the data  -- the data is a colour index
 */
int	formatrasterdata(data,count)
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

	memset(temp,0,dataoutsize);

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
			ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
			return (-1);
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
				ESPRINTF(E_UNKNOWN, ("Invalid graphcap entry",NULL));
				return(-1);
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

	return (0);
}

/*
 *	Displays the cell array using the RASTER_HORIZONAL instruction.
 *	The cell array must be rectangular and stored in packed index
 *	encoding. 
 *	
 */
int	raster(c)
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
	SignedChar	s_char_;

	Runlength = FALSE; 	/* default is packed encoding	*/
	
	nx = c->i[0];		/* number of cells in x direction	*/
	ny = c->i[1];		/* number of cells in y direction	*/

	index_array = (Ctype *) malloc ((unsigned) nx * sizeof (Ctype) + 1);  
	if (! index_array) {
		ESprintf(errno, "malloc(%d)", nx * sizeof(Ctype) + 1);
		return(-1);
	}

	/*
	 * get boundry coordinates
	 */
	px = R_XConvert(c->p[0].x); py = R_YConvert(c->p[0].y);
	rx = R_XConvert(c->p[2].x); ry = R_YConvert(c->p[2].y);
	qy = R_YConvert(c->p[1].y);

	snx = labs((long) rx - px);
	sny = labs((long) ry - qy);
	
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
		for (j = 0; j < nx; j++) {

			/* make sure data available in cgmc     */
			if (index >= c->Cnum && c->more) {
				if (Instr_Dec(c) < 1) {
					return (-1);
				}
				
				index = 0;
			}

			index_array[j] = c->c[index];
			index++;
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
			for(k=0;k<RASTER_HOR_START_SIZE;k++) {
				s_char_ = (SignedChar) RASTER_HOR_START[k];
				switch ((int) s_char_) {
				case XYC:
					(void)formatcoord(px,py,2);
					break;
				case XC:
					(void)formatcoord(px,(long)0,1);
					break;
				case YC:
					(void)formatcoord(py,(long)0,1);
					break;
				case VC:
					(void)formatrasterveccnt(nx);
					break;
				case RL:
					Runlength = TRUE;
					break;
				default:
					buffer(&s_char_,1);
					break;
				}
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
					ESprintf(
						E_UNKNOWN, 
						"Can't plot cell array"
					);
					return(-1);
				}
			}

			buffer(RASTER_HOR_TERM, RASTER_HOR_TERM_SIZE);
		}
	}

	free((Voidptr) index_array);
	return(0);
}



/*
 *	CellArray_
 *	[internal]
 *
 *	render a rectangular cell array
 *
 * on entry
 *	*c		: cgmc containing cell array instruction
 *	P,Q,R		: corners of the cell array (See CGM standard)
 *	nx		: number of cells in x direction
 *	ny		: number of cells in y direction
 * on exit
 *	return		: 0 => Ok, else error
 */
/*ARGSUSED*/
int	CellArray_(c, P, Q, R, nx, ny)
	CGMC		*c;
	Ptype	P, Q, R;
	int	nx, ny;
{
	unsigned int	image_height,	/* image height in pixels	*/
			image_width;	/* image width in pixels	*/

	int		step_y;		/* step size for incrementing in
					 * y direction within the image
					 */

	long		start_x, 
			start_y;	/* where does raster image start */

	int		*rows, 
			*cols;		/* information about the number of
					 * pixels making up a row (col) in
					 * a the cell at row (col)[i]
					 */
	unsigned char	*index_array;	/* color indeces for a cell row	*/
	int		cgmc_index;	/* index into the cgmc		*/
	boolean		swap;
	SignedChar	s_char_;


	register int	i,j,k;

	image_width = ABS(P.x - Q.x) + 1;
	image_height = ABS(P.y - Q.y) + 1;

	/*
	 * don't know how to handle a cell array with zero dimension
	 */
	if (nx == 0 || ny == 0) return (0);

	rows = (int *) malloc ((unsigned) ny * sizeof (int));
	if (! rows) {
		ESprintf(errno, "malloc(%d)", ny * sizeof(int));
		return(-1);
	}
	cols = (int *) malloc ((unsigned) nx * sizeof (int));
	if (! cols) {
		ESprintf(errno, "malloc(%d)", nx * sizeof(int));
		return(-1);
	}
	index_array = (unsigned char *) malloc ((unsigned) nx * sizeof (int));
	if (! index_array) {
		ESprintf(errno, "malloc(%d)", nx * sizeof(Etype));
		return(-1);
	}

	/*
	 * determine starting addresses for raster instructions based on 
	 * orientation of cell array as described by P and Q
	 */
	set_up_addressing(P, Q, &step_y, &start_x, &start_y, &swap);
 
	/*
	 * set up rows and cols arrays with info about number of pixels
	 * making up each cell. We do this to avoid floating point arithmatic
	 * later on
	 */
	SetUpCellArrayIndexing(image_width, image_height, rows, cols, 
					(unsigned) nx, (unsigned) ny);


	/*
	 * process the rows
	 */
	cgmc_index = 0;
	for (i=0; i<ny; i++) {

		/* 
		 * load array of color indecies for row[i] of cells
		 */
                for (k=0; k<nx; k++, cgmc_index++) {
                        index_array[k] = (unsigned char) c->c[cgmc_index];

			/* make sure data available in cgmc     */
			if (cgmc_index == c->Cnum && c->more) {
				if (Instr_Dec(c) < 1) {
					return (-1);
				}
				cgmc_index = 0;
			}

                }

		/*
		 * if cell array rows run backwords from raster rows
		 * swap the order of the cell array row indecis
		 */
		if (swap) {
			swap_array(index_array, nx);
		}

                /*      
		 * the rows of pixels per cell[i]
		 */
                for (j=0; j < rows[i]; j++) {

			/*
			 * format the begin raster instruction
			 */
			for(k=0;k<RASTER_HOR_START_SIZE;k++) {
				s_char_ = (SignedChar) RASTER_HOR_START[k];
				switch ((int) s_char_) {
				case XYC:
					(void)formatcoord(start_x,start_y,2);
					break;
				case XC:
					(void)formatcoord(start_x,(long)0,1);
					break;
				case YC:
					(void)formatcoord(start_y,(long)0,1);
					break;
				case VC:
					(void)formatrasterveccnt((long) nx);
					break;
				case RL:
					Runlength = TRUE;
					break;
				default:
					buffer(&s_char_,1);
					break;
				}
			}
			if (! Runlength) {
				ESprintf(E_UNKNOWN, "Can't plot cell array");
				return(-1);
			}

			start_y += step_y;	/* next row of pixels	*/

			/*
			 * the coloumns
			 */
			for (k=0; k<nx; k++) {

				/*
				 * the coloums of pixels per cell
				 */
				(void)formatrasterdata(index_array[k], cols[k]);
			
			}

			buffer(RASTER_HOR_TERM, RASTER_HOR_TERM_SIZE);
		}
	}

	free((Voidptr) rows);
	free((Voidptr) cols);
	free((Voidptr) index_array);

	return(0);
}

