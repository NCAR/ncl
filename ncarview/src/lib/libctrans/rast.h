/*
 *	$Id: rast.h,v 1.9 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1994                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef	_color_
#define	_color_

#define	MAX_COLOR	256
#define	MAX_COLOR_INDEX	256

typedef	struct	{
	unsigned char	red,
			green,
			blue;
	} Rgb;

/*
 *	we use this struct to maintain a default and a current colormap.
 */	
typedef	struct	{
	Rgb	rgb[MAX_COLOR_INDEX];
	} RasColrTab;

#define	RAS_PUT_PIX(ras, x,y, index, table, direct) \
	if (direct) { \
		DIRECT_RED((ras),(x),(y)) = (table).rgb[(index)].red; \
		DIRECT_GREEN((ras),(x),(y)) = (table).rgb[(index)].green; \
		DIRECT_BLUE((ras),(x),(y)) = (table).rgb[(index)].blue; \
	} \
	else { \
		INDEXED_PIXEL((ras),(x),(y)) = (index); \
	} 
#endif

