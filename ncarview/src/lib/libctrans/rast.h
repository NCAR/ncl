/*
 *	$Id: rast.h,v 1.5 1992-09-01 23:42:59 clyne Exp $
 */
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
	Rgb	default_rgb[MAX_COLOR_INDEX];
	Rgb	current_rgb[MAX_COLOR_INDEX];
	Rgb	*rgb;
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

