/*
 *	$Id: rast.h,v 1.3 1992-04-03 20:58:04 clyne Exp $
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
#endif;
