/*
 *	$Id: rasdraw.h,v 1.2 1991-06-18 14:54:43 clyne Exp $
 */
#ifndef	_rasdraw_
#define	_rasdraw_


typedef	struct	{
	Boolean	use_xputpixel;	/* do we need to use XPutPixel		*/
	Boolean	lsbFirst;	/* is machine byte order lsbFirst	*/
	int	bytes_per_pixel;/* bytes per pixel in an ximage		*/
	long	image_size;	/* size in byte of image		*/
	} ImageInfo;

typedef	struct	{
	int	dsp_depth;	/* number of bit-planes in visual	*/
	long	max_colors;	/* maximum number of colors possible	*/
	Boolean	writeable;	/* is cmap writeable			*/
	long 	*static_pal;	/* lookup table for immutable color map	*/
	} CMapInfo;

typedef	struct	{
	unsigned char	*red,
			*green,
			*blue;
	unsigned int	ncolor;
	} RGBList;

typedef	struct	{
	Widget	toplevel;	/* the toplevel widget			*/
	XtAppContext	app_con;/* application context for this intance	*/
	Widget	canvas;		/* we draw in this widgets window	*/
	Boolean	batch;		/* Should DrawRas interact with user	*/
	Boolean	load_pal;	/* Should DrawRas load pal from *ras	*/
	XColor	*xcolors;	/* color map for image			*/
	RGBList	default_pal;	/* default palette to use if !load_pal	*/
	int	xcolor_size;	/* memory allocated to colors		*/
	Visual	*visual;	/* The visual for the graphics canvas	*/
	int	vis_class;	/* class of visual			*/
	unsigned encoding;	/* available encodings (8 or 24 bit	*/
	XImage	*ximage;	/* The image				*/
	ImageInfo	image_info;
	Colormap	cmap;	/* The colormap				*/
	CMapInfo	cmap_info;
	Display	*dpy;
	GC	gc;		/* gc for graphics primitives		*/
	} Context;

#define	RASDRAW_0BIT	0
#define	RASDRAW_8BIT	(1L<<0)
#define	RASDRAW_24BIT	(1L<<1)

#ifndef	MIN
#define	MIN(X,Y)	((X) < (Y) ? (X) : (Y))
#endif	MIN
#endif	_rasdraw_
