#ifndef	_rasdraw_
#define	_rasdraw_

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>     /* Get standard string definations. */
#include <X11/Shell.h>

typedef	struct	{
	Boolean	use_xputpixel;	/* do we need to use XPutPixel		*/
	Boolean	lsbFirst;	/* is machine byte order lsbFirst	*/
	int	bytes_per_pixel;/* bytes per pixel in an ximage		*/
	long	image_size;	/* size in byte of image		*/
	char	*data;		/* the raw image			*/
	} ImageInfo;

typedef	struct	{
	Colormap	cmap;	/* The colormap				*/
	Visual	*visual;	/* The default visual for this server	*/
	int	dsp_depth;	/* number of bit-planes in display	*/
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
	Boolean	first;		/* False until DrawRas first called	*/
	Boolean	batch;		/* Should DrawRas interact with user	*/
	Boolean	load_pal;	/* Should DrawRas load pal from *ras	*/
	XImage	*ximage;	/* The image				*/
	XColor	*xcolors;	/* color map for image			*/
	RGBList	default_pal;	/* default palette to use if !load_pal	*/
	int	xcolor_size;	/* memory allocated to colors		*/
	ImageInfo	image_info;
	CMapInfo	cmap_info;
	Display	*dpy;
	GC	gc;		/* gc for graphics primitives		*/
	} Context;

#ifndef	MIN
#define	MIN(X,Y)	((X) < (Y) ? (X) : (Y))
#endif	MIN
#endif	_rasdraw_
