/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#define	MAX_COLOR_SIZE	256

typedef	unsigned long	Pixeltype;

#define POINTS_ALLOCED  1024
typedef	struct  {
        XPoint  *P;
        int     size;   /* size of array P      */
        } Point_buf;	/* the point buffer     */


#define	X_MAX_RGB	65535

#ifdef	X11_class0


Display		*dpy;		/* X server connection		*/
Drawable	win;		/* Window ID 			*/
Drawable	drawable;	/* the drawable			*/
Visual		*visual;
int	DspDepth;		/* depth in pixels of display	*/

GC	lineGC;			/* line GC 	See section 5.3	*/
GC	markerGC;		/* polymarker GC		*/
GC	polygonGC;		/* polygon GC			*/
GC	cellGC;			/* cell array GC		*/
GC	tileGC;			/* GC for fill polygon tileing	*/

Point_buf	Points = {NULL, 0};

XGCValues	gcv;		/* structure for manipulating a GC	*/
#else

extern	Display		*dpy;		
extern	Drawable	win;	
extern	Drawable	drawable;	
extern	Visual		*visual;
extern	int		DspDepth;

extern	GC	lineGC;	
extern	GC	markerGC;
extern	GC	polygonGC;
extern	GC	cellGC;
extern	GC	tileGC;

extern	Point_buf	Points;

extern	XGCValues	gcv;

#endif	/*	X11_class0	*/

