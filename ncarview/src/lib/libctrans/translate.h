#ifndef	_translate_
#define	_translate_

/*
 */
#define	DEVWIN_LLX	0
#define	DEVWIN_LLY	0
#define	DEVWIN_URX	32767
#define	DEVWIN_URY	32767

/*
 *	macros to convert from virtual coordinates to device and device
 *	raster coordinates
 */
#define	XConvert(x)	(long) (((x) * (X_Scale)) + X_Off)
#define	YConvert(y)	(long) (((y) * (Y_Scale)) + Y_Off)
#define	R_XConvert(x)	(long) (((x) * (R_X_Scale)) + R_X_Off)
#define	R_YConvert(y)	(long) (((y) * (R_Y_Scale)) + R_Y_Off)

/*
 * Macros to convert VDC x and y's into device x and y's
 */
#define	XScale(x)	(long) (labs(X_Scale * (x)))
#define	YScale(y)	(long) (labs(Y_Scale * (y)))

/*
 *	structure to define a coordinate system
 */
typedef	struct	{
	long	llx,	/* lower left x		*/
		lly;	/* lower left y		*/
	long	urx,	/* upper right x	*/
		ury;	/* upper right y	*/
	} CoordRect;

/*
 *	additional translation and scaling to perform on coordinate 
 *	translation
 */
typedef	struct	{
	long	x_off,		/* offset to be added to an X coordinate*/
		y_off;		/* offset to be added to an Y coordinate*/
	double	x_scale,	/* additional X scaling			*/
		y_scale;	/* additional Y scaling			*/
	} CoordModifier;

extern	long	X_Off;
extern	long	Y_Off;
extern	double	X_Scale;
extern	double	Y_Scale;

extern	long	R_X_Off;
extern	long	R_Y_Off;
extern	double	R_X_Scale;
extern	double	R_Y_Scale;

#endif

