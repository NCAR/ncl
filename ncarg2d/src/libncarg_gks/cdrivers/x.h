/*
 *	$Id: x.h,v 1.3 1996-01-12 21:13:08 boote Exp $
 */
/*
 *      File:		x.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	Some common defines for the x device driver
 *
 */
#ifndef	_x_h_
#define	_x_h_

#define	DEFAULT_WIDTH	512
#define	DEFAULT_HEIGHT	512

#define	MIN_WIDTH	10
#define	MIN_HEIGHT	10

/*
 * maximum X11 color intensity
 */
#define	MAX_INTENSITY	(65535)
/*
 * max distance in RGB space ie.
 *	SQRT(MAX_INTENSITY^2 + MAX_INTENSITY^2 + MAX_INTENSITY^2)
 */
#define	MAX_INTEN_DIST	(113509)

#define	MAX_DPY_LEN	(80)

typedef	unsigned long	Pixeltype;

enum XWorkType_ { XREG = 8, XUSRWIN = 7 };

typedef enum XWorkType_ XWorkType;

typedef enum XColModel_{
	CM_UNDEFINED = -1,
	CM_SHARED = 0,
	CM_PRIVATE = 1,
	CM_MIXED = 2
} XColModel;

#endif	/*	_x_h_	*/
