/*
 *	$Id: x.h,v 1.1 1994-03-30 02:11:35 fred Exp $
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
#define	MAX_INTENSITY	65535

typedef	unsigned long	Pixeltype;

#endif	/*	_x_h_	*/
