/*
 *	$Id: rastdev.h,v 1.2 1991-06-18 15:03:57 clyne Exp $
 */
#ifndef	_rastdev_
#define	_rastdev_

extern	void	rast_open(), rast_close(), rast_pointflush(), rast_line(), 
		rast_devline(), rast_linestyle(), rast_linecolour(), 
		rast_fillcolour(), rast_linewidth();
#endif
