/*
 *	$Id: rastdev.h,v 1.4 1992-04-03 20:58:11 clyne Exp $
 */
#ifndef	_rastdev_
#define	_rastdev_

extern	void	rast_open(), rast_close(), rast_pointflush(), rast_line(), 
		rast_devline(), rast_linestyle(), rast_linecolour(), 
		rast_fillcolour(), rast_linewidth(), rast_update_color_table();
#endif
