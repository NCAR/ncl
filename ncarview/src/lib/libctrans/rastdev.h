/*
 *	$Id: rastdev.h,v 1.5 1993-04-27 20:42:27 clyne Exp $
 */
#ifndef	_rastdev_
#define	_rastdev_

extern	void	rast_open(), rast_close(), rast_pointflush(), rast_line(), 
		rast_devline(), rast_linestyle(), rast_linecolour(), 
		rast_fillcolour(), rast_linewidth();
extern	int	rast_update_color_table();
#endif
