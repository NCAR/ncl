/*
 *	$Id: gcapdev.h,v 1.3 1991-10-04 15:19:25 clyne Exp $
 */
#ifndef	_gcapdev_
#define	_gcapdev_

extern	void	gcap_open(), gcap_close(), gcap_pointflush(), gcap_line(), 
		gcap_devline(), gcap_linestyle(), gcap_linecolour(), 
		gcap_fillcolour(), gcap_linewidth(), gcap_update_color_table();
#endif
