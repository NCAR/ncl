/*
 *	$Id: gcapdev.h,v 1.4 1992-04-03 20:57:38 clyne Exp $
 */
#ifndef	_gcapdev_
#define	_gcapdev_

extern	void	gcap_open(), gcap_close(), gcap_pointflush(), gcap_line(), 
		gcap_devline(), gcap_linestyle(), gcap_linecolour(), 
		gcap_fillcolour(), gcap_linewidth(), gcap_update_color_table();
#endif
