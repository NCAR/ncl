/*
 *	$Id: gcapdev.h,v 1.5 1993-04-27 20:32:52 clyne Exp $
 */
#ifndef	_gcapdev_
#define	_gcapdev_

extern	void	gcap_open(), gcap_close(), gcap_pointflush(), gcap_line(), 
		gcap_devline(), gcap_linestyle(), gcap_linecolour(), 
		gcap_fillcolour(), gcap_linewidth();
extern	int	gcap_update_color_table();
#endif
