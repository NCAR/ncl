/*
 *	$Id: gcapdev.h,v 1.8 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1993                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef	_gcapdev_
#define	_gcapdev_

extern	void	gcap_open(), gcap_close(), gcap_pointflush(), gcap_line(), 
		gcap_devline(), gcap_linestyle(), gcap_linecolour(), 
		gcap_fillcolour(), gcap_linewidth();
extern	int	gcap_update_color_table();
#endif
