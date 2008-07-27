/*
 *	$Id: rastdev.h,v 1.8 2008-07-27 03:22:39 haley Exp $
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

#ifndef	_rastdev_
#define	_rastdev_

extern	void	rast_open(), rast_close(), rast_pointflush(), rast_line(), 
		rast_devline(), rast_linestyle(), rast_linecolour(), 
		rast_fillcolour(), rast_linewidth();
extern	int	rast_update_color_table();
#endif
