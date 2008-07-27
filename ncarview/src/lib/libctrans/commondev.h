/*
 *	$Id: commondev.h,v 1.8 2008-07-27 03:22:39 haley Exp $
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

#ifndef	_commondev_
#define	_commondev_

typedef	struct	ComDev_ {
	char	*name;
	void	(*open)();
	void	(*close)();
	void	(*point_flush)();
	void	(*line)();
	void	(*devline)();
	void	(*setlinestyle)();
	void	(*setlinecolour)();
	void	(*setfillcolour)();
	void	(*setlinewidth)();
	int	(*update_color_table)();
	} ComDev;
	

#endif
