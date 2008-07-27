/*
 *	$Id: fontlist.h,v 1.5 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1995                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef	_fontlist_h_
#define	_fontlist_h_

#include "cgmc.h"

extern void	InitFontList(
#ifdef	NeedFuncProto
	const char	*default_font
#endif
);

extern	int	FontList(
#ifdef	NeedFuncProto
	CGMC	*c
#endif
);

extern	int	setFont(
#ifdef	NeedFuncProto
	IXtype	font_index
#endif
);

#endif	/* _fontlist_h_	*/
