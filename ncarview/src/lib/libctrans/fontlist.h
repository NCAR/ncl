/*
 *	$Id: fontlist.h,v 1.1 1994-03-14 20:55:41 clyne Exp $
 */
#ifndef	_fontlist_h_
#define	_fontlist_h_

#include "cgmc.h"

extern void	InitFontList();

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
