/*
 *	$Id: fontlist.h,v 1.2 1995-07-07 22:49:39 clyne Exp $
 */
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
