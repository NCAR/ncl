#ifndef	_w_text_
#define	_w_text_

#include <X11/Intrinsic.h>

extern	Widget	InitText(
#ifdef	NeedFuncProto
	Widget	parent
#endif
);

extern	void	AppendText(
#ifdef	NeedFuncProto
	char	*t
#endif
);

#endif	/* _w_text_	*/
