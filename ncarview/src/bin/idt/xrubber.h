#ifndef	_xrubber_
#define	_xrubber_

#include <X11/Xlib.h>

extern	char	*ZoomCoords(
#ifdef	NeedFuncProto
	Display	*dpy,
	Window	root,
	float	*llx, 
	float	*lly, 
	float	*urx, 
	float	*ury
#endif
);

#endif	/* _xrubber_	*/
