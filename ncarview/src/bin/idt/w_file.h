#ifndef	_w_file_
#define	_w_file_

#include <X11/Intrinsic.h>
#include "idt.h"

extern	void	CreateFileSelectPopup(
#ifdef	NeedFuncProto
	Widget	button,
	FuncPtrPasser	*select_action
#endif
);

extern	void	FinderTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);

extern	void	OkFileTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);



extern	void SelectFileTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);

#endif	/* _w_file.h_	*/
