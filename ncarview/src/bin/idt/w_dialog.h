#ifndef	_w_dialog_
#define	_w_dialog_

#include <X11/Intrinsic.h>

extern	void	OkSDTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);

typedef void (*SelectFunc)(
#ifdef	NeedFuncProto
	Voidptr,
	char*
#endif
);

extern	void	CreateSimpleDialogPopup(
#ifdef	NeedFuncProto
	Widget		button,
	char		*label,
	SelectFunc	select,
	Voidptr		data,
	char		*default_value
#endif
);

#endif	/*	_w_dialog_	*/
