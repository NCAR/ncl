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

extern	void	CreateSimpleDialogPopup(
#ifdef	NeedFuncProto
	Widget	button,
	char	*label,
	void	(*select)(Voidptr, char *),
	Voidptr		data,
	char	*default_value
#endif
);

#endif	/*	_w_dialog_	*/
