/*
 *	$Id: w_dialog.c,v 1.11 2008-07-27 03:18:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	w_dialog.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Sep 13 11:43:30 MDT 1990
 *
 *	This file manages a simple popup dialog widget. The widget
 *	has a cancel and select button. Upon activation of the select button
 *	a user supplied function is invoked. The label for the box is 
 *	settable as well.
 *
 */
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>

#include <ncarg/c.h>
#include "idt.h"
#include "w_dialog.h"

/*
 * callbacks
 */

typedef	struct	{
	Voidptr	data;		/* data to invoke func() with		*/
	Widget	popup;		/* the popup widget			*/
	Widget	dialog;		/* the dialog widget			*/
#ifdef	__STDC__
	void	(*func)(Voidptr, char *);	/* func to call when selected*/
#else
	void	(*func)();	/* func to call when selected*/
#endif
	} CallbackData;

static	Widget	dialog;

static	void	destroy_popup(popup)
	Widget	popup;
{
	XtDestroyWidget(popup);
}

/*
 *	Callbacks
 */
/*ARGSUSED*/
static	void	Ok(widget, client_data, call_data)
	Widget	widget;
	XtPointer	client_data,	/* call data	*/
			call_data;	/* unused	*/
{
	CallbackData	*cd = (CallbackData *) client_data;

	char	*value;	/* dialog value	*/

	/*
	 * get user input to dialog box
	 */
	value = XawDialogGetValueString(cd->dialog);

	/*
	 * invoke the select function with the given data and the dialog value 
	 */
	((*(cd->func)) (cd->data, value));


	/*
	 * popdown the popup. We're done
	 */
	destroy_popup(cd->popup);
}


/*ARGSUSED*/
static	void	Cancel(widget, client_data, call_data)
	Widget	widget;
	XtPointer	client_data,	/* call data	*/
			call_data;	/* unused 	*/
{
	CallbackData	*cd = (CallbackData *) client_data;

	destroy_popup(cd->popup);

}

/*
 *	The actions for the dialog box
 */

/*
 *	OkSDTranslation
 *
 *	May be invoked to call the Ok() callback
 */
/*ARGSUSED*/
void	OkSDTranslation(widget, event, params, num_params)
	Widget widget;          /* not used     */
	XEvent *event;          /* not used     */
	String *params;         /* not used     */
	Cardinal *num_params;   /* not used     */
{

	XtCallCallbacks(XtNameToWidget(dialog, "ok"),
		XtNcallback, (XtPointer) NULL);
}


/*
 *	CreateSimpleDialogPopup()
 *	[exported]
 *
 *	This routine creates a modal popup containing  a generic configurable
 *	dialog box. 
 *	It is important that the popup is modal, and hence grabs the server,
 *	since this prevents multible instantiations of the dialog box. The
 *	current code does not provide for multible dialog boxes active at
 *	the same time. 
 *
 *	The function select is defined:
 *		void	select(Voidptr data, char *value)
 *
 *	select() is invoked when the "OK" button is selected from the
 *	dialog box. 'data' is passed unmolested to select as it is
 *	received by CreateSimplePopup(). 'value' is the return value
 *	of the dialog box.
 *		
 *
 * on entry
 *	button		: widget that determines position of popup
 *	*label		: label to appear in box
 *	*select()	: function to be called when OK is selected
 *	*data		: data to pass in select as arg 1
 *	*default_value	: default value to be displayed
 */
void	CreateSimpleDialogPopup(button, label, select, data, default_value)
	Widget	button;
	char	*label;
#ifdef	__STDC__
	void	(*select)(Voidptr, char *);
#else
	void	(*select)();
#endif
	Voidptr		data;
	char	*default_value;
{

	Arg		args[10];
	Widget	popup; 
	Position	x, y;		/* position of popup		*/
	Dimension	width, height;	/* dimension of button		*/
	Cardinal	n;


	static	CallbackData	callback_data;

	
	/*
	* This will position the upper left hand corner of the popup at the
	* center of the widget which invoked this callback, which will also
	* become the parent of the popup.  I don't deal with the possibility
	* that the popup will be all or partially off the edge of the screen.
	*/

	n = 0;
	XtSetArg(args[0], XtNwidth, &width); n++;
	XtSetArg(args[1], XtNheight, &height); n++;
	XtGetValues(button, args, n);
	XtTranslateCoords(button, (Position) (width / 2), 
		(Position) (height / 2), &x, &y);

	n = 0;
	XtSetArg(args[n], XtNx, x);				n++;
	XtSetArg(args[n], XtNy, y);				n++;

	popup = XtCreatePopupShell("simpleDialog", transientShellWidgetClass, 
		button, args, n);


	if (! default_value) default_value = "";

	/*
	 * The dialog box with the requested label 
	 */
	n = 0;
	XtSetArg(args[n], XtNvalue, default_value);	n++;
	XtSetArg(args[n], XtNlabel, label);	n++;
	dialog = XtCreateManagedWidget("dialog",dialogWidgetClass,popup,args,n);

	/*
	 * data for the callbacks
	 */
	callback_data.popup = popup;
	callback_data.dialog = dialog;
	callback_data.data = data;
	callback_data.func = select;

	XawDialogAddButton(dialog, "ok", Ok, (XtPointer) &callback_data);

	XawDialogAddButton(dialog, "cancel", Cancel,(XtPointer) &callback_data);


	/*
	 * pop the popup in modal mode
	 */
	XtPopup(popup, XtGrabExclusive);
}


