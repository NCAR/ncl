/*
 *	$Id: w_text.c,v 1.13 2008-07-27 03:18:39 haley Exp $
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
 *	w_text.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Oct 18 18:24:43 MDT 1990
 *
 *	This file manages the text widget in the main idt display
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <ncarg/c.h>
#include "idt.h"
#include "text.h"

static	Widget	textWidget;

#define	MSGBUFSIZE	4096

/*
 * message buffer used internally by text widget
 */
static	char	msgBuf[MSGBUFSIZE];


/*
 *	strccpy
 *	[internal]
 *	
 *	like strcpy, only copies until null byte OR char c is reached
 */
static	char	*strccpy(s, t, c)
	char	*s, *t;
	char	c;
{
	char	*cptr = s;

	/*SUPPRESS 570*/
	while (*t && (*t != c))
		*s++ = *t++;

	*s = '\0';

	return(cptr);
}


/*
 *	append_line
 *	[internal]
 *
 *	Append a single line of text to the text widget. The line must be
 *	terminated with a new line.
 * on entry
 *	*s		: line to be appended
 */
static	void	append_line(s)
	char	*s;
{
	static long ptr = 0;	/* # of chars currently used in msg buf */

	XawTextBlock text_block;

	int slength = strlen(s);


	/* 
	 * Will this message fit in the message buff?
	 */
	if((slength + ptr) > sizeof(msgBuf)) {
		/* reset message buffer */
		int i;
		ptr = 0;
		for(i=0; i<sizeof(msgBuf); i++) msgBuf[i] = ' ';
		for(i=0; i<sizeof(msgBuf); i+=10) msgBuf[i] = '\n';
		msgBuf[sizeof(msgBuf)-1] = '\0';
	}

	strncpy((msgBuf+ptr),s,slength); /* copy mesg except for null */

	text_block.ptr = s;
	text_block.firstPos = 0;
	text_block.length = slength;
	text_block.format = FMT8BIT;
	(void) XawTextReplace(textWidget,ptr,strlen(s)+ptr,&text_block);

	XawTextDisplay(textWidget);
	XawTextSetInsertionPoint(textWidget,ptr+slength);
	ptr = ptr + slength;

}

/*
 *	InitText
 *	[exported]
 *
 *	init the module
 * on entry
 *	parent		: text widget to manage text of
 *	maxlines	: maximum lines to be displayed in the widget
 */
Widget	InitText(parent) 
	Widget	parent;
{
	Arg	args[10];
	Cardinal	n = 0;

	n = 0;
	XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways); n++;
	XtSetArg(args[n], XtNsensitive, TRUE);		n++;
	XtSetArg(args[n], XtNlength, sizeof(msgBuf));	n++;
	XtSetArg(args[n], XtNstring, msgBuf);		n++;
	XtSetArg(args[n], XtNeditType, XawtextEdit);	n++;
	XtSetArg(args[n], XtNuseStringInPlace, True);	n++;

	textWidget = XtCreateManagedWidget(
		"text", asciiTextWidgetClass, parent, args, n
	);

	return(textWidget);
}

/*
 *	AppendText
 *	[exported]
 *
 *	Append a string of text to text being displayed in the text widget
 * on entry
 *	*t		: text to append
 */
void	AppendText(t)
	char	*t;
{
	char	*buf;


	if (! (buf = malloc((unsigned) (strlen(t) + 1)))) {
		perror("malloc()");
		return;
	}
	
	/*
	 * break up the text string into lines and append the lines one at
	 * a time.
	 */
	while (*(strccpy(buf, t, '\n'))) {
		(void) strcat(buf, "\n");

		append_line(buf);

		if (!(t = strchr(t, '\n'))) break;
		t++;
	}

	free((Voidptr) buf);
}
