/*
 *	$Id: w_text.c,v 1.4 1992-04-03 23:21:34 clyne Exp $
 */
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
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/AsciiText.h>

#include <ncarv.h>
#include "idt.h";

static	Widget	textWidget;
static	int	maxLines;
static	int	numLines;


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

	while ((*t != c) && (*s++ = *t++))
		;

	if (*s)
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
 *	*line		: line to be appended
 */
static	void	append_line(line)
	char	*line;
{
	String		text;
	int		len_text,
			len_line;

	Arg		args[5];
	Cardinal	n;


	String		buf, bufptr;

	/*
	 * see if there is already text in the text widget
	 */
	if (numLines != 0) {
		n = 0;
		XtSetArg(args[n], XtNstring, &text);	n++;
		XtGetValues(textWidget, args, n);
	}
	else 
		text = "";

	/*
	 * delete the first line if append will overflow max lines
	 */
	if (numLines == maxLines) {
		while (*text != '\n')
			text++;
		text++;
		numLines--;
	}

	/*
	 * concat new line to origial text
	 */
	len_text = strlen((char * ) text);
	len_line = strlen((char * ) line);

	buf = icMalloc((unsigned) (len_text + len_line + 1));
	bufptr = buf;

	bcopy((char *) text, (char *) bufptr, len_text);
	bufptr += len_text;
	bcopy((char *) line, (char *) bufptr, len_line);
	bufptr += len_line;
	*bufptr = '\0';

	/*
	 * display the nex text in the text widget
	 */
	n = 0;
	XtSetArg(args[n], XtNstring, buf);	n++;
	XtSetValues(textWidget, args, n);

	numLines++;

	cfree(buf);
}

/*
 *	InitText
 *	[exported]
 *
 *	init the module
 * on entry
 *	text		: text widget to manage text of
 *	maxlines	: maximum lines to be displayed in the widget
 */
void	InitText(text, maxlines) 
	Widget	text;
	int	maxlines;
{
	if (maxlines < 1) {
		(void) fprintf(stderr, 
			"Text widget requires at least one line\n");

		exit(1);
	}
	textWidget = text;
	maxLines = maxlines;
	numLines = 0;
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
	char	*s;
	int	i;
	String		text;
	Arg		args[5];
	Cardinal	n;
	XawTextPosition	position = 0;	/* char displayed in upper left */

	extern	char	*strrchr();
	extern	char	*strchr();
	extern AppData App_Data;


	buf = icMalloc((unsigned) (strlen(t) + 1));
	
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


	/*
	 * if we have more then x lines of text when we append make sure 
	 * the last few lines in the text widget are the ones displayed. By
	 * default the text widget will display the first character in the
	 * top left corner of the display
	 */
	if (numLines >= App_Data.message_height) {
		n = 0;
		XtSetArg(args[n], XtNstring, &text);	n++;
		XtGetValues(textWidget, args, n);

		s = strrchr(text, '\n');
		for (i = 0; i < App_Data.message_height - 1; i++) {
			s--;
			while(*s != '\n') s--;	
		}
		s++;
		position = s - text;

		n = 0;
		XtSetArg(args[n], XtNdisplayPosition, position);	n++;
		XtSetValues(textWidget, args, n);
	}

	cfree(buf);
}
