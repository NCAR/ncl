#ifndef	_idt_
#define	_idt_

#define	DEBUG

typedef	struct	{
	XFontStruct	*x_font;
	char		*font;
	char		*device;
	Boolean		history;
	} AppData, *AppDataPtr;


#endif

#define	MAX_TEXT_LINES	20
