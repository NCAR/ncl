#ifndef	_idt_
#define	_idt_

typedef	struct	{
	XFontStruct	*x_font;
	/*
	 * the rest are translator args
	 */
	char		*font;
	char		*device;
	Boolean		history;
	Boolean		soft;
	char		*lmin,
			*lmax,
			*lscale;
	char		*foreground,
			*background;
	Boolean		reverse;
	} AppData, *AppDataPtr;

#define	TRANS_ARG_COUNT	10	/* number of translator args	*/

/*
 * names of options recognized by translator
 */
#define	TR_FONT		"-font"
#define	TR_DEVICE	"-device"
#define	TR_SOFT		"-soft"
#define	TR_LMIN		"-lmin"
#define	TR_LMAX		"-lmax"
#define	TR_LSCALE	"-lscale"
#define	TR_FOREGROUND	"-foreground"
#define	TR_BACKGROUND	"-background"
#define	TR_REVERSE	"-reverse"

#define	MAX_TEXT_LINES	20

#endif
