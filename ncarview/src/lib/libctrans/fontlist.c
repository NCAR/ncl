/*
 *	$Id: fontlist.c,v 1.18 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	<errno.h>
#include	<ncarg/c.h>
#include	"default.h"
#include	"defines.h"
#include	"ctrans.h"
#include	"fontlist.h"
/*	fontlist.c:
 * 
 *	Author	John Clyne	(clyne@bierstadt.UCAR.EDU)
 *		7/18/88
 *
 *
 *		This file handles the CGM functions "Font List" and
 *	"Set Font Index". When a new Font List command is received
 *	the old list is written over. Subsequent Set Font Index commands
 *	cause the text processor to be initialized with the corresponding
 *	font. Currently there is a default font list that is based on
 *	the Hershey fonts. 
 */


static	char *my_strdup(const char *s1)
{
	char	*s2;

	s2 = malloc(strlen(s1) + 1);
	if (! s2) return (NULL);

	strcpy(s2, s1);

	return(s2);
}

#define	MAXFONT		25	/* maximum number of fonts in the fontlist */	


static	char	*Fontlist[MAXFONT];
static	boolean	DefaultFont = FALSE;

extern	int	Init_Font();
extern	char	msg[];

static	int	fontindex = -1;	

/*	initfontlist:
 *
 *		intitialize the fontlist table
 */
void	InitFontList(const char *default_font)
{
	int	i;

	static	boolean	initialized = FALSE;

	if (initialized) {
		for (i=0; i<MAXFONT; i++) {
			if (Fontlist[i]) free(Fontlist[i]);
		}
	}
	DefaultFont = FALSE;

	i = 0;
	if (default_font) {
		Fontlist[i++] = my_strdup(default_font);
		DefaultFont = TRUE;
	}
	else {
		Fontlist[i++] = my_strdup("DEFAULT");
	}


	Fontlist[i++] = my_strdup("HERSHEY:CARTOGRAPHIC_ROMAN");
	Fontlist[i++] = my_strdup("HERSHEY:CARTOGRAPHIC_GREEK");
	Fontlist[i++] = my_strdup("HERSHEY:SIMPLEX_ROMAN");
	Fontlist[i++] = my_strdup("HERSHEY:SIMPLEX_GREEK");
	Fontlist[i++] = my_strdup("HERSHEY:SIMPLEX_SCRIPT");
	Fontlist[i++] = my_strdup("HERSHEY:COMPLEX_ROMAN");
	Fontlist[i++] = my_strdup("HERSHEY:COMPLEX_GREEK");
	Fontlist[i++] = my_strdup("HERSHEY:COMPLEX_SCRIPT");
	Fontlist[i++] = my_strdup("HERSHEY:COMPLEX_ITALIC");
	Fontlist[i++] = my_strdup("HERSHEY:COMPLEX_CYRILLIC");
	Fontlist[i++] = my_strdup("HERSHEY:DUPLEX_ROMAN");
	Fontlist[i++] = my_strdup("HERSHEY:TRIPLEX_ROMAN");
	Fontlist[i++] = my_strdup("HERSHEY:TRIPLEX_ITALIC");
	Fontlist[i++] = my_strdup("HERSHEY:GOTHIC_GERMAN");
	Fontlist[i++] = my_strdup("HERSHEY:GOTHIC_ENGLISH");
	Fontlist[i++] = my_strdup("HERSHEY:GOTHIC_ITALIAN");
	Fontlist[i++] = my_strdup("HERSHEY:MATH_SYMBOLS");
	Fontlist[i++] = my_strdup("HERSHEY:SYMBOL_SET1");
	Fontlist[i++] = my_strdup("HERSHEY:SYMBOL_SET2");

	for(; i<MAXFONT; i++) {
		Fontlist[i] = NULL;
	}

	initialized = TRUE;
}



/*
 *
 *		This routine builds the table of indexable fonts
 *	on entry:
 *		c : is the cgmc containing the font list
 *	on exit:
 *		Fontlist contains the list of fonts
 */
int	FontList(c)
CGMC *c;
{
	int	i;
	int	status = 0;

	/* reset fontindex to null	*/
	fontindex = -1;


	if (c->Snum > MAXFONT) {
		ESprintf(E_UNKNOWN, "Font list too large(%d)", c->Snum);
		status = -1;
		c->Snum = MAXFONT;
	}

	/* copy fontlist to the table	*/
	for (i=0; i < c->Snum; i++) {
		if (! (i==0 && DefaultFont)) {

			if (Fontlist[i]) free(Fontlist[i]);
			Fontlist[i] = my_strdup(c->s->string[i]);
			i++;
		}

	}
	return(status);
}

int	setFont(font_index)
	IXtype	font_index;
{
	char	*fcap;
	char	*font;
	int	status = 0;


	if (font_index < 0 || font_index >= MAXFONT) {
		ESprintf(E_UNKNOWN, "Invalid font index(%d)", font_index);;
		return(-1);
	}

	font = Fontlist[font_index - 1];

	if (! font) {
		ESprintf(E_UNKNOWN, "Invalid font index(%d)", font_index);;
		return(-1);
	}

	/* this is a hack to prevent ctrans from reinitializeing the 
	 * text processor if it has already been initialized with
	 * the same font. (apparently NCAR's Fortran GKS binding
	 * issues a TEXT FONT INDEX command every time it issues
	 * a text commamnd
	 */
	if (fontindex == font_index)
		return(0);
	else
		fontindex = font_index;


	/*
	 *	map the HERSHEY fonts into font1 -> font20. 
	 */
	if (strcmp(font, "DEFAULT") == 0) {
		font = DEFAULTFONT;
	} else if (strcmp(font,"HERSHEY:CARTOGRAPHIC_ROMAN")== 0){
		font = "font2";
	} else if (strcmp(font, "HERSHEY:CARTOGRAPHIC_GREEK")== 0) {
		font = "font3";
	} else if (strcmp(font, "HERSHEY:SIMPLEX_ROMAN")== 0) {
		font = "font4";
	} else if (strcmp(font, "HERSHEY:SIMPLEX_GREEK")== 0) {
		font = "font5";
	} else if (strcmp(font, "HERSHEY:SIMPLEX_SCRIPT")== 0) {
		font = "font6";
	} else if (strcmp(font, "HERSHEY:COMPLEX_ROMAN")== 0) {
		font = "font7";
	} else if (strcmp(font, "HERSHEY:COMPLEX_GREEK")== 0) {
		font = "font8";
	} else if (strcmp(font, "HERSHEY:COMPLEX_SCRIPT")== 0) {
		font = "font9";
	} else if (strcmp(font, "HERSHEY:COMPLEX_ITALIC")== 0) {
		font = "font10";
	} else if (strcmp(font, "HERSHEY:COMPLEX_CYRILLIC")== 0) {
		font = "font11";
	} else if (strcmp(font, "HERSHEY:DUPLEX_ROMAN")== 0) {
		font = "font12";
	} else if (strcmp(font, "HERSHEY:TRIPLEX_ROMAN")== 0) {
		font = "font13";
	} else if (strcmp(font, "HERSHEY:TRIPLEX_ITALIC")== 0) {
		font = "font14";
	} else if (strcmp(font, "HERSHEY:GOTHIC_GERMAN")== 0) {
		font = "font15";
	} else if (strcmp(font, "HERSHEY:GOTHIC_ENGLISH")== 0) {
		font = "font16";
	} else if (strcmp(font, "HERSHEY:GOTHIC_ITALIAN")== 0) {
		font = "font17";
	} else if (strcmp(font, "HERSHEY:MATH_SYMBOLS")== 0) {
		font = "font18";
	} else if (strcmp(font, "HERSHEY:SYMBOL_SET1")== 0) {
		font = "font19";
	} else if (strcmp(font, "HERSHEY:SYMBOL_SET2")== 0) {
		font = "font20";
	}

	/*
	 *	building the full path to the font cap
	 */
	if ( !(fcap = getFcapname(font))) {
		return(-1);
	}

	/* initialize the font processor with the selected font	*/

	if(Init_Font(fcap) != 0) {
		return (-1);
	}

	return (status);
}
