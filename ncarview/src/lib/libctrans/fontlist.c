/*
 *	$Id: fontlist.c,v 1.9 1992-09-09 15:09:26 clyne Exp $
 */
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
#include	<errno.h>
#include	<ncarg/c.h>
#include	"default.h"
#include	"defines.h"
#include	"ctrans.h"
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


#define	MAXFONT		25	/* maximum number of fonts in the fontlist */	
#define	MAX_F_NAME_LEN	40	/* max length of a font name		*/


static	char	Fontlist[MAXFONT][MAX_F_NAME_LEN];

extern	int	Init_Font();
extern	char	msg[];

static	int	fontindex = -1;	

/*	initfontlist:
 *
 *		intitialize the fontlist table
 */
InitFontList()
{
	int	i;

	static	boolean	initialized = FALSE;

	



	if (initialized) return; 
	
	(void) strncpy(Fontlist[0], "DEFAULT", MAX_F_NAME_LEN - 1);
	(void) strncpy(
		Fontlist[1], "HERSHEY:CARTOGRAPHIC_ROMAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[2], "HERSHEY:CARTOGRAPHIC_GREEK", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[3], "HERSHEY:SIMPLEX_ROMAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[4], "HERSHEY:SIMPLEX_GREEK", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[5], "HERSHEY:SIMPLEX_SCRIPT", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[6], "HERSHEY:COMPLEX_ROMAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[7], "HERSHEY:COMPLEX_GREEK", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[8], "HERSHEY:COMPLEX_SCRIPT", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[9], "HERSHEY:COMPLEX_ITALIC", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[10], "HERSHEY:COMPLEX_CYRILLIC", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[11], "HERSHEY:DUPLEX_ROMAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[12], "HERSHEY:TRIPLEX_ROMAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[13], "HERSHEY:TRIPLEX_ITALIC", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[14], "HERSHEY:GOTHIC_GERMAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[15], "HERSHEY:GOTHIC_ENGLISH", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[16], "HERSHEY:GOTHIC_ITALIAN", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(
		Fontlist[17], "HERSHEY:MATH_SYMBOLS", MAX_F_NAME_LEN - 1
	);
	(void) strncpy(Fontlist[18], "HERSHEY:SYMBOL_SET1", MAX_F_NAME_LEN - 1);
	(void) strncpy(Fontlist[19], "HERSHEY:SYMBOL_SET2", MAX_F_NAME_LEN - 1);

	for(i=20; i<MAXFONT; i++) {
		Fontlist[i][0] = '\0';
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
	for (i=0; (unsigned short) i < c->Snum; i++) {
		(void) strncpy(Fontlist[i],c->s->string[i], MAX_F_NAME_LEN - 1);
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
		ESprintf(EINVAL, "Invalid font index(%d)", font_index);;
		status = -1;
		font_index = 1;
	}

	font = Fontlist[font_index - 1];

	if (! *font) {
		status = -1;
		font = "DEFAULT";
	}

	/* this is a hack to prevent ctrans from reinitializeing the 
	 * text processor if it has already been initialized with
	 * the same font. (apparently NCAR's Fortran GKS binding
	 * issues a TEXT FONT INDEX command every time it issues
	 * a text commamnd
	 */
	if (fontindex == font_index)
		return(status);
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
