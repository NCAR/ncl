/*
 *	$Id: fontlist.c,v 1.5 1992-02-07 17:39:06 clyne Exp $
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

#ifdef	SYSV
#include	<string.h>
#else
#include	<strings.h>
#endif SYSV

#include	<cterror.h>
#include	<ncarv.h>
#include	"default.h"
#include	"defines.h"
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


char	*Fontlist[MAXFONT];

extern	char	*getFcapname();
extern	Ct_err	Init_Font();
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
	

	/* build default font list.
	 * This table is built dynamically so it can accomodate different
	 * sized font lists
 	 */
	Fontlist[0] = (char *) icMalloc ((unsigned) strlen("DEFAULT")+1);
	(void) strcpy(Fontlist[0], "DEFAULT");

	Fontlist[1] = (char *) icMalloc
			((unsigned) strlen("HERSHEY:CARTOGRAPHIC_ROMAN")+1);
	(void) strcpy(Fontlist[1], "HERSHEY:CARTOGRAPHIC_ROMAN");

	Fontlist[2] = (char *) icMalloc
			((unsigned) strlen("HERSHEY:CARTOGRAPHIC_GREEK")+1);
	(void) strcpy(Fontlist[2], "HERSHEY:CARTOGRAPHIC_GREEK");

	Fontlist[3] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:SIMPLEX_ROMAN") + 1);
	(void) strcpy(Fontlist[3], "HERSHEY:SIMPLEX_ROMAN");

	Fontlist[4] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:SIMPLEX_GREEK") + 1);
	(void) strcpy(Fontlist[4], "HERSHEY:SIMPLEX_GREEK");

	Fontlist[5] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:SIMPLEX_SCRIPT") + 1);
	(void) strcpy(Fontlist[5], "HERSHEY:SIMPLEX_SCRIPT");

	Fontlist[6] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:COMPLEX_ROMAN") + 1);
	(void) strcpy(Fontlist[6], "HERSHEY:COMPLEX_ROMAN");

	Fontlist[7] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:COMPLEX_GREEK") + 1);
	(void) strcpy(Fontlist[7], "HERSHEY:COMPLEX_GREEK");

	Fontlist[8] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:COMPLEX_SCRIPT") + 1);
	(void) strcpy(Fontlist[8], "HERSHEY:COMPLEX_SCRIPT");

	Fontlist[9] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:COMPLEX_ITALIC") + 1);
	(void) strcpy(Fontlist[9], "HERSHEY:COMPLEX_ITALIC");

	Fontlist[10] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:COMPLEX_CYRILLIC")+1);
	(void) strcpy(Fontlist[10], "HERSHEY:COMPLEX_CYRILLIC");

	Fontlist[11] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:DUPLEX_ROMAN") + 1);
	(void) strcpy(Fontlist[11], "HERSHEY:DUPLEX_ROMAN");

	Fontlist[12] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:TRIPLEX_ROMAN") + 1);
	(void) strcpy(Fontlist[12], "HERSHEY:TRIPLEX_ROMAN");

	Fontlist[13] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:TRIPLEX_ITALIC") + 1);
	(void) strcpy(Fontlist[13], "HERSHEY:TRIPLEX_ITALIC");

	Fontlist[14] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:GOTHIC_GERMAN") + 1);
	(void) strcpy(Fontlist[14], "HERSHEY:GOTHIC_GERMAN");

	Fontlist[15] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:GOTHIC_ENGLISH") + 1);
	(void) strcpy(Fontlist[15], "HERSHEY:GOTHIC_ENGLISH");

	Fontlist[16] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:GOTHIC_ITALIAN") + 1);
	(void) strcpy(Fontlist[16], "HERSHEY:GOTHIC_ITALIAN");

	Fontlist[17] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:MATH_SYMBOLS") + 1);
	(void) strcpy(Fontlist[17], "HERSHEY:MATH_SYMBOLS");

	Fontlist[18] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:SYMBOL_SET1") + 1);
	(void) strcpy(Fontlist[18], "HERSHEY:SYMBOL_SET1");

	Fontlist[19] = (char *) icMalloc 
			((unsigned) strlen("HERSHEY:SYMBOL_SET2") + 1);
	(void) strcpy(Fontlist[19], "HERSHEY:SYMBOL_SET2");

	for (i=20; i<MAXFONT; i++) {
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
Ct_err	FontList(c)
CGMC *c;
{
	int	i;
#ifdef DEBUG
	(void)fprintf(stderr,"FontList\n");
#endif

	/* reset fontindex to null	*/
	fontindex = -1;


	if (c->Snum > MAXFONT) {
		ct_error(NT_FLTB,"");
		c->Snum = MAXFONT;
	}

	/* copy fontlist to the table	*/
	for (i=0;i<c->Snum;i++) {
		if (Fontlist[i] != NULL) free ((char *) Fontlist[i]);

		Fontlist[i] = (char *) icMalloc ((unsigned) 
			((strlen(c->s->string[i]) + 1) * sizeof(char)));

		(void) strcpy(Fontlist[i],c->s->string[i]);
	}
	return(OK);
}

Ct_err	setFont(font_index)
	IXtype	font_index;
{
	char	*fcap;
	char	*font;


	if (font_index < 0 || font_index >= MAXFONT) {
		ct_error(NT_UFONT, "using default font");
		font_index = 1;
	}

	font = Fontlist[font_index - 1];

	if (! font) {
		ct_error(NT_UFONT, "using default font");
		font = "DEFAULT";
	}

	/* this is a hack to prevent ctrans from reinitializeing the 
	 * text processor if it has already been initialized with
	 * the same font. (apparently NCAR's Fortran GKS binding
	 * issues a TEXT FONT INDEX command every time it issues
	 * a text commamnd
	 */
	if (fontindex == font_index)
		return(OK);
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
	fcap = getFcapname(font);

	/* initialize the font processor with the selected font	*/

	if(Init_Font(fcap) != OK) {
		return (pre_err);
	}

	return (OK);
}
