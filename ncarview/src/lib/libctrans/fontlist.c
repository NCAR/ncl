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
#define MAXFONTSTR	30	/* intial space allocated for a string	*/


char	*Fontlist[MAXFONT];

extern	char	*getFcapname();
extern	Ct_err	Init_Font();
extern	char	msg[];
extern	char	*malloc();

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
	
	for (i=0;i<MAXFONT;i++) {
		Fontlist[i] = (char *) malloc ((MAXFONTSTR + 1) * sizeof(char));
	}

	/* build default font list.
	 * This table is built dynamically so it can accomodate different
	 * sized font lists
 	 */
	(void) strcpy(Fontlist[0], DEFAULTFONT);
	(void) strcpy(Fontlist[1], "HERSHEY:CARTOGRAPHIC_ROMAN");
	(void) strcpy(Fontlist[2], "HERSHEY:CARTOGRAPHIC_GREEK");
	(void) strcpy(Fontlist[3], "HERSHEY:SIMPLEX_ROMAN");
	(void) strcpy(Fontlist[4], "HERSHEY:SIMPLEX_GREEK");
	(void) strcpy(Fontlist[5], "HERSHEY:SIMPLEX_SCRIPT");
	(void) strcpy(Fontlist[6], "HERSHEY:COMPLEX_ROMAN");
	(void) strcpy(Fontlist[7], "HERSHEY:COMPLEX_GREEK");
	(void) strcpy(Fontlist[8], "HERSHEY:COMPLEX_SCRIPT");
	(void) strcpy(Fontlist[9], "HERSHEY:COMPLEX_ITALIC");
	(void) strcpy(Fontlist[10], "HERSHEY:COMPLEX_CYRILLIC");
	(void) strcpy(Fontlist[11], "HERSHEY:DUPLEX_ROMAN");
	(void) strcpy(Fontlist[12], "HERSHEY:TRIPLEX_ROMAN");
	(void) strcpy(Fontlist[13], "HERSHEY:TRIPLEX_ITALIC");
	(void) strcpy(Fontlist[14], "HERSHEY:GOTHIC_GERMAN");
	(void) strcpy(Fontlist[15], "HERSHEY:GOTHIC_ENGLISH");
	(void) strcpy(Fontlist[16], "HERSHEY:GOTHIC_ITALIAN");
	(void) strcpy(Fontlist[17], "HERSHEY:MATH_SYMBOLS");
	(void) strcpy(Fontlist[18], "HERSHEY:SYMBOL_SET1");
	(void) strcpy(Fontlist[19], "HERSHEY:SYMBOL_SET2");

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
		if (strlen(c->s->string[i]) > MAXFONTSTR) {
			if (Fontlist[i] != NULL) cfree ((char *) Fontlist[i]);
			Fontlist[i] = (char *) malloc ((unsigned) 
				((strlen(c->s->string[i]) + 1) * sizeof(char)));
		}

		(void) strcpy(Fontlist[i],c->s->string[i]);
	}
}

Ct_err	setFont(font_index)
	IXtype	font_index;
{
	char	*fcap;
	char	*font;


	font = Fontlist[font_index - 1];

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
