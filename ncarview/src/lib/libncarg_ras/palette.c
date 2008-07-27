/*
 *	$Id: palette.c,v 1.8 2008-07-27 03:18:46 haley Exp $
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

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "ncarg_ras.h"


/*LINTLIBRARY*/

int
PaletteRead(name, format, colors)
	char		*name;
	char		*format;
	unsigned char	colors[768];
{
	char		*errmsg = "PaletteRead(\"%s\",\"%s\",...)";
	int		status;
	FILE		*fp;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, errmsg, name, format);
		return(RAS_ERROR);
	}

	if (!strcmp(name, "stdin")) {
		fp = stdin;
	}
	else {
		fp = fopen(name, "r");
		if (fp == (FILE *) NULL) {
			(void) ESprintf(errno, errmsg, name, format);
			return(RAS_ERROR);
		}
	}

	if (format == (char *) NULL) {
		format = strrchr(name, '.');
		if (format != (char *) NULL) {
			format++;
		}
		else {
			(void) ESprintf(RAS_E_NO_FORMAT_SPECIFIED,
				errmsg, name, format);
			return(RAS_ERROR);
		}
	}

	if (!strcmp(format, "txt")) {
		status = PaletteReadText(fp, colors);
	}
	else if (!strcmp(format, "pal")) {
		status = PaletteReadHDF(fp, colors);
	}
	else {
		(void) ESprintf(RAS_E_UNKNOWN_FORMAT, errmsg, name, format);
		return(RAS_ERROR);
	}

	return(status);
}

int
PaletteReadText(fp, colors)
	FILE		*fp;
	unsigned char	colors[768];
{
	int		i;
	int		index, r, g, b;
	int		i1, r1, g1, b1;
	int		i2, r2, g2, b2;
	float		fr, fg, fb, dr, dg, db;
	int		set[256];
	unsigned char	*red, *green, *blue;

	red   = &colors[0];
	green = &colors[256];
	blue  = &colors[512];

	/* Initialize the color table */

	for(i=0; i<256; i++) {
		set[i] = False;
		red[i] = 0; green[i] = 0; blue[i] = 0;
	}

	while(fscanf(fp,"%d %d %d %d", &index, &r, &g, &b) == 4) {
		if (index < 0 || index > 255) {
			(void) ESprintf(RAS_E_INVALID_COLORMAP,
			"PaletteReadText() - Bogus palette index of %d", index);
			return(RAS_ERROR);
		}
		set[index] = True;
		red[index] = r; green[index] = g; blue[index] = b;
	}

	(void) fclose(fp);

	if (set[0] == False) {
		set[0] = True;
		red[0] = 0; green[0] = 0; blue[0] = 0;
	}

	if (set[255] == False) {
		set[255] = True;
		red[255] = 255; green[255] = 255; blue[255] = 255;
	}

	for(i1=0; i1<256; i1++) {
		if (set[i1] != False) continue;

		r1 = red[i1-1];
		g1 = green[i1-1];
		b1 = blue[i1-1];

		for(i2=i1; i2<256; i2++) {
			if (set[i2] == True) break;
		}

		r2 = red[i2];
		g2 = green[i2];
		b2 = blue[i2];

		dr = (float) (r2 - r1) / (float) (i2 - i1 + 1);
		dg = (float) (g2 - g1) / (float) (i2 - i1 + 1);
		db = (float) (b2 - b1) / (float) (i2 - i1 + 1);

		fr = (float) r1;
		fg = (float) g1;
		fb = (float) b1;

		for(i=i1; i<i2; i++) {
			fr += dr; fg += dg; fb += db;
			set[i]   = True;
			red[i]   = (int) (fr + .5);
			green[i] = (int) (fg + .5);
			blue[i]  = (int) (fb + .5);
		}
	}
	return(RAS_OK);
}

int
PaletteReadHDF(fp, colors)
	FILE		*fp;
	unsigned char	*colors;
{
	int		status;

	status = fread((char *) colors, 1, 768, fp);
	if (status != 768) return(RAS_EOF);
	return(RAS_OK);
}

int
PaletteWrite(name, format, colors)
	char		*name;
	char		*format;
	unsigned char	*colors;
{
	char		*errmsg = "PaletteWrite(\"%s\", \"%s\", ...)";
	int		status;
	FILE		*fp;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, errmsg, name, format);
		return(RAS_ERROR);
	}

	if (!strcmp(name, "stdout")) {
		fp = stdout;
	}
	else {
		fp = fopen(name, "w");
		if (fp == (FILE *) NULL) {
			(void) ESprintf(errno, errmsg, name, format);
			return(RAS_ERROR);
		}
	}

	if (format == (char *) NULL) {
		format = strrchr(name, '.');
		if (format != (char *) NULL) {
			format++;
		}
		else {
			(void) ESprintf(RAS_E_UNKNOWN_FORMAT,
					errmsg, name, format);
			return(RAS_ERROR);
		}
	}

	if (!strcmp(format, "txt")) {
		status = PaletteWriteText(fp, colors);
	}
	else if (!strcmp(format, "pal")) {
		status = PaletteWriteHDF(fp, colors);
	}
	else {
		(void) ESprintf(RAS_E_UNKNOWN_FORMAT, errmsg, name, format);
		return(RAS_ERROR);
	}

	if (fp != stdout) {
		(void) fclose(fp);
	}

	return(status);
}

int
PaletteWriteText(fp, colors)
	FILE		*fp;
	unsigned char	*colors;
{
	int		i;

	for(i=0; i<256; i++) {
		(void) fprintf(fp, "%3d %3d %3d %3d\n",
			i, colors[i], colors[i+256], colors[i+512]);
	}
	return(RAS_OK);
}

int
PaletteWriteHDF(fp, colors)
	FILE		*fp;
	unsigned char	*colors;
{
	int		status;

	status = fwrite((char *) colors, 1, 768, fp);
	if (status != 768) return(RAS_EOF);
	return(RAS_OK);
}
