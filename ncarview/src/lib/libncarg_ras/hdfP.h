/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hdf.h
 *
 *	Author:		Don Middleton
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Aug 12 10:18:35 MDT 1992
 *
 *	Description:	This include file has definitions
 *			that are used only by hdf.c.
 */

typedef enum {
	HDF_IL_PIXEL = 0,
	HDF_IL_SCANLINE,
	HDF_IL_SCANPLANE
} HDFInterlaceType;

typedef struct HDFInfoStruct {
	int			palette_exists; /* 8-bit files  */
	HDFInterlaceType	interlace;	/* 24-bit files */
} HDFInfo;
