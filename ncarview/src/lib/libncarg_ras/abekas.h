
/*
 *      $Id: abekas.h,v 1.1 1992-09-10 20:10:05 don Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		abekas.h
 *
 *	Author:		Don Middleton
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 10 14:05:51 MDT 1992
 *
 *	Description:	Local header file for Abekas A60 image driver.
 */

#define RAS_ABEKAS_NX	720
#define RAS_ABEKAS_NY	486

typedef enum {
	ABEKAS_RGB,
	ABEKAS_YUV
} AbekasEncoding;


typedef struct _AbekasInfo {
	AbekasEncoding	encoding;
} AbekasInfo;
