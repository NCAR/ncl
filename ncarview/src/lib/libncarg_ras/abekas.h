/*
 *      $Id: abekas.h,v 1.4 2008-07-27 03:22:41 haley Exp $
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
