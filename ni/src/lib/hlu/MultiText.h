/*
 *      $Id: MultiText.h,v 1.6 2001-12-05 00:19:04 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MultiText.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Dec 3 11:29:52 MST 1992
 *
 *	Description:	Public header file - for MultiText item.
 *			This object uses an internal TextItem child.
 */
#ifndef _NMultiText_h
#define _NMultiText_h

#include <ncarg/hlu/View.h>
#include <ncarg/hlu/TextItem.h>

/*
 * Resource name definitions
 */

#define NhlNMtextNumStrings	"mtext.NumStrings"
#define NhlCMtextNumStrings	"Mtext.NumStrings"
#define NhlNMtextStrings	"mtext.Strings"
#define NhlCMtextStrings	"Mtext.Strings"
#define NhlNMtextOrientation	"mtext.Orientation"
#define NhlCMtextOrientation	"Mtext.Orientation"
#define NhlNMtextConstPosF	"mtext.ConstPosF"
#define NhlCMtextConstPosF	"Mtext.ConstPosF"
#define NhlNMtextPosArray	"mtext.PosArray"
#define NhlCMtextPosArray	"Mtext.PosArray"
#define NhlNMtextMaxLenF	"mtext.MaxLenF"
#define NhlCMtextMaxLenF	"Mtext.MaxLenF"
#define NhlNMtextAutoStride	"mtext.AutoStride"
#define NhlCMtextAutoStride	"Mtext.AutoStride"
#define NhlNMtextKeepEndItems	"mtext.KeepEndItems"
#define NhlCMtextKeepEndItems	"Mtext.KeepEndItems"

/*
 * New type name definitions
 */

#define NhlTMTextOrientationType	"mtextOrientationType"

/*
 * Other new usefull definitions
 */
typedef enum _NhlMTextOrientatonType{
	NhlMTEXT_X_CONST,
	NhlMTEXT_Y_CONST
} NhlMTextOrientatonType;

/*
 * Definition and declaration of new class for global use
 */

extern NhlClass NhlmultiTextClass;

#endif  /* _NMultiText_h */
