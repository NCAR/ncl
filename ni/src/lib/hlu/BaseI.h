/*
 *      $Id: BaseI.h,v 1.4 1997-01-17 18:57:17 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		BaseI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 13 15:16:46 MDT 1996
 *
 *	Description:	
 */
#ifndef _NBaseI_h
#define _NBaseI_h

#include <ncarg/hlu/Base.h>

#define	_NhlCBobjDestroy	"CBobjDestroy"	/* cbdata.ptrval is NhlLayer */

typedef struct _NhlValueSetCBDataRec 
	_NhlValueSetCBDataRec, *_NhlValueSetCBData;

struct _NhlValueSetCBDataRec {
	int		id;	/* layer id */
	NrmQuark	resq;	/* resource name quark */
};

#define	_NhlCBobjValueSet	"CBobjValueSet"	/* cbdata.ptrval is 
						   _NhlValueSetCBData */

typedef struct _NhlobjChangeChildRec _NhlobjChangeChildRec, *_NhlobjChangeChild;
typedef enum _NhlobjCCType{
	_NhlobjCCAdd,
	_NhlobjCCRemove,
	_NhlobjCCMove
} _NhlobjCCType;

struct _NhlobjChangeChildRec{
	_NhlobjCCType	reason;
	int		new;	/* valid for _NhlobjCCAdd,_NhlobjCCChange */
	int		old;	/* valid for _NhlobjCCRemove,_NhlobjCCChange */
	int		child;	/* valid for all	*/
};

/*
 * cbdata.ptrval is _NhlobjChangeChild
 */
#define _NhlCBobjChildChange	"CBobjChildChange"

#endif  /* _NBaseI_h */
