/*
 *      $Id: BaseI.h,v 1.3 1996-11-28 01:14:19 dbrown Exp $
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

#define	_NhlCBresValueSet	"CBresValueSet"	/* cbdata.ptrval is 
						   _NhlValueSetCBData */

#endif  /* _NBaseI_h */
