/*
 *      $Id: stringutil.h,v 1.1 1997-06-04 18:08:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		stringutil.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 11:15:34 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_STRINGUTIL_H
#define	_NG_STRINGUTIL_H

#include <ncarg/ngo/go.h>
#include <ncarg/hlu/NresDB.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif


/*
 * Public api
 */

extern void
NgRemoveZeros
(
	char		*fstr
        );

extern void
NgFixFloat
(
	char		*fstr
        );

extern char *
NgTypeString(
	int type
        );

extern char *NgTypedValueToString(
        NclExtValueRec	*val,
        int		index,
        NhlBoolean	short_form,
        int		*vlen
        );

#endif	/* _NG_SORT_H */
