/*
 *      $Id: nclapi.h,v 1.1 2000-06-28 19:24:03 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		nclapi.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun  9 12:22:00 MDT 2000
 *
 *	Description:	
 */
#ifndef	_NG_NCLAPI_H
#define	_NG_NCLAPI_H

#include <ncarg/ngo/ncl.h>

/*
 * Public api
 */

/*
 * Reads att value for any kind of variable, depending on which
 * quarks are set to NrmNULLQUARK. The results are cached and 
 * therefore the caller should not free.
 */
extern NclExtValueRec *
NgNclReadAtt(
	NrmQuark	qfile,
	NrmQuark	qvar,
	NrmQuark	qcoord,
	NrmQuark	qatt
	);

/*
 * Reads value for file vars, reg vars, and reg var coords, depending on which
 * quarks are set to NrmNULLQUARK. Coordinate variables are cached until
 * the variable is deleted. Other variables may be removed if the cache becomes
 * full. The caller should not free.
 */

extern NclExtValueRec *
NgNclReadVarValue(
	NrmQuark	qfile,
	NrmQuark	qvar,
	NrmQuark	qcoord,
	long		*start,
	long		*finish,
	long		*stride
	);

/*
 * Reads coord value for a dim index. May return NULL if there is no 
 * coord var. Coordinate variables are cached until
 * the variable is deleted. The caller should not free.
 */

extern NclExtValueRec *
NgNclReadCoordValue(
	NrmQuark	qfile,
	NrmQuark	qvar,
	int		dim_ix,
	long		*start,
	long		*finish,
	long		*stride
	);

/*
 * Returns the value of an element of any numeric array 
 * converted to double
 */

extern double
NgNumericValToDouble(
        NclExtValueRec	*val,
        int		index
        );


#endif	/* _NG_NCLAPI_H */
