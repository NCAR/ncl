/*
 *      $Id: ConvertersP.h,v 1.1 1994-07-12 20:51:28 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ConvertersP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 4 01:37:44 MDT 1994
 *
 *	Description:	
 */

#ifndef	_CNVTRS_P_H
#define	_CNVTRS_P_H

#define	_NhlSTACK_ARGS_SIZE	(100)

typedef struct _NhlEnumVals_ _NhlEnumVals;

struct _NhlEnumVals_ {
	int		value;
	NhlString	name;
};

extern NhlErrorTypes _NhlRegisterEnumType(
#if	NhlNeedProto
	NhlString	enum_name,
	_NhlEnumVals	*enum_vals,
	int		nvals
#endif
);

#endif	/* _CNVTRS_P_H */
