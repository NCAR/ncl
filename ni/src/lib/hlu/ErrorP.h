/*
 *      $Id: ErrorP.h,v 1.4 1994-01-27 21:23:01 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ErrorP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Oct 20 11:43:12 MDT 1992
 *
 *	Description:	This is the private header file for the error object.
 */
#ifndef _NErrorP_h
#define _NErrorP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Error.h>

typedef struct _NhlErrorLayerRec *NhlErrorLayer;
typedef struct _NhlErrorLayerClassRec *NhlErrorLayerClass;

/*
 * Private Global functions
 */
extern void _NhlInitError(
#if	NhlNeedProto
	void
#endif
);

extern void _NhlCloseError(
#if	NhlNeedProto
	void
#endif
);

/*
 * Private structures & definitions
 */

typedef struct _NhlETable{
	unsigned	start;
	unsigned	len;
	Const char	**errs;
} NhlETable;


/*
 * Class declarations
 */
typedef struct _NhlErrorLayerPart {
	/* User setable resource fields */
	NhlBoolean	buffer_errors;
	NhlErrorTypes	error_level;
	NhlBoolean	print_errors;
	char		*error_file;

	/* Internal private fields */
	FILE		*error_fp;

	int		num_emsgs;
	int		len_emsgs;
	NhlErrMsgList	emsgs;

	int		num_etables;
	int		len_etables;
	NhlETable	*etables;

} NhlErrorLayerPart;

typedef struct _NhlErrorLayerRec {
	NhlObjLayerPart		base;
	NhlErrorLayerPart	error;
} NhlErrorLayerRec;

typedef struct _NhlErrorLayerClassPart {
	int num_error_instances;
} NhlErrorLayerClassPart;

typedef struct _NhlErrorLayerClassRec {
	NhlObjLayerClassPart	base_class;
	NhlErrorLayerClassPart	error_class;
} NhlErrorLayerClassRec;

extern NhlErrorLayerClassRec NhlerrorLayerClassRec;

#endif /* _NErrorP_h */	
