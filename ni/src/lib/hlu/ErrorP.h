/*
 *      $Id: ErrorP.h,v 1.1 1993-04-30 17:22:01 boote Exp $
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

#define TABLELISTINC	10
#define ERRLISTINC	32

typedef struct _ETable{
	unsigned	start;
	unsigned	len;
	Const char	**errs;
} ETable;


/*
 * Class declarations
 */
typedef struct _ErrorLayerPart {
	/* User setable resource fields */
	NhlBoolean	buffer_errors;
	NhlErrorTypes	error_level;
	NhlBoolean	print_errors;
	char		*error_file;

	/*
	 * Temporary resource used to determine if UNSUPPORTED message
	 * should be printed.
	 */
	NhlBoolean	unsupported_msg;

	/* Internal private fields */
	FILE		*error_fp;

	int		num_emsgs;
	int		len_emsgs;
	NhlErrMsgList	emsgs;

	int		num_etables;
	int		len_etables;
	ETable		*etables;

} ErrorLayerPart;

typedef struct _ErrorLayerRec {
	BaseLayerPart	base;
	ErrorLayerPart	error;
} ErrorLayerRec;

typedef struct _ErrorLayerClassPart {
	int num_error_instances;
} ErrorLayerClassPart;

typedef struct _ErrorLayerClassRec {
	BaseLayerClassPart	base_class;
	ErrorLayerClassPart	error_class;
} ErrorLayerClassRec;

extern ErrorLayerClassRec errorLayerClassRec;

#endif /* _NErrorP_h */	
