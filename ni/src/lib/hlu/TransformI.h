/*
 *      $Id: TransformI.h,v 1.4 1996-06-13 02:05:58 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransformI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 25 16:58:45 MST 1995
 *
 *	Description:	
 */
#ifndef	_NTRANSFORMI_h
#define	_NTRANSFORMI_h

/* Note:
 * Does not include Transform.h -- because a number of objects that 
 * need these functions are not Transform class objects.
 */

extern NhlBoolean _NhlIsOverlayMember(
#if	NhlNeedProto
	int	pid
#endif
);

/* 
 * private versions of the Annotation interface functions:
 * assumes layer pointers are valid. If entry_name string is NULL,
 * one is supplied.
 */

extern int _NhlAddAnnotation(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	anno_view,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhlRemoveAnnotation(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	annomanager,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes NhlRegisterAnnotation(
#if	NhlNeedProto
        int	plot_id,
	int	annomanager_id
#endif
);

extern NhlErrorTypes NhlUnregisterAnnotation(
#if	NhlNeedProto
        int	plot_id,
	int	annomanager_id
#endif
);

extern NhlErrorTypes _NhlRegisterAnnotation(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	annomanager,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhlUnregisterAnnotation(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	annomanager,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhltfDrawSegment(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	trobj,
        NhlTransDat	*transdat,    				       
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhltfInitSegment(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	trobj,
	NhlTransDat	**transdat,					      
	NhlString	entry_name
#endif
);

#endif	/* _NTRANSFORMI_h */
