/*
 *      $Id: TransformI.h,v 1.2 1995-03-21 22:37:03 dbrown Exp $
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
#ifndef	_NTransformI_h
#define	_NTransformI_h

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
        NhlLayer	overlay,
	NhlLayer	anno_view,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhlRemoveAnnotation(
#if	NhlNeedProto
        NhlLayer	overlay,
	NhlLayer	annotation,
	NhlString	entry_name
#endif
);

#if 0
int _NhlGetAnnotationId(
#if	NhlNeedProto
        NhlLayer	overlay,
	NhlLayer	anno_view,
	NhlString	entry_name
#endif
);

#endif

extern NhlErrorTypes NhlRegisterAnnotation(
#if	NhlNeedProto
        int	overlay_plot_id,
	int	annotation_id
#endif
);

extern NhlErrorTypes NhlUnregisterAnnotation(
#if	NhlNeedProto
        int	overlay_plot_id,
	int	annotation_id
#endif
);

extern NhlErrorTypes _NhlRegisterAnnotation(
#if	NhlNeedProto
        NhlLayer	overlay,
	NhlLayer	annotation,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhlUnregisterAnnotation(
#if	NhlNeedProto
        NhlLayer	overlay,
	NhlLayer	annotation,
	NhlString	entry_name
#endif
);

#endif	/* _NTransformI_h */
