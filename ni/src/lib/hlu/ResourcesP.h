/*
 *      $Id: ResourcesP.h,v 1.2 1993-10-19 17:52:12 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ResourcesP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:39:54 MDT 1992
 *
 *	Description:	This file contains all the external declarations
 *			neccessary for layer writers to access the resource
 *			stuff in the hlu's.
 */
#ifndef NRESP_H
#define NRESP_H

#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ConvertP.h>

typedef struct _NrmResource {
	NrmQuark	nrm_name;
	NrmQuark	nrm_class;
	NrmQuark	nrm_type;
	unsigned int	nrm_size;
	unsigned int	nrm_offset;
	NrmQuark	nrm_default_type;
	NhlPointer	nrm_default_addr;
} NrmResource, *NrmResourceList;

typedef NhlErrorTypes (*NrmResourceDefaultProc)(
#if	NhlNeedProto
	NrmName,	/* resource name	*/
	NrmClass,	/* resource class	*/
	NhlPointer,	/* base addr		*/
	unsigned int	/* resource_offset	*/
#endif
);

extern NhlErrorTypes _NhlGetResources(
#if	NhlNeedProto
	_NhlConvertContext	ctxt,	/* convert context		*/
	Layer			l,	/* layer to set resources of	*/
	_NhlExtArgList		args,	/* args to override res defaults*/
	int			num_args,/* number of args		*/
	NrmQuarkList		child	/* layer is auto-managed chld	*/
#endif
);

extern void _NhlResourceListInitialize(
#ifdef NhlNeedProto
	void	/* none	*/
#endif
);

extern void _NhlGroupResources(
#ifdef	NhlNeedProto
	LayerClass lc	/* LayerClass to create full resource list for	*/
#endif
);

extern void _NhlCompileResourceList(
#if	NhlNeedProto
	NhlResourceList,	/* resource list	*/
	int			/* number of resources	*/
#endif
);

extern void _NhlCopyFromArg(
#ifdef	NhlNeedProto
	_NhlArgVal,	/* src	*/
	void*,		/* dst	*/
	unsigned int	/* size	*/
#endif
);

extern void _NhlCopyToArg(
#ifdef	NhlNeedProto
	void*,		/* src	*/
	_NhlArgVal*,	/* dst	*/
	unsigned int	/* size	*/
#endif
);

extern void _NhlMergeArgLists(
#if	NhlNeedProto
	_NhlExtArgList	ret_args,	/* return args		*/
	int		*num_ret_args,	/* num ret_args		*/
	_NhlExtArgList	oargs,		/* over-ride args	*/
	int		num_oargs,	/* num oargs		*/
	_NhlExtArgList	args,		/* args			*/
	int		num_args	/* num args		*/
#endif
);

extern NhlErrorTypes _NhlSortChildArgs(
#if	NhlNeedProto
	Layer			l,		/* layer		*/
	_NhlExtArgList		args_in,	/* args to sort		*/
	int			nargs_in,	/* number args to sort	*/
	_NhlExtArgList		*args_out,	/* args not forwarded	*/
	int			*nargs_out,	/* num args_out		*/
	_NhlChildArgList	*forw_list,	/* list of args to frwd	*/
	NhlBoolean		*args_used,	/* args used		*/
	NhlBoolean		getvalues	/* called frm getvalues	*/
#endif
);

extern void _NhlFreeChildArgs(
#if	NhlNeedProto
	_NhlChildArgList	list	/* child arg list to free	*/
#endif
);

extern NhlBoolean _NhlResInClass(
#if	NhlNeedProto
	LayerClass	lc,	/* class to check for res	*/
	NrmQuark	res	/* resource to look for		*/
#endif
);

extern void _NhlDestroyResDatabase(
#if	NhlNeedProto
	void
#endif
);

#endif /* NRESP_H */
