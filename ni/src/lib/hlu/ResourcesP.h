/*
 *      $Id: ResourcesP.h,v 1.18 1997-08-14 16:30:26 dbrown Exp $
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
	_NhlArgVal	nrm_default_val;
	unsigned int	res_info;
	NhlFreeFunc	free_func;
	NhlClass	nhlclass;
} NrmResource, *NrmResourceList;

typedef NhlErrorTypes (*NrmResourceDefaultProc)(
#if	NhlNeedProto
	NrmName,	/* resource name	*/
	NrmClass,	/* resource class	*/
	NhlPointer,	/* base addr		*/
	unsigned int	/* resource_offset	*/
#endif
);

/*
 * The class MUST be initialized before either of these is called!
 */
extern NrmResource *
_NhlGetResInfo(
#if	NhlNeedProto
	NhlClass	lc,
	NrmQuark	res
#endif
);

extern void _NhlGetNativeResInfo(
#if	NhlNeedProto
        NhlClass class,
        NrmName  res,
        int	 *count,
        NrmResource **native_res
#endif
        );

extern int _NhlGetNamesAndClasses(
#if	NhlNeedProto
	NhlLayer	layer,
	NrmNameList	names,
	NrmClassList	classes,
	int		length
#endif
        );

/*
 * This will initialize the class if necessary
 */

extern NrmNameList
_NhlGetUserResources(
#if	NhlNeedProto
	NhlClass	lc,
        int		*res_count
#endif
);

extern NhlErrorTypes _NhlGetResources(
#if	NhlNeedProto
	_NhlConvertContext	ctxt,
	NrmDatabase		resdb,
	char			*base,
	NrmQuarkList		nameQ,
	NrmQuarkList		classQ,
	NrmResourceList		res,
	int			num_res,
	_NhlArgList		args,
	int			num_args,
	NrmQuarkList		*child
#endif
);

extern NhlErrorTypes _NhlSetValues(
#if	NhlNeedProto
	_NhlConvertContext	ctxt,
	char			*base,
	NrmResourceList		res,
	int			nres,
	_NhlArgList		args,
	int			nargs
#endif
);

extern NhlErrorTypes _NhlSetLayerValues(
#if	NhlNeedProto
	NhlLayer	l,		/* layer instance	*/
	_NhlArgList	args,		/* args to change	*/
	int		nargs		/* number of args	*/
#endif
);

extern NhlErrorTypes _NhlGetValues(
#if	NhlNeedProto
	char		*base,
	NrmResourceList	res,
	int		nres,
	_NhlArgList	args,
	int		nargs
#endif
);

extern NhlErrorTypes _NhlGetLayerResources(
#if	NhlNeedProto
	_NhlConvertContext	ctxt,	/* convert context		*/
	NhlLayer		l,	/* layer to set resources of	*/
	_NhlArgList		args,	/* args to override res defaults*/
	int			num_args,/* number of args		*/
	NrmQuarkList		*child	/* layer is auto-managed chld	*/
#endif
);

extern void _NhlResourceListInitialize(
#if	NhlNeedProto
	void	/* none	*/
#endif
);

extern void _NhlGroupResources(
#if	NhlNeedProto
	NhlClass lc	/* Class to create full reslist for	*/
#endif
);

extern void _NhlCompileResourceList(
#if	NhlNeedProto
	NhlClass		lc,
	NhlResourceList		res,	/* resource list	*/
	int			nres	/* number of resources	*/
#endif
);

extern void _NhlCopyFromArgVal(
#if	NhlNeedProto
	_NhlArgVal,	/* src	*/
	void*,		/* dst	*/
	unsigned int	/* size	*/
#endif
);

extern NhlBoolean _NhlConvertArg(
#if	NhlNeedProto
	_NhlArg		*arg,
	NrmQuark	name,
	unsigned int	size
#endif
);

extern void _NhlCopyToArg(
#if	NhlNeedProto
	void*,		/* src	*/
	_NhlArgVal*,	/* dst	*/
	unsigned int	/* size	*/
#endif
);

extern void _NhlMergeArgLists(
#if	NhlNeedProto
	_NhlArgList	ret_args,	/* return args		*/
	int		*num_ret_args,	/* num ret_args		*/
	_NhlArgList	oargs,		/* over-ride args	*/
	int		num_oargs,	/* num oargs		*/
	_NhlArgList	args,		/* args			*/
	int		num_args	/* num args		*/
#endif
);

extern NhlErrorTypes _NhlSortChildArgs(
#if	NhlNeedProto
	NhlLayer		l,		/* layer		*/
	_NhlArgList		args_in,	/* args to sort		*/
	int			nargs_in,	/* number args to sort	*/
	_NhlArgList		*args_out,	/* args not forwarded	*/
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
	NhlClass	lc,	/* class to check for res	*/
	NrmQuark	res	/* resource to look for		*/
#endif
);

extern void _NhlDestroyResDatabase(
#if	NhlNeedProto
	void
#endif
);


typedef struct _NhlValSetCBInfoRec _NhlValSetCBInfoRec, *_NhlValSetCBInfo;

struct _NhlValSetCBInfoRec {
	NrmQuark	nameq;
	NhlBoolean	forwarded;
	_NhlCB		forwarded_cb;
	NhlBoolean	forwarded_value_set;
	int		resl_id;
	NhlLayer	resl;
	unsigned int    offset;
	unsigned int	size;
};

extern NhlErrorTypes _NhlResValueSetCBTask(
#if	NhlNeedProto
	NhlPointer	proc_data,
	_NhlCBTask	task,
	NhlArgVal	selector,
	NhlBoolean	*do_it,					
	NhlArgVal	*cbdata,				     
	NhlPointer	*cbnode_data
#endif
);

#endif /* NRESP_H */
