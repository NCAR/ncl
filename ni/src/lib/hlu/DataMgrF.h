/*
 *      $Id: DataMgrF.h,v 1.6 1995-12-19 20:39:05 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataMgrF.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 9 12:52:23 MDT 1993
 *
 *	Description:	This file contains all the declarations that are
 *			needed for "friend" classes of the DataMgr.  At
 *			this time it should only be included by DataComm.c,
 *			DataItem.c and DataMgr.c (of course).
 */
#ifndef	_NDataMgrF_H
#define	_NDataMgrF_H

#include <ncarg/hlu/DataMgr.h>

/*
 * Private API to be used by the DataComm class only.
 */

extern _NhlDHandle _NhlInitDataConnection(
#if	NhlNeedProto
	NhlLayer	item,		/* DataItem sub-class		*/
	NhlLayer	dcomm,		/* datacomm layer		*/
	NrmQuark	res_name,	/* resource name		*/
	NrmQuark	*type_req,	/* array of type wanted		*/
	NrmQuark	*type_ret	/* type will be created		*/
#endif
);

extern NhlLayer _NhlRetrieveData(
#if	NhlNeedProto
	NhlLayer		item,		/* dataItem sub-class	*/
	_NhlDHandle		dhandle,	/* id for Connection	*/
	NhlBoolean		*new		/* is data new/changed	*/
#endif
);

extern void _NhlCloseDataConnection(
#if	NhlNeedProto
	NhlLayer	item,	/* DataItem sub-class	*/
	_NhlDHandle		dhandle	/* id for Connection	*/
#endif
);

/*
 * Private API to be used by the DataItem class only.
 */

extern void _NhlDataItemModified(
#if	NhlNeedProto
	NhlLayer	mgr	/* DataMgr	*/
#endif
);

extern NhlErrorTypes _NhlNotifyDataComm(
#if	NhlNeedProto
	NhlLayer	mgr	/* DataMgr	*/
#endif
);

#endif	/* _NDataMgrF_H */
