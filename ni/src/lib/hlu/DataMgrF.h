/*
 *      $Id: DataMgrF.h,v 1.1 1993-07-12 22:36:17 boote Exp $
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
#ifdef	NhlNeedProto
	DataItemLayer	item,		/* DataItem sub-class	*/
	NrmQuark	type_req	/* type wanted		*/
#endif
);

extern NhlBoolean _NhlRetrieveData(
#ifdef	NhlNeedProto
	DataItemLayer	item,		/* dataItem sub-class	*/
	_NhlDHandle	dhandle,	/* id for Connection	*/
	NhlBoolean	*new,		/* is data new/changed	*/
	int		*dset_ret	/* rtrn dataset object	*/
#endif
);

extern void _NhlCloseDataConnection(
#ifdef	NhlNeedProto
	DataItemLayer	item,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle	/* id for Connection	*/
#endif
);

/*
 * Private API to be used by the DataItem class only.
 */

extern void _NhlDataItemModified(
#ifdef	NhlNeedProto
	DataMgrLayer	mgr	/* DataMgr	*/
#endif
);

#endif	/* _NDataMgrF_H */
