/*
 *      $Id: DataMgrF.h,v 1.2 1993-10-19 17:50:34 boote Exp $
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
	DataItemLayer	item,		/* DataItem sub-class		*/
	int		dcommid,	/* id for datacomm layer	*/
	NrmQuark	res_name,	/* resource name		*/
	NrmQuark	*type_req,	/* array of type wanted		*/
	NrmQuark	*type_ret	/* type will be created		*/
#endif
);

extern Layer _NhlRetrieveData(
#ifdef	NhlNeedProto
	DataItemLayer	item,		/* dataItem sub-class	*/
	_NhlDHandle	dhandle,	/* id for Connection	*/
	NhlBoolean	*new		/* is data new/changed	*/
#endif
);

extern void _NhlCloseDataConnection(
#ifdef	NhlNeedProto
	DataItemLayer	item,	/* DataItem sub-class	*/
	_NhlDHandle	dhandle	/* id for Connection	*/
#endif
);

/*
 * Private API for the DataSpec class only
 */

extern NhlBoolean _NhlRegisterDSpec(
#ifdef	NhlNeedProto
	DataItemLayer	item,		/* dataitem sub-class		*/
	int		dspecid		/* id for dataspec layer	*/
#endif
);

extern void _NhlUnRegisterDSpec(
#ifdef	NhlNeedProto
	DataItemLayer	item,		/* dataitem sub-class		*/
	int		dspecid		/* id for dataspec layer	*/
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

extern NhlErrorTypes _NhlNotifyDataComm(
#ifdef	NhlNeedProto
	DataMgrLayer	mgr	/* DataMgr	*/
#endif
);

#endif	/* _NDataMgrF_H */
