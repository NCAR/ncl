/*
 *      $Id: DataComm.h,v 1.8 1995-02-17 18:26:23 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataComm.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 16 17:45:07 MDT 1993
 *
 *	Description:	Public header file for the DataComm class.
 */
#ifndef _NDataComm_h
#define _NDataComm_h
#include <ncarg/hlu/Transform.h>

#define	NhlNdcDelayCompute	"dcDelayCompute"
#define	NhlCdcDelayCompute	"DcDelayCompute"

#define NhlTObjId		"ObjId"
#define NhlTObjIdGenArray	"ObjIdGenArray"

/*
 * Class declarations
 */
extern NhlLayerClass NhldataCommLayerClass;
extern NhlLayerClass NhldataSpecLayerClass;

/*
 * Public API
 */

extern NhlErrorTypes NhlUpdateData(
#if	NhlNeedProto
	int	dcommid		/* id of dcomm object	*/
#endif
);

extern NhlErrorTypes NhlAddData(
#if	NhlNeedProto
	int		dcommid,	/* id of layer w/ data resource	*/
	NhlString	res_name,	/* name of data resource	*/
	int		ditemid		/* id of data to add		*/
#endif
);

extern NhlErrorTypes NhlRemoveData(
#if	NhlNeedProto
	int		dcommid,	/* id of layer w/ data resource	*/
	NhlString	res_name,	/* name of data resource	*/
	int		ditemid		/* id of data item to remove	*/
#endif
);

extern NhlBoolean NhlIsDataComm(
#if	NhlNeedProto
	int	pid
#endif
);

extern NhlBoolean NhlIsDataSpec(
#if	NhlNeedProto
	int	pid
#endif
);

#endif /*_NDataComm_h */
