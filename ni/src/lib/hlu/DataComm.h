/*
 *      $Id: DataComm.h,v 1.17 2007-10-29 22:06:51 dbrown Exp $
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

/* Level selection modes */

typedef enum _NhlLevelSelectionMode { 
	NhlAUTOMATICLEVELS = 0, 
	NhlMANUALLEVELS = 1, 
	NhlEXPLICITLEVELS = 2, 
	NhlEQUALSPACEDLEVELS = 3
} NhlLevelSelectionMode;

#define NhlTLevelSelectionMode "LevelSelectionMode"

/* Scaling mode */

typedef enum _NhlScalingMode {
	NhlSCALEFACTOR = 0, 
	NhlCONFINETORANGE = 1,
	NhlTRIMZEROS = 2,
	NhlMAXSIGDIGITSLEFT = 3,
	NhlALLINTEGERS = 4,
        NhlINTEGERLINELABELS = 4       /* Obsolete synonym */
} NhlScalingMode;

#define NhlTScalingMode "ScalingMode"

typedef enum _NhlLabelBarEndStyle {
	NhlINCLUDEOUTERBOXES,
	NhlINCLUDEMINMAXLABELS,
	NhlEXCLUDEOUTERBOXES
} NhlLabelBarEndStyle;

#define NhlTLabelBarEndStyle	"LabelBarEndStyle"

/*
 * Class declarations
 */
extern NhlClass NhldataCommClass;
extern NhlClass NhldataSpecClass;

/*
 * Public API
 */

extern NhlErrorTypes NhlUpdateData(
#if	NhlNeedProto
	int	dcommid		/* id of dcomm object	*/
#endif
);

extern int NhlAddData(
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

/* Common class resources */

#define NhlCLevelSelectionMode		"LevelSelectionMode"
#define NhlCMaxLevelCount		"MaxLevelCount"
#define NhlCLevelSpacingF		"LevelSpacingF"
#define NhlCMinLevelValF		"MinLevelValF"
#define NhlCMaxLevelValF		"MaxLevelValF"
#define NhlCLevels			"Levels"

#endif /*_NDataComm_h */
