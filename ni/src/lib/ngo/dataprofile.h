/*
 *      $Id: dataprofile.h,v 1.2 1999-02-23 03:56:45 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		dataprofile.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATAPROFILE_H
#define	_NG_DATAPROFILE_H

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/browse.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif


/*
 * Public api
 */

typedef struct _brDataLinkReqRec {	/* message type _NgDATALINK_REQ */
	NhlBoolean 	on;
	int		link_ditem;
} brDataLinkReqRec, *brDataLinkReq;

/* cflag (change flag) values */

#define _NgSYMBOL_CHANGE	0x01
#define _NgRANK_CHANGE		0x02
#define _NgSHAPE_CHANGE		0x04
#define _NgALL_CHANGE	_NgSYMBOL_CHANGE | _NgRANK_CHANGE | _NgSHAPE_CHANGE


/* set_state values */

typedef enum _NgVarDataSetState {
	_NgVAR_UNSET,		/* no var set */
	_NgDEFAULT_VAR,		/* var and shape determined by default */
	_NgDEFAULT_SHAPE,	/* explicitly specified var default shape */
	_NgSHAPED_VAR,		/* var and shape specified */
	_NgEXPRESSION,		/* specified expression */
	_NgUNKNOWN_DATA		/* data source has not been determined */
} NgVarDataSetState;

typedef struct _NgVarDataRec
{
        NrmQuark 		qfile;
        NrmQuark 		qvar;
	NclApiDataList          *dl;
	NclVarTypes		type;
        int			ndims;       /* current shape info follows */
	int     		dims_alloced;
	int			rank;	 /* this is the effective dim count */
	NhlBoolean		size_only; /* only the size of each dim is
			 	          known; size -1 is stored in finish */
        long			*start;
        long			*finish;
        long			*stride;
        int			data_ix;     /* not sure we need this */
	NgGO			go;	     /* required for expressions */
	NhlString		expr_val;    /* sent verbatim to ncl */
	NrmQuark		qexpr_var;   /* variable holding expr eval */
	NgVarDataSetState	set_state;
	char			cflags;      /* flags for what has changed */
} NgVarDataRec, *NgVarData;

typedef enum {
	_NgDEFAULT,
	_NgCONTOURPLOT,
	_NgSTREAMLINEPLOT,
	_NgVECTORPLOT,
	_NgXYPLOT,
	_NgCOORDARRAY,
	_NgSCALARFIELD,
	_NgVECTORFIELD
} NgClassType;

typedef enum {
	_NgDATAVAR,
	_NgCOORDVAR,
	_NgMISSINGVAL,
	_NgDATAOBJ,
	_NgCONFIG
} NgDataItemType;

struct _NgDataItemRec;
struct _NgDataProfileRec;

typedef NhlBoolean (*GetValue) 
(
	struct _NgDataItemRec	*ditem,
	NhlPointer		*value,
	NrmQuark		*type,
	NhlBoolean		private
);

typedef NhlBoolean (*ValueDefined) 
(
	struct _NgDataProfileRec	*dprof,
	int				dix
);
	
typedef struct _NgDataItemRec
{
	NhlString	name;
	NhlString	resname;
	NrmQuark	resq;
	NgClassType	class_type;
	NgDataItemType  item_type;
	int		maxdims;
	int		mindims;
	int		coord_num; 	/* applies to NgDataItemType 
					   _NgCOORDVAR only -- Fortan
					   conventions used here for processing
					   convenience: 1 is fastest moving
					   dimension */
	NhlBoolean	required;
	NhlBoolean	vis;
	NgVarData	vdata;
	GetValue	get_val;
	ValueDefined	val_defined;
	struct _NgDataItemRec *ref_ditem;
	int		hlu_id;
	NgCBWP		svcb;
} NgDataItemRec, *NgDataItem;

typedef struct _NgDataProfileRec
{
	NgClassType	type;
        NhlString	class_name;
	NhlClass	class;
        int		n_dataitems;
	int		master_data_ix;
	NhlBoolean	linked;
	NgDataItem	*ditems;
	NhlFreeFunc	free;
} NgDataProfileRec, *NgDataProfile;

extern
NgDataProfile NgCopyDataProfile
(
	NgDataProfile data_profile
);

extern
NhlErrorTypes NgTransferDataProfileInfo
(
	NgDataProfile to_data_profile,
	NgDataProfile from_data_profile
);
	
extern
NhlBoolean NgHasDataProfile
(
	NgGO		go,
	NhlString	class_name
        );

extern
NgDataProfile NgNewDataProfile
(
	NgGO		go,
	NhlString	class_name
        );

extern
void NgFreeDataProfile
(
	NgDataProfile data_profile
	);	

extern
NhlBoolean NgSetDataProfileVar
(
	NgDataProfile	data_profile,
	NgVarData     	vdata,
	NhlBoolean	init_master_ix,
	NhlBoolean	set_dependencies
	);	


NhlBoolean NgSetDependentVarData		
(
	NgDataProfile	dp,
	int		index,
	NhlBoolean	init
);

extern
NgVarData NgNewVarData
(
	void
	);

extern
void NgFreeVarData
(
	NgVarData var_data
	);

extern
NhlBoolean NgSetVarData
(
	NclApiDataList		*dl,
	NgVarData		var_data,
	NrmQuark		qfile,
	NrmQuark		qvar,
	int			set_dim_count,
	long			*start,
	long			*finish,
	long			*stride,
	NgVarDataSetState	set_state
);

extern
int NgVarDataRank
(
	NgVarData	vdata
	);

extern NhlBoolean NgSetUnknownDataItem
(
	NgDataItem ditem
	);

NhlBoolean NgSetExpressionVarData
(
	int		go_id,
	NgVarData	var_data,
	NhlString	expr_val
	);

NhlBoolean NgDeleteExpressionVarData
(
	int		go_id,
	NgVarData	vdata
);

NhlBoolean NgConformingDataItem
(
	NgDataItem	ditem
	);


#endif	/* _NG_DATAPROFILE_H */


