/*
 *      $Id: dataprofile.h,v 1.11 2000-01-20 03:38:20 dbrown Exp $
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

/*
 * The generic Plot pseudo-class-name
 */
#define NGPLOTCLASS "ngPlotClass"

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
	_NgUSER_EXPRESSION,	/* expression set by the user */
	_NgUNKNOWN_DATA,	/* data source has not been determined */
	_NgBOGUS_EXPRESSION, 	/* an expression found to cause errors */
	_NgUSER_DISABLED 	/* disabled by the user */
} NgVarDataSetState;

typedef struct _NgVarDataRec
{
	struct _NgVarDataRec	*next;
        NrmQuark 		qfile;
        NrmQuark 		qvar;
	NrmQuark		qcoord;
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
	long			*size;
	long			*order_ix;
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
	_NgVECTORFIELD,
	_NgPLOT
} NgClassType;

typedef enum {
	_NgDATAVAR,
	_NgCOORDVAR,
	_NgMISSINGVAL,
	_NgDATAOBJ,
	_NgCONFIG,
	_NgSYNTHETIC,
	_NgUPDATEFUNC
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

typedef char NgValType;     /* types of resource values and res func args */ 

#define _NgEXPR			0   /* anything with more than one term */
#define _NgFUNC			1
#define _NgARRAY		2
#define _NgOBJ_REF		3
#define _NgDATA_REF		4
#define _NgDATA_ATTR_REF	5
#define _NgDATA_COORD_REF	6
#define _NgDATA_COORD_ATTR_REF	7


typedef struct _NgArgInfoRec {
	NhlString		sval;
	NhlPointer		edata; /*  opaque edit data */
	NhlFreeFunc		free_edata;
	NhlBoolean		modified;
	NrmQuark		qargdatatype;
	NrmQuark		qargname;
	NgValType		valtype;
	NrmQuark		qvalsym;
	int			argcount;
	struct _NgArgInfoRec  	*args;
} NgArgInfoRec, *NgArgInfo;

typedef struct _NgResInfoRec {
	NhlPointer 	rdata;		/* resource data-allocated elsewhere */
	NhlPointer	edata;		/* opaque edit data */
	NhlFreeFunc	free_edata;	/* needed to free the edit data */
	NgValType	valtype;
	NrmQuark	qsym;		/* single-term func */
	int		argcount;
	NgArgInfo	args;		/* func arg values */
	NgVarDataSetState init_state;
	NgVarDataSetState last_state;
} NgResInfoRec, *NgResInfo;

extern NgResInfo NgNewResInfo(
	void
);

extern void NgFreeResInfo(
	NgResInfo res_info
);

extern void NgFreeArgInfo(
	NgArgInfo 	arg_info,
	int	  	count
);

extern NgArgInfo NgNewArgInfo(
	int	count
);

typedef struct _NgDataItemRec
{
	NhlString	name;
	NhlString	resname;
	NrmQuark	resq;
	NrmQuark	qhlu_name;
	NgClassType	class_type;
	NgDataItemType  item_type;
	int		maxdims;
	int		mindims;
	NhlPointer	data;     /* used for DataItemType-specific data:
				     it's the coord ix (1 == fastest) for
				     _NgCOORDVAR items. It's the sequence 
				     number for _NgUPDATEFUNC items. */
	NhlBoolean	required;
	NhlBoolean	vis;
	NgVarData	vdata;
	GetValue	get_val;
	ValueDefined	val_defined;
	struct _NgDataItemRec *ref_ditem;
	int		hlu_id;
	NgCBWP		svcb;
	NhlString	raw_val;
	NgResInfo	res_info;
	NhlBoolean	set_only;
	NhlBoolean	save_to_compare;
	NhlBoolean	init_only;
} NgDataItemRec, *NgDataItem;

typedef struct _NgPlotDataRec {
	NrmQuark 	qname;
	int		conform_group;
	NhlString	description;
	NhlBoolean	required;
	int		ndims;
	NgVarData	vdata;
} NgPlotDataRec, *NgPlotData;

extern void NgFreePlotDataRecs
(
	NgPlotData	plotdata,
	int		count
);

typedef struct _NgDataProfileRec
{
	NgClassType	type;
        NhlString	class_name;
	NhlClass	class;
        int		n_dataitems;
	int		master_data_ix;
	NhlBoolean	linked;
	NgDataItem	*ditems;
	NrmQuark	qpstyle;
	int		obj_count;
	NrmQuark	*qobjects;   /* read-only */
	NhlClass	*obj_classes; /* read-only */
	int		plotdata_count;
	NgPlotData	plotdata;
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
NgDataProfile NgMergeDataProfiles
(
	NgGO		go,
	NgDataProfile	data_profile,
	NhlString	hluname,
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


extern 
NgDataItem NgNewDataItem
(
	NrmQuark	qname,
	NrmQuark	qresname,
	NrmQuark	qhlu_name,
	NhlClass	class,
	NgDataItemType	item_type,
	int		min_dims,
	int		max_dims,
	NhlPointer	data,
	NhlBoolean	required,
	NhlBoolean	visible,
	NhlBoolean	set_only,
	NhlBoolean	save_to_compare,
	NhlBoolean	init_only
);

extern NhlErrorTypes
NgAppendDataProfileItem
(
	NgDataProfile	data_profile,
	NgDataItem	data_item,
	NrmQuark	qref_res
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
	NrmQuark		qcoord,
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

NhlBoolean NgCopyVarData
(
	NgVarData	to_var_data,
	NgVarData	from_var_data
	);

#define _NgNOEVAL 0
#define _NgCONDITIONAL_EVAL 1
#define _NgFORCED_EVAL 2

typedef char NgEvalAction;

NhlBoolean NgSetExpressionVarData
(
	int		go_id,
	NgVarData	var_data,
	NhlString	expr_val,
	NgEvalAction	eval_action,
	NhlBoolean	user_set
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


