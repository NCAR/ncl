/*
 *      $Id: dataprofile.h,v 1.1 1999-01-11 19:36:23 dbrown Exp $
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

typedef struct _NgVarDataRec
{
        NrmQuark 		qfile;
        NrmQuark 		qvar;
	NclApiDataList          *dl;
	NclVarTypes		type;
        int			ndims;       /* current shape info follows */
	int     		dims_alloced;
        long			*start;
        long			*finish;
        long			*stride;
        int			data_ix;     /* not sure we need this */
	NhlBoolean		new_val;     /* set T when val changes */
	NhlBoolean		set;  /* T if explicit non-null value is set */
	NhlString		expr_val;    /* sent verbatim to ncl */
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

typedef void (*GetValue) 
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
	int		n_dims;
	NgClassType	class_type;
	NgDataItemType  item_type;
	NhlBoolean	required;
	NhlBoolean	vis;
	NhlBoolean	slave;
	NgVarData	vdata;
	GetValue	get_val;
	ValueDefined	val_defined;
	struct _NgDataItemRec *ref_ditem;
} NgDataItemRec, *NgDataItem;

typedef struct _NgDataProfileRec
{
	NgClassType	type;
        NhlString	class_name;
	NhlClass	class;
        int		n_dataitems;
	int		master_data_ix;
	int		coord_items[3];
	NhlBoolean	linked;
	NgDataItem	*ditems;
} NgDataProfileRec, *NgDataProfile;

NgDataProfile NgGetDataProfile
(
	NgGO		go,
	NhlString	class_name
        );

NgDataProfile NgCopyDataProfile
(
	NgDataProfile data_profile
);
	
NhlBoolean NgHasDataProfile
(
	NgGO		go,
	NhlString	class_name
        );

void NgFreeDataProfile
(
	NgDataProfile data_profile
	);	

#endif	/* _NG_DATAPROFILE_H */


