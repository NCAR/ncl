/*
 *      $Id: dataprofile.c,v 1.1 1999-01-11 19:36:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		dataprofile.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/dataprofile.h>
#include <ncarg/ngo/stringutil.h>

static NhlBoolean ScalarDataDefined(
	struct _NgDataProfileRec	*dprof,
	int				dix
)
{
	int i;
	NgDataItem ditem = dprof->ditems[dix];

	if (ditem->ref_ditem->vdata->qvar != NrmNULLQUARK)
		return True;
	return False;
}

/* 
 * This function assumes that vector data resources are next to each other
 * in the the ditems list
 */
static NhlBoolean VectorDataDefined(
	struct _NgDataProfileRec	*dprof,
	int				dix
)
{
	int i;
	NgDataItem ref_ditem1 = dprof->ditems[dix]->ref_ditem;
	int dix_2 = ref_ditem1->vdata->data_ix + 1;
	NgDataItem ref_ditem2 = dprof->ditems[dix_2];

	if (ref_ditem1->vdata->qvar != NrmNULLQUARK &&
	    ref_ditem2->vdata->qvar != NrmNULLQUARK)
		return True;

	return False;
}

static NhlBoolean CoordArrayDataDefined(
	struct _NgDataProfileRec	*dprof,
	int				dix
)
{
	int i;
	NgDataItem ref_ditem1 = dprof->ditems[dix]->ref_ditem;
	int dix_2 = ref_ditem1->vdata->data_ix + 1;
	NgDataItem ref_ditem2 = dprof->ditems[dix_2];

	if (ref_ditem1->vdata->qvar != NrmNULLQUARK ||
	    ref_ditem2->vdata->qvar != NrmNULLQUARK)
		return True;

	return False;
}

/* scalar field and contour plot data items */

static NgDataItemRec sfdataarray = {
	"scalar field", "sfDataArray", NrmNULLQUARK,  
	2,_NgSCALARFIELD,_NgDATAVAR,
	True,True,False,NULL,NULL,NULL,NULL };
static NgDataItemRec sfxarray = {
	"x coord", "sfXArray",NrmNULLQUARK,
	2,_NgSCALARFIELD,_NgCOORDVAR,
	False,True,True,NULL,NULL,NULL,&sfdataarray };
static NgDataItemRec sfyarray = {
	"y coord", "sfYArray", NrmNULLQUARK,
	2,_NgSCALARFIELD,_NgCOORDVAR,
	False,True,True,NULL,NULL,NULL,&sfdataarray };
static NgDataItemRec sfmissingvaluev =	{
	"missing value", "sfMissingValueV", NrmNULLQUARK,
	0,_NgSCALARFIELD,_NgMISSINGVAL,
	False,False,True,NULL,NULL,NULL,&sfdataarray };


static NgDataItemRec cnscalarfielddata = {
	"scalar field object", "cnScalarFieldData", NrmNULLQUARK,
	0,_NgCONTOURPLOT,_NgDATAOBJ,
	False,False,False,NULL,NULL,ScalarDataDefined,&sfdataarray };

static NgDataItem ScalarFieldItems[] = {
	&sfdataarray,&sfxarray,&sfyarray,&sfmissingvaluev };

static NgDataItem ContourPlotItems[] = {
	&sfdataarray,&sfxarray,&sfyarray,&sfmissingvaluev,&cnscalarfielddata };

/* vector field, vector plot and streamline plot  data items */

static NgDataItemRec vfudataarray = {
	"u vector field", "vfUDataArray", NrmNULLQUARK,
	2,_NgVECTORFIELD,_NgDATAVAR,
	True,True,False,NULL,NULL,NULL,NULL };
static NgDataItemRec vfvdataarray = {
	"v vector field", "vfVDataArray", NrmNULLQUARK,
	2,_NgVECTORFIELD,_NgDATAVAR,
	True,True,True,NULL,NULL,NULL,&vfudataarray};
static NgDataItemRec vfxarray = {
	"x coord", "vfXArray", NrmNULLQUARK,
	2,_NgVECTORFIELD,_NgCOORDVAR,
	False,True,True,NULL,NULL,NULL,&vfudataarray };
static NgDataItemRec vfyarray = {
	"y coord", "vfYArray", NrmNULLQUARK,
	2,_NgVECTORFIELD,_NgCOORDVAR,
	False,True,True,NULL,NULL,NULL,&vfudataarray };
static NgDataItemRec vfmissinguvaluev = {
	"u missing value", "vfMissingUValueV", NrmNULLQUARK,
	0,_NgVECTORFIELD,_NgMISSINGVAL,
	False,False,True,NULL,NULL,NULL,&vfudataarray };
static NgDataItemRec vfmissingvvaluev = {
	"v missing value", "vfMissingVValueV", NrmNULLQUARK,
	0,_NgVECTORFIELD,_NgMISSINGVAL,
	False,False,True,NULL,NULL,NULL,&vfvdataarray };
static NgDataItemRec vcvectorfielddata = {
	"vector field object", "vcVectorFieldData", NrmNULLQUARK,
	0,_NgVECTORPLOT,_NgDATAOBJ,
	False,False,True,NULL,NULL,VectorDataDefined,&vfudataarray };
static NgDataItemRec vcscalarfielddata = {
	"scalar field object", "vcScalarFieldData", NrmNULLQUARK,
	0,_NgVECTORPLOT,_NgDATAOBJ,
	False,False,False,NULL,NULL,ScalarDataDefined,&sfdataarray };
static NgDataItemRec stvectorfielddata = {
	"vector field object", "stVectorFieldData", NrmNULLQUARK,
	0,_NgSTREAMLINEPLOT,_NgDATAOBJ,
	False,False,False,NULL,NULL,VectorDataDefined,&vfudataarray };

static NgDataItem VectorFieldItems[] = {
	&vfudataarray,&vfvdataarray,&vfxarray,&vfyarray,
	&vfmissinguvaluev,&vfmissingvvaluev };

static NgDataItem VectorPlotItems[] = {
	&vfudataarray,&vfvdataarray,&vfxarray,&vfyarray,
	&vfmissinguvaluev,&vfmissingvvaluev,
	&sfdataarray,&sfmissingvaluev,&vcvectorfielddata,&vcscalarfielddata };

static NgDataItem StreamlinePlotItems[] = {
	&vfudataarray,&vfvdataarray,&vfxarray,&vfyarray,
	&vfmissinguvaluev,&vfmissingvvaluev,&stvectorfielddata };

/* coord array and xyplot data items */

static NgDataItemRec cayarray = {
	"y array", "caYArray", NrmNULLQUARK,
	-2,_NgCOORDARRAY,_NgDATAVAR,
	False,True,False,NULL,NULL,NULL,NULL };
static NgDataItemRec caxarray = {
	"x array", "caXArray", NrmNULLQUARK,
	-2,_NgCOORDARRAY,_NgDATAVAR,
	False,True,True,NULL,NULL,NULL,&cayarray };
static NgDataItemRec caymissingv = {
	"y missing value", "caYMissingV", NrmNULLQUARK,
	0,_NgCOORDARRAY,_NgMISSINGVAL,
	False,False,True,NULL,NULL,NULL,&cayarray };
static NgDataItemRec caxmissingv = {
	"x missing value", "caXMissingV", NrmNULLQUARK,
	0,_NgCOORDARRAY,_NgMISSINGVAL,
	False,False,True,NULL,NULL,NULL,&caxarray };
static NgDataItemRec caycast = {
	"x missing value", "caYCast", NrmNULLQUARK,
	0,_NgCOORDARRAY,_NgCONFIG,
	False,False,True,NULL,NULL,NULL,&cayarray };
static NgDataItemRec caxcast = {
	"x missing value", "caXCast", NrmNULLQUARK,
	0,_NgCOORDARRAY,_NgCONFIG,
	False,False,True,NULL,NULL,NULL,&caxarray };
static NgDataItemRec xycoorddata = {
	"coord arrays object", "xyCoordData", NrmNULLQUARK,
	0,_NgXYPLOT,_NgDATAOBJ,
	False,False,True,NULL,NULL,CoordArrayDataDefined,&cayarray };

static NgDataItem CoordArrayItems[] = {
	&cayarray,&caxarray,&caymissingv,&caxmissingv,&caycast,&caxcast
};

static NgDataItem XyPlotItems[] = {
	&cayarray,&caxarray,&caymissingv,&caxmissingv,
	&caycast,&caxcast,&xycoorddata
};



static NgDataProfileRec DataProfs[] = {
	{_NgDEFAULT,NULL,NULL,0,0,{-1,-1,},False,NULL},
	{_NgCONTOURPLOT,"contourPlotClass",NULL,
         NhlNumber(ContourPlotItems),0,{1,2},False,ContourPlotItems },
	{_NgSTREAMLINEPLOT,"streamlinePlotClass",NULL,
	 NhlNumber(StreamlinePlotItems),0,{2,3},False,StreamlinePlotItems },
	{_NgVECTORPLOT,"vectorPlotClass",NULL,
         NhlNumber(VectorPlotItems),0,{2,3},False,VectorPlotItems },
	{_NgXYPLOT,"xyPlotClass",NULL,
         NhlNumber(XyPlotItems),0,{-2,-2},False,XyPlotItems },
	{_NgCOORDARRAY,"coordArraysClass",NULL,
         NhlNumber(CoordArrayItems),0,{-2,-2},False,CoordArrayItems },
	{_NgSCALARFIELD,"scalarFieldClass",NULL,
         NhlNumber(ScalarFieldItems),0,{1,2},False,ScalarFieldItems },
	{_NgVECTORFIELD,"vectorFieldClass",NULL,
         NhlNumber(VectorFieldItems),0,{2,3},False,VectorFieldItems }
};


static void InitializeDataProfile(
	void
)
{
	int i,j;

	for (i = 0; i < NhlNumber(DataProfs); i++) {
		for (j = 0; j < DataProfs[i].n_dataitems; j++) {
			NgDataItem ditem = DataProfs[i].ditems[j];
			ditem->resq = NrmStringToQuark(ditem->resname);
		}
	}
	return;
}
			

NgDataProfile NgGetDataProfile(
	NgGO		go,
	NhlString	class_name
        )
{
	int i, j, ix = -1;
	NgDataProfile	dprof,ref_prof = NULL;
	NgDataItem	*ditems;
	static NhlBoolean first = True;

	if (first) {
		InitializeDataProfile();
		first = False;
	}

	if (! class_name)
		ref_prof = &DataProfs[0];
	else {
		for (i = 1; i < NhlNumber(DataProfs); i++) {
			if (! strcmp(class_name,DataProfs[i].class_name)) {
				ref_prof = &DataProfs[i];
				break;
			}
		}
	}
	if (! ref_prof) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "No data profile for class %s",class_name));
		return NULL;
	}
	dprof = NhlMalloc(sizeof(NgDataProfileRec));
	memcpy(dprof,ref_prof,sizeof(NgDataProfileRec));

	ditems =  NhlMalloc(ref_prof->n_dataitems * sizeof(NgDataItem));
	for (i = 0; i < ref_prof->n_dataitems; i++) {
		NgDataItem sditem = ref_prof->ditems[i];
		ditems[i] = NhlMalloc( sizeof(NgDataItemRec));
		memcpy(ditems[i],sditem,sizeof(NgDataItemRec));
		ditems[i]->vdata = NhlMalloc(sizeof(NgVarDataRec));
		memset(ditems[i]->vdata,(char)0,sizeof(NgVarDataRec));
		ditems[i]->vdata->data_ix = i;
	}
	/* update the reference data item pointers */
	for (i = 0; i < ref_prof->n_dataitems; i++) {
		NgDataItem sditem = ref_prof->ditems[i];
		if (sditem->ref_ditem) {
			for (j = 0; j < ref_prof->n_dataitems; j++) {
				if (sditem->ref_ditem == ref_prof->ditems[j]) {
					ditems[i]->ref_ditem = ditems[j];
					break;
				}
			}
		}
	}
	dprof->ditems = ditems;

	return dprof;
}

NhlBoolean NgHasDataProfile(
	NgGO		go,
	NhlString	class_name
        )
{
	int i;

	if (! class_name) /* this is the non-graphic data profile */
		return True;

	for (i = 1; i < NhlNumber(DataProfs); i++) {
		if (! strcmp(class_name,DataProfs[i].class_name)) {
			return True;
		}
	}
	return False;
}

NgDataProfile  NgCopyDataProfile(
	NgDataProfile data_profile
	)	
{
	NgDataProfile dprof;
	NgDataItem *ditems;
	int i;

	if (! data_profile)
		return NULL;

	dprof = NhlMalloc(sizeof(NgDataProfileRec));
	memcpy(dprof,data_profile,sizeof(NgDataProfileRec));

	ditems =  NhlMalloc(data_profile->n_dataitems * sizeof(NgDataItem));
	for (i = 0; i < data_profile->n_dataitems; i++) {
		NgVarData frvdata,tovdata;
		int size;

		ditems[i] = NhlMalloc(sizeof(NgDataItemRec));
		memcpy(ditems[i],
		       data_profile->ditems[i],sizeof(NgDataItemRec));
		frvdata = data_profile->ditems[i]->vdata;
		tovdata = NhlMalloc(sizeof(NgVarDataRec));
		memcpy(tovdata,frvdata,sizeof(NgDataItemRec));
		tovdata->dl = NULL;
		ditems[i]->vdata = tovdata;
		if (! frvdata->ndims)
			continue;
		size = frvdata->ndims * sizeof(int);
		tovdata->start = NhlMalloc(size);
		tovdata->finish = NhlMalloc(size);
		tovdata->stride = NhlMalloc(size);
		memcpy(tovdata->start,frvdata->start,size);
		memcpy(tovdata->finish,frvdata->finish,size);
		memcpy(tovdata->stride,frvdata->stride,size);
		tovdata->dims_alloced = tovdata->ndims = frvdata->ndims;
	}
	/* update the reference data item pointers */
	for (i = 0; i < data_profile->n_dataitems; i++) {
		NgDataItem sditem = data_profile->ditems[i];
		if (sditem->ref_ditem) {
			int j;
			for (j = 0; j < data_profile->n_dataitems; j++) {
				if (sditem->ref_ditem == 
				    data_profile->ditems[j]) {
					ditems[i]->ref_ditem = ditems[j];
					break;
				}
			}
		}
	}
	dprof->ditems = ditems;

	return dprof;
}

void NgFreeDataProfile(
	NgDataProfile data_profile
	)	
{
	int i;

	if (! data_profile)
		return;

	for (i = 0; i < data_profile->n_dataitems; i++) {
		NgVarData vdata = data_profile->ditems[i]->vdata;
		if (vdata) {
			if (vdata->ndims) {
				NhlFree(vdata->start);
				NhlFree(vdata->finish);
				NhlFree(vdata->stride);
			}
                        if (vdata->dl)
                                NclFreeDataList(vdata->dl);
			NhlFree(vdata);
		}
		NhlFree(data_profile->ditems[i]);
	}
	NhlFree(data_profile->ditems);
	NhlFree(data_profile);

	return;
}

		
