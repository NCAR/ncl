/*
 *      $Id: dataprofile.c,v 1.13 2000-01-10 21:08:12 dbrown Exp $
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
#include <ncarg/ngo/XmL.h>
#include <ncarg/ngo/nclstate.h>


typedef struct _TmpVarRec {
	struct _TmpVarRec	*next;
	NrmQuark		qsym;
	int			ref_count;
} TmpVarRec, *TmpVar;

TmpVar TmpVarList = NULL;

static void NewTmpVarRef
(
	NgGO		go,
	NrmQuark	qsym
)
{
	TmpVar tmp_var = TmpVarList;

	if (qsym == NrmNULLQUARK)
		return;

	while (tmp_var) {
		if (tmp_var->qsym == qsym) {
			tmp_var->ref_count++;
			return;
		}
		tmp_var = tmp_var->next;
	}
	tmp_var = NhlMalloc(sizeof(TmpVarRec));
	if (! tmp_var) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}
	tmp_var->next = TmpVarList;
	tmp_var->qsym = qsym;
	tmp_var->ref_count = 1;
	TmpVarList = tmp_var;

	return;
}
		

static void DeleteTmpVarRef
(
	NgGO		go,
	NrmQuark	qsym
)
{
	TmpVar *tmp_var = &TmpVarList;
	TmpVar dtmp_var = NULL;

	if (qsym == NrmNULLQUARK)
		return;

	while (*tmp_var) {
		if ((*tmp_var)->qsym == qsym) {
			(*tmp_var)->ref_count--;
			if ((*tmp_var)->ref_count > 0)
				return;
			dtmp_var = *tmp_var;
			*tmp_var = (*tmp_var)->next;
			break;
		}
		tmp_var = &((*tmp_var)->next);
	}
	if (dtmp_var) {
		char buf[256];
		NhlString sym = NrmQuarkToString(dtmp_var->qsym);
		if (sym && NclSymbolDefined(sym)) {
			sprintf(buf,"delete(%s)\n",sym);
			(void)NgNclSubmitBlock
				(go->go.nclstate,buf);
		}
		NhlFree(dtmp_var);
	}
	return;
}
		



static NrmQuark QString = NrmNULLQUARK;

static NhlBoolean ScalarDataDefined(
	struct _NgDataProfileRec	*dprof,
	int				dix
)
{
	NgDataItem ref_ditem = dprof->ditems[dix]->ref_ditem;

	switch (ref_ditem->vdata->set_state) {
	case _NgBOGUS_EXPRESSION:
	case _NgUSER_DISABLED:
		return False;
	case _NgUNKNOWN_DATA:
		return True;
	case _NgEXPRESSION:
	case _NgUSER_EXPRESSION:
		if (ref_ditem->vdata->expr_val)
			return True;
		break;
	default:
		if (ref_ditem->vdata->qvar != NrmNULLQUARK)
			return True;
		break;
	}
	return False;
}

/* 
 * This function assumes that vector data resources are next to each other
 * in the the ditems list. Both resources must be defined.
 */
static NhlBoolean VectorDataDefined(
	struct _NgDataProfileRec	*dprof,
	int				dix
)
{
	int i;
	NgDataItem ref_ditems[2];

	ref_ditems[0] = dprof->ditems[dix]->ref_ditem;
	ref_ditems[1] = dprof->ditems[ref_ditems[0]->vdata->data_ix +1];

	for (i = 0; i < NhlNumber(ref_ditems); i++) {
		switch (ref_ditems[i]->vdata->set_state) {
		case _NgBOGUS_EXPRESSION:
		case _NgUSER_DISABLED:
			return False;
		case _NgUNKNOWN_DATA:
			break;
		case _NgEXPRESSION:
		case _NgUSER_EXPRESSION:
			if (! ref_ditems[i]->vdata->expr_val)
				return False;
			break;
		default:
			if (ref_ditems[i]->vdata->qvar == NrmNULLQUARK)
				return False;
			break;
		}
	}
	return True;
}
/* 
 * This function assumes that coord array data resources are next to each other
 * in the the ditems list. Only one resource must be defined.
 */

static NhlBoolean CoordArrayDataDefined(
	struct _NgDataProfileRec	*dprof,
	int				dix
)
{
	int i;
	NgDataItem ref_ditems[2];

	ref_ditems[0] = dprof->ditems[dix]->ref_ditem;
	ref_ditems[1] = dprof->ditems[ref_ditems[0]->vdata->data_ix +1];

	for (i = 0; i < NhlNumber(ref_ditems); i++) {
		switch (ref_ditems[i]->vdata->set_state) {
		case _NgBOGUS_EXPRESSION:
		case _NgUSER_DISABLED:
			return False;
		case _NgUNKNOWN_DATA:
			return True;
		case _NgEXPRESSION:
		case _NgUSER_EXPRESSION:
			if (ref_ditems[i]->vdata->expr_val)
				return True;
			break;
		default:
			if (ref_ditems[i]->vdata->qvar != NrmNULLQUARK)
				return True;
			break;
		}
	}
	return False;
}


/*
 * CoordArrays configuration.
 * This only sets the X/YCast values for now
 */

static NhlBoolean GetCoordArrayValue 
(
	NgDataItem	ditem,
	NhlPointer	*value,
	NrmQuark	*type,
	NhlBoolean	private
)
{
	char buf[128];
	NhlString str;
	
	*value = NULL;
	*type = NrmNULLQUARK;

	if (private)
		buf[0] = '\0';
	else
		strcpy(buf,"\"");

	if (! ditem->vdata->cflags & _NgRANK_CHANGE)
		return False;
	
	if (ditem->ref_ditem->vdata->rank > 1) {
		strcat(buf,"MULTIPLEVECTORS");
	}
	else {
		strcat(buf,"SINGLEVECTOR");
	}
	if (! private)
		strcat(buf,"\"");
	str = NhlMalloc(strlen(buf)+1);
	strcpy(str,buf);
	*value = (NhlPointer) str;
	*type = QString;
	return True;
}

/*
 * VectorPlot configuration.
 * This handles resources that need to be changed to switch smoothly between
 * using a ScalarField and not using it.
 */

static NhlBoolean GetVectorPlotValue 
(
	NgDataItem	ditem,
	NhlPointer	*value,
	NrmQuark	*type,
	NhlBoolean	private
)
{
	char buf[128];
	NhlString str;
	static NhlBoolean first = True;
	static NrmQuark qdelay_compute, quse_scalar_array,
		qmono_fillcolor,qmono_linecolor;

	if (first) {
		first = False;
		qdelay_compute = NrmStringToQuark("dcDelayCompute");
		quse_scalar_array = NrmStringToQuark("vcUseScalarArray");
		qmono_fillcolor = NrmStringToQuark("vcMonoFillArrowFillColor");
		qmono_linecolor = NrmStringToQuark("vcMonoLineArrowColor");
	}
	*value = NULL;
	*type = NrmNULLQUARK;

	if (private)
		buf[0] = '\0';
	else
		strcpy(buf,"\"");

	if (! ditem->vdata->cflags & _NgSYMBOL_CHANGE)
		return False;
	
	if (ditem->resq == qdelay_compute ||
	    ditem->resq == quse_scalar_array) {
		if (ditem->ref_ditem->vdata->qvar) {
			strcat(buf,"True");
		}
		else {
			strcat(buf,"False");
		}
	}
	else if (ditem->resq == qmono_fillcolor ||
		 ditem->resq == qmono_linecolor) {
		if (ditem->ref_ditem->vdata->qvar) {
			strcat(buf,"False");
		}
		else {
			strcat(buf,"True");
		}
	}
	if (! private)
		strcat(buf,"\"");
	str = NhlMalloc(strlen(buf)+1);
	strcpy(str,buf);
	*value = (NhlPointer) str;
	*type = QString;
	return True;
}

/*
 * Fillvalue retrieval routine
 */

static NhlBoolean GetFillValue
(
	NgDataItem	ditem,
	NhlPointer	*value,
	NrmQuark	*type,
	NhlBoolean	private
)
{

	int i;
	NclApiVarInfoRec *vinfo;
	NclApiDataList   *dl = NULL;
        static NrmQuark qfillvalue = NrmNULLQUARK;
        static NrmQuark qmissingvalue = NrmNULLQUARK;
	NclExtValueRec *val = NULL;
	char *lvalue;
	NgVarData vdata;
	NrmQuark qfile,qvar;

	*value = NULL;
	*type = QString;

	vdata = ditem->ref_ditem->vdata;
	qfile = vdata->qfile;
	if (vdata->set_state == _NgEXPRESSION ||
	    vdata->set_state == _NgUSER_EXPRESSION)
		qvar = vdata->qexpr_var;
	else
		qvar = vdata->qvar;

	if (!qvar)
		return False;

	if (! (vdata->cflags & _NgSYMBOL_CHANGE))
		return False;

        if (qfillvalue == NrmNULLQUARK) {
                qfillvalue = NrmStringToQuark("_FillValue"); 
                qmissingvalue = NrmStringToQuark("missing_value"); 
        }
	if (vdata->dl) {
		vinfo = vdata->dl->u.var;
	}
	else {
		if (qfile > NrmNULLQUARK)
			dl = NclGetFileVarInfo(qfile,qvar);
		else 	
			dl = NclGetVarInfo(qvar);
		vinfo = dl->u.var;
	}

	for (i = 0; i < vinfo->n_atts; i++) {
                if (vinfo->attnames[i] == qfillvalue)
                        break;
        }
	if (i == vinfo->n_atts) {
		for (i = 0; i < vinfo->n_atts; i++) {
			if (vinfo->attnames[i] == qmissingvalue)
				break;
		}
		if (i == vinfo->n_atts) {
			if (dl)
				NclFreeDataList(dl);
			return False;
		}
        }
        
	if (qfile >  NrmNULLQUARK)
                val = NclReadFileVarAtt(qfile,qvar,vinfo->attnames[i]);
        else 
                val = NclReadVarAtt(qvar,vinfo->attnames[i]);

        if (val) {
                lvalue = (NhlPointer) NclTypeToString(val->value,val->type);
		if (lvalue) {
			if (private) {
				*value = NhlMalloc(strlen(lvalue)+1);
				strcpy(*value,lvalue);
			}
			else {
				*value = NhlMalloc(strlen(lvalue)+3);
				strcpy(*value,"\"");
				strcat(*value,lvalue);
				strcat(*value,"\"");
			}
			NclFree(lvalue);
		}
                if (val->constant != 0)
                        NclFree(val->value);
                NclFreeExtValue(val);
        }
	if (dl)
		 NclFreeDataList(dl);
	return *value ? True : False;
}
	

/* scalar field and contour plot data items */

static NgDataItemRec sfdataarray = {
	"scalar field", "sfDataArray", NrmNULLQUARK,NrmNULLQUARK,  
	_NgSCALARFIELD,_NgDATAVAR,2,2,NULL,
	True,True,NULL,NULL,NULL,NULL,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec sfxarray = {
	"x coord", "sfXArray",NrmNULLQUARK,NrmNULLQUARK,
	_NgSCALARFIELD,_NgCOORDVAR,1,1,(NhlPointer)1,
	False,True,NULL,NULL,NULL,&sfdataarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec sfyarray = {
	"y coord", "sfYArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgSCALARFIELD,_NgCOORDVAR,1,1,(NhlPointer)2,
	False,True,NULL,NULL,NULL,&sfdataarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec sfmissingvaluev =	{
	"missing value", "sfMissingValueV", NrmNULLQUARK,NrmNULLQUARK,
	_NgSCALARFIELD,_NgMISSINGVAL,0,0,NULL,
	False,False,NULL,GetFillValue,NULL,
	&sfdataarray,0,NULL,NULL,NULL,False,False,False };


static NgDataItemRec cnscalarfielddata = {
	"scalar field object", "cnScalarFieldData", NrmNULLQUARK,NrmNULLQUARK,
	_NgCONTOURPLOT,_NgDATAOBJ,0,0,NULL,
	False,False,NULL,NULL,ScalarDataDefined,
	&sfdataarray,0,NULL,NULL,NULL,False,False,False};

static NgDataItem ScalarFieldItems[] = {
	&sfdataarray,&sfxarray,&sfyarray,&sfmissingvaluev };

static NgDataItem ContourPlotItems[] = {
	&sfdataarray,&sfxarray,&sfyarray,&sfmissingvaluev,&cnscalarfielddata };
static NgDataItem ContourPlot0Items[] = { &cnscalarfielddata };


/* vector field, vector plot and streamline plot  data items */

static NgDataItemRec vfudataarray = {
	"u vector field", "vfUDataArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORFIELD,_NgDATAVAR,2,2,NULL,
	True,True,NULL,NULL,NULL,NULL,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vfvdataarray = {
	"v vector field", "vfVDataArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORFIELD,_NgDATAVAR,2,2,NULL,
	True,True,NULL,NULL,NULL,&vfudataarray,
	0,NULL,NULL,NULL,False,False,False};
static NgDataItemRec vfxarray = {
	"x coord", "vfXArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORFIELD,_NgCOORDVAR,1,1,(NhlPointer)1,
	False,True,NULL,NULL,NULL,&vfudataarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vfyarray = {
	"y coord", "vfYArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORFIELD,_NgCOORDVAR,1,1,(NhlPointer)2,
	False,True,NULL,NULL,NULL,&vfudataarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vfmissinguvaluev = {
	"u missing value", "vfMissingUValueV", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORFIELD,_NgMISSINGVAL,0,0,NULL,
	False,False,NULL,GetFillValue,NULL,
	&vfudataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vfmissingvvaluev = {
	"v missing value", "vfMissingVValueV", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORFIELD,_NgMISSINGVAL,0,0,NULL,
	False,False,NULL,GetFillValue,NULL,
	&vfvdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vf_sfdataarray = {
	"scalar field", "sfDataArray", NrmNULLQUARK,NrmNULLQUARK,  
	_NgSCALARFIELD,_NgDATAVAR,2,2,NULL,
	False,True,NULL,NULL,NULL,
	&vfudataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vf_sfmissingvaluev =	{
	"missing value", "sfMissingValueV", NrmNULLQUARK,NrmNULLQUARK,
	_NgSCALARFIELD,_NgMISSINGVAL,0,0,NULL,
	False,False,NULL,GetFillValue,NULL,
	&vf_sfdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec dcdelaycompute =	{
	"delay compute", "dcDelayCompute", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORPLOT,_NgCONFIG,0,0,NULL,
	False,False,NULL,GetVectorPlotValue,NULL,
	&vf_sfdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vcusescalararray =	{
	"use scalar array", "vcUseScalarArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORPLOT,_NgCONFIG,0,0,NULL,
	False,False,NULL,GetVectorPlotValue,NULL,
	&vf_sfdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vcmonofillarrowfillcolor =	{
	"mono fill arrow color", "vcMonoFillArrowFillColor", 
	NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORPLOT,_NgCONFIG,0,0,NULL,
	False,False,NULL,GetVectorPlotValue,NULL,
	&vf_sfdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vcmonolinearrowcolor =	{
	"mono line arrow color", "vcMonoLineArrowColor", 
	NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORPLOT,_NgCONFIG,0,0,NULL,
	False,False,NULL,GetVectorPlotValue,NULL,
	&vf_sfdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vcvectorfielddata = {
	"vector field object", "vcVectorFieldData", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORPLOT,_NgDATAOBJ,0,0,NULL,
	False,False,NULL,NULL,VectorDataDefined,
	&vfudataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec vcscalarfielddata = {
	"scalar field object", "vcScalarFieldData", NrmNULLQUARK,NrmNULLQUARK,
	_NgVECTORPLOT,_NgDATAOBJ,0,0,NULL,
	False,False,NULL,NULL,ScalarDataDefined,
	&vf_sfdataarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec stvectorfielddata = {
	"vector field object", "stVectorFieldData", NrmNULLQUARK,NrmNULLQUARK,
	_NgSTREAMLINEPLOT,_NgDATAOBJ,0,0,NULL,
	False,False,NULL,NULL,VectorDataDefined,
	&vfudataarray,0,NULL,NULL,NULL,False,False,False };

static NgDataItem VectorFieldItems[] = {
	&vfudataarray,&vfvdataarray,&vfxarray,&vfyarray,
	&vfmissinguvaluev,&vfmissingvvaluev };


static NgDataItem VectorPlotItems[] = {
	&vfudataarray,&vfvdataarray,&vfxarray,&vfyarray,
	&vfmissinguvaluev,&vfmissingvvaluev,
	&vf_sfdataarray,&vf_sfmissingvaluev,
	&vcvectorfielddata,&vcscalarfielddata,
	&dcdelaycompute,&vcusescalararray,
	&vcmonofillarrowfillcolor,&vcmonolinearrowcolor };

static NgDataItem VectorPlot0Items[] = {
	&vcvectorfielddata,&vcscalarfielddata,
	&dcdelaycompute,&vcusescalararray,
	&vcmonofillarrowfillcolor,&vcmonolinearrowcolor };

static NgDataItem StreamlinePlotItems[] = {
	&vfudataarray,&vfvdataarray,&vfxarray,&vfyarray,
	&vfmissinguvaluev,&vfmissingvvaluev,&stvectorfielddata };

static NgDataItem StreamlinePlot0Items[] = { &stvectorfielddata };


/* coord array and xyplot data items */

static NgDataItemRec cayarray = {
	"y array", "caYArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgCOORDARRAY,_NgDATAVAR,2,1,(NhlPointer)1,
	False,True,NULL,NULL,NULL,NULL,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec caxarray = {
	"x array", "caXArray", NrmNULLQUARK,NrmNULLQUARK,
	_NgCOORDARRAY,_NgDATAVAR,2,1,(NhlPointer)1,
	False,True,NULL,NULL,NULL,&cayarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec caymissingv = {
	"y missing value", "caYMissingV", NrmNULLQUARK,NrmNULLQUARK,
	_NgCOORDARRAY,_NgMISSINGVAL,0,0,NULL,
	False,False,NULL,GetFillValue,NULL,&cayarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec caxmissingv = {
	"x missing value", "caXMissingV", NrmNULLQUARK,NrmNULLQUARK,
	_NgCOORDARRAY,_NgMISSINGVAL,0,0,NULL,
	False,False,NULL,GetFillValue,NULL,&caxarray,
	0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec caycast = {
	"y array type", "caYCast", NrmNULLQUARK,NrmNULLQUARK,
	_NgCOORDARRAY,_NgCONFIG,0,0,NULL,
	False,False,NULL,GetCoordArrayValue,NULL,
	&cayarray,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec caxcast = {
	"x array type", "caXCast", NrmNULLQUARK,NrmNULLQUARK,
	_NgCOORDARRAY,_NgCONFIG,0,0,NULL,
	False,False,NULL,GetCoordArrayValue,NULL,
	&caxarray ,0,NULL,NULL,NULL,False,False,False };
static NgDataItemRec xycoorddata = {
	"coord arrays object", "xyCoordData", NrmNULLQUARK,NrmNULLQUARK,
	_NgXYPLOT,_NgDATAOBJ,0,0,NULL,
	False,False,NULL,NULL,CoordArrayDataDefined,
	&cayarray,0,NULL,NULL,NULL,False,False,False };

static NgDataItem CoordArrayItems[] = {
	&cayarray,&caxarray,&caymissingv,&caxmissingv,&caycast,&caxcast
};


static NgDataItem XyPlotItems[] = {
	&cayarray,&caxarray,&caymissingv,&caxmissingv,
	&caycast,&caxcast,&xycoorddata
};
static NgDataItem XyPlot0Items[] = {&xycoorddata};


#define FreeFunc ((NhlFreeFunc) NgFreeDataProfile)

static NgDataProfileRec DataProfs[] = {
	{_NgDEFAULT,NULL,NULL,0,0,False,NULL,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc},
	{_NgCONTOURPLOT,"contourPlotClass",NULL,
         NhlNumber(ContourPlotItems),0,False,ContourPlotItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgSTREAMLINEPLOT,"streamlinePlotClass",NULL,
	 NhlNumber(StreamlinePlotItems),0,False,StreamlinePlotItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgVECTORPLOT,"vectorPlotClass",NULL,
         NhlNumber(VectorPlotItems),0,False,VectorPlotItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgXYPLOT,"xyPlotClass",NULL,
         NhlNumber(XyPlotItems),0,False,XyPlotItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgCOORDARRAY,"coordArraysClass",NULL,
         NhlNumber(CoordArrayItems),0,False,CoordArrayItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgSCALARFIELD,"scalarFieldClass",NULL,
         NhlNumber(ScalarFieldItems),0,False,ScalarFieldItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgVECTORFIELD,"vectorFieldClass",NULL,
         NhlNumber(VectorFieldItems),0,False,VectorFieldItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgPLOT,NGPLOTCLASS,NULL,0,0,False,NULL,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc }
};

/*
 * This is an alternate version of the DataProfs, where each individual
 * data prof only has items belonging to its own class. (the data object
 * items are eliminated from the plot objects.) This is to allow
 * more straightforward processing for plot styles where the data objects
 * are explicitly defined and where there may be more than one object of
 * a given type.
 */

static NgDataProfileRec PlotDataProfs[] = {
	{_NgDEFAULT,NULL,NULL,0,0,False,NULL,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc},
	{_NgCONTOURPLOT,"contourPlotClass",NULL,
         NhlNumber(ContourPlot0Items),0,False,ContourPlot0Items,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgSTREAMLINEPLOT,"streamlinePlotClass",NULL,
	 NhlNumber(StreamlinePlot0Items),0,False,StreamlinePlot0Items,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgVECTORPLOT,"vectorPlotClass",NULL,
         NhlNumber(VectorPlot0Items),0,False,VectorPlot0Items,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgXYPLOT,"xyPlotClass",NULL,
         NhlNumber(XyPlot0Items),0,False,XyPlot0Items,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgCOORDARRAY,"coordArraysClass",NULL,
         NhlNumber(CoordArrayItems),0,False,CoordArrayItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgSCALARFIELD,"scalarFieldClass",NULL,
         NhlNumber(ScalarFieldItems),0,False,ScalarFieldItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc },
	{_NgVECTORFIELD,"vectorFieldClass",NULL,
         NhlNumber(VectorFieldItems),0,False,VectorFieldItems,
	 NrmNULLQUARK,0,NULL,NULL,0,NULL,FreeFunc }
};

#undef FreeFunc

static void InitializeDataProfile(
	NgDataProfile dprof_list,
	int	      dprof_count
)
{
	int i,j;

	QString = NrmStringToQuark(NhlTString);

	for (i = 0; i < dprof_count; i++) {
		for (j = 0; j < dprof_list[i].n_dataitems; j++) {
			NgDataItem ditem = dprof_list[i].ditems[j];
			ditem->resq = NrmStringToQuark(ditem->resname);
		}
	}
	return;
}

extern NgArgInfo NgNewArgInfo(
	count
)
{
	NgArgInfo args = NhlMalloc(count * sizeof(NgArgInfoRec));

	if (! args) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	memset(args,(char)0,count * sizeof(NgArgInfoRec));

	return args;
}

void NgFreeArgInfo(
	NgArgInfo	arg_info,
	int		count
)
{
	int i;

	if (! arg_info)
		return;

	for (i = 0; i < count; i++) {
		NgArgInfo arg = &arg_info[i];
		if (arg->sval)
			NhlFree(arg->sval);
		if (arg->edata && arg->free_edata)
			(*arg->free_edata)(arg->edata);
		if (arg->argcount)
			NgFreeArgInfo(arg->args,arg->argcount);
	}
	NhlFree(arg_info);

	return;
}

extern NgResInfo NgNewResInfo(
	void
)
{
	NgResInfo rinfo = NhlMalloc(sizeof(NgResInfoRec));

	if (! rinfo) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	memset(rinfo,(char)0,sizeof(NgResInfoRec));

	return rinfo;
}

void NgFreeResInfo(
	NgResInfo res_info
)
{
	int i;

	if (! res_info)
		return;

	if (res_info->argcount)
		NgFreeArgInfo(res_info->args,res_info->argcount);
	if (res_info->edata && res_info->free_edata) 
		(*res_info->free_edata)(res_info->edata);

	NhlFree(res_info);

	return;
}

NgVarData NgNewVarData
(
	void
	)
{
	NgVarData vdata = NhlMalloc(sizeof(NgVarDataRec));

	if (! vdata) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	vdata->next = NULL;
	vdata->rank = vdata->ndims = vdata->dims_alloced = 0;
	vdata->start = vdata->finish = vdata->stride = NULL;
	vdata->size = vdata->order_ix = NULL;
	vdata->dl = NULL;
	vdata->qfile = vdata->qvar = vdata->qcoord = NrmNULLQUARK;
	vdata->size_only = False;
	vdata->type = NORMAL;
	vdata->data_ix = -1;
	vdata->cflags = 0;
	vdata->set_state = _NgVAR_UNSET;
	vdata->expr_val = NULL;
	vdata->qexpr_var = NrmNULLQUARK;
	vdata->go = NULL;
	return vdata;
}
	
void NgFreeVarData
(
	NgVarData var_data
	)

{
	if (!var_data)
		return;

	if (var_data->dims_alloced) {
		NhlFree(var_data->start);
		NhlFree(var_data->finish);
		NhlFree(var_data->stride);
		NhlFree(var_data->size);
		NhlFree(var_data->order_ix);
	}
	if (var_data->dl)
		NclFreeDataList(var_data->dl);
	if (var_data->expr_val)
		NhlFree(var_data->expr_val);
	if (var_data->qexpr_var) {
		DeleteTmpVarRef(var_data->go,var_data->qexpr_var);
	}
	NhlFree(var_data);
	return;
}

NhlBoolean NgCopyVarData
(
	NgVarData	to_var_data,
	NgVarData	from_var_data
	)
{
	int size = from_var_data->ndims * sizeof(long);

/*
 * False is returned only for invalid input or for a memory allocation problem
 */
        if (! (to_var_data && from_var_data)) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: invalid input","NgCopyVarData"));
		return False;
	}
	if (to_var_data == from_var_data) {
		return True;
	}	
	to_var_data->cflags = from_var_data->cflags;  
	to_var_data->qvar = from_var_data->qvar;
	to_var_data->qfile = from_var_data->qfile;
	to_var_data->qcoord = from_var_data->qcoord;
	to_var_data->type = from_var_data->type;
	to_var_data->ndims = from_var_data->ndims;
	to_var_data->size_only = from_var_data->size_only;
	to_var_data->rank = from_var_data->rank;
	to_var_data->data_ix = from_var_data->data_ix;
	to_var_data->set_state = from_var_data->set_state;

	if (to_var_data->qexpr_var != from_var_data->qexpr_var) {
		if (to_var_data->qexpr_var != NrmNULLQUARK)
			DeleteTmpVarRef
				(to_var_data->go,to_var_data->qexpr_var);
		to_var_data->qexpr_var = from_var_data->qexpr_var;
		to_var_data->go = from_var_data->go;
		if (to_var_data->qexpr_var)
			NewTmpVarRef(to_var_data->go,to_var_data->qexpr_var);
		
	}

#if 0
	if (from_var_data->set_state == _NgEXPRESSION ||
	    from_var_data->set_state == _NgBOGUS_EXPRESSION) {
#endif
		if (from_var_data->expr_val) {
			if (to_var_data->expr_val) {
				if (strcmp(to_var_data->expr_val,
					   from_var_data->expr_val)) {
					NhlFree(to_var_data->expr_val);
				}
			}
			to_var_data->expr_val = 
				NhlMalloc(strlen(from_var_data->expr_val)+1);
			if (! to_var_data->expr_val) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return False;
			}
			strcpy(to_var_data->expr_val,from_var_data->expr_val);
		}
		else if (to_var_data->expr_val) {
			NhlFree(to_var_data->expr_val);
			to_var_data->expr_val = NULL;
		}
#if 0
	}
#endif

	if (from_var_data->ndims <= 0) {
		return True;
	}

        if (from_var_data->ndims > to_var_data->dims_alloced) {
                to_var_data->start = NhlRealloc(to_var_data->start,size);
                to_var_data->finish = NhlRealloc(to_var_data->finish,size);
                to_var_data->stride = NhlRealloc(to_var_data->stride,size);
                to_var_data->size = NhlRealloc(to_var_data->size,size);
                to_var_data->order_ix = NhlRealloc(to_var_data->order_ix,size);
		if (! (to_var_data->start && 
		       to_var_data->finish && to_var_data->stride &&
		       to_var_data->size && to_var_data->order_ix)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		to_var_data->dims_alloced = from_var_data->ndims;
        }

	memcpy(to_var_data->start,from_var_data->start,size);
	memcpy(to_var_data->finish,from_var_data->finish,size);
	memcpy(to_var_data->stride,from_var_data->stride,size);
	memcpy(to_var_data->size,from_var_data->size,size);
	memcpy(to_var_data->order_ix,from_var_data->order_ix,size);

	return True;
}

extern
int NgVarDataRank(
	NgVarData	vdata
)
{
	int i,rank;

	for (i = 0,rank = 0; i < vdata->ndims; i++) {
		if (((vdata->finish[i]-vdata->start[i])/vdata->stride[i]) > 0)
			rank++;
	}
	return rank;
}
 
/*
 * This routine does a getvalues if it can to at least find out the
 * dim sizes for an unknown variable
 */

extern NhlBoolean NgSetUnknownDataItem
(
	NgDataItem ditem
)
{
	static	int grlist = -1;
	NgVarData vdata = ditem->vdata;
	int i,size;
	NhlGenArray gen;

	if (grlist < 0)
		grlist = NhlRLCreate(NhlGETRL);
	else
		NhlRLClear(grlist);

	if (! ditem->hlu_id)
		return False;

	NhlRLGet(grlist,ditem->resname,NhlTGenArray,&gen);
	NhlGetValues(ditem->hlu_id,grlist);

	vdata->set_state = _NgUNKNOWN_DATA;
	if (vdata->expr_val) {
		NhlFree(vdata->expr_val);
		vdata->expr_val = NULL;
	}
	if (vdata->qexpr_var) {
		DeleteTmpVarRef(vdata->go,vdata->qexpr_var);
		vdata->qexpr_var = NrmNULLQUARK;
		vdata->go = NULL;
	}
		
	if (!gen) {
		vdata->rank = vdata->ndims = 0;
		vdata->cflags = 0; /* don't want vars specified like this used
				      in a setvalues call */
		vdata->qfile = vdata->qvar = NrmNULLQUARK;
		return True;
		/* is this okay ????? */
	}

	if (gen->num_dimensions > vdata->ndims) {
		size = gen->num_dimensions * sizeof(long);
                vdata->start = NhlRealloc(vdata->start,size);
                vdata->finish = NhlRealloc(vdata->finish,size);
                vdata->stride = NhlRealloc(vdata->stride,size);
                vdata->size = NhlRealloc(vdata->size,size);
                vdata->order_ix = NhlRealloc(vdata->order_ix,size);
		if (! (vdata->start && vdata->finish && vdata->stride &&
			vdata->size && vdata->order_ix)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		vdata->dims_alloced = gen->num_dimensions;
	}
	for (i = 0; i < gen->num_dimensions; i++) {
		vdata->start[i] = 0;
		vdata->finish[i] = gen->len_dimensions[i] - 1;
		vdata->stride[i] = 1;
		vdata->size[i] = gen->len_dimensions[i];
		vdata->order_ix[i] = -1;
	}
	vdata->size_only = True;
	vdata->rank = vdata->ndims = gen->num_dimensions;
	vdata->cflags = 0; /* don't want vars specified like this used
			      in a setvalues call */
	vdata->qfile = vdata->qvar = vdata->qcoord = NrmNULLQUARK;
	vdata->type = NORMAL;

	NhlFreeGenArray(gen);
	return True;
}

NclApiDataList	*EvaluateExpression
(
	NgGO		go,
	NhlString	expr_val,
	NrmQuark	*qexpr_var
)
{
	NclExtValueRec	*val = NULL;
	NclApiDataList	*dl;
	char 		buf[512];
	NhlString 	tmp_var;
	NrmQuark	qtmp_var;

/*
 * the old variable should be deleted only if evaluation of the expression
 * succeeds; but then the new var should be assigned to the old var name
 * in order to ensure that other references to the variable are still
 * legitimate.
 */
        XDefineCursor(go->go.x->dpy,
                      XtWindow(go->go.manager),go->go.x->wait);
	XSync(go->go.x->dpy,False);
		
	tmp_var = NgNclGetSymName(go->go.nclstate,_NgTMP_VAR,True);

	sprintf(buf,"%s = %s\n",tmp_var,expr_val);
	(void)NgNclSubmitBlock(go->go.nclstate,buf);
	qtmp_var = NrmStringToQuark(tmp_var);
	
	dl = NclGetVarInfo(qtmp_var);
#if 0
	val = NclReadVar(qtmp_var,NULL,NULL,NULL);
#endif

	if (! dl) {
		if (NclSymbolDefined(tmp_var)) {
			sprintf(buf,
				"delete(%s)\n",tmp_var);
			(void)NgNclSubmitBlock(go->go.nclstate,buf);
		}
	}
	else {
		if (*qexpr_var == qtmp_var)
			;
		else if (*qexpr_var == NrmNULLQUARK) {
			*qexpr_var = qtmp_var;
			NewTmpVarRef(go,*qexpr_var);
		}
		else {
			sprintf(buf,"delete(%s)\n",
				NrmQuarkToString(*qexpr_var));
			sprintf(&buf[strlen(buf)],"%s=%s\n",
				NrmQuarkToString(*qexpr_var),
				NrmQuarkToString(qtmp_var));
			sprintf(&buf[strlen(buf)],"delete(%s)\n",
				NrmQuarkToString(qtmp_var));
			(void)NgNclSubmitBlock(go->go.nclstate,buf);
			
		}
	}
        XUndefineCursor(go->go.x->dpy,XtWindow(go->go.manager));

	if (val) 
		NclFreeExtValue(val);

	return dl;
}

/*
 * This function just gets rid of the tmpvar after the data has
 * been used. The vdata is still of type _NgEXPRESSION, however, and
 * if necessary, the data can be reconstructed.
 */

NhlBoolean NgDeleteExpressionVarData
(
	int		go_id,
	NgVarData	vdata
)
{
	NgGO		go = (NgGO) _NhlGetLayer(go_id);

	if (! go) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid go id"));
		return False;
	}

	if (! (vdata && vdata->qexpr_var))
		return False;

	DeleteTmpVarRef(vdata->go,vdata->qexpr_var);

	vdata->qexpr_var = NrmNULLQUARK;

	return True;
}
	
NhlBoolean NgSetExpressionVarData
(
	int		go_id,
	NgVarData	vdata,
	NhlString	expr_val,
	NgEvalAction	eval_action,
	NhlBoolean	user_set
)
{
	NclApiDataList	*dl = NULL;
	NhlBoolean	copy_expr = False;
	int 		i,size;
	NgGO		go = (NgGO) _NhlGetLayer(go_id);
	NhlBoolean	do_eval = False;

	if (! go) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid go id"));
		return False;
	}

	if (! (vdata && expr_val && strlen(expr_val)))
		return False;

	if (! vdata->expr_val) {
		copy_expr = True;
	}
	else if (strcmp(vdata->expr_val,expr_val)) {
		NhlFree(vdata->expr_val);
		vdata->expr_val = NULL;
		copy_expr = True;
	}
	if (copy_expr) {
		vdata->expr_val = NhlMalloc(strlen(expr_val)+1);
		if (! vdata->expr_val) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		strcpy(vdata->expr_val,expr_val);
	}
	vdata->go = go;

	switch (eval_action) {
	case _NgNOEVAL:
	default:
		break;
	case _NgCONDITIONAL_EVAL:
		if (copy_expr || vdata->qexpr_var <= NrmNULLQUARK)
			do_eval = True;
		break;
	case _NgFORCED_EVAL:
		do_eval = True;
		break;
	}

	if (do_eval) {
		dl = EvaluateExpression(go,expr_val,&vdata->qexpr_var);

                if (! (dl && vdata->qexpr_var)) {
			if (dl)
				NclFreeDataList(dl);
			vdata->set_state = _NgBOGUS_EXPRESSION;
			return False;
                }
		vdata->go = go;
	}
	if (do_eval || copy_expr) {
		vdata->cflags = _NgSYMBOL_CHANGE | _NgSHAPE_CHANGE;
		vdata->qfile = vdata->qvar = vdata->qcoord = NrmNULLQUARK;

		if (! dl) {
			vdata->ndims = -1;
		}
		else {
			if (dl->u.var->n_dims > vdata->ndims) {
				size = dl->u.var->n_dims * sizeof(long);
				vdata->start = NhlRealloc(vdata->start,size);
				vdata->finish = NhlRealloc(vdata->finish,size);
				vdata->stride = NhlRealloc(vdata->stride,size);
				vdata->size = NhlRealloc(vdata->size,size);
				vdata->order_ix = 
					NhlRealloc(vdata->order_ix,size);
				if (! (vdata->start && 
				       vdata->finish && vdata->stride &&
					vdata->size && vdata->order_ix)) {
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return False;
				}
				vdata->dims_alloced = dl->u.var->n_dims;
			}
			for (i = 0; i < dl->u.var->n_dims; i++) {
				vdata->start[i] = 0;
				vdata->finish[i] = 
					dl->u.var->dim_info[i].dim_size - 1;
				vdata->stride[i] = 1;
				vdata->size[i] = 
					dl->u.var->dim_info[i].dim_size;
				vdata->order_ix[i] = -1;
				if (vdata->rank != dl->u.var->n_dims)
					vdata->cflags |= _NgRANK_CHANGE;
				vdata->ndims = vdata->rank = dl->u.var->n_dims;
			}
		}
		vdata->size_only = True;
		vdata->set_state = user_set ? 
			_NgUSER_EXPRESSION : _NgEXPRESSION;
		vdata->type = NORMAL;
	}
        if (dl)
                NclFreeDataList(dl);
        
	return True;
}

/*
 * if the set_state is any kind of expression or _NgUNKNOWN_DATA then 
 * NgSetVarData should not be used.
 */

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
)
{
	NclApiVarInfoRec	*vinfo;
	NhlBoolean		allocated = False;
	int 			i,size,rank;
/*
 * dl, the NclApiDataList may optionally be supplied if the caller has
 * access to it. That will save the allocation and freeing of an 
 * NclApiDataList in this routine. Of course, if supplied it needs to
 * match the symbols. This routine does not check for that condition.
 *
 *
 * If start, finish, and stride are not supplied this routine defaults to
 * a MIN(set_dim_count,vinfo->n_dims) slice along the fastest varying 
 * dimensions. If start, finish, and stride are supplied, set_dim_count
 * specifies how many elements they contain.
 */
	if (! var_data) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: invalid var_data parameter","NgSetVarData"));
		return False;
	}
	switch (set_state) {
	default:
		break;
	case _NgEXPRESSION:
	case _NgUNKNOWN_DATA:
	case _NgBOGUS_EXPRESSION:
	case _NgUSER_EXPRESSION:
	case _NgUSER_DISABLED:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: invalid set_state parameter","NgSetVarData"));
		return False;
	}
	if (var_data->expr_val) {
		NhlFree(var_data->expr_val);
		var_data->expr_val = NULL;
	}
	if (var_data->qexpr_var) {
		DeleteTmpVarRef(var_data->go,var_data->qexpr_var);
		var_data->qexpr_var = NrmNULLQUARK;
		var_data->go = NULL;
	}
		
	var_data->set_state = set_state;
	if (qvar == NrmNULLQUARK) {
		if (var_data->qvar)
			var_data->cflags = _NgALL_CHANGE;
		var_data->qvar = var_data->qfile = 
			var_data->qcoord = NrmNULLQUARK;
		var_data->rank = var_data->ndims = 0;
		return True;
	}
	if (! dl) {
		if (qfile && qcoord)
			dl = NclGetFileVarCoordInfo(qfile,qvar,qcoord);
		else if (qfile)
			dl = NclGetFileVarInfo(qfile,qvar);
		else if (qcoord) 
			dl = NclGetVarCoordInfo(qvar,qcoord);
		else
			dl = NclGetVarInfo(qvar);
		if (! (dl)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		allocated = True;
	}

	vinfo = dl->u.var;
	size = vinfo->n_dims * sizeof(long);
	if (qfile != var_data->qfile || qvar != var_data->qvar ||
		qcoord != var_data->qcoord)
		var_data->cflags |= _NgSYMBOL_CHANGE;

        if (vinfo->n_dims > var_data->dims_alloced) {
                var_data->start = NhlRealloc(var_data->start,size);
                var_data->finish = NhlRealloc(var_data->finish,size);
                var_data->stride = NhlRealloc(var_data->stride,size);
                var_data->size = NhlRealloc(var_data->size,size);
                var_data->order_ix = NhlRealloc(var_data->order_ix,size);
		if (! (var_data->start && 
		       var_data->finish && var_data->stride &&
			var_data->size && var_data->order_ix)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		for (i = var_data->dims_alloced; i < vinfo->n_dims; i++)
			var_data->start[i] = var_data->finish[i] = 
				var_data->stride[i] =  var_data->size[i] =
				var_data->order_ix[i] = -1;

		var_data->dims_alloced = vinfo->n_dims;
		var_data->cflags |= _NgSHAPE_CHANGE;
        }
	if (! (start && finish && stride)) {
		/* default to the fastest varying slice of rank 2 or 1 */
		for (i = 0; i< vinfo->n_dims; i++) {
			var_data->start[i] = 0;
			var_data->stride[i] = 1;
			if (i >= vinfo->n_dims - set_dim_count)
				var_data->finish[i] =
					vinfo->dim_info[i].dim_size - 1;
			else
				var_data->finish[i] = 0;
			var_data->size[i] = vinfo->dim_info[i].dim_size; 
			
		}
		var_data->cflags |= _NgSHAPE_CHANGE;
        }
	else if (set_dim_count != vinfo->n_dims) {
		for (i = 0; i < vinfo->n_dims; i++) {
			int ix = i + set_dim_count - vinfo->n_dims;
			if (ix < 0) {
				var_data->start[i] = 0;
				var_data->finish[i] = 0;
				var_data->stride[i] = 1;
			}
			else {
				if (vinfo->dim_info[i].dim_size <
				    MAX(start[ix],finish[ix]) - 1) {
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				        "%s: invalid input","NgSetVarData"));
					if (allocated)
						NclFreeDataList(dl);
					return False;
				}
				var_data->start[i] = start[ix];
				var_data->finish[i] = finish[ix];
				var_data->stride[i] = stride[ix];
			}
			var_data->size[i] = vinfo->dim_info[i].dim_size;
		}
		var_data->cflags |= _NgSHAPE_CHANGE;
	}
	else {
		if (var_data->ndims != vinfo->n_dims)
			var_data->cflags |= _NgSHAPE_CHANGE;
		if (memcmp(var_data->start,start,size) ||
		    memcmp(var_data->finish,finish,size) ||
		    memcmp(var_data->stride,stride,size) ) {
			var_data->cflags |= _NgSHAPE_CHANGE;
			memcpy(var_data->start,start,size);
			memcpy(var_data->finish,finish,size);
			memcpy(var_data->stride,stride,size);
		}
		for (i = 0; i < vinfo->n_dims; i++) {
			var_data->size[i] = vinfo->dim_info[i].dim_size;
		}

		if (! var_data->cflags) {
			if (allocated)
				NclFreeDataList(dl);
			return True;
		}
	}

	var_data->size_only = False;
	var_data->qvar = qvar;
	var_data->qfile = qfile;
	var_data->qcoord = qcoord;
	var_data->ndims = vinfo->n_dims;

	if (qfile && qcoord)
		var_data->type = COORD;
	else if (qfile)
		var_data->type = FILEVAR;
	else if (qcoord)
		var_data->type = COORD;
	else
		var_data->type = NORMAL; 

	rank = NgVarDataRank(var_data);
	if (var_data->rank != rank) {
		var_data->rank = rank;
		var_data->cflags |= _NgRANK_CHANGE;
	}

	if (allocated)
		NclFreeDataList(dl);
	return True;
}

/*
 * still using this with PlotDataProfs because the type is the same
 */
static
NgClassType ClassType
(
	NhlClass class
)
{
	int i;

	for (i = 0; i < NhlNumber(DataProfs); i++) {
		if (class == DataProfs[i].class) {
			return DataProfs[i].type;
		}
	}
	return _NgDEFAULT;
}

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
)
{
	NgDataItem ditem = NhlMalloc(sizeof(NgDataItemRec));
	NgVarData vdata = NgNewVarData();

	if (!(ditem && vdata)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	ditem->name = NrmQuarkToString(qname);
	ditem->resname = NrmQuarkToString(qresname);
	ditem->resq = qresname;
	ditem->qhlu_name = qhlu_name;
	ditem->class_type = ClassType(class);
	ditem->item_type = item_type;
	ditem->mindims = min_dims;
	ditem->maxdims = max_dims;
	ditem->data = data;
	ditem->required = required;
	ditem->vis = visible;
	ditem->vdata = vdata;
	ditem->set_only = set_only;
	ditem->save_to_compare = save_to_compare;
	ditem->init_only = init_only;
	ditem->get_val = NULL;
	ditem->val_defined = NULL;
	ditem->ref_ditem = NULL;
	ditem->hlu_id = NhlNULLOBJID;
	ditem->svcb = NULL;
	ditem->raw_val = NULL;
	ditem->res_info = NULL;

	return ditem;
	
}
	
NhlErrorTypes
NgAppendDataProfileItem
(
	NgDataProfile	data_profile,
	NgDataItem	data_item,
	NrmQuark	qref_res
)
{
	int i;

	data_profile->ditems = NhlRealloc
		(data_profile->ditems,
		 sizeof(NgDataItem) * (data_profile->n_dataitems + 1));
	if (! data_profile->ditems) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	data_profile->ditems[data_profile->n_dataitems] = data_item;
	
	for (i = 0; i < data_profile->n_dataitems; i++) {
		NgDataItem ref_ditem = data_profile->ditems[i];
		if (ref_ditem->resq == qref_res) {
			data_item->ref_ditem = ref_ditem;
			break;
		}
	}

	/*
	 * still need to deal with the value
	 */

	data_profile->n_dataitems++;

	return NhlNOERROR;
}

static NgDataProfile NewDataProfile(
	NgGO		go,
	NgDataProfile	dprof_list,
	int		dprof_count,
	NhlString	class_name
        )
{
	int i, j;
	NgDataProfile	dprof,ref_prof = NULL;
	NgDataItem	*ditems = NULL;
	static 	NgDataProfile  dlists[2] = { NULL, NULL };
	NhlBoolean	do_init = True;

	for (i = 0; i < 2; i++) {
		if (dlists[i] == dprof_list) {
			do_init = False;
			break;
		}
	}
	if (do_init) {
		InitializeDataProfile(dprof_list,dprof_count);
		for (i = 0; i < 2; i++) {
			if (dlists[i] == NULL) {
				dlists[i] = dprof_list;
				break;
			}
		}
	}

	if (! class_name)
		ref_prof = &dprof_list[0];
	else {
		for (i = 1; i < dprof_count; i++) {
			if (! strcmp(class_name,dprof_list[i].class_name)) {
				ref_prof = &dprof_list[i];
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
	if (! dprof) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	if (ref_prof->n_dataitems) {
		ditems =  NhlMalloc
			(ref_prof->n_dataitems * sizeof(NgDataItem));
		if (! ditems) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
	}
	memcpy(dprof,ref_prof,sizeof(NgDataProfileRec));

	for (i = 0; i < ref_prof->n_dataitems; i++) {
		NgDataItem sditem = ref_prof->ditems[i];
		ditems[i] = NhlMalloc( sizeof(NgDataItemRec));
		if (! ditems[i]) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
		memcpy(ditems[i],sditem,sizeof(NgDataItemRec));
		ditems[i]->vdata = NgNewVarData();
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
			if (j == ref_prof->n_dataitems) /* not found */
				ditems[i]->ref_ditem = NULL;
		}
	}
	dprof->ditems = ditems;

	return dprof;
}

NgDataProfile NgNewDataProfile(
	NgGO		go,
	NhlString	class_name
        )
{
	return NewDataProfile(go,DataProfs,NhlNumber(DataProfs),class_name);
}

static char *GetHluSuffix(
	NgClassType	class_type
)
{
	switch (class_type) {
	case _NgCONTOURPLOT:
		return "cn";
	case _NgSTREAMLINEPLOT:
		return "sf";
	case _NgVECTORPLOT:
		return "vc";
	case _NgXYPLOT:
		return "xy";
	case _NgCOORDARRAY:
		return "ca";
	case _NgSCALARFIELD:
		return "sf";
	case _NgVECTORFIELD:
		return "vc";
	default:
		return "";
	}
}

NgDataProfile NgMergeDataProfiles(
	NgGO		go,
	NgDataProfile	data_profile,
	NhlString	hluname,
	NhlString	class_name
        )
{
	NgDataProfile	dprof;
	int		i,j,n_dataitems;
	

	dprof = NewDataProfile
		(go,PlotDataProfs,NhlNumber(PlotDataProfs),class_name);
	if (! dprof)
		return data_profile;

	for (i = 0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];

		ditem->qhlu_name = NrmStringToQuark(hluname);
	}
	if (! data_profile) {
		return dprof;
	}

	n_dataitems = data_profile->n_dataitems + dprof->n_dataitems;

	data_profile->ditems = NhlRealloc
		(data_profile->ditems,n_dataitems * sizeof(NgDataItem));

	j = data_profile->n_dataitems;
	for (i = 0; i < dprof->n_dataitems; i++,j++){
		data_profile->ditems[j] = dprof->ditems[i];
		data_profile->ditems[j]->vdata->data_ix = j;
		dprof->ditems[i] = NULL;
	}
	NgFreeDataProfile(dprof);
		 
	data_profile->n_dataitems = n_dataitems;

	return data_profile;
}

/*
 * this still works with PlotDataProfs because the same classes are 
 * supported.
 */
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
	ditems =  NhlMalloc(data_profile->n_dataitems * sizeof(NgDataItem));
	if (! (dprof && ditems)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	memcpy(dprof,data_profile,sizeof(NgDataProfileRec));

	for (i = 0; i < data_profile->n_dataitems; i++) {
		NgVarData frvdata,tovdata;

		ditems[i] = NhlMalloc(sizeof(NgDataItemRec));
		if (! ditems[i]) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
		memcpy(ditems[i],
		       data_profile->ditems[i],sizeof(NgDataItemRec));
		frvdata = data_profile->ditems[i]->vdata;
		tovdata = NgNewVarData();
		NgCopyVarData(tovdata,frvdata);
		ditems[i]->vdata = tovdata;
		ditems[i]->svcb = NULL;
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

	if (data_profile->plotdata_count) {
		dprof->plotdata = NhlMalloc
			(data_profile->plotdata_count * sizeof(NgPlotDataRec));
		memcpy(dprof->plotdata,data_profile->plotdata,
		       data_profile->plotdata_count * sizeof(NgPlotDataRec));
		for (i = 0; i < data_profile->plotdata_count; i++) {
			NgVarData frvdata,tovdata;
			frvdata = data_profile->plotdata[i].vdata;
			tovdata = NgNewVarData();
			NgCopyVarData(tovdata,frvdata);
			dprof->plotdata[i].vdata = tovdata;
		}
	}

	return dprof;
}

/*
 * this routine copies information from one DataProfile to another.
 * the profiles need not be the same type: in this case the copies
 * are based on matching the data item names
 */
NhlErrorTypes  NgTransferDataProfileInfo(
	NgDataProfile to_data_profile,
	NgDataProfile from_data_profile
	)	
{
	char func[] = "NgTransferDataProfileInfo"; 
	NgDataProfile todp = to_data_profile, frdp = from_data_profile;
	int i,j;

	if (! (todp && frdp )) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:one or both data profiles invalid",func));
		return NhlFATAL;
	}
	if (todp == frdp) {
		/* they are actually the same */
		return NhlNOERROR;
	}

	if (todp->type == frdp->type) {
		NgDataItem *toditems = todp->ditems;

		memcpy(todp,frdp,sizeof(NgDataProfileRec));
		todp->ditems = toditems;

		for (i = 0; i < frdp->n_dataitems; i++) {
			NgVarData frvdata,tovdata;
			NgDataItem todi = todp->ditems[i];
			NgDataItem frdi = frdp->ditems[i];
			tovdata = todi->vdata;
			frvdata = frdi->vdata;
			todi->hlu_id = frdi->hlu_id;
#if 0
			/* can't copy the setval cb */
			todi->svcb = NULL;
#endif
			NgCopyVarData(tovdata,frvdata);
		}
		/* update the reference data item pointers */
		for (i = 0; i < frdp->n_dataitems; i++) {
			NgDataItem todi = todp->ditems[i];
			NgDataItem frdi = frdp->ditems[i];
			if (frdi->ref_ditem) {
				for (j = 0; j < frdp->n_dataitems; j++) {
					if (frdi->ref_ditem == 
					    frdp->ditems[j]) {
						todi->ref_ditem = 
							todp->ditems[j];
						break;
					}
				}
			}
		}
		return NhlNOERROR;
	}

	for (i = 0; i < todp->n_dataitems; i++) {
		NgDataItem frdi,todi = todp->ditems[i];
		for (j = 0; j < frdp->n_dataitems; j++) {
			frdi = frdp->ditems[j];
			if (todi->class_type == frdi->class_type &&
			    ! strcmp(todi->name,frdi->name))
				break;
		}
		if (j == frdp->n_dataitems)
			continue;

		todi->hlu_id = frdi->hlu_id;
		NgCopyVarData(todi->vdata,frdi->vdata);
	}
	return NhlNOERROR;
}

void NgFreeDataProfile(
	NgDataProfile data_profile
	)	
{
	int i;

	if (! data_profile)
		return;

	for (i = 0; i < data_profile->n_dataitems; i++) {
		if (! data_profile->ditems[i])
			continue;
		NgFreeVarData(data_profile->ditems[i]->vdata);
		if (data_profile->ditems[i]->svcb)
			NgCBWPDestroy(data_profile->ditems[i]->svcb);
		NgFreeResInfo(data_profile->ditems[i]->res_info);
		NhlFree(data_profile->ditems[i]);
	}
	NhlFree(data_profile->ditems);
	for (i = 0; i < data_profile->plotdata_count; i++) {
		NgFreeVarData(data_profile->plotdata[i].vdata);
	}
	NhlFree(data_profile->plotdata);
	NhlFree(data_profile);

	return;
}

static void SetMasterIndex
(
	NgDataProfile	dprof,
	NgVarData     	vdata
)
{
	char testch[] = { 'u','U','v','V','\0' };
	int i,mix,omix;
	NgDataItem m_ditem, old_m_ditem;
	NhlString vname,fname;

	dprof->master_data_ix = 0;
	if (vdata->data_ix < 0)
		vdata->data_ix = 0;

	switch (dprof->type) {

	default:
		return;
	case _NgVECTORPLOT:
	case _NgVECTORFIELD:
	case _NgSTREAMLINEPLOT:
		break;
	}
	if (! vdata->qvar)
		return;

	vname = NrmQuarkToString(vdata->qvar);
	if (vdata->qfile) 
		fname = vdata->qfile ? NrmQuarkToString(vdata->qfile) : NULL;
	for (mix = -1,i = 0; testch[i] != '\0'; i++) {
		char *cp = strchr(vname,testch[i]);
		if (! cp)
			continue;
		mix = (i < 2) ? 0 : 1;
		omix = dprof->ditems[0]->ref_ditem == NULL ? 0 : 1;
		break;
	}
	if (mix < 0 && fname) {
		for (i = 0; testch[i] != '\0'; i++) {
			char *cp = strchr(fname,testch[i]);
			if (! cp)
				continue;
			mix = (i < 2) ? 0 : 1;
			omix = dprof->ditems[0]->ref_ditem == NULL ? 0 : 1;
			break;
		}
	}
	if (mix < 0)
		return;

	dprof->master_data_ix = mix;
	vdata->data_ix = mix;
	if (mix == omix)
		return;

	m_ditem = dprof->ditems[mix];
	/* 
	 * the master data item is always the 1st or second: whichever
	 * doesn't have a ref_ditem
	 */	
	old_m_ditem = dprof->ditems[omix];

	for (i = 0; i < dprof->n_dataitems; i++) {
		if (i == mix)
			dprof->ditems[i]->ref_ditem = NULL;
		else if (i == omix)
			dprof->ditems[i]->ref_ditem = m_ditem;
		else if (dprof->ditems[i]->ref_ditem == old_m_ditem)
			dprof->ditems[i]->ref_ditem = m_ditem;
	}
	return;
}


/*
 * this struct is used locally to make the dimensional information
 * contained in a variable's shape more accessible for processing.
 */

typedef struct _DimProfileRec {
	int eff_dim_count; /* effective dim count: dims w/ n elements >1 */
	int last_dim;	     /* last dim with n elements > 1 */
	int  dim_sizes[32]; /* actually this contains the dim_size - 1  */
} DimProfileRec, *DimProfile;
	
       
static void
SetDimProfile(
	NgVarData	vdata,
	DimProfile	dim_prof
)
{
	int i;

	dim_prof->eff_dim_count = dim_prof->last_dim = 0;

	for (i = 0; i < vdata->ndims; i++) {
		dim_prof->dim_sizes[i] = 
			(vdata->finish[i]-vdata->start[i])/vdata->stride[i];
		if (dim_prof->dim_sizes[i] > 0) {
			dim_prof->eff_dim_count++;
			dim_prof->last_dim = i;
		}
	}
	return;
}

/*
 * A conforming var matches in number and size of each dimension. If
 * dimensions are named then the names must match as well.
 */
static NhlBoolean ConformingVar
(
	NrmQuark		qfile,
	NrmQuark 		qvar,
	NclApiVarInfoRec	*vinfo	
)
{
	NclApiDataList  *dl;
	NclApiVarInfoRec *tvinfo;
	int i;

	if (qfile > NrmNULLQUARK)
		dl = NclGetFileVarInfo(qfile,qvar);
	else
		dl = NclGetVarInfo(qvar);

	if (! dl)
		return False;

	tvinfo = dl->u.var;

	if (tvinfo->n_dims != vinfo->n_dims) {
		NclFreeDataList(dl);
		return False;
	}

	for (i = 0; i < vinfo->n_dims; i++) {
		if (tvinfo->dim_info[i].dim_quark != 
		    vinfo->dim_info[i].dim_quark ||
		    tvinfo->dim_info[i].dim_size != 
		    vinfo->dim_info[i].dim_size) {
			NclFreeDataList(dl);
			return False;
		}
	}

	NclFreeDataList(dl);
	return True;
}

static NhlBoolean UpdateDependentDataShape
(
	NgDataProfile		dp,
	NgVarData		rvdata,
	NclApiVarInfoRec	*rvinfo,
	DimProfile		dim_prof,
	NgDataItem		ditem
)
{
	NgVarData vdata = ditem->vdata;
	int cflags;

	if (! ConformingVar(vdata->qfile,vdata->qvar,rvinfo))
		return False;

	cflags = vdata->cflags;

	NgSetVarData(NULL,vdata,vdata->qfile,vdata->qvar,NrmNULLQUARK,
		     rvdata->ndims,
		     rvdata->start,rvdata->finish,rvdata->stride,
		     _NgDEFAULT_SHAPE);
	vdata->cflags |= cflags;

	return True;
}
	

static NhlBoolean GetImpliedVectorDataItem
(
	NgDataProfile		dp,
	NgVarData		rvdata,
	NclApiVarInfoRec	*rvinfo,
	DimProfile		dim_prof,
	NgDataItem		ditem
)
{
	char func[] = "GetImpliedVectorDataItem";
	NgVarData vdata = ditem->vdata;
	char vtestch[] = { 'v','V','\0' };
	char utestch[] = { 'u','U','\0' };
	NhlString tfname = NULL, tvname;
	NhlBoolean matched_var = False,matched_file = False;
	NclApiDataList 	*vdl = NULL,*fdl = NULL;
	NclApiDataList *finfo,*vinfo;
	char match_var[256],match_file[256];
	char *cp,*var;
	NrmQuark qvar,qfile;
	int i;

	vdata->qvar = NrmNULLQUARK;

	tvname = NrmQuarkToString(rvdata->qvar);
	if (rvdata->qfile)
		tfname = NrmQuarkToString(rvdata->qfile);

	if (rvdata->data_ix == 0) { /* have u, look for v */
	        for (i = 0; utestch[i] != '\0'; i++) { /* match on varname */
			cp = strchr(tvname,utestch[i]);
			if (! cp)
				continue;
			strcpy(match_var,tvname);
			match_var[cp-tvname] = vtestch[i];
			matched_var = True;
			break;
		}
		if (tfname) {  /* match on filename */
			for (i = 0; utestch[i] != '\0'; i++) { 
				cp = strchr(tfname,utestch[i]);
				if (! cp)
					continue;
				strcpy(match_file,tfname);
				match_file[cp-tfname] = vtestch[i];
				matched_file = True;
				break;
			}
		}
	}
	else if (rvdata->data_ix == 1) { /* have v, look for u */
	        for (i = 0; vtestch[i] != '\0'; i++) { /* match on varname */
			cp = strchr(tvname,vtestch[i]);
			if (! cp)
				continue;
			strcpy(match_var,tvname);
			match_var[cp-tvname] = utestch[i];
			matched_var = True;
			break;
		}
		if (tfname) {  /* match on filename */
			for (i = 0; vtestch[i] != '\0'; i++) { 
				cp = strchr(tfname,vtestch[i]);
				if (! cp)
					continue;
				strcpy(match_file,tfname);
				match_file[cp-tfname] = utestch[i];
				matched_file = True;
				break;
			}
		}
	}
	else {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: invalid data index",func));
		return False;
	}

	if (! (matched_var || matched_file))
		return False;

	/* First see if there is a matching var in the same file */
	if (matched_var && rvdata->qfile) {
		NclApiDataList *finfo = NclGetFileInfo(rvdata->qfile);

		if (! finfo)
			return False;

		for (i = 0; i < finfo->u.file->n_vars; i++) {
			var = NrmQuarkToString
				(finfo->u.file->var_names[i]);
			if (strcmp(var,match_var) ||
			    ! ConformingVar(rvdata->qfile,
					    finfo->u.file->var_names[i],
					    rvinfo))
				continue;
			qfile = rvdata->qfile;
			qvar = finfo->u.file->var_names[i];
                        NclFreeDataList(finfo);
			goto Found;
		}
                NclFreeDataList(finfo);
	}
	/* Then see if there is a matching var in a matching file */
	if (matched_var && matched_file) {
		NhlBoolean found = False;
		fdl = NclGetFileList();
		if (! fdl)
			return False;

		for (finfo = fdl; finfo; finfo = finfo->next) {
			char *file;
			file = NrmQuarkToString(finfo->u.file->name);
			if (! strcmp(file,match_file)) {
				found = True;
				break;
			}
		}
		if (found) {
			for (i = 0; i < finfo->u.file->n_vars; i++) {
				var = NrmQuarkToString
					(finfo->u.file->var_names[i]);
				if (strcmp(var,match_var) ||
				    ! ConformingVar
				    (finfo->u.file->name,
				     finfo->u.file->var_names[i],rvinfo))
					continue;
				qfile = finfo->u.file->name;
				qvar =  finfo->u.file->var_names[i];
				goto Found;
			}
		}
	}
	/* 
	 * if a match can be found on the var name only, look first at
	 * regular vars, then at vars in other files.
	 */
	if (matched_var) {
		vdl = NclGetVarList();
		if (! vdl)
			return False;
		for (vinfo = vdl; vinfo; vinfo = vinfo->next) {
			var = NrmQuarkToString(vinfo->u.var->name);
			if (strcmp(var,match_var) ||
			    ! ConformingVar(NrmNULLQUARK,
					    vinfo->u.var->name,rvinfo))
				continue;
			qfile = NrmNULLQUARK;
			qvar = vinfo->u.var->name;
			goto Found;
		}
		if (! fdl) {
			fdl = NclGetFileList();
			if (! fdl)
				return False;
		}
		for (finfo = fdl; finfo; finfo = finfo->next) {
			for (i = 0; i < finfo->u.file->n_vars; i++) {
				var = NrmQuarkToString
					(finfo->u.file->var_names[i]);
				if (strcmp(var,match_var) ||
				    ! ConformingVar
				    (finfo->u.file->name,
				     finfo->u.file->var_names[i],rvinfo))
					continue;
				qfile = finfo->u.file->name;
				qvar = finfo->u.file->var_names[i];
				goto Found;
			}
		}
	}
	if (fdl)
		NclFreeDataList(fdl);
	if (vdl)
		NclFreeDataList(vdl);
	return False;	

 Found:
	/* 
	 * Since we know this is a conforming variable, the reference 
	 * variable's dimensionality must match this variable and therefore
	 * we can use the shape variables from the reference variable as
	 * a template for this dependent variable.
	 */
	NgSetVarData(NULL,vdata,qfile,qvar,NrmNULLQUARK,rvdata->ndims,
		     rvdata->start,rvdata->finish,rvdata->stride,
		     _NgDEFAULT_VAR);

	if (fdl)
		NclFreeDataList(fdl);
	if (vdl)
		NclFreeDataList(vdl);
	return True;	
}

static NhlBoolean GetOtherCoordArray
(
	NgDataProfile		dp,
	NclApiVarInfoRec	*rvinfo,
	DimProfile		dim_prof,
	NgDataItem		ditem
)
{
	return True;
}

NhlBoolean NgSetDependentVarData		
(
	NgDataProfile	dp,
	int		index,
	NhlBoolean	init
)
{
	DimProfileRec	rdim_prof_rec;
	int		i;
	NgVarData	vdata,rvdata = NULL;
	int		dim;
	long		coord_num;
	NclApiDataList          *rdl = NULL;
	NclApiVarInfoRec	*rvinfo = NULL;	
	int		start,end;

	if (index < 0) {
		start = 0;
		end = dp->n_dataitems;
	}
	else {
		start = index;
		end = index + 1;
	}

/*
 * 'r' prefix stands for 'ref_ditem' info -- i.e. the reference data item.
 * For example, the reference data item for coord variable data items is
 * the main data var item for a plot.
 */
	for (i = start; i < end; i++) {
		if (! dp->ditems[i]->ref_ditem)
			continue;

		if (! rvdata ||
		    rvdata != dp->ditems[i]->ref_ditem->vdata) {
			rvdata = dp->ditems[i]->ref_ditem->vdata;
			if (rvdata->qvar)
				SetDimProfile(rvdata,&rdim_prof_rec);
			if (rdl)
				NclFreeDataList(rdl);
			rdl = NULL;
			rvinfo = NULL;
			if (rvdata->qfile && rvdata->qvar)
				rdl = NclGetFileVarInfo
					(rvdata->qfile,rvdata->qvar);
			else if (rvdata->qvar)	
				rdl = NclGetVarInfo(rvdata->qvar);
			if (rdl)
				rvinfo = rdl->u.var;
		}
		vdata = dp->ditems[i]->vdata;
		switch (dp->ditems[i]->item_type) {
		case  _NgDATAVAR:
			if (! rvdata->qvar)
				continue;
			if (vdata->qvar) {
				UpdateDependentDataShape
					(dp,rvdata,rvinfo,
					 &rdim_prof_rec,dp->ditems[i]);
			}
			else if (dp->ditems[i]->class_type == _NgVECTORFIELD) {
				GetImpliedVectorDataItem
					(dp,rvdata,rvinfo,
					 &rdim_prof_rec,dp->ditems[i]);
			}
			else if (dp->ditems[i]->class_type == _NgCOORDARRAY) {
				GetOtherCoordArray
					(dp,rvinfo,
					 &rdim_prof_rec,dp->ditems[i]);
			}				
			continue;
		case _NgCOORDVAR:
			if (! rvdata->qvar || 
			    vdata->set_state > _NgDEFAULT_SHAPE)
				continue;
			/* 
			 * This initializes a coord variable iff a
			 * coordinate variable is defined for the dimension
			 */
			dim = rdim_prof_rec.last_dim;
			coord_num = (long) dp->ditems[i]->data;
			while (--coord_num) {
				dim--;
				while (dim > -1 &&
				       ! rdim_prof_rec.dim_sizes[dim])
					dim--;
			}
			if (dim < 0)
				continue;
			if (vdata->set_state == _NgDEFAULT_SHAPE) {
				NgSetVarData(NULL,vdata,vdata->qfile,
					     vdata->qvar,vdata->qcoord,1,
					     &rvdata->start[dim],
					     &rvdata->finish[dim],
					     &rvdata->stride[dim],
					     _NgDEFAULT_SHAPE);
			}
			else {
				if (rvinfo->coordnames[dim] <= NrmNULLQUARK)
					continue;
				if (rvdata->qfile) 
					NgSetVarData(NULL,vdata,rvdata->qfile,
						     rvinfo->coordnames[dim],
						     NrmNULLQUARK,1,
						     &rvdata->start[dim],
						     &rvdata->finish[dim],
						     &rvdata->stride[dim],
						     _NgDEFAULT_VAR);
				else 
					NgSetVarData(NULL,vdata,NrmNULLQUARK,
						     rvdata->qvar,
						     rvinfo->coordnames[dim],1,
						     &rvdata->start[dim],
						     &rvdata->finish[dim],
						     &rvdata->stride[dim],
						     _NgDEFAULT_VAR);
			}
			continue;

		case _NgMISSINGVAL:
		case _NgCONFIG:
		case _NgDATAOBJ:
			vdata->cflags = rvdata->cflags;
			continue;
		}
	}
	if (rdl)
		NclFreeDataList(rdl);
	return True;
}	
	

NhlBoolean NgSetDataProfileVar
(
	NgDataProfile	data_profile,
	NgVarData     	vdata,
	NhlBoolean      init_master_ix,
	NhlBoolean	set_dependencies
	)
{
	NgDataProfile 	dp = data_profile;
	DimProfileRec	dim_prof_rec;

	if (init_master_ix)
		SetMasterIndex(dp,vdata);

	if (vdata->data_ix < 0 || vdata->data_ix >= dp->n_dataitems)
		return False;

	SetDimProfile(vdata,&dim_prof_rec);

	if (dp->ditems[vdata->data_ix]->required &&
	    dim_prof_rec.eff_dim_count <
	    dp->ditems[vdata->data_ix]->mindims) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
   "%s as currently shaped contains fewer dimensions than required for %s %s",
			   NrmQuarkToString(vdata->qvar),
			   dp->class_name,dp->ditems[vdata->data_ix]->name));
		return False;
	}
	
	/*
	 * Copy the data if the vdata reference is separate from the 
	 *  data profile.
	 */
	if (vdata != dp->ditems[vdata->data_ix]->vdata) {
		if (! NgCopyVarData(dp->ditems[vdata->data_ix]->vdata,vdata))
			return False;
	}

	if (set_dependencies) {
		return NgSetDependentVarData(dp,-1,init_master_ix);
	}
	
	return True;
}

/*
 * This routine checks for conformance of number of dimensions and dimension
 * size only. It is mainly used to check values set as an expression.
 */

NhlBoolean NgConformingDataItem
(
	NgDataItem	ditem
)
{
	DimProfileRec	ref_dim_prof,dim_prof;
	int i,j;
	int count = 0;

	if (! ditem->ref_ditem) { 
		/* nothing to conform to; however name_conformance is
		   set False if the size_only member is True */
		return True;
		
	}
	SetDimProfile(ditem->vdata,&dim_prof);
	SetDimProfile(ditem->ref_ditem->vdata,&ref_dim_prof);

	if (dim_prof.eff_dim_count < ditem->mindims ||
	    dim_prof.eff_dim_count > ditem->maxdims)
		return False;

	if (ditem->item_type == _NgCOORDVAR) {
		int dim = ref_dim_prof.last_dim;
		long coord_num = (long)ditem->data;
		while (--coord_num) {
			dim--;
			while (dim > -1 && ! ref_dim_prof.dim_sizes[dim])
				dim--;
		}
		if (dim < 0)
			return True; /* nothing to conform to */
		return (ref_dim_prof.dim_sizes[dim] == dim_prof.dim_sizes[0]) ?
			True : False;
	}

	if (! ref_dim_prof.eff_dim_count) /* nothing to compare */
		return True;

	for (i = dim_prof.last_dim,j = ref_dim_prof.last_dim;
	     count < dim_prof.eff_dim_count; ) {
		count++;
		if (dim_prof.dim_sizes[i] != ref_dim_prof.dim_sizes[j])
			return False;
		i--;
		while (i > 0 && ! dim_prof.dim_sizes[i])
			i--;
		j--;
		while (j > 0 && ! dim_prof.dim_sizes[j])
			j--;
	}
	return True;
}
