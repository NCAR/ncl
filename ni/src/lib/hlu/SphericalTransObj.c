/*
 *      $Id: SphericalTransObj.c,v 1.5 2004-03-11 02:00:32 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SphericalTransObj.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 12 12:35:56 MDT 2002
 *
 *	Description:	
 *
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/SphericalTransObjP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/SphericalGeometryP.h>
#include <math.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlSphericalTransObjLayerRec,
			    sptrans.x_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlSphericalTransObjLayerRec,
			    sptrans.y_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL) ,0,(NhlFreeFunc)NhlFreeGenArray }

/* End-documented-resources */
        
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  SpTransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes SpTransInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);


static NhlErrorTypes SpTransDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);


/*
* TransObjClass Methods defined
*/

static NhlErrorTypes SpSetTrans(
#if	NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);


static NhlErrorTypes SpWinToNDC(
#if	NhlNeedProto
NhlLayer	/*instance*/,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/,
int * 	/* status */
#endif
);


static NhlErrorTypes SpNDCToWin(
#if	NhlNeedProto
NhlLayer	/*instance*/,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/,
int *   /* status */
#endif
);


static NhlErrorTypes SpDataToCompc(
#if	NhlNeedProto
NhlLayer   /*instance */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/,
int *   /* status */
#endif
);

static NhlErrorTypes SpCompcToData(
#if	NhlNeedProto
NhlLayer   /*instance */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/,
int *   /* status */
#endif
);

static NhlErrorTypes SpWinToCompc(
#if	NhlNeedProto
NhlLayer   /*instance */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/,
int *   /* status */
#endif
);



static NhlErrorTypes SpNDCLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes SpDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes SpWinLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes SpDataPolygon(
#if     NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* n */
#endif
);

static NhlErrorTypes SetUpTrans(
#if NhlNeedProto
        NhlLayer   /*new*/,
        NhlLayer   /*old*/,
	int	/*c_or_s*/,
	_NhlArgList /*args*/, 
	int /*nargs*/
#endif
);

static NhlBoolean compare_check(
#if NhlNeedProto
	NhlSphericalTransObjLayerPart *spp,
	float	*x,
 	float	*y,
	int	type /* data 0, compc 1 */
#endif
);

static NhlBoolean compare_view(
#if NhlNeedProto
        float *x,
        float *y,
        float xmin,
        float xmax,
        float ymin,
        float ymax
#endif
);

static NhlErrorTypes SpTransGetValues(
#if NhlNeedProto
	NhlLayer /* l */,
	_NhlArgList /*args */,
	int	/*nargs*/
#endif
);

static NhlErrorTypes 	SpTransClassInitialize(
#if NhlNeedProto
	void
#endif
);

#define CREATE  1
#define SET 0

#define NhlspDATA 0
#define NhlspCOMPC 1

static NrmQuark QtrXCoordPoints;
static NrmQuark QtrYCoordPoints;

NhlSphericalTransObjClassRec NhlsphericalTransObjClassRec = {
        {
/* class_name			*/	"sphericalTransformationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlSphericalTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhltransObjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	SpTransClassInitialize,
/* layer_initialize		*/	SpTransInitialize,
/* layer_set_values		*/	SpTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	SpTransGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	SpTransDestroy
        },
        {
/* set_trans		*/	SpSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	SpWinToNDC,
/* ndc_to_win		*/	SpNDCToWin,
/* data_to_win		*/	SpDataToCompc, 
/* win_to_data		*/	SpCompcToData, 
/* data_to_compc	*/	SpDataToCompc,
/* compc_to_data	*/	SpCompcToData,
/* win_to_compc		*/	SpWinToCompc,
/* compc_to_win		*/	SpWinToCompc,
/* data_lineto 		*/      SpDataLineTo,
/* compc_lineto		*/      SpWinLineTo,
/* win_lineto 		*/      SpWinLineTo,
/* NDC_lineto 		*/      SpNDCLineTo,
/* data_polygon		*/      SpDataPolygon

        }
};

NhlClass NhlsphericalTransObjClass =
			(NhlClass)&NhlsphericalTransObjClassRec;


#define INCREASING 0
#define DECREASING 1
#define NONMONOTONIC 2
#define NOSPAN 3


/*
 * Function:	SpTransSetValues
 *
 * Description:	SetValues method for SphericalTrans Objects
 *
 * In Args: 	All standard ones for set_values method.
 *
 * Out Args:	Same as all set_values methods;
 *
 * Return Values: Error status
 *
 * Side Effects: Allocates and copies space for array resources
 */
/*ARGSUSED*/
static NhlErrorTypes SpTransSetValues
#if	NhlNeedProto
(NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args, int num_args)
#else
(old,reference, new,args,num_args)
	NhlLayer old;
	NhlLayer reference;
	NhlLayer new;
	_NhlArgList args;
	int	num_args;
#endif
{

	return(SetUpTrans(new,old,SET,args,num_args));	
}


/*
 * Function:	SpTransInitialize
 *
 * Description: Initialize function for SphericalTransObjs. Performs same
 *		operations as set_values for copying array resources
 *
 * In Args:  	Standard layer_initialize arguments.
 *
 * Out Args:	Standard layer_initialize output.
 *
 * Return Values: Error Status
 *
 * Side Effects: allocates space and copies valus of array resources.
 */
/*ARGSUSED*/
static NhlErrorTypes SpTransInitialize
#if	NhlNeedProto
( NhlClass class, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        NhlClass	class;
        NhlLayer		req;
        NhlLayer		new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	return(SetUpTrans(new,NULL,CREATE,args,num_args));
}

/*
 * Function:	SpTransDestroy
 *
 * Description:
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes SpTransDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlSphericalTransObjLayer sp = 
		(NhlSphericalTransObjLayer)inst;

	NhlFreeGenArray(sp->sptrans.x_sph_ga);
	NhlFreeGenArray(sp->sptrans.y_sph_ga);

	if (sp->sptrans.ixmin)
		NhlFree(sp->sptrans.ixmin);
	if (sp->sptrans.ixmax)
		NhlFree(sp->sptrans.ixmax);
	if (sp->sptrans.iymin)
		NhlFree(sp->sptrans.iymin);
	if (sp->sptrans.iymax)
		NhlFree(sp->sptrans.iymax);
	if (sp->sptrans.llcs)
		NhlFree(sp->sptrans.llcs);

 	free(sp->sptrans.xmin_dat);
	free(sp->sptrans.xmax_dat);
	free(sp->sptrans.ymin_dat);
	free(sp->sptrans.ymax_dat);
 	free(sp->sptrans.compc_xmin_dat);
	free(sp->sptrans.compc_xmax_dat);
	free(sp->sptrans.compc_ymin_dat);
	free(sp->sptrans.compc_ymax_dat);

	return NhlNOERROR;
}

#define MIN4(a,b,c,d) MIN(MIN((a),(b)),MIN((c),(d)))
#define MAX4(a,b,c,d) MAX(MAX((a),(b)),MAX((c),(d)))
#define DEGTORAD 0.017453292519943
#define RADTODEG 57.2957795130823

static void SetUpLLCosSinArray
#if	NhlNeedProto
(
	NhlSphericalTransObjLayerPart *spp
)
#else
(spp)
	NhlSphericalTransObjLayerPart *spp;
#endif
{
	int i,j;
	float *xp,*yp;
	xp = (float *) spp->x_sph_ga->data;
	yp = (float *) spp->y_sph_ga->data;

	for (j = 0; j < spp->yaxis_size; j++) {
		for (i = 0; i < spp->xaxis_size; i++) {
			int off = i + j * spp->xaxis_size;
			LLCosSin *llcs = spp->llcs + off;
			llcs->latcos = cos(DEGTORAD * (double) *(yp + off));
			llcs->latsin = sin(DEGTORAD * (double) *(yp + off));
			llcs->loncos = cos(DEGTORAD * (double) *(xp + off));
			llcs->lonsin = sin(DEGTORAD * (double) *(xp + off));
		}
	}
}

static NhlErrorTypes SetUpTrans
#if	NhlNeedProto
(NhlLayer new, NhlLayer old, int c_or_s,_NhlArgList args, int nargs)
#else
(new,old,c_or_s,args, nargs)
	NhlLayer 	new;
	NhlLayer	old;
	int c_or_s;
	_NhlArgList args;
	int nargs;
#endif
{
	NhlSphericalTransObjLayer inew = (NhlSphericalTransObjLayer)new;
	NhlSphericalTransObjLayer iold = (NhlSphericalTransObjLayer)old;
	NhlSphericalTransObjLayerPart *spp = &inew->sptrans;
	NhlTransObjLayerPart	*tp = &inew->trobj;
	NhlTransObjLayerPart	*otp = &iold->trobj;
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;
	NhlBoolean new_coords = False;
        NhlBoolean x_sph_coords_set = False,y_sph_coords_set = False;
        NhlBoolean data_extent_def;
        NhlBoolean new_data_extent;
	int old_xaxis_size,old_yaxis_size;
	
	tp->change_count++;
	
        data_extent_def = (tp->data_xstart == 0.0 && tp->data_xend == 0.0)
		|| (tp->data_ystart == 0.0 && tp->data_yend == 0.0) ?
                False : True;
	
	if(c_or_s == SET) {

		error_lead = "SpTransSetValues";

		if (_NhlArgIsSet(args,nargs,NhlNtrXCoordPoints))
                        x_sph_coords_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrYCoordPoints))
                        y_sph_coords_set = True;
		if (x_sph_coords_set || y_sph_coords_set)
			new_coords = True;
                if (! data_extent_def &&
                    (tp->x_min != otp->x_min ||
                     tp->x_max != otp->x_max ||
		     tp->y_min != otp->y_min ||
                     tp->y_max != otp->y_max))
			new_data_extent = True;
		else {
			new_data_extent =
				tp->data_xstart != otp->data_xstart ||
				tp->data_xend != otp->data_xend ||
				tp->data_ystart != otp->data_ystart ||
				tp->data_yend != otp->data_yend;
		}
		old_xaxis_size = spp->xaxis_size;
		old_yaxis_size = spp->yaxis_size;
	}
	else {
		error_lead = "SpTransInitialize";
		new_coords = True;
                new_data_extent = True;
                if (spp->x_coord_points_ga)
                       x_sph_coords_set = True; 
                if (spp->y_coord_points_ga)
                       y_sph_coords_set = True; 
		if (! (x_sph_coords_set && y_sph_coords_set)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: Both %s and %s must be set",
			  error_lead,NhlNtrXCoordPoints,NhlNtrYCoordPoints);
			return NhlFATAL;
		}
		spp->x_sph_ga = NULL;
		spp->y_sph_ga = NULL; 
                spp->x_sph_min = 0.0;
                spp->x_sph_max = 0.0;
                spp->y_sph_min = 0.0;
                spp->y_sph_max = 0.0;
                spp->xaxis_size = 0;
                spp->yaxis_size = 0;

                spp->ul = spp->ub = 0.0;
                spp->ur = spp->ut = 1.0;
		spp->ixmin = spp->ixmax = NULL;
		spp->iymin = spp->iymax = NULL;
		old_xaxis_size = -1;
		old_yaxis_size = -1;
		spp->llcs = NULL;
	}

	if (data_extent_def) {
		spp->x_sph_min = MIN(tp->data_xstart,tp->data_xend);
		spp->x_sph_max = MAX(tp->data_xstart,tp->data_xend);
		spp->y_sph_min = MIN(tp->data_ystart,tp->data_yend);
		spp->y_sph_max = MAX(tp->data_ystart,tp->data_yend);
	}
	if (x_sph_coords_set) {
		if (spp->x_sph_ga)
			NhlFreeGenArray(spp->x_sph_ga);
		spp->x_sph_ga = 
			_NhlCopyGenArray(spp->x_coord_points_ga,True);
		if (! spp->x_sph_ga) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		spp->x_coord_points_ga = NULL;
	}
	if (y_sph_coords_set) {
		if (spp->y_sph_ga)
			NhlFreeGenArray(spp->y_sph_ga);
		spp->y_sph_ga = 
			_NhlCopyGenArray(spp->y_coord_points_ga,True);
		if (! spp->y_sph_ga) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		spp->y_coord_points_ga = NULL;
	}
	if (spp->y_sph_ga->len_dimensions[1] != 
	    spp->x_sph_ga->len_dimensions[1] ||
	    spp->x_sph_ga->len_dimensions[0] != 
	    spp->x_sph_ga->len_dimensions[0]) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: %s and %s must have equal dimension lengths",
			  error_lead,NhlNtrXCoordPoints,NhlNtrYCoordPoints);
		return NhlFATAL;
	}
		

	if (tp->x_min < spp->x_sph_min) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: X minimum less than minimum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->x_min = spp->x_sph_min;
	}
	if (tp->x_max > spp->x_sph_max) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: X maximum greater than maximum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->x_max = spp->x_sph_max;
	}
	if (tp->y_min < spp->y_sph_min) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Y minimum less than minimum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->y_min = spp->y_sph_min;
	}
	if (tp->y_max > spp->y_sph_max) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Y maximum greater than maximum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->y_max = spp->y_sph_max;
	}

	spp->ul = 0;
	spp->ur = spp->x_sph_ga->len_dimensions[1] - 1;
	spp->ub = 0;
	spp->ut = spp->x_sph_ga->len_dimensions[0] - 1;
	spp->xaxis_size = spp->x_sph_ga->len_dimensions[1];
	spp->yaxis_size = spp->x_sph_ga->len_dimensions[0];

        
	if(tp->x_reverse) {
		float tmpf = spp->ur;
		spp->ur = spp->ul;
		spp->ul = tmpf;
	}
	if(tp->y_reverse) {
		float tmpf = spp->ut;
		spp->ut = spp->ub;
		spp->ub = tmpf;
	}
	spp->compc_x_min = MIN(spp->ul,spp->ur);
	spp->compc_x_max = MAX(spp->ul,spp->ur);
	spp->compc_y_min = MIN(spp->ut,spp->ub);
	spp->compc_y_max = MAX(spp->ut,spp->ub);
	spp->ixb = spp->compc_x_min;
	spp->ixe = spp->compc_x_max;
	spp->iyb = spp->compc_y_min;
	spp->iye = spp->compc_y_max;
	

	spp->log_lin_value = 1;
	spp->x_min = tp->x_min;
	spp->y_min = tp->y_min;
	spp->x_max = tp->x_max;
	spp->y_max = tp->y_max;
	spp->x_reverse = tp->x_reverse;
	spp->y_reverse = tp->y_reverse;

	if (new_coords || new_data_extent) {
		int size = spp->xaxis_size * spp->yaxis_size;

		if (spp->xaxis_size != old_xaxis_size || 
		    spp->yaxis_size != old_yaxis_size) {
			if (spp->llcs)
				NhlFree(spp->llcs);
			spp->llcs = NhlMalloc(sizeof(LLCosSin) * size);
			if (! spp->llcs) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
		}
		SetUpLLCosSinArray(spp);
	}


	if (c_or_s == CREATE) {
		if ((spp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((spp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((spp->ymin_dat =_NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((spp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((spp->compc_xmin_dat =
		     _NhlCmpFSetup(spp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_xmin_dat = spp->compc_xmin_dat;
		if ((spp->compc_xmax_dat = 
		     _NhlCmpFSetup(spp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_xmax_dat = spp->compc_xmax_dat;
		if ((spp->compc_ymin_dat = 
		     _NhlCmpFSetup(spp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_ymin_dat = spp->compc_ymin_dat;
		if ((spp->compc_ymax_dat = 
		     _NhlCmpFSetup(spp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_ymax_dat = spp->compc_ymax_dat;

                tp->x_min_set = tp->x_max_set = False;
                tp->x_reverse_set = False;
                tp->y_min_set = tp->y_max_set = False;
                tp->y_reverse_set = False;
        
		return(ret);
	}
        if (tp->x_min != iold->sptrans.x_min) {
                free(spp->xmin_dat);
                if ((spp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->x_max != iold->sptrans.x_max) {
		free(spp->xmax_dat);
		if ((spp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_min != iold->sptrans.y_min) {
		free(spp->ymin_dat);
		if ((spp->ymin_dat = _NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_max != iold->sptrans.y_max) {
		free(spp->ymax_dat);
		if ((spp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (spp->compc_x_min != iold->sptrans.compc_x_min) {
		free(spp->compc_xmin_dat);
		if ((spp->compc_xmin_dat =
		     _NhlCmpFSetup(spp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_xmin_dat = spp->compc_xmin_dat;
	}
	if (spp->compc_x_max != iold->sptrans.compc_x_max) {
		free(spp->compc_xmax_dat);
		if ((spp->compc_xmax_dat = 
		     _NhlCmpFSetup(spp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_xmax_dat = spp->compc_xmax_dat;
	}
	if (spp->compc_y_min != iold->sptrans.compc_y_min) {
		free(spp->compc_ymin_dat);
		if ((spp->compc_ymin_dat = 
		     _NhlCmpFSetup(spp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_ymin_dat = spp->compc_ymin_dat;
	}
	if (spp->compc_y_max != iold->sptrans.compc_y_max) {
		free(spp->compc_ymax_dat);
		if ((spp->compc_ymax_dat = 
		     _NhlCmpFSetup(spp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                spp->save_compc_ymax_dat = spp->compc_ymax_dat;
	}
        
        tp->x_min_set = tp->x_max_set = False;
        tp->x_reverse_set = False;
        tp->y_min_set = tp->y_max_set = False;
        tp->y_reverse_set = False;
        
	return(ret);

}

/*
 * Function:	SpSetTrans
 *
 * Description: set_trans method for SphericalTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the SphericalTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes SpSetTrans
#if	NhlNeedProto
(
	NhlLayer	tobj,
	NhlLayer	vobj
) 
#else
(tobj,vobj)
	NhlLayer	tobj;
	NhlLayer	vobj;
#endif
{
	NhlSphericalTransObjLayer	to=(NhlSphericalTransObjLayer)tobj;
	NhlTransObjLayerPart		*top = &to->trobj;
	NhlSphericalTransObjLayerPart	*tp = &to->sptrans;
	NhlErrorTypes ret;

	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	return(_NhlTransLLUSet(top->x,top->x+top->width,
			       top->y-top->height,top->y,
			       tp->ul,tp->ur,tp->ub,tp->ut,
			       tp->log_lin_value,
                               &top->off_screen,
                               "SpSetTrans"));

}

/*
 * Function:	compare_check
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlBoolean compare_check
#if	NhlNeedProto
(
	NhlSphericalTransObjLayerPart *spp,
	float	*x,
 	float	*y,
	int	type /* data 0, compc 1 */
)
#else
(spp,x,y,type)
	NhlSphericalTransObjLayerPart *spp;
	float	*x;
	float	*y;
	int	type;
#endif
{
	int xmndif,xmxdif,ymndif,ymxdif;

	if (type == NhlspDATA) {
		if ((xmndif = _NhlCmpF(*x,spp->xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,spp->xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,spp->ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,spp->ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = spp->x_min;
		}
		else if (xmxdif == 0) {
			*x = spp->x_max;
		}
		if (ymndif == 0) {
			*y = spp->y_min;
		}
		else if (ymxdif == 0) {
			*y = spp->y_max;
		}
	}
	else {
		if ((xmndif = _NhlCmpF(*x,spp->compc_xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,spp->compc_xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,spp->compc_ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,spp->compc_ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = spp->compc_x_min;
		}
		else if (xmxdif == 0) {
			*x = spp->compc_x_max;
		}
		if (ymndif == 0) {
			*y = spp->compc_y_min;
		}
		else if (ymxdif == 0) {
			*y = spp->compc_y_max;
		}
	}
	return True;
}

/*
 * Function:    compare_view
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlBoolean compare_view
#if	NhlNeedProto
(
        float *x,
        float *y,
        float xmin,
        float xmax,
        float ymin,
        float ymax
)
#else
(x,y,xmin,xmax,ymin,ymax)
        float *x;
        float *y;
        float xmin;
        float xmax;
        float ymin;
        float ymax;
#endif
{
        int xmndif,xmxdif,ymndif,ymxdif;

        if ((xmndif = _NhlCmpFAny2(*x,xmin,5,_NhlMIN_NONZERO)) < 0 ||
            (xmxdif = _NhlCmpFAny2(*x,xmax,5,_NhlMIN_NONZERO)) > 0 ||
            (ymndif = _NhlCmpFAny2(*y,ymin,5,_NhlMIN_NONZERO)) < 0 ||
            (ymxdif = _NhlCmpFAny2(*y,ymax,5,_NhlMIN_NONZERO)) > 0) {
                return False;
        }

        if (xmndif == 0) {
                *x = xmin;
        }
        else if (xmxdif == 0) {
                *x = xmax;
        }
        if (ymndif == 0) {
                *y = ymin;
        }
        else if (ymxdif == 0) {
                *y = ymax;
        }
        return True;
}


/*
 * Function:	SpWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the SphericalTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes SpWinToNDC
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float* xmissing,float* ymissing,int *status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer   instance;
	float   *x;
	float   *y;
	int	n;
	float*  xout;
	float*  yout;
	float*  xmissing;
	float*	ymissing;
	int * status;
#endif
{
	NhlSphericalTransObjLayer	iinstance =
				(NhlSphericalTransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;
	int i;
	
	*status = 0;	
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n ; i++) {
/*
* Compc and Window are identical coordinates in this object
*/
			if((x[i] > iinstance->sptrans.compc_x_max)
			   ||(x[i] < iinstance->sptrans.compc_x_min)
			   ||(y[i] > iinstance->sptrans.compc_y_max)
			   ||(y[i] < iinstance->sptrans.compc_y_min)) {
				if (! compare_check(&iinstance->sptrans,
						    &x[i],&y[i],NhlspCOMPC)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}

			} 
			strans(iinstance->sptrans.ul,
			       iinstance->sptrans.ur,
			       iinstance->sptrans.ub,
			       iinstance->sptrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n ; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
			   ||((ymissing != NULL)&&(*ymissing == y[i]))
			   ||(x[i] > iinstance->sptrans.compc_x_max)
			   ||(x[i] < iinstance->sptrans.compc_x_min)
			   ||(y[i] > iinstance->sptrans.compc_y_max)
			   ||(y[i] < iinstance->sptrans.compc_y_min)) {
				if (! compare_check(&iinstance->sptrans,
						    &x[i],&y[i],NhlspCOMPC)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}
			}
			strans(iinstance->sptrans.ul,
			       iinstance->sptrans.ur,
			       iinstance->sptrans.ub,	
			       iinstance->sptrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	}

	return(NhlNOERROR);
}


/*
 * Function:	SpNDCToWin
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes SpNDCToWin
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float * xmissing, float *ymissing,int *status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer   instance;
	float   *x;
	float   *y;
	int	n;
	float*  xout;
	float*  yout;
	float*	xmissing;
	float*	ymissing;
	int* status;
#endif
{
	float x1;
	float y1;
	int i;
	NhlSphericalTransObjLayer	iinstance = 
				(NhlSphericalTransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;

	*status = 0;
	x1 = tp->x + tp->width;
	y1 = tp->y - tp->height;
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n; i++) {
			if((x[i] > x1)
			   ||(x[i] < tp->x)
			   ||(y[i] > tp->y)
			   ||(y[i] < y1)) {
				
				if (! compare_view(&x[i],&y[i],
						   tp->x,x1,tp->y,y1)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}
			}
			strans(tp->x,x1,y1,tp->y,iinstance->sptrans.ul,
			       iinstance->sptrans.ur, iinstance->sptrans.ub,
			       iinstance->sptrans.ut, x[i],y[i],
			       &(xout[i]),&(yout[i]));
		}
	} 
	else {
		for(i = 0; i< n; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
			   ||((ymissing != NULL)&&(*ymissing == y[i]))
			   ||(x[i] > x1)
			   ||(x[i] < tp->x)
			   ||(y[i] > tp->y)
			   ||(y[i] < y1)) {
				if (! compare_view(&x[i],&y[i],
						   tp->x,x1,tp->y,y1)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}

			}
			strans(tp->x,x1,y1,tp->y,
			       iinstance->sptrans.ul,
			       iinstance->sptrans.ur, 
			       iinstance->sptrans.ub,
			       iinstance->sptrans.ut, x[i],y[i],
			       &(xout[i]),&(yout[i]));
		}
	}
	return(NhlNOERROR);
}

/*
 * this returns a negative value if point (xp,yp) is left of 
 * line (x0,y0),(x1,y1)
 */

#define RIGHTOF(xp,yp,x0,y0,x1,y1) \
 	((x0)-(xp))*((yp)-(y1))-((x1)-(xp))*((yp)-(y0))
#define RIGHTOFD(xp,yp,x0,y0,x1,y1) \
 	(double)((x0)-(xp))*(double)((yp)-(y1))-\
	(double)((x1)-(xp))*(double)((yp)-(y0))


/*
 * Function:	IsNearby
 *
 * Description: return True if the point (x,y) is within 2 grid boxes
 * of the 'current' box location
 *		
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlBoolean IsNearby
#if	NhlNeedProto
(
	NhlSphericalTransObjLayerPart *spp,
	double *sc
)
#else
(spp,sc)
	NhlSphericalTransObjLayerPart *spp;
	double *sc;
#endif
{
	int icdp;

	if (! (spp->ixe == spp->ixb+1 && spp->iye == spp->iyb+1))
		return False;

	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)        
		return True; /* same box */
	spp->ixb = MAX(0,spp->ixb-1);
	spp->ixe = MIN(spp->xaxis_size-1,spp->ixe+1);
	spp->iyb = MAX(0,spp->iyb-1);
	spp->iye = MIN(spp->yaxis_size-1,spp->iye+1);
	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)       
		return True; /* adjacent box */
	spp->ixb = MAX(0,spp->ixb-1);
	spp->ixe = MIN(spp->xaxis_size-1,spp->ixe+1);
	spp->iyb = MAX(0,spp->iyb-1);
	spp->iye = MIN(spp->yaxis_size-1,spp->iye+1);
	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)
		return True; /* near-by box */

	return False;
}

/*
 * Function:	SetQuadrant
 *
 * Description: sets the current grid bounding indexes to the grid
 * quadrant in which the point (x,y) lies. 
 *		
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values: returns True if the point (x,y) can be located in
 *  a grid quandrant, False otherwise.
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlBoolean SetQuadrant
#if	NhlNeedProto
(
	NhlSphericalTransObjLayerPart *spp,
	double *sc
)
#else
(spp,sc)
	NhlSphericalTransObjLayerPart *spp;
	double *sc;
#endif
{
	int icdp;

 	/* lower left quadrant */
	spp->ixb = 0;
	spp->ixe = (int) (spp->xaxis_size / 2.0);
	spp->iyb = 0;
	spp->iye = (int) (spp->yaxis_size / 2.0);
	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)        
		return True;

	/* lower right quadrant */
	spp->ixb = (int) (spp->xaxis_size / 2.0);
	spp->ixe = spp->xaxis_size - 1;
	spp->iyb = 0;
	spp->iye = (int) (spp->yaxis_size / 2.0);
	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)        
		return True;

 	/* upper left quadrant */
	spp->ixb = 0;
	spp->ixe = (int) (spp->xaxis_size / 2.0);
	spp->iyb = (int) (spp->yaxis_size / 2.0);
	spp->iye = spp->yaxis_size - 1;
	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)        
		return True;

 	/* upper right quadrant */
	spp->ixb = (int) (spp->xaxis_size / 2.0);
	spp->ixe = spp->xaxis_size - 1;
	spp->iyb = (int) (spp->yaxis_size / 2.0);
	spp->iye = spp->yaxis_size - 1;
	icdp = icegdp(sc,(double *)spp->llcs,spp->xaxis_size,spp->yaxis_size,
		       spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp == 0)        
		return True;

	return False;
}

#define EPS 1.0e-8

/*
 * Function:	GetFracCoords
 *
 * Description: Once it is known what compc grid box a data point lies,
 *              this routine determines the fractional component within
 *              the grid box, and sets the final data values.
 *              Note this routine assumes that 
 *              (ixe == ixb+1 && iye == iyb+1)
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values: False if out of range; True otherwise;
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlBoolean GetFracCoords
#if	NhlNeedProto
(
	NhlSphericalTransObjLayerPart *spp,
	double *sc,
	float  oor,
        float*  xout,
        float*  yout
)
#else
(spp,x,y,xout,yout)
	NhlSphericalTransObjLayerPart *spp;
	double *sc;
	float  oor;
        float*  xout;
        float*  yout;

#endif
{
	double icdp;
	int mid;
	double xf,yf;
	LLCosSin *llcs = spp->llcs;
	int xsz = spp->xaxis_size;

	while (1) {
		if (spp->ixe - spp->ixb >= spp->iye - spp->iyb) {
			if (spp->ixe - spp->ixb == 1) {
				break;
			}
			mid = (spp->ixb + spp->ixe) / 2;
			icdp = icegdp(sc,(double *)spp->llcs,
				       spp->xaxis_size,spp->yaxis_size,
				       spp->ixb,mid,spp->iyb,spp->iye);
			if (icdp != 0)        
				spp->ixb = mid;
			else
				spp->ixe = mid;
		}
		else {
			mid = (spp->iyb + spp->iye) / 2;
			icdp = icegdp(sc,(double *)spp->llcs,
				       spp->xaxis_size,spp->yaxis_size,
				       spp->ixb,spp->ixe,spp->iyb,mid);
			if (icdp != 0)        
				spp->iyb = mid;
			else
				spp->iye = mid;
		}
	}
/*
	icdp = icegdp(sc,(double *)spp->llcs,
		      spp->xaxis_size,spp->yaxis_size,
		      spp->ixb,spp->ixe,spp->iyb,spp->iye);
	if (icdp != 0) {
		*xout = *yout = oor;
		return False;
	}
*/

	fpiqdp(&((llcs+xsz*spp->iyb+spp->ixb)->latcos),
	       &((llcs+xsz*spp->iyb+spp->ixe)->latcos),
	       &((llcs+xsz*spp->iye+spp->ixb)->latcos),
	       &((llcs+xsz*spp->iye+spp->ixe)->latcos),
	       sc,&xf,&yf);

	if (xf < 0.0) {
		*xout = *yout = oor;
		return False;
	}
	*xout = spp->ixb + xf;
	*yout = spp->iyb + yf;

	return True;
}


/*
 * Function:	SpDataToCompc
 *
 * Description: Transforms data coordinates into the computation coordinate
 *		system
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SpDataToCompc
#if	NhlNeedProto
(
	NhlLayer instance,
	float *x,
	float *y,
	int n,
	float* xout,
	float* yout,
	float *xmissing,
	float *ymissing, 
	int *status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*  xmissing;
	float*	ymissing;
	int * status;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlSphericalTransObjLayer iinstance =
                (NhlSphericalTransObjLayer)instance;
	NhlSphericalTransObjLayerPart *spp = &iinstance->sptrans;
	int i;
	double sc[4];

	*status = 0;
	for(i=0; i< n;i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
#if 0
/* will this work? */
			||(x[i] < spp->x_min)	
			||(x[i] > spp->x_max)
#endif
			||(y[i] < spp->y_min)
			||(y[i] > spp->y_max)) {
		
			if (! compare_check(spp,&x[i],&y[i],NhlspDATA)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		sc[0] = cos(DEGTORAD * (double)y[i]);
		sc[1] = sin(DEGTORAD * (double)y[i]);
		sc[2] = cos(DEGTORAD * (double)x[i]);
		sc[3] = sin(DEGTORAD * (double)x[i]);
		
		if (! IsNearby(spp,sc)) {
			if (! SetQuadrant(spp,sc)) {
				*status = 1;
				xout[i]= yout[i] = 
					iinstance->trobj.out_of_range;
				continue;	
			}
		}
		if (! GetFracCoords(spp,sc,iinstance->trobj.out_of_range,
				    &(xout[i]),&(yout[i]))) {
			*status = 1;
		}
	}
	return(ret);
}

/*
 * Function:	SpCompcToData
 *
 * Description:	transforms computational coordinates into data coordinates.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SpCompcToData
#if	NhlNeedProto
(NhlLayer instance,
 float *x,
 float *y,
 int n,
 float* xout,
 float* yout,
 float *xmissing,
 float *ymissing,
 int* status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*	xmissing;
	float*	ymissing;
	int *   status;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlSphericalTransObjLayer iinstance = 
		(NhlSphericalTransObjLayer)instance;
	NhlSphericalTransObjLayerPart *spp = &iinstance->sptrans;
	int i;
	int ix,iy;
	int xsz = spp->xaxis_size;
	LLCosSin *llcs = spp->llcs;
	double xf,yf;
	double out[4],dlon,dlat;

	*status = 0;
	for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] > spp->compc_x_max)
			||(x[i] < spp->compc_x_min)
			||(y[i] > spp->compc_y_max)
			||(y[i] < spp->compc_y_min)) {

			if (! compare_check(spp,&x[i],&y[i],NhlspCOMPC)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		ix = MAX(0,MIN(spp->xaxis_size-2,(int)x[i]));
		iy = MAX(0,MIN(spp->yaxis_size-2,(int)y[i]));
		xf = (double)x[i] - (double)ix;
		yf = (double)y[i] - (double)iy;
		ipiqdp(&((llcs+xsz*iy+ix)->latcos),
		       &((llcs+xsz*iy+ix+1)->latcos),
		       &((llcs+xsz*(iy+1)+ix)->latcos),
		       &((llcs+xsz*(iy+1)+ix+1)->latcos),
		       &xf,&yf,out);

		dlat = RADTODEG * asin(out[1]);
		if (out[2] == 0.0 && out[3] == 0.0) {
			dlon = 0.0;
		}
		else {
			dlon = RADTODEG * atan2(out[3],out[2]);
		}
		yout[i] = (float) dlat;
		if (dlon < spp->x_sph_min)
			xout[i] = (float) (dlon + 360.0);
		else if (dlon > spp->x_sph_max)
			xout[i] = (float) (dlon - 360.0);
		else
			xout[i] = (float) dlon;
	}
	return(ret);
}

/*ARGSUSED*/
static NhlErrorTypes SpWinToCompc
#if	NhlNeedProto
(NhlLayer instance, float* x,float* y,int n,float* xout,float* yout,float* xmissing,float* ymissing,int* status)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
NhlLayer instance;
float* x;
float* y;
int n;
float* xout;
float* yout;
float* xmissing;
float* ymissing;
int* status;
#endif
{
        NhlErrorTypes ret = NhlNOERROR;
        NhlSphericalTransObjLayer iinstance = (NhlSphericalTransObjLayer)instance;
        int i;

        *status = 0;
        for(i = 0 ; i< n; i++) {
                if(((xmissing != NULL)&&(*xmissing == x[i]))
                        || ((ymissing != NULL)&&(*ymissing == y[i]))
                        ||(x[i] < iinstance->sptrans.compc_x_min)
                        ||(x[i] > iinstance->sptrans.compc_x_max)
                        ||(y[i] < iinstance->sptrans.compc_y_min)
                        ||(y[i] > iinstance->sptrans.compc_y_max)) {

			if (! compare_check(&iinstance->sptrans,
					    &x[i],&y[i],NhlspCOMPC)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		yout[i] = y[i];
		xout[i] = x[i];
        }
        return(ret);
}

static NhlErrorTypes AdjustToEdge
#if	NhlNeedProto
(NhlSphericalTransObjLayer spinst, 
float xclip, 
float yclip, 
float x,
float y, 
float *xd, 
float *yd,
float *xc, 
float *yc
)
#else
(spinst,xclip,yclip,x,y,xd, yd,xc,yc)
NhlSphericalTransObjLayer spinst;
float xclip;
float yclip; 
float x;
float y; 
float *xd; 
float *yd;
float *xc; 
float *yc;
#endif
{
	NhlSphericalTransObjLayerPart *spp = 
		(NhlSphericalTransObjLayerPart *) &spinst->sptrans;
	float xt,yt;
	int i,status = 1;

	xt = xclip;
	yt = yclip;

	for (i=0; i < 2; i++) {

		if (x != xclip) {
			if (_NhlCmpF(xt,spp->xmin_dat) < 0.0) {
				*xd = spp->x_min;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
			else if (_NhlCmpF(xt,spp->xmax_dat) > 0.0) {
				*xd = spp->x_max;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
		}
		if (y != yclip) {
			if (_NhlCmpF(yt,spp->ymin_dat) < 0.0) {
				*yd = spp->y_min;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
			else if (_NhlCmpF(yt,spp->ymax_dat) > 0.0) {
				*yd = spp->y_max;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
		}
		SpDataToCompc((NhlLayer)spinst,xd,yd,1,
				      xc,yc,NULL,NULL,&status);
		if (status) {
			xt = *xd;
			yt = *yd;
		}
	}
	if (status) 
		return NhlWARNING;

	return NhlNOERROR;
}


static NhlErrorTypes SpDataLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown )
#else
(instance, x, y, upordown )
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
	NhlSphericalTransObjLayer spinst = (NhlSphericalTransObjLayer)instance;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	float holdx,holdy;
	int status;
	int i,npoints = 256;
	float xdist,ydist,xc,yc,xd,yd;

	npoints = spinst->trobj.point_count;
/*
* if true the moveto is being performed
*/
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd = 1;
		return(NhlNOERROR);
	} 
/*
 * first find out whether the line is clipped
 */
	currentx = x;
	currenty = y;
	holdx = lastx;
	holdy = lasty;
	_NhlTransClipLine(spinst->sptrans.x_min,
			  spinst->sptrans.x_max,
			  spinst->sptrans.y_min,
			  spinst->sptrans.y_max,
			  &lastx,
			  &lasty,
			  &currentx,
			  &currenty,
			  spinst->trobj.out_of_range);

	if((lastx == spinst->trobj.out_of_range)
	   ||(lasty == spinst->trobj.out_of_range)
	   ||(currentx == spinst->trobj.out_of_range)
	   ||(currenty == spinst->trobj.out_of_range)){
/*
 * Line has gone completely out of window
 */
		lastx = x;	
		lasty = y;
		call_frstd = 1;
		return(NhlNOERROR);
	}
/*
 * sample the line and draw
 */ 
	xpoints[0] = lastx;
	xpoints[1] = currentx;
	ypoints[0] = lasty;
	ypoints[1] = currenty;
/*
 * Use the linear clipped length to determine the number of points to use
 */
	SpDataToCompc(instance,xpoints,ypoints,2,
		      xpoints,ypoints,NULL,NULL,&status);

	xdist = c_cufx(xpoints[1]) - c_cufx(xpoints[0]);
	ydist = c_cufy(ypoints[1]) - c_cufy(ypoints[0]);
	npoints = (int) ((float)npoints * (fabs(xdist)+fabs(ydist)));
	npoints = npoints < 1 ? 1 : npoints;

/*
 * If not clipped things are simpler
 */
	if (lastx == holdx && currentx == x && 
	    lasty == holdy && currenty == y) {
		if (call_frstd == 1) {
			_NhlWorkstationLineTo(spinst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
			call_frstd = 2;
		}
		xdist = currentx - lastx;
		ydist = currenty - lasty;
		for (i = 0; i<npoints; i++) {
			xd = lastx + xdist *(i+1)/ (float)npoints;
			yd = lasty + ydist *(i+1)/ (float)npoints;
			SpDataToCompc(instance,&xd,&yd,1,
				      &xc,&yc,NULL,NULL,&status);
			if (! status)
				_NhlWorkstationLineTo(spinst->trobj.wkptr,
						      c_cufx(xc),c_cufy(yc),0);
		}
		lastx = x;
		lasty = y;
		return(NhlNOERROR);
	}
	xdist = x - holdx;
	ydist = y - holdy;
/*
 * If the beginning of the line is clipped find the first visible point
 * and move there.
 */
	if((lastx != holdx)||(lasty!= holdy)) {
		if (AdjustToEdge(spinst,holdx,holdy,x,y,&xd,&yd,&xc,&yc)
			< NhlNOERROR)
			return NhlFATAL;
		lastx = xd;
		lasty = yd;
		xdist = x - lastx;
		ydist = y - lasty;

		_NhlWorkstationLineTo(spinst->trobj.wkptr,
				      c_cufx(xc),c_cufy(yc),1);
	}
	else if (call_frstd == 1) {
		_NhlWorkstationLineTo(spinst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
	}
	call_frstd = 2;

	for (i = 0; i< npoints; i++) {
		xd = lastx + xdist *(i+1)/(float)npoints;
		yd = lasty + ydist *(i+1)/(float)npoints;
		SpDataToCompc(instance,&xd,&yd,1,&xc,&yc,NULL,NULL,&status);
		if (status) {
			if (AdjustToEdge(spinst,x,y,holdx,holdy,
					 &xd,&yd,&xc,&yc) < NhlNOERROR)
				return NhlFATAL;
		}
		_NhlWorkstationLineTo(spinst->trobj.wkptr,
				      c_cufx(xc),c_cufy(yc),0);
		if (status) {
			break;
		}
	}
	lastx = x;
	lasty = y;
	return(NhlNOERROR);
}

/*ARGSUSED*/
static NhlErrorTypes SpWinLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance, x, y, upordown)
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
	NhlSphericalTransObjLayer spinst = (NhlSphericalTransObjLayer)instance;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float holdx, holdy; /* * if true the moveto is being performed */

	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd = 1;
		return(NhlNOERROR);
	} else {
		currentx = x;
		currenty = y;
		holdx = lastx;
		holdy = lasty;
		_NhlTransClipLine(
/*
* Window and compc are identical for this object
*/
			spinst->sptrans.compc_x_min, 
			spinst->sptrans.compc_x_max, 
			spinst->sptrans.compc_y_min, 
			spinst->sptrans.compc_y_max,
			&lastx, &lasty, &currentx, &currenty,
			spinst->trobj.out_of_range);
		if((lastx == spinst->trobj.out_of_range)
			||(lasty == spinst->trobj.out_of_range)
			||(currentx == spinst->trobj.out_of_range)
			||(currenty == spinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
			return(_NhlWorkstationLineTo(spinst->trobj.wkptr,c_cufx(x),c_cufy(y),1));
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }

			if(call_frstd == 1) {
				_NhlWorkstationLineTo(spinst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);

				call_frstd = 2;
			}
			_NhlWorkstationLineTo(spinst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);

			lastx = x;
			lasty = y;
			return(NhlNOERROR);
		}
			
			
	}
	
}


/*ARGSUSED*/
static NhlErrorTypes SpNDCLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown )
#else
(instance, x, y, upordown )
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
	NhlSphericalTransObjLayer iinstance= (NhlSphericalTransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float holdx,holdy;

/*
* if true the moveto is being performed
*/
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd = 1;
		return(NhlNOERROR);
	} else {
		currentx = x;
		currenty = y;
		holdx = lastx;
		holdy = lasty;

		_NhlTransClipLine(
			tp->x, tp->x+tp->width, tp->y-tp->height, tp->y,
			&lastx, &lasty, &currentx, &currenty,
			iinstance->trobj.out_of_range);
		if((lastx == iinstance->trobj.out_of_range)
			||(lasty == iinstance->trobj.out_of_range)
			||(currentx == iinstance->trobj.out_of_range)
			||(currenty == iinstance->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
			return(_NhlWorkstationLineTo(iinstance->trobj.wkptr,x,y,1));

		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }

			if(call_frstd == 1) {
				call_frstd = 2;
				_NhlWorkstationLineTo(iinstance->trobj.wkptr,lastx,lasty,1);
			}
			_NhlWorkstationLineTo(iinstance->trobj.wkptr,currentx,currenty,0);

			lastx = x;
			lasty = y;
			return(NhlNOERROR);
		}
			
			
	}
	
}

static int ResizeOut
#if	NhlNeedProto
(float **xout, float **yout, int n )
#else
(xout, yout, n )
NhlLayer instance;
float **xout;
float **yout;
int n;
#endif
{
	n *= 2;

	*xout = NhlRealloc(*xout,n * sizeof(float));
	*yout = NhlRealloc(*yout,n * sizeof(float));
	if (xout == NULL || yout == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return (int) NhlFATAL;
	}
	return n;
}
		

/*ARGSUSED*/
static NhlErrorTypes SpDataPolygon
#if	NhlNeedProto
(NhlLayer instance, float *x, float *y, int n )
#else
(instance, x, y, n )
NhlLayer instance;
float *x;
float *y;
int n;
#endif
{
	NhlErrorTypes ret;
	NhlSphericalTransObjLayer spinst = (NhlSphericalTransObjLayer)instance;
	NhlSphericalTransObjLayerPart *sptp = 
		(NhlSphericalTransObjLayerPart *) &spinst->sptrans;
	NhlString e_text;
	NhlString entry_name = "SpDataPolygon";
	float out_of_range = spinst->trobj.out_of_range;
	int i,j,ixout;
	float px,py,cx,cy,dx,dy,tx,ty;
	float *xbuf,*ybuf,*dbuf,*xout,*yout;
	int *ixbuf;
	NhlBoolean open, done = False, first, firstpoint;
	int count, pcount, cix, pix, status = 0, npoints = 256;
	float xdist,ydist,tdist;
	int outcount;

	npoints = spinst->trobj.point_count;
	open = (x[0] != x[n-1] || y[0] != y[n-1]) ?  True : False;
	count = pcount = open ? n + 1 : n; 

	xbuf = NhlMalloc(2 * count * sizeof(float));
	ybuf = NhlMalloc(2 * count * sizeof(float));
	dbuf = NhlMalloc(2 * count * sizeof(float));
	ixbuf = NhlMalloc(2 * count * sizeof(int));
	
	if (xbuf == NULL || ybuf == NULL || ixbuf == NULL || dbuf == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	xbuf[0] = x[0];
	ybuf[0] = y[0];
	ixbuf[0] = 0;

	j = 1;
	i = 0;
	while (! done) {
		if (i < n - 1) {
			px = x[i];
			py = y[i];
			i++;
			cx = x[i];
			cy = y[i];
			cix = i;
			pix = i-1;
		}
		else {
			if (! open) 
				break;
			else {
				px = x[i];
				py = y[i];
				cx = x[0];
				cy = y[0];
				cix = 0;
				pix = i;
				done = True;
			}
		}
		_NhlTransClipLine(sptp->x_min,sptp->x_max,
				  sptp->y_min,sptp->y_max,
				  &px,&py,&cx,&cy,out_of_range);

		if (px == out_of_range) {
			xbuf[j] = x[cix];
			ybuf[j] = y[cix];
			ixbuf[j] = cix;
			j++;
		}
		else {
			if (px != x[pix] || py != y[pix]) {
				xbuf[j] = px;
				ybuf[j] = py;
				ixbuf[j] = -1;
				j++;
			}
			xbuf[j] = cx;
			ybuf[j] = cy;
			if (cx == x[cix] && cy == y[cix]) {
				ixbuf[j] = cix;
				j++;
			}
			else  {
				ixbuf[j] = -1;
				j++;
				xbuf[j] = x[cix];
				ybuf[j] = y[cix];
				ixbuf[j] = cix;
				j++;
			}
		}
	}
	count = j;
	ret = SpDataToCompc(instance,xbuf,ybuf,count,
			    xbuf,ybuf,NULL,NULL,&status);
	tdist = 0.0;

/*
 * First handle the simpler situation where no clipping is required.
 */
	if (! status) {
		xbuf[0] = c_cufx(xbuf[0]);
		ybuf[0] = c_cufy(ybuf[0]);
		for (i = 1; i < count; i++) {
			xbuf[i] = c_cufx(xbuf[i]);
			ybuf[i] = c_cufy(ybuf[i]);
			dbuf[i-1] = fabs(xbuf[i]-xbuf[i-1]) 
				+ fabs(ybuf[i]-ybuf[i-1]);
			tdist += dbuf[i-1];
		}
		if (spinst->trobj.point_count > 1)
			npoints *= (tdist);
		/* include some extra space for safety */
		xout = NhlMalloc((npoints+count)*sizeof(float));
		yout = NhlMalloc((npoints+count)*sizeof(float));
		outcount = npoints+count;
		if (xout == NULL || yout == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		xout[0] = xbuf[0];
		yout[0] = ybuf[0];
		ixout = 0;
		for (i=1; i<pcount; i++) {
			float ratio;
			int lcount;
			ratio = dbuf[i-1] / tdist;
			lcount = (int) (ratio * (float)npoints);
			lcount = lcount > 1 ? lcount : 1;
			xdist = x[i] - x[i-1];
			ydist = y[i] - y[i-1];
			for (j=0; j < lcount; j++) {
				cx = x[i-1] + xdist * (j+1) / (float)lcount;
				cy = y[i-1] + ydist * (j+1) / (float)lcount;
				SpDataToCompc(instance,&cx,&cy,1,
				      &cx,&cy,NULL,NULL,&status);
				if (! status) {
					ixout++;
					if (ixout >= outcount) {
						outcount = 
							ResizeOut(&xout,&yout,
								  outcount);
						if (outcount < 0)
							return NhlFATAL;
					}
					xout[ixout] = c_cufx(cx);
					yout[ixout] = c_cufy(cy);
				}
			}
		}
		if (outcount < ixout+1) {
			e_text = "%s: internal error: memory overrun";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
			
				
#if 0
		printf("count,pcount,npoints,ixout+1,%d,%d,%d,%d\n",
		       count,pcount,npoints,ixout+1);
#endif
		ret = _NhlWorkstationFill(spinst->trobj.wkptr,
					  xout,yout,ixout+1);

		NhlFree(xbuf);
		NhlFree(ybuf);
		NhlFree(dbuf);
		NhlFree(xout);
		NhlFree(yout);
		return ret;
	}
/*
 * The clipped array as it stands does not work because the points on the
 * clipping boundary may not be correct. However, it hopefully gives a close
 * enough measure of the length to compute the number of points required.
 * Replace out of bounds points with points far enough outside the 
 * NDC viewspace that lines extending from the viewport edge to these
 * lines will be fully clipped.
 */
	pix = -1;
	for (i = 0; i < count; i++) {
		if (ixbuf[i] < 0) {
			if (i > 0) dbuf[i-1] = -1;
			continue;
		}
		if (xbuf[i] == out_of_range ||
		    ybuf[i] == out_of_range) {
			NhlBoolean xorange, yorange;
			if (ixbuf[i] == -1) {
				e_text = "%s: internal error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			cx = x[ixbuf[i]];
			cy = y[ixbuf[i]];
			xorange = cx < sptp->x_min || cx >sptp->x_max ? 
				True : False;
			yorange = cy < sptp->y_min || cy >sptp->y_max ? 
				True : False;
			status = 0;
			if (xorange && ! yorange) {
				if (cx < sptp->x_min) {
					cx = sptp->x_min;
					xbuf[i] = sptp->x_reverse ?
						1.1 : -.1;
				}
				else {
					cx = sptp->x_max;
					xbuf[i] = sptp->x_reverse ?
						-.1 : 1.1;
				}
				SpDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				ybuf[i] = c_cufy(cy);
			}
			else if (yorange && ! xorange) {
				if (cy < sptp->y_min) {
					cy = sptp->y_min;
					ybuf[i] = sptp->y_reverse ?
						1.1 : -.1;
				}
				else {
					cy = sptp->y_max;
					ybuf[i] = sptp->y_reverse ?
						-.1 : 1.1;
				}
				SpDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				xbuf[i] = c_cufx(cx);
			}
			else {
				if (sptp->x_reverse)
					xbuf[i] = cx < sptp->x_min ? 
						1.1 : -.1;
				else
					xbuf[i] = cx < sptp->x_min ? 
						-.1 : 1.1;

				if (sptp->y_reverse)
					ybuf[i] = cy < sptp->y_min ? 
						1.1 : -.1;
				else
					ybuf[i] = cy < sptp->y_min ? 
						-.1 : 1.1;
			}
			if (status) {
				e_text = "%s: internal error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
		}
		else {
			xbuf[i] = c_cufx(xbuf[i]);
			ybuf[i] = c_cufy(ybuf[i]);
		}
		if (i > 1 && ixbuf[i-1] == -1) {
			dbuf[i-1] = fabs(xbuf[i]-xbuf[i-2]) +
				fabs(ybuf[i]-ybuf[i-2]);
			tdist += dbuf[i-1];
		}
		else if (i > 0) {
			dbuf[i-1] = fabs(xbuf[i]-xbuf[i-1]) +
				fabs(ybuf[i]-ybuf[i-1]);
			tdist += dbuf[i-1];
		}
	}
	if (spinst->trobj.point_count > 1)
		npoints *= tdist;
	/* include some extra space for safety */
	xout = NhlMalloc((npoints+count)*sizeof(float));
	yout = NhlMalloc((npoints+count)*sizeof(float));
	if (xout == NULL || yout == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	outcount = npoints+count;
	xout[0] = xbuf[0];
	yout[0] = ybuf[0];
	ixout = 0;
	firstpoint = True;
	first = True;
	for (i=0; i< count; i++) {
		float ratio;
		int lcount;
		NhlBoolean started;
		if (ixbuf[i] < 0) {
			continue;
		}
		else if (firstpoint == True) {
			firstpoint = False;
			px = x[ixbuf[i]];
			py = y[ixbuf[i]];
			continue;
		}
		cx = x[ixbuf[i]];
		cy = y[ixbuf[i]];
		ratio = dbuf[i-1] / tdist;
		lcount = (int) (ratio * (float)npoints);
		lcount = lcount > 1 ? lcount : 1;
		xdist = cx - px;
		ydist = cy - py;
		started = False;
		for (j=0; j < lcount; j++) {
			dx = px + xdist * (j+1) / (float)lcount;
			dy = py + ydist * (j+1) / (float)lcount;
			SpDataToCompc(instance,&dx,&dy,1,
				      &tx,&ty,NULL,NULL,&status);
			if (! status) {
				if (! started) {
					started = True;
					if (lcount == 1) j--;
					if (AdjustToEdge(spinst,px,py,cx,cy,
							 &dx,&dy,&tx,&ty) 
					    < NhlNOERROR)
						return NhlFATAL;
				}
				ixout++;
				if (ixout >= outcount) {
					outcount = ResizeOut(&xout,&yout,
							     outcount);
					if (outcount < 0)
						return NhlFATAL;
				}
				xout[ixout] = c_cufx(tx);
				yout[ixout] = c_cufy(ty);
			}
			else if (started) break;
		}
		if (status) {
			if (started) {
				if (AdjustToEdge(spinst,cx,cy,px,py,
						 &dx,&dy,&tx,&ty) < NhlNOERROR)
					return NhlFATAL;
				ixout++;
				if (ixout >= outcount) {
					outcount = ResizeOut(&xout,&yout,
							     outcount);
					if (outcount < 0)
						return NhlFATAL;
				}
				xout[ixout] = c_cufx(tx);
				yout[ixout] = c_cufy(ty);
			}
			ixout++;
			if (ixout >= outcount) {
				outcount = ResizeOut(&xout,&yout,
						     outcount);
				if (outcount < 0)
					return NhlFATAL;
			}
			xout[ixout] = xbuf[i];
			yout[ixout] = ybuf[i];
		}
		px = x[ixbuf[i]];
		py = y[ixbuf[i]];
	}
	if (xout[ixout] != xout[0] || yout[ixout] != yout[0]) {
		ixout++;
		if (ixout >= outcount) {
			outcount = ResizeOut(&xout,&yout,
					     outcount);
			if (outcount < 0)
				return NhlFATAL;
		}
		xout[ixout] = xout[0];
		yout[ixout] = yout[0];
	}
	if (outcount < ixout+1) {
		e_text = "%s: internal error: memory overrun";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

#if 0
	printf("count,pcount,npoints,ixout+1,%d,%d,%d,%d\n",
	       count,pcount,npoints,ixout+1);
#endif
	ret = _NhlWorkstationFill(spinst->trobj.wkptr,xout,yout,ixout+1);

	NhlFree(xbuf);
	NhlFree(ybuf);
	NhlFree(dbuf);
	NhlFree(xout);
	NhlFree(yout);
	return ret;
	
}
static NhlErrorTypes SpTransGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int nargs)
#else
(l, args, nargs)
NhlLayer l;
_NhlArgList args;
int nargs;
#endif
{
	NhlSphericalTransObjLayerPart* spp = 
		(&((NhlSphericalTransObjLayer)l)->sptrans);
	int i;
	NhlGenArray ga;


	for( i = 0; i < nargs ; i++) {
		ga = NULL;
		if(args[i].quark == QtrXCoordPoints) {
			if(spp->x_sph_ga != NULL)
				ga = spp->x_sph_ga;
		}
		if(args[i].quark == QtrYCoordPoints) {
			if(spp->y_sph_ga != NULL)
				ga = spp->y_sph_ga;
		}
		if (ga) {
			NhlGenArray ga_out;

			ga_out = _NhlCopyGenArray(ga,True);

			if (! ga_out) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga_out;
		}
	}
	return(NhlNOERROR);
}

/*
 * Function:	SpTransClassInitialize
 *
 * Description: 
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    SpTransClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{

	QtrXCoordPoints = NrmStringToQuark(NhlNtrXCoordPoints);
	QtrYCoordPoints = NrmStringToQuark(NhlNtrYCoordPoints);

	return(NhlNOERROR);	
}
