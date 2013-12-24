/*
 *      $Id: TriMeshTransObj.c,v 1.2 2004-10-05 22:50:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TriMeshTransObj.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 16 17:41:11 MDT 2003
 *
 *	Description:	
 *
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TriMeshTransObjP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/ConvertersP.h>
#include <math.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlTriMeshTransObjLayerRec,
			    tmtrans.x_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlTriMeshTransObjLayerRec,
			    tmtrans.y_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL) ,0,(NhlFreeFunc)NhlFreeGenArray }

/* End-documented-resources */
        
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  TmTransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes TmTransInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);


static NhlErrorTypes TmTransDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);


/*
* TransObjClass Methods defined
*/

static NhlErrorTypes TmSetTrans(
#if	NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);


static NhlErrorTypes TmWinToNDC(
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


static NhlErrorTypes TmNDCToWin(
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


static NhlErrorTypes TmDataToCompc(
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

static NhlErrorTypes TmCompcToData(
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

static NhlErrorTypes TmWinToCompc(
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



static NhlErrorTypes TmNDCLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes TmDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes TmWinLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes TmDataPolygon(
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
	NhlTriMeshTransObjLayerPart *tmp,
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

static NhlErrorTypes TmTransGetValues(
#if NhlNeedProto
	NhlLayer /* l */,
	_NhlArgList /*args */,
	int	/*nargs*/
#endif
);

static NhlErrorTypes 	TmTransClassInitialize(
#if NhlNeedProto
	void
#endif
);

#define CREATE  1
#define SET 0

#define NhltmDATA 0
#define NhltmCOMPC 1

static NrmQuark QtrXCoordPoints;
static NrmQuark QtrYCoordPoints;

NhlTriMeshTransObjClassRec NhltriMeshTransObjClassRec = {
        {
/* class_name			*/	"triMeshTransformationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlTriMeshTransObjLayerRec),
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
/* class_initialize		*/	TmTransClassInitialize,
/* layer_initialize		*/	TmTransInitialize,
/* layer_set_values		*/	TmTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	TmTransGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	TmTransDestroy
        },
        {
/* set_trans		*/	TmSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	TmWinToNDC,
/* ndc_to_win		*/	TmNDCToWin,
/* data_to_win		*/	TmDataToCompc, 
/* win_to_data		*/	TmCompcToData, 
/* data_to_compc	*/	TmDataToCompc,
/* compc_to_data	*/	TmCompcToData,
/* win_to_compc		*/	TmWinToCompc,
/* compc_to_win		*/	TmWinToCompc,
/* data_lineto 		*/      TmDataLineTo,
/* compc_lineto		*/      TmWinLineTo,
/* win_lineto 		*/      TmWinLineTo,
/* NDC_lineto 		*/      TmNDCLineTo,
/* data_polygon		*/      TmDataPolygon

        }
};

NhlClass NhltriMeshTransObjClass =
			(NhlClass)&NhltriMeshTransObjClassRec;


#define INCREASING 0
#define DECREASING 1
#define NONMONOTONIC 2
#define NOSPAN 3


/*
 * Function:	TmTransSetValues
 *
 * Description:	SetValues method for TriMeshTrans Objects
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
static NhlErrorTypes TmTransSetValues
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
 * Function:	TmTransInitialize
 *
 * Description: Initialize function for TriMeshTransObjs. Performs same
 *		operations as set_values for copying array resources
 *
 * In Args:  	Standard layer_initialize arguments.
 *
 * Out Args:	Standard layer_initialize output.
 *
 * Return Values: Error Status
 *
 * Side Effects: allocates tmace and copies valus of array resources.
 */
/*ARGSUSED*/
static NhlErrorTypes TmTransInitialize
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
 * Function:	TmTransDestroy
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
static NhlErrorTypes TmTransDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlTriMeshTransObjLayer tm = 
		(NhlTriMeshTransObjLayer)inst;

	if (tm->tmtrans.x_trm_ga)
		NhlFreeGenArray(tm->tmtrans.x_trm_ga);
	if (tm->tmtrans.y_trm_ga)
		NhlFreeGenArray(tm->tmtrans.y_trm_ga);

	if (tm->tmtrans.ixmin)
		NhlFree(tm->tmtrans.ixmin);
	if (tm->tmtrans.ixmax)
		NhlFree(tm->tmtrans.ixmax);
	if (tm->tmtrans.iymin)
		NhlFree(tm->tmtrans.iymin);
	if (tm->tmtrans.iymax)
		NhlFree(tm->tmtrans.iymax);

 	free(tm->tmtrans.xmin_dat);
	free(tm->tmtrans.xmax_dat);
	free(tm->tmtrans.ymin_dat);
	free(tm->tmtrans.ymax_dat);
 	free(tm->tmtrans.compc_xmin_dat);
	free(tm->tmtrans.compc_xmax_dat);
	free(tm->tmtrans.compc_ymin_dat);
	free(tm->tmtrans.compc_ymax_dat);

	return NhlNOERROR;
}

#define MIN4(a,b,c,d) MIN(MIN((a),(b)),MIN((c),(d)))
#define MAX4(a,b,c,d) MAX(MAX((a),(b)),MAX((c),(d)))
#define DEGTORAD 0.017453292519943
#define RADTODEG 57.2957795130823

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
	NhlTriMeshTransObjLayer inew = (NhlTriMeshTransObjLayer)new;
	NhlTriMeshTransObjLayer iold = (NhlTriMeshTransObjLayer)old;
	NhlTriMeshTransObjLayerPart *tmp = &inew->tmtrans;
	NhlTransObjLayerPart	*tp = &inew->trobj;
	NhlTransObjLayerPart	*otp = &iold->trobj;
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;
	NhlBoolean new_coords = False;
        NhlBoolean data_extent_def;
        NhlBoolean new_data_extent;
	
	tp->change_count++;
	
        data_extent_def = (tp->data_xstart == 0.0 && tp->data_xend == 0.0)
		|| (tp->data_ystart == 0.0 && tp->data_yend == 0.0) ?
                False : True;
	
	if(c_or_s == SET) {

		error_lead = "TmTransSetValues";

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
	}
	else {
		error_lead = "TmTransInitialize";
		new_coords = True;
                new_data_extent = True;
		tmp->x_trm_ga = NULL;
		tmp->y_trm_ga = NULL; 
                tmp->x_trm_min = 0.0;
                tmp->x_trm_max = 0.0;
                tmp->y_trm_min = 0.0;
                tmp->y_trm_max = 0.0;

                tmp->ul = tmp->ub = 0.0;
                tmp->ur = tmp->ut = 1.0;
		tmp->ixmin = tmp->ixmax = NULL;
		tmp->iymin = tmp->iymax = NULL;
	}

	if (data_extent_def) {
		tmp->x_trm_min = MIN(tp->data_xstart,tp->data_xend);
		tmp->x_trm_max = MAX(tp->data_xstart,tp->data_xend);
		tmp->y_trm_min = MIN(tp->data_ystart,tp->data_yend);
		tmp->y_trm_max = MAX(tp->data_ystart,tp->data_yend);
	}
		

	if (tp->x_min < tmp->x_trm_min) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: X minimum less than minimum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->x_min = tmp->x_trm_min;
	}
	if (tp->x_max > tmp->x_trm_max) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: X maximum greater than maximum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->x_max = tmp->x_trm_max;
	}
	if (tp->y_min < tmp->y_trm_min) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Y minimum less than minimum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->y_min = tmp->y_trm_min;
	}
	if (tp->y_max > tmp->y_trm_max) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Y maximum greater than maximum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->y_max = tmp->y_trm_max;
	}

        
	tmp->compc_x_min = MIN(tmp->ul,tmp->ur);
	tmp->compc_x_max = MAX(tmp->ul,tmp->ur);
	tmp->compc_y_min = MIN(tmp->ut,tmp->ub);
	tmp->compc_y_max = MAX(tmp->ut,tmp->ub);
	tmp->ixb = tmp->compc_x_min;
	tmp->ixe = tmp->compc_x_max;
	tmp->iyb = tmp->compc_y_min;
	tmp->iye = tmp->compc_y_max;
	

	tmp->log_lin_value = 1;
	tmp->x_min = tp->x_min;
	tmp->y_min = tp->y_min;
	tmp->x_max = tp->x_max;
	tmp->y_max = tp->y_max;
	tmp->x_reverse = tp->x_reverse;
	tmp->y_reverse = tp->y_reverse;
	tmp->ul = tmp->x_min;
	tmp->ur = tmp->x_max;
	tmp->ub = tmp->y_min;
	tmp->ut = tmp->y_max;
	if(tp->x_reverse) {
		float tmpf = tmp->ur;
		tmp->ur = tmp->ul;
		tmp->ul = tmpf;
	}
	if(tp->y_reverse) {
		float tmpf = tmp->ut;
		tmp->ut = tmp->ub;
		tmp->ub = tmpf;
	}


	if (c_or_s == CREATE) {
		if ((tmp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((tmp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((tmp->ymin_dat =_NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((tmp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((tmp->compc_xmin_dat =
		     _NhlCmpFSetup(tmp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_xmin_dat = tmp->compc_xmin_dat;
		if ((tmp->compc_xmax_dat = 
		     _NhlCmpFSetup(tmp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_xmax_dat = tmp->compc_xmax_dat;
		if ((tmp->compc_ymin_dat = 
		     _NhlCmpFSetup(tmp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_ymin_dat = tmp->compc_ymin_dat;
		if ((tmp->compc_ymax_dat = 
		     _NhlCmpFSetup(tmp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_ymax_dat = tmp->compc_ymax_dat;

                tp->x_min_set = tp->x_max_set = False;
                tp->x_reverse_set = False;
                tp->y_min_set = tp->y_max_set = False;
                tp->y_reverse_set = False;
        
		return(ret);
	}
        if (tp->x_min != iold->tmtrans.x_min) {
                free(tmp->xmin_dat);
                if ((tmp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->x_max != iold->tmtrans.x_max) {
		free(tmp->xmax_dat);
		if ((tmp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_min != iold->tmtrans.y_min) {
		free(tmp->ymin_dat);
		if ((tmp->ymin_dat = _NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_max != iold->tmtrans.y_max) {
		free(tmp->ymax_dat);
		if ((tmp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tmp->compc_x_min != iold->tmtrans.compc_x_min) {
		free(tmp->compc_xmin_dat);
		if ((tmp->compc_xmin_dat =
		     _NhlCmpFSetup(tmp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_xmin_dat = tmp->compc_xmin_dat;
	}
	if (tmp->compc_x_max != iold->tmtrans.compc_x_max) {
		free(tmp->compc_xmax_dat);
		if ((tmp->compc_xmax_dat = 
		     _NhlCmpFSetup(tmp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_xmax_dat = tmp->compc_xmax_dat;
	}
	if (tmp->compc_y_min != iold->tmtrans.compc_y_min) {
		free(tmp->compc_ymin_dat);
		if ((tmp->compc_ymin_dat = 
		     _NhlCmpFSetup(tmp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_ymin_dat = tmp->compc_ymin_dat;
	}
	if (tmp->compc_y_max != iold->tmtrans.compc_y_max) {
		free(tmp->compc_ymax_dat);
		if ((tmp->compc_ymax_dat = 
		     _NhlCmpFSetup(tmp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                tmp->save_compc_ymax_dat = tmp->compc_ymax_dat;
	}
        
        tp->x_min_set = tp->x_max_set = False;
        tp->x_reverse_set = False;
        tp->y_min_set = tp->y_max_set = False;
        tp->y_reverse_set = False;
        
	return(ret);

}

/*
 * Function:	TmSetTrans
 *
 * Description: set_trans method for TriMeshTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the TriMeshTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes TmSetTrans
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
	NhlTriMeshTransObjLayer	to=(NhlTriMeshTransObjLayer)tobj;
	NhlTransObjLayerPart		*top = &to->trobj;
	NhlTriMeshTransObjLayerPart	*tp = &to->tmtrans;
	NhlErrorTypes ret;

	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	return(_NhlTransLLUSet(top->x,top->x+top->width,
			       top->y-top->height,top->y,
			       tp->ul,tp->ur,tp->ub,tp->ut,
			       tp->log_lin_value,
                               &top->off_screen,
                               "TmSetTrans"));

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
	NhlTriMeshTransObjLayerPart *tmp,
	float	*x,
 	float	*y,
	int	type /* data 0, compc 1 */
)
#else
(tmp,x,y,type)
	NhlTriMeshTransObjLayerPart *tmp;
	float	*x;
	float	*y;
	int	type;
#endif
{
	int xmndif,xmxdif,ymndif,ymxdif;

	if (type == NhltmDATA) {
		if ((xmndif = _NhlCmpF(*x,tmp->xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,tmp->xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,tmp->ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,tmp->ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = tmp->x_min;
		}
		else if (xmxdif == 0) {
			*x = tmp->x_max;
		}
		if (ymndif == 0) {
			*y = tmp->y_min;
		}
		else if (ymxdif == 0) {
			*y = tmp->y_max;
		}
	}
	else {
		if ((xmndif = _NhlCmpF(*x,tmp->compc_xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,tmp->compc_xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,tmp->compc_ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,tmp->compc_ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = tmp->compc_x_min;
		}
		else if (xmxdif == 0) {
			*x = tmp->compc_x_max;
		}
		if (ymndif == 0) {
			*y = tmp->compc_y_min;
		}
		else if (ymxdif == 0) {
			*y = tmp->compc_y_max;
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
 * Function:	TmWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the TriMeshTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes TmWinToNDC
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
	NhlTriMeshTransObjLayer	iinstance =
				(NhlTriMeshTransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;
	int i;
	
	*status = 0;	
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n ; i++) {
/*
* Compc and Window are identical coordinates in this object
*/
			if((x[i] > iinstance->tmtrans.x_max)
			   ||(x[i] < iinstance->tmtrans.x_min)
			   ||(y[i] > iinstance->tmtrans.y_max)
			   ||(y[i] < iinstance->tmtrans.y_min)) {
				if (! compare_check(&iinstance->tmtrans,
						    &x[i],&y[i],NhltmDATA)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}

			} 
			strans(iinstance->tmtrans.ul,
			       iinstance->tmtrans.ur,
			       iinstance->tmtrans.ub,
			       iinstance->tmtrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n ; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
			   ||((ymissing != NULL)&&(*ymissing == y[i]))
			   ||(x[i] > iinstance->tmtrans.x_max)
			   ||(x[i] < iinstance->tmtrans.x_min)
			   ||(y[i] > iinstance->tmtrans.y_max)
			   ||(y[i] < iinstance->tmtrans.y_min)) {
				if (! compare_check(&iinstance->tmtrans,
						    &x[i],&y[i],NhltmDATA)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}
			}
			strans(iinstance->tmtrans.ul,
			       iinstance->tmtrans.ur,
			       iinstance->tmtrans.ub,	
			       iinstance->tmtrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	}

	return(NhlNOERROR);
}


/*
 * Function:	TmNDCToWin
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
static NhlErrorTypes TmNDCToWin
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
	NhlTriMeshTransObjLayer	iinstance = 
				(NhlTriMeshTransObjLayer)instance;
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
			strans(tp->x,x1,y1,tp->y,iinstance->tmtrans.ul,
			       iinstance->tmtrans.ur, iinstance->tmtrans.ub,
			       iinstance->tmtrans.ut, x[i],y[i],
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
			       iinstance->tmtrans.ul,
			       iinstance->tmtrans.ur, 
			       iinstance->tmtrans.ub,
			       iinstance->tmtrans.ut, x[i],y[i],
			       &(xout[i]),&(yout[i]));
		}
	}
	return(NhlNOERROR);
}


/*
 * Function:	TmDataToCompc
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
static NhlErrorTypes TmDataToCompc
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
	NhlTriMeshTransObjLayer iinstance =
                (NhlTriMeshTransObjLayer)instance;
	NhlTriMeshTransObjLayerPart *tmp = &iinstance->tmtrans;
	int i;

	*status = 0;
	for(i=0; i< n;i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
#if 0
/* will this work? */
			||(x[i] < tmp->x_min)	
			||(x[i] > tmp->x_max)
#endif
			||(y[i] < tmp->y_min)
			||(y[i] > tmp->y_max)) {
		
			if (! compare_check(tmp,&x[i],&y[i],NhltmDATA)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		xout[i] = x[i];
		yout[i] = y[i];

#if 0
		sc[0] = cos(DEGTORAD * (double)y[i]);
		sc[1] = sin(DEGTORAD * (double)y[i]);
		sc[2] = cos(DEGTORAD * (double)x[i]);
		sc[3] = sin(DEGTORAD * (double)x[i]);
		
		if (! IsNearby(tmp,sc)) {
			if (! SetQuadrant(tmp,sc)) {
				*status = 1;
				xout[i]= yout[i] = 
					iinstance->trobj.out_of_range;
				continue;	
			}
		}
		if (! GetFracCoords(tmp,sc,iinstance->trobj.out_of_range,
				    &(xout[i]),&(yout[i]))) {
			*status = 1;
		}
#endif
	}
	return(ret);
}

/*
 * Function:	TmCompcToData
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
static NhlErrorTypes TmCompcToData
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
	NhlTriMeshTransObjLayer iinstance = 
		(NhlTriMeshTransObjLayer)instance;
	NhlTriMeshTransObjLayerPart *tmp = &iinstance->tmtrans;
	int i;

	/* 
	 * this code assumes that the X coordinates have been put into the correct cyclic period prior to this call
         * (except that it doesn't work ; dib 12/30/2011 -- needs investigation)
         */
	*status = 0;
	for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
#if 0
			||(x[i] > tmp->x_max)
			||(x[i] < tmp->x_min)
#endif
			||(y[i] > tmp->y_max)
			||(y[i] < tmp->y_min)) {

			if (! compare_check(tmp,&x[i],&y[i],NhltmDATA)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		xout[i] = x[i];
		yout[i] = y[i];
	}
	return(ret);
}

/*ARGSUSED*/
static NhlErrorTypes TmWinToCompc
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
        NhlTriMeshTransObjLayer iinstance = (NhlTriMeshTransObjLayer)instance;
        int i;

        *status = 0;
        for(i = 0 ; i< n; i++) {
                if(((xmissing != NULL)&&(*xmissing == x[i]))
                        || ((ymissing != NULL)&&(*ymissing == y[i]))
                        ||(x[i] <= iinstance->tmtrans.x_min)
                        ||(x[i] >= iinstance->tmtrans.x_max)
                        ||(y[i] <= iinstance->tmtrans.y_min)
                        ||(y[i] >= iinstance->tmtrans.y_max)) {
#if 0
			if (! compare_check(&iinstance->tmtrans,
					    &x[i],&y[i],NhltmCOMPC)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
#endif 
		}
		yout[i] = y[i];
		xout[i] = x[i];
        }
        return(ret);
}

static NhlErrorTypes AdjustToEdge
#if	NhlNeedProto
(NhlTriMeshTransObjLayer tminst, 
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
(tminst,xclip,yclip,x,y,xd, yd,xc,yc)
NhlTriMeshTransObjLayer tminst;
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
	NhlTriMeshTransObjLayerPart *tmp = 
		(NhlTriMeshTransObjLayerPart *) &tminst->tmtrans;
	float xt,yt;
	int i,status = 1;

	xt = xclip;
	yt = yclip;

	for (i=0; i < 2; i++) {

		if (x != xclip) {
			if (_NhlCmpF(xt,tmp->xmin_dat) < 0.0) {
				*xd = tmp->x_min;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
			else if (_NhlCmpF(xt,tmp->xmax_dat) > 0.0) {
				*xd = tmp->x_max;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
		}
		if (y != yclip) {
			if (_NhlCmpF(yt,tmp->ymin_dat) < 0.0) {
				*yd = tmp->y_min;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
			else if (_NhlCmpF(yt,tmp->ymax_dat) > 0.0) {
				*yd = tmp->y_max;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
		}
		TmDataToCompc((NhlLayer)tminst,xd,yd,1,
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


static NhlErrorTypes TmDataLineTo
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
	NhlTriMeshTransObjLayer tminst = (NhlTriMeshTransObjLayer)instance;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	float holdx,holdy;
	int status;
	int i,npoints = 256;
	float xdist,ydist,xc,yc,xd,yd;

	npoints = tminst->trobj.point_count;
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
	_NhlTransClipLine(tminst->tmtrans.x_min,
			  tminst->tmtrans.x_max,
			  tminst->tmtrans.y_min,
			  tminst->tmtrans.y_max,
			  &lastx,
			  &lasty,
			  &currentx,
			  &currenty,
			  tminst->trobj.out_of_range);

	if((lastx == tminst->trobj.out_of_range)
	   ||(lasty == tminst->trobj.out_of_range)
	   ||(currentx == tminst->trobj.out_of_range)
	   ||(currenty == tminst->trobj.out_of_range)){
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
	TmDataToCompc(instance,xpoints,ypoints,2,
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
			_NhlWorkstationLineTo(tminst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
			call_frstd = 2;
		}
		xdist = currentx - lastx;
		ydist = currenty - lasty;
		for (i = 0; i<npoints; i++) {
			xd = lastx + xdist *(i+1)/ (float)npoints;
			yd = lasty + ydist *(i+1)/ (float)npoints;
			TmDataToCompc(instance,&xd,&yd,1,
				      &xc,&yc,NULL,NULL,&status);
			if (! status)
				_NhlWorkstationLineTo(tminst->trobj.wkptr,
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
		if (AdjustToEdge(tminst,holdx,holdy,x,y,&xd,&yd,&xc,&yc)
			< NhlNOERROR)
			return NhlFATAL;
		lastx = xd;
		lasty = yd;
		xdist = x - lastx;
		ydist = y - lasty;

		_NhlWorkstationLineTo(tminst->trobj.wkptr,
				      c_cufx(xc),c_cufy(yc),1);
	}
	else if (call_frstd == 1) {
		_NhlWorkstationLineTo(tminst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
	}
	call_frstd = 2;

	for (i = 0; i< npoints; i++) {
		xd = lastx + xdist *(i+1)/(float)npoints;
		yd = lasty + ydist *(i+1)/(float)npoints;
		TmDataToCompc(instance,&xd,&yd,1,&xc,&yc,NULL,NULL,&status);
		if (status) {
			if (AdjustToEdge(tminst,x,y,holdx,holdy,
					 &xd,&yd,&xc,&yc) < NhlNOERROR)
				return NhlFATAL;
		}
		_NhlWorkstationLineTo(tminst->trobj.wkptr,
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
static NhlErrorTypes TmWinLineTo
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
	NhlTriMeshTransObjLayer tminst = (NhlTriMeshTransObjLayer)instance;
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
			tminst->tmtrans.compc_x_min, 
			tminst->tmtrans.compc_x_max, 
			tminst->tmtrans.compc_y_min, 
			tminst->tmtrans.compc_y_max,
			&lastx, &lasty, &currentx, &currenty,
			tminst->trobj.out_of_range);
		if((lastx == tminst->trobj.out_of_range)
			||(lasty == tminst->trobj.out_of_range)
			||(currentx == tminst->trobj.out_of_range)
			||(currenty == tminst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
			return(_NhlWorkstationLineTo(tminst->trobj.wkptr,c_cufx(x),c_cufy(y),1));
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }

			if(call_frstd == 1) {
				_NhlWorkstationLineTo(tminst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);

				call_frstd = 2;
			}
			_NhlWorkstationLineTo(tminst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);

			lastx = x;
			lasty = y;
			return(NhlNOERROR);
		}
			
			
	}
	
}


/*ARGSUSED*/
static NhlErrorTypes TmNDCLineTo
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
	NhlTriMeshTransObjLayer iinstance= (NhlTriMeshTransObjLayer)instance;
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
static NhlErrorTypes TmDataPolygon
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
	NhlTriMeshTransObjLayer tminst = (NhlTriMeshTransObjLayer)instance;
	NhlTriMeshTransObjLayerPart *tmtp = 
		(NhlTriMeshTransObjLayerPart *) &tminst->tmtrans;
	NhlString e_text;
	NhlString entry_name = "TmDataPolygon";
	float out_of_range = tminst->trobj.out_of_range;
	int i,j,ixout;
	float px,py,cx,cy,dx,dy,tx,ty;
	float *xbuf,*ybuf,*dbuf,*xout,*yout;
	int *ixbuf;
	NhlBoolean open, done = False, first, firstpoint;
	int count, pcount, cix, pix, status = 0, npoints = 256;
	float xdist,ydist,tdist;
	int outcount;

	npoints = tminst->trobj.point_count;
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
		_NhlTransClipLine(tmtp->x_min,tmtp->x_max,
				  tmtp->y_min,tmtp->y_max,
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
	ret = TmDataToCompc(instance,xbuf,ybuf,count,
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
		if (tminst->trobj.point_count > 1)
			npoints *= (tdist);
		/* include some extra tmace for safety */
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
			xdist = x[i%n] - x[i-1];
			ydist = y[i%n] - y[i-1];
			for (j=0; j < lcount; j++) {
				cx = x[i-1] + xdist * (j+1) / (float)lcount;
				cy = y[i-1] + ydist * (j+1) / (float)lcount;
				TmDataToCompc(instance,&cx,&cy,1,
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
		ret = _NhlWorkstationFill(tminst->trobj.wkptr,
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
 * NDC viewtmace that lines extending from the viewport edge to these
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
			xorange = cx < tmtp->x_min || cx >tmtp->x_max ? 
				True : False;
			yorange = cy < tmtp->y_min || cy >tmtp->y_max ? 
				True : False;
			status = 0;
			if (xorange && ! yorange) {
				if (cx < tmtp->x_min) {
					cx = tmtp->x_min;
					xbuf[i] = tmtp->x_reverse ?
						1.1 : -.1;
				}
				else {
					cx = tmtp->x_max;
					xbuf[i] = tmtp->x_reverse ?
						-.1 : 1.1;
				}
				TmDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				ybuf[i] = c_cufy(cy);
			}
			else if (yorange && ! xorange) {
				if (cy < tmtp->y_min) {
					cy = tmtp->y_min;
					ybuf[i] = tmtp->y_reverse ?
						1.1 : -.1;
				}
				else {
					cy = tmtp->y_max;
					ybuf[i] = tmtp->y_reverse ?
						-.1 : 1.1;
				}
				TmDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				xbuf[i] = c_cufx(cx);
			}
			else {
				if (tmtp->x_reverse)
					xbuf[i] = cx < tmtp->x_min ? 
						1.1 : -.1;
				else
					xbuf[i] = cx < tmtp->x_min ? 
						-.1 : 1.1;

				if (tmtp->y_reverse)
					ybuf[i] = cy < tmtp->y_min ? 
						1.1 : -.1;
				else
					ybuf[i] = cy < tmtp->y_min ? 
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
	if (tminst->trobj.point_count > 1)
		npoints *= tdist;
	/* include some extra tmace for safety */
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
			TmDataToCompc(instance,&dx,&dy,1,
				      &tx,&ty,NULL,NULL,&status);
			if (! status) {
				if (! started) {
					started = True;
					if (lcount == 1) j--;
					if (AdjustToEdge(tminst,px,py,cx,cy,
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
				if (AdjustToEdge(tminst,cx,cy,px,py,
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
	ret = _NhlWorkstationFill(tminst->trobj.wkptr,xout,yout,ixout+1);

	NhlFree(xbuf);
	NhlFree(ybuf);
	NhlFree(dbuf);
	NhlFree(xout);
	NhlFree(yout);
	return ret;
	
}
static NhlErrorTypes TmTransGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int nargs)
#else
(l, args, nargs)
NhlLayer l;
_NhlArgList args;
int nargs;
#endif
{
	NhlTriMeshTransObjLayerPart* tmp = 
		(&((NhlTriMeshTransObjLayer)l)->tmtrans);
	int i;
	NhlGenArray ga;


	for( i = 0; i < nargs ; i++) {
		ga = NULL;
		if(args[i].quark == QtrXCoordPoints) {
			if(tmp->x_trm_ga != NULL)
				ga = tmp->x_trm_ga;
		}
		if(args[i].quark == QtrYCoordPoints) {
			if(tmp->y_trm_ga != NULL)
				ga = tmp->y_trm_ga;
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
 * Function:	TmTransClassInitialize
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
static NhlErrorTypes    TmTransClassInitialize
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
