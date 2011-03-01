/*
 *      $Id: LogLinTransObj.c,v 1.39 2006-08-01 18:49:29 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Nov 4 16:38:57 MST 1992
 *
 *	Description:	
 */
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/View.h>
#include <math.h>


static NhlResource resources[] =  {
	{ NhlNtrXLog,NhlCtrXLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.x_log),
		NhlTImmediate,_NhlUSET(False),0,NULL},
	{ NhlNtrYLog,NhlCtrYLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.y_log),
		NhlTImmediate,_NhlUSET(False),0,NULL},
};


/*
* BaseClass Methods defined
*/

static NhlErrorTypes  LlTransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes LlTransInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LlTransDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);

/*
* TransObjClass Methods defined
*/

static NhlErrorTypes LlNDCLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes LlDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes LlDataPolygon(
#if     NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* n */
#endif
);

static NhlErrorTypes LlSetTrans(
#if	NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);

static NhlErrorTypes LlDataToWin(
#if	NhlNeedProto
NhlLayer	/*instance*/,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*  /*ymissing*/,
int* 	/*status*/
#endif
);
static NhlErrorTypes LlWinToNDC(
#if	NhlNeedProto
NhlLayer	/*instance*/,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*  /*ymissing*/,
int* 	/*status*/
#endif
);


static NhlErrorTypes LlNDCToWin(
#if	NhlNeedProto
NhlLayer	/*instance*/,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*  /*ymissing*/,
int* 	/*status*/
#endif
);


NhlLogLinTransObjClassRec NhllogLinTransObjClassRec = {
        {
/* class_name			*/	"logLinTransformationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLogLinTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhltransObjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/   	resources,
/* num_resources		*/     	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	LlTransInitialize,
/* layer_set_values		*/	LlTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LlTransDestroy
        },
        {
/* set_trans		*/	LlSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	LlWinToNDC,
/* ndc_to_win		*/	LlNDCToWin,
/* data_to_win		*/	LlDataToWin, /* One To One for this Transformation */
/* win_to_data		*/	LlDataToWin, /* One To One for this Transformation */
/* data_to_compc	*/	LlDataToWin,
/* compc_to_data	*/	LlDataToWin,
/* win_to_compc		*/	LlDataToWin,
/* compc_to_win		*/	LlDataToWin,
/* data_lineto 		*/      LlDataLineTo,
/* compc_lineto 	*/      LlDataLineTo,
/* win_lineto 		*/      LlDataLineTo,
/* NDC_lineto 		*/      LlNDCLineTo,
/* data_polygon		*/      LlDataPolygon 
        },
	{
		NULL
	}
};

NhlClass NhllogLinTransObjClass = (NhlClass)&NhllogLinTransObjClassRec;




/*
 * Function:	LlTransSetValues
 *
 * Description:	SetValues method for LogLinTrans Objects
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
static NhlErrorTypes LlTransSetValues
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
	NhlLogLinTransObjLayer lnew = (NhlLogLinTransObjLayer) new;
	NhlLogLinTransObjLayer lold = (NhlLogLinTransObjLayer) old;
	NhlString e_text, entry_name = "LlSetValues";
	float tmp;
	NhlTransObjLayerPart	*tp = &lnew->trobj;
        NhlErrorTypes ret = NhlNOERROR;

        if (lnew->lltrans.x_log && tp->x_min <= 0.0) {
                e_text =
                    "%s: invalid range for X Axis log mode; setting %s off";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrXLog);
                lnew->lltrans.x_log = False;
                ret = NhlWARNING;
	}
        if (lnew->lltrans.y_log && tp->y_min <= 0.0) {
                e_text =
                    "%s: invalid range for Y Axis log mode; setting %s off";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrYLog);
                lnew->lltrans.y_log = False;
                ret = NhlWARNING;
	}

        lnew->lltrans.x_min = tp->x_min;
        lnew->lltrans.y_min = tp->y_min;
        lnew->lltrans.x_max = tp->x_max;
        lnew->lltrans.y_max = tp->y_max;
        lnew->lltrans.x_reverse = tp->x_reverse;
        lnew->lltrans.y_reverse = tp->y_reverse;
        
	lnew->lltrans.ul = lnew->lltrans.x_min;
	lnew->lltrans.ur = lnew->lltrans.x_max;
	lnew->lltrans.ut = lnew->lltrans.y_max;
	lnew->lltrans.ub = lnew->lltrans.y_min;
	if(lnew->lltrans.x_reverse) {
		tmp = lnew->lltrans.ul;
		lnew->lltrans.ul = lnew->lltrans.ur;
		lnew->lltrans.ur = tmp;
	}
	if(lnew->lltrans.y_reverse) {
		tmp = lnew->lltrans.ut;
		lnew->lltrans.ut = lnew->lltrans.ub;
		lnew->lltrans.ub = tmp;
	}
	if((lnew->lltrans.y_log)&&(lnew->lltrans.x_log)) {
		lnew->lltrans.log_lin_value = 4;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrXMinF,NhlNtrXMaxF);
			return(NhlFATAL);
		}
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrYMinF,NhlNtrYMaxF);
			return(NhlFATAL);
		}
	} else if(lnew->lltrans.x_log) {
		lnew->lltrans.log_lin_value = 3;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrXMinF,NhlNtrXMaxF);
			return(NhlFATAL);
		}
	} else if(lnew->lltrans.y_log) {
		lnew->lltrans.log_lin_value = 2;
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrYMinF,NhlNtrYMaxF);
			return(NhlFATAL);
		}
	} else {
		lnew->lltrans.log_lin_value = 1;
	}

	if (lnew->lltrans.ul != lold->lltrans.ul ||
	    lnew->lltrans.ur != lold->lltrans.ur ||
	    lnew->lltrans.ub != lold->lltrans.ub ||
	    lnew->lltrans.ut != lold->lltrans.ut ||
	    lnew->lltrans.log_lin_value != lold->lltrans.log_lin_value)
		lnew->trobj.change_count++;

	if (lnew->lltrans.x_min != lold->lltrans.x_min) {
		free(lnew->lltrans.xmin_dat);
		if ((lnew->lltrans.xmin_dat = 
		     _NhlCmpFSetup(lnew->lltrans.x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (lnew->lltrans.x_max != lold->lltrans.x_max) {
		free(lnew->lltrans.xmax_dat);
		if ((lnew->lltrans.xmax_dat = 
		     _NhlCmpFSetup(lnew->lltrans.x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (lnew->lltrans.y_min != lold->lltrans.y_min) {
		free(lnew->lltrans.ymin_dat);
		if ((lnew->lltrans.ymin_dat =
		     _NhlCmpFSetup(lnew->lltrans.y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (lnew->lltrans.y_max != lold->lltrans.y_max) {
	        free(lnew->lltrans.ymax_dat);
		if ((lnew->lltrans.ymax_dat = 
		     _NhlCmpFSetup(lnew->lltrans.y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (lnew->lltrans.x_log) {
		if (lnew->lltrans.x_log != lold->lltrans.x_log ||
                        lnew->lltrans.x_min != lold->lltrans.x_min) {
			if (lnew->lltrans.log_xmin_dat != NULL)
				free(lnew->lltrans.log_xmin_dat);
			if ((lnew->lltrans.log_xmin_dat = 
			  _NhlCmpFSetup((float)log10(lnew->lltrans.x_min),5)) 
			    == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				    "%s: error setting up compare information",
					  entry_name);
				return(NhlFATAL);
			}
		}
		if (lnew->lltrans.x_log != lold->lltrans.x_log ||
                    lnew->lltrans.x_max != lold->lltrans.x_max) {
			if (lnew->lltrans.log_xmax_dat != NULL)
				free(lnew->lltrans.log_xmax_dat);
			if ((lnew->lltrans.log_xmax_dat = 
			  _NhlCmpFSetup((float)log10(lnew->lltrans.x_max),5)) 
			    == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				    "%s: error setting up compare information",
					  entry_name);
				return(NhlFATAL);
			}
		}
	}

	if (lnew->lltrans.y_log) {
		if (lnew->lltrans.y_log != lold->lltrans.y_log ||
                    lnew->lltrans.y_min != lold->lltrans.y_min) {
			if (lnew->lltrans.log_ymin_dat != NULL)
				free(lnew->lltrans.log_ymin_dat);
			if ((lnew->lltrans.log_ymin_dat = 
			  _NhlCmpFSetup((float)log10(lnew->lltrans.y_min),5)) 
			    == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				    "%s: error setting up compare information",
					  entry_name);
				return(NhlFATAL);
			}
		}
		if (lnew->lltrans.y_log != lold->lltrans.y_log ||
                    lnew->lltrans.y_max != lold->lltrans.y_max) {
			if (lnew->lltrans.log_ymax_dat != NULL)
				free(lnew->lltrans.log_ymax_dat);
			if ((lnew->lltrans.log_ymax_dat = 
			  _NhlCmpFSetup((float)log10(lnew->lltrans.y_max),5)) 
			    == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
				    "%s: error setting up compare information",
					  entry_name);
				return(NhlFATAL);
			}
		}
	}
            /* HACK!! Setting superclass private values -- otherwise would
             need to use a SetValues hook for TransObj -- also would need to
             add an Initialize hook. Note that LogLinTransObj does not
             care about these values but IrregularTransObj needs them to be
             up to date */
        
        tp->x_min_set = tp->x_max_set = False;
        tp->y_min_set = tp->y_max_set = False;
	
	return(ret);

}

/*
 * Function:	LlTransInitialize
 *
 * Description: Initialize function for LogLinTransObjs. Performs same
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
static NhlErrorTypes LlTransInitialize
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
	NhlLogLinTransObjLayer lnew = (NhlLogLinTransObjLayer) new;
	NhlString e_text, entry_name = "LlSetValues";
	NhlTransObjLayerPart	*tp = &lnew->trobj;
	float tmp;
        NhlErrorTypes ret = NhlNOERROR;

	tp->change_count++;
        
        if (! tp->x_min_set) {
                tp->x_min = lnew->lltrans.x_log ? 0.1 : 0.0;
        }
        else if (lnew->lltrans.x_log && tp->x_min <= 0.0) {
                e_text =
                    "%s: invalid range for X Axis log mode; setting %s off";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrXLog);
                lnew->lltrans.x_log = False;
                ret = NhlWARNING;
	}
        if (! tp->y_min_set) {
                tp->y_min = lnew->lltrans.y_log ? 0.1 : 0.0;
        }
        else if (lnew->lltrans.y_log && tp->y_min <= 0.0) {
                e_text =
                    "%s: invalid range for Y Axis log mode; setting %s off";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrYLog);
                lnew->lltrans.y_log = False;
                ret = NhlWARNING;
	}
        lnew->lltrans.x_min = tp->x_min;
        lnew->lltrans.y_min = tp->y_min;
        lnew->lltrans.x_max = tp->x_max;
        lnew->lltrans.y_max = tp->y_max;
        lnew->lltrans.x_reverse = tp->x_reverse;
        lnew->lltrans.y_reverse = tp->y_reverse;
        
	lnew->lltrans.ul = lnew->lltrans.x_min;
	lnew->lltrans.ur = lnew->lltrans.x_max;
	lnew->lltrans.ut = lnew->lltrans.y_max;
	lnew->lltrans.ub = lnew->lltrans.y_min;
	lnew->lltrans.xmin_dat = lnew->lltrans.xmax_dat = 
		lnew->lltrans.ymin_dat = lnew->lltrans.ymax_dat = NULL;
	lnew->lltrans.xmin_ndc_dat = lnew->lltrans.xmax_ndc_dat = 
		lnew->lltrans.ymin_ndc_dat = lnew->lltrans.ymax_ndc_dat = NULL;
	lnew->lltrans.log_xmin_dat = lnew->lltrans.log_xmax_dat = 
		lnew->lltrans.log_ymin_dat = lnew->lltrans.log_ymax_dat = NULL;
				
	if(lnew->lltrans.x_reverse) {
		tmp = lnew->lltrans.ul;
		lnew->lltrans.ul = lnew->lltrans.ur;
		lnew->lltrans.ur = tmp;
	}
	if(lnew->lltrans.y_reverse) {
		tmp = lnew->lltrans.ut;
		lnew->lltrans.ut = lnew->lltrans.ub;
		lnew->lltrans.ub = tmp;
	}
	if((lnew->lltrans.x_log)&&(lnew->lltrans.y_log)) {
		lnew->lltrans.log_lin_value = 4;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrXMinF,NhlNtrXMaxF);
			return(NhlFATAL);
		}
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrYMinF,NhlNtrYMaxF);
			return(NhlFATAL);
		}
	} else if(lnew->lltrans.x_log) {
		lnew->lltrans.log_lin_value = 3;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrXMinF,NhlNtrXMaxF);
			return(NhlFATAL);
		}
	} else if(lnew->lltrans.y_log) {
		lnew->lltrans.log_lin_value = 2;
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){
			e_text = 
	   "%s: Either %s or %s has been set to <= 0 for a log transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtrYMinF,NhlNtrYMaxF);
			return(NhlFATAL);
		}
	} else {
		lnew->lltrans.log_lin_value = 1;
	}

	if ((lnew->lltrans.xmin_dat = 
	     _NhlCmpFSetup(lnew->lltrans.x_min,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if ((lnew->lltrans.xmax_dat = 
	     _NhlCmpFSetup(lnew->lltrans.x_max,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if ((lnew->lltrans.ymin_dat =
	     _NhlCmpFSetup(lnew->lltrans.y_min,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if ((lnew->lltrans.ymax_dat = 
	     _NhlCmpFSetup(lnew->lltrans.y_max,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if (lnew->lltrans.x_log) {
		if ((lnew->lltrans.log_xmin_dat = 
		     _NhlCmpFSetup((float)log10(lnew->lltrans.x_min),5)) 
		    == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
		if ((lnew->lltrans.log_xmax_dat = 
		     _NhlCmpFSetup((float)log10(lnew->lltrans.x_max),5)) 
		    == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (lnew->lltrans.y_log) {
		if ((lnew->lltrans.log_ymin_dat =
		     _NhlCmpFSetup((float)log10(lnew->lltrans.y_min),5)) 
		    == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
		if ((lnew->lltrans.log_ymax_dat = 
		     _NhlCmpFSetup((float)log10(lnew->lltrans.y_max),5)) 
		    == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
            /* HACK!! Setting superclass private values -- otherwise would
             need to use a SetValues hook for TransObj -- also would need to
             add an Initialize hook. Note that LogLinTransObj does not
             care about these values but IrregularTransObj needs them to be
             up to date */
        
        tp->x_min_set = tp->x_max_set = False;
        tp->y_min_set = tp->y_max_set = False;

	return(ret);

}

/*
 * Function:	LlTransDestroy
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
static NhlErrorTypes LlTransDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlLogLinTransObjLayer ll = 
		(NhlLogLinTransObjLayer)inst;


 	free(ll->lltrans.xmin_dat);
	free(ll->lltrans.xmax_dat);
	free(ll->lltrans.ymin_dat);
	free(ll->lltrans.ymax_dat);
 	free(ll->lltrans.xmin_ndc_dat);
	free(ll->lltrans.xmax_ndc_dat);
	free(ll->lltrans.ymin_ndc_dat);
	free(ll->lltrans.ymax_ndc_dat);
	if (ll->lltrans.log_xmin_dat != NULL)
		free(ll->lltrans.log_xmin_dat);
	if (ll->lltrans.log_xmax_dat != NULL)
		free(ll->lltrans.log_xmax_dat);
	if (ll->lltrans.log_ymin_dat != NULL)
		free(ll->lltrans.log_ymin_dat);
	if (ll->lltrans.log_ymax_dat != NULL)
		free(ll->lltrans.log_ymax_dat);

	return NhlNOERROR;
}


/*
 * Function:	LlSetTrans
 *
 * Description: set_trans method for LogLinTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the LogLinTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes LlSetTrans
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
	NhlLogLinTransObjLayer	linstance = (NhlLogLinTransObjLayer)tobj;
	NhlTransObjLayerPart	*tp = &linstance->trobj;
	NhlString		entry_name = "LlSetTrans";
	NhlErrorTypes		ret;
	float xr,yb;
	
	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	xr = tp->x + tp->width;
	yb = tp->y - tp->height;
        
	return(_NhlTransLLUSet(tp->x,xr,yb,tp->y,
			       linstance->lltrans.ul,linstance->lltrans.ur,
			       linstance->lltrans.ub,linstance->lltrans.ut,
			       linstance->lltrans.log_lin_value,
                               &linstance->trobj.off_screen,
                               entry_name));
	
}

/*
 * Function:	win_compare_check
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
static NhlBoolean win_compare_check
#if	NhlNeedProto
(
	NhlLogLinTransObjLayerPart *llp,
	float	*x,
 	float	*y
)
#else
(llp,x,y)
	NhlLogLinTransObjLayerPart *llp;
	float	*x;
	float	*y;
#endif
{
        int xmndif,xmxdif,ymndif,ymxdif;

	if ((xmndif = _NhlCmpF(*x,llp->xmin_dat)) < 0 ||
	    (xmxdif = _NhlCmpF(*x,llp->xmax_dat)) > 0 ||
	    (ymndif = _NhlCmpF(*y,llp->ymin_dat)) < 0 ||
	    (ymxdif = _NhlCmpF(*y,llp->ymax_dat)) > 0) {
		return False;
	}
	return True;
}


/*
 * Function:	log_compare_check
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
static NhlBoolean log_compare_check
#if	NhlNeedProto
(
	NhlLogLinTransObjLayerPart *llp,
	float	*x,
 	float	*y,
	int logmode
)
#else
(llp,x,y,logmode)
	NhlLogLinTransObjLayerPart *llp;
	float	*x;
	float	*y;
	int     logmode;
#endif
{
        int xmndif,xmxdif,ymndif,ymxdif;

	switch (logmode) {
	case 4:

		if ((xmndif = _NhlCmpF(*x,llp->log_xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,llp->log_xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,llp->log_ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,llp->log_ymax_dat)) > 0) {
			return False;
		}
		break;
	case 3:
		if ((xmndif = _NhlCmpF(*x,llp->log_xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,llp->log_xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,llp->ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,llp->ymax_dat)) > 0) {
			return False;
		}
		break;
	case 2:
		if ((xmndif = _NhlCmpF(*x,llp->xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,llp->xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,llp->log_ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,llp->log_ymax_dat)) > 0) {
			return False;
		}
		break;
	default:
		if ((xmndif = _NhlCmpF(*x,llp->xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,llp->xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,llp->ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,llp->ymax_dat)) > 0) {
			return False;
		}
		break;
	}
	return True;
}

/*ARGSUSED*/
static NhlErrorTypes LlDataToWin
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
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
	int *	status;
#endif
{
	NhlLogLinTransObjLayer linst = (NhlLogLinTransObjLayer)instance;
	int i; 

	*status = 0;

	for(i = 0; i< n; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
		   ||((ymissing != NULL)&&(*ymissing == y[i]))
		   ||(x[i] < linst->lltrans.x_min)
		   ||(x[i] > linst->lltrans.x_max)
		   ||(y[i] < linst->lltrans.y_min)
		   ||(y[i] > linst->lltrans.y_max)) {
			if (! win_compare_check(&linst->lltrans,&x[i],&y[i])) {
				*status = 1;
				xout[i]=yout[i]=linst->trobj.out_of_range;
				continue;
			}
		}
		xout[i] = x[i];
		yout[i] = y[i];

	}
	return(NhlNOERROR);
}

/*
 * Function:	LlWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the LogLinTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes LlWinToNDC
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
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
	int *	status;
#endif
{
	NhlLogLinTransObjLayer	linstance = (NhlLogLinTransObjLayer)instance;
	NhlTransObjLayerPart	*tp = &linstance->trobj;
	int i;
	float urtmp,ultmp,uttmp,ubtmp;
	float xmin,ymin,xmax,ymax;
	float tmpx,tmpy;
	
	*status = 0;
	switch(linstance->lltrans.log_lin_value) {
	case 4:
/*
*XLogYLog case
*/
		urtmp = (float)log10(linstance->lltrans.ur);
		ultmp = (float)log10(linstance->lltrans.ul);
		uttmp = (float)log10(linstance->lltrans.ut);
		ubtmp = (float)log10(linstance->lltrans.ub);
	
		xmin = MIN(urtmp,ultmp);
		xmax = MAX(urtmp,ultmp);
		ymin = MIN(uttmp,ubtmp);
		ymax = MAX(uttmp,ubtmp);
	
		for(i = 0; i< n; i++) {
			if((x[i] > 0.0)||(y[i] > 0.0)) {
				tmpx = log10(x[i]);
				tmpy = log10(y[i]);

				if(((xmissing != NULL) &&(*xmissing == x[i]))
				   ||((ymissing != NULL) &&(*ymissing == y[i]))
				   ||(tmpx < xmin)||(tmpx > xmax)
				   ||(tmpy < ymin)||(tmpy > ymax)) {
					if (! log_compare_check(
					  &linstance->lltrans,&tmpx,&tmpy,4)) {
						*status = 1;
						xout[i]=yout[i]=
					        linstance->trobj.out_of_range;
						continue;
					}
				}
				strans(ultmp,urtmp,ubtmp,uttmp, 
				       tp->x,tp->x+tp->width,
				       tp->y-tp->height,tp->y,
				       tmpx, tmpy, 
				       &(xout[i]),&(yout[i]));
			} else {
				*status = 1;	
				xout[i] = yout[i] =
					linstance->trobj.out_of_range;
			}
		}
		break;
	case 3:
/*
*XLogYLin case
*/
		urtmp = (float)log10(linstance->lltrans.ur);
		ultmp = (float)log10(linstance->lltrans.ul);
		xmin = MIN(urtmp,ultmp);
		xmax = MAX(urtmp,ultmp);
		
		for(i = 0; i< n; i++) {
			if(x[i] > 0) {
				tmpx = log10(x[i]);
				if(((xmissing != NULL)&&(*xmissing == x[i]))
				   ||((ymissing != NULL)&&(*ymissing == y[i]))
				   ||(tmpx < xmin)||(tmpx > xmax)
				   ||(y[i]<linstance->lltrans.y_min)
				   ||(y[i]>linstance->lltrans.y_max)) {
					if (! log_compare_check(
					 &linstance->lltrans,&tmpx,&y[i],3)) {
						*status = 1;
						xout[i]=yout[i]=
					        linstance->trobj.out_of_range;
						continue;
					}
				}
				strans(ultmp,urtmp,
				       linstance->lltrans.ub, 
				       linstance->lltrans.ut,
				       tp->x,tp->x+tp->width,
				       tp->y-tp->height,tp->y,
				       tmpx,y[i], 
				       &(xout[i]),&(yout[i]));
			}
			else {
				*status = 1;
				xout[i]=yout[i]=linstance->trobj.out_of_range;
			}
		}
		break;
	case 2:
/*
*XLinYLog case
*/
		uttmp = (float)log10(linstance->lltrans.ut);
		ubtmp = (float)log10(linstance->lltrans.ub);
		ymin = MIN(uttmp,ubtmp);
		ymax = MAX(uttmp,ubtmp);
		for(i = 0; i< n; i++) {
			if(y[i] > 0) {
				tmpy = log10(y[i]);
				if(((xmissing != NULL) &&(*xmissing == x[i]))
				   ||((ymissing != NULL)&&(*ymissing == y[i]))
				   ||(x[i] < linstance->lltrans.x_min)
				   ||(x[i] > linstance->lltrans.x_max)
				   ||(tmpy < ymin)
				   ||(tmpy > ymax)) {
					if (! log_compare_check(
					 &linstance->lltrans,&x[i],&tmpy,2)) {
						*status = 1;
						xout[i]=yout[i]=
					        linstance->trobj.out_of_range;
						continue;
					}
				}
				strans(linstance->lltrans.ul, 
				       linstance->lltrans.ur,
				       ubtmp,uttmp, 
				       tp->x,tp->x+tp->width,
				       tp->y-tp->height,tp->y,
				       x[i],tmpy,
				       &(xout[i]),&(yout[i]));
			} else {	
				*status = 1;
				xout[i]=yout[i]=linstance->trobj.out_of_range;
			}
		}
		break;
	case 1:
/*
*XLinYLin
*/
		for(i = 0; i< n; i++) {
			if(((xmissing != NULL) &&(*xmissing == x[i]))
			   ||((ymissing != NULL) &&(*ymissing == y[i])) 
			   ||(x[i] < linstance->lltrans.x_min)
			   ||(x[i] > linstance->lltrans.x_max)
			   ||(y[i] < linstance->lltrans.y_min)
			   ||(y[i] > linstance->lltrans.y_max)) {
				if (! log_compare_check(
					 &linstance->lltrans,&x[i],&y[i],1)) {
					*status = 1;
					xout[i]=yout[i]=
					        linstance->trobj.out_of_range;
					continue;
				}
			}
			strans(linstance->lltrans.ul, 
			       linstance->lltrans.ur, 
			       linstance->lltrans.ub, 
			       linstance->lltrans.ut, 
			       tp->x,tp->x+tp->width,
			       tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
		break;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Internal Error in LlNDCToWin");
		return(NhlFATAL);
	}

	return NhlNOERROR;
}


/*
 * Function:	LlNDCToWin
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
static NhlErrorTypes LlNDCToWin
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int *status)
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
	int *status;
#endif
{
	int i;
	NhlLogLinTransObjLayer linstance = (NhlLogLinTransObjLayer)instance;
	float urtmp,ultmp,uttmp,ubtmp;
	float xmin,ymin,xmax,ymax;
	
	
	xmin = linstance->trobj.x;
	xmax = linstance->trobj.x + linstance->trobj.width;
	ymin = linstance->trobj.y - linstance->trobj.height;
	ymax = linstance->trobj.y;

	*status = 0;
	switch(linstance->lltrans.log_lin_value) {
		case 4:
/*
*XLogYLog case
*/
			urtmp = (float)log10(linstance->lltrans.ur);
			ultmp = (float)log10(linstance->lltrans.ul);
			uttmp = (float)log10(linstance->lltrans.ut);
			ubtmp = (float)log10(linstance->lltrans.ub);
	

			for(i = 0; i< n; i++) {
				if(((xmissing != NULL) && (*xmissing == x[i]))
					||
					((ymissing != NULL) &&
							(*ymissing == y[i]))
					||(x[i] < xmin)
					||(x[i] > xmax)
					||(y[i] < ymin)
					||(y[i] > ymax)) {
			
					*status = 1;
					xout[i] = yout[i] =
						linstance->trobj.out_of_range;
				} else {
		
					strans(xmin,xmax,ymin,ymax,ultmp,urtmp,
						ubtmp, uttmp, x[i],y[i], 
						&(xout[i]),&(yout[i]));
					xout[i]=(float)pow((double)10.0,
							(double)xout[i]);
					yout[i]=(float) pow((double)10.0,
							(double)yout[i]);
				}
			}
		
			break;
		case 3:
/*
*XLogYLin case
*/
			urtmp = (float)log10(linstance->lltrans.ur);
			ultmp = (float)log10(linstance->lltrans.ul);
		
			for(i = 0; i< n; i++) {
				if(((xmissing != NULL)&&(*xmissing == x[i]))
					||((ymissing != NULL)&&(*ymissing == y[i]))
					||(x[i] < xmin)
					||(x[i] > xmax)
					||(y[i] < ymin)
					||(y[i] > ymax)) {
		
					*status = 1;
					xout[i]=yout[i]=linstance->trobj.out_of_range;
				} else {
					strans(xmin,xmax,ymin,ymax,ultmp,
						urtmp, linstance->lltrans.ub,
						linstance->lltrans.ut, x[i],y[i],
						&(xout[i]),&(yout[i]));
						xout[i] = (float) pow((double)10.0,
								(double) xout[i]);
				}
			}
			break;
		case 2:
/*
*XLinYLog case
*/
			uttmp = (float)log10(linstance->lltrans.ut);
			ubtmp = (float)log10(linstance->lltrans.ub);

			for(i = 0; i< n; i++) {
				if(((xmissing != NULL)&&(*xmissing == x[i]))
					||((ymissing != NULL)&&(*ymissing == y[i]))
					||(x[i] < xmin)
					||(x[i] > xmax)
					||(y[i] < ymin)
					||(y[i] > ymax)) {

					*status = 1;
					xout[i]=yout[i]=linstance->trobj.out_of_range;
				} else {
					strans(xmin,xmax,ymin,ymax,
						linstance->lltrans.ul,
						linstance->lltrans.ur, 
						ubtmp,uttmp, 
						x[i],y[i],
						&(xout[i]),&(yout[i]));
					yout[i]=(float) pow((double)10.0,
							(double)yout[i]);
				}
			}
			break;
		case 1:
/*
*XLinYLin
*/
			for(i = 0; i< n; i++) {
				if(((xmissing != NULL)&&(*xmissing == x[i]))
					||((ymissing != NULL)&&(*ymissing == y[i]))
					||(x[i] < xmin)
					||(x[i] > xmax)
					||(y[i] < ymin)
					||(y[i] > ymax)) {

					*status = 1;
					xout[i]=yout[i]=linstance->trobj.out_of_range;
				} else {
					strans(xmin,xmax,ymin,ymax,
						linstance->lltrans.ul,
						linstance->lltrans.ur, 
						linstance->lltrans.ub,
						linstance->lltrans.ut, 
						x[i],y[i],
						&(xout[i]),&(yout[i]));
				}
			}
			break;
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Internal Error in LlNDCToWin");
			return(NhlFATAL);
	}
	return NhlNOERROR;
}


static NhlErrorTypes AdjustToEdge
#if	NhlNeedProto
(NhlLogLinTransObjLayer llinst, 
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
(irinst,xclip,yclip,x,y,xd, yd,xc,yc)
NhlLogLinTransObjLayer llinst;
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
	NhlLogLinTransObjLayerPart *llp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	float xt,yt;
	int i,status = 1;

	xt = xclip;
	yt = yclip;

	for (i=0; i < 2; i++) {

		if (x != xclip) {
			if (_NhlCmpFAny2(xt,llp->x_min,5,1e-36) <= 0) {
				*xd = llp->x_min;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
			else if (_NhlCmpFAny2(xt,llp->x_max,6,1e-36) >= 0.0) {
				*xd = llp->x_max;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
		}
		if (y != yclip) {
			if (_NhlCmpFAny2(yt,llp->y_min,6,1e-36) <= 0.0) {
				*yd = llp->y_min;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
			else if (_NhlCmpFAny2(yt,llp->y_max,6,1e-36) >= 0.0) {
				*yd = llp->y_max;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
		}
		LlDataToWin((NhlLayer)llinst,xd,yd,1,
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



static NhlErrorTypes LogAdjustToEdge
#if	NhlNeedProto
(NhlLogLinTransObjLayer llinst, 
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
(irinst,xclip,yclip,x,y,xd, yd,xc,yc)
NhlLogLinTransObjLayer llinst;
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
	NhlLogLinTransObjLayerPart *llp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	float xt,yt;
	int i,status = 1;
	float lg_xclip,lg_yclip,lg_x,lg_y;

	xt = xclip;
	yt = yclip;
	if (llp->x_log && llp->y_log) {
		lg_xclip = log10(xclip);
		lg_x = log10(x);
		lg_yclip = log10(yclip);
		lg_y = log10(y);
	}
	else if (llp->x_log) {
		lg_xclip = log10(xclip);
		lg_x = log10(x);

	}
	else if (llp->y_log) {
		lg_yclip = log10(yclip);
		lg_y = log10(y);
	}


	for (i=0; i < 2; i++) {
		int keep = 0;

		if (x != xclip) {
			if (_NhlCmpFAny2(xt,llp->x_min,5,1e-36) <= 0.0) {
				*xd = llp->x_min;
				keep = 1;
			}
			else if (_NhlCmpFAny2(xt,llp->x_max,5,1e-36) >= 0.0) {
				*xd = llp->x_max;
				keep = 1;
			}

			if (llp->x_log && llp->y_log) {
				float l_yd = lg_yclip + (lg_y - lg_yclip) *
					(log10(*xd)-lg_xclip)/(lg_x-lg_xclip);
				*yd = pow(10.0,l_yd);
			}
			else if (llp->x_log) {
				*yd = yclip + (y-yclip) *
					(log10(*xd)-lg_xclip)/(lg_x-lg_xclip);
			}
			else if (llp->y_log) {
				float l_yd = lg_yclip + (lg_y - lg_yclip) *
					(*xd - xclip)/(x-xclip);
				*yd = pow(10.0,l_yd);
			}
			else {
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}

		}
		if (y != yclip && ! keep) {
			if (_NhlCmpFAny2(yt,llp->y_min,5,1e-36) <= 0.0) {
				*yd = llp->y_min;
			}
			else if (_NhlCmpFAny2(yt,llp->y_max,5,1e-36) >= 0.0) {
				*yd = llp->y_max;
			}

			if (llp->x_log && llp->y_log) {
				float l_xd = lg_xclip + (lg_x - lg_xclip) *
					(log10(*yd)-lg_yclip)/(lg_y-lg_yclip);
				*xd = pow(10.0,l_xd);
			}
			else if (llp->x_log) {
				float l_xd = lg_xclip + (lg_x - lg_xclip) *
					(*yd-yclip)/(y-yclip);
				*xd = pow(10.0,l_xd);
			}
			else if (llp->y_log) {
				*xd = xclip +(x-xclip) * 
					(log10(*yd)-lg_yclip)/(lg_y-lg_yclip);
			}
			else {
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
		}
		LlDataToWin((NhlLayer)llinst,xd,yd,1,
				      xc,yc,NULL,NULL,&status);
		if (status) {
			xt = *xd;
			yt = *yd;
			continue;
		}
	}
	if (status) {
		/* default to x and y (which are supposed to be inside) */
		*xd = x;
		*yd = y;
		LlDataToWin((NhlLayer)llinst,xd,yd,1,
				      xc,yc,NULL,NULL,&status);

		if (status)
			return NhlWARNING;
	}
	return NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes LogDataLineTo
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
	NhlLogLinTransObjLayer llinst = (NhlLogLinTransObjLayer)instance;
	NhlLogLinTransObjLayerPart *ltp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float holdx,holdy;
	float xpoints[2];
	float ypoints[2];
	int status;
	int i,npoints = 256;
	float xdist,ydist,xc,yc,xd,yd;

	npoints = llinst->trobj.point_count;
/*
* if true the moveto is being performed
*/
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd =1;
		return NhlNOERROR;
	} 

	currentx = x;
	currenty = y;
	holdx = lastx;
	holdy = lasty;
	_NhlTransClipLine(ltp->x_min,ltp->x_max,ltp->y_min,ltp->y_max,
			  &lastx,&lasty,&currentx,&currenty,
			  llinst->trobj.out_of_range);
	if((lastx == llinst->trobj.out_of_range)
	   ||(lasty == llinst->trobj.out_of_range)
	   ||(currentx == llinst->trobj.out_of_range)
	   ||(currenty == llinst->trobj.out_of_range)){
		/*
		 * Line has gone completely out of window
		 */
		lastx = x;	
		lasty = y;
		call_frstd = 1;
		return NhlNOERROR;
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
	LlWinToNDC(instance,xpoints,ypoints,2,
		   xpoints,ypoints,NULL,NULL,&status);

	xdist = xpoints[1] - xpoints[0];
	ydist = ypoints[1] - ypoints[0];
	npoints = (int) ((float)npoints * (fabs(xdist)+fabs(ydist)));
	npoints = npoints < 1 ? 1 : npoints;

/*
 * If not clipped things are simpler
 */
	if (lastx == holdx && currentx == x && 
	    lasty == holdy && currenty == y) {
		if (call_frstd == 1) {
			_NhlWorkstationLineTo(llinst->trobj.wkptr,
				      xpoints[0],ypoints[0],1);
			call_frstd = 2;
		}
		xdist = currentx - lastx;
		ydist = currenty - lasty;
		for (i = 0; i<npoints; i++) {
			xd = lastx + xdist *(i+1)/ (float)npoints;
			yd = lasty + ydist *(i+1)/ (float)npoints;
			_NhlWorkstationLineTo(llinst->trobj.wkptr,
					      c_cufx(xd),c_cufy(yd),0);
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
		if (npoints == 1) {
			if (LogAdjustToEdge(llinst,holdx,holdy,
					 x,y,&xd,&yd,&xc,&yc) < NhlNOERROR)
				return NhlFATAL;
		}
		else if (AdjustToEdge(llinst,holdx,holdy,
				      x,y,&xd,&yd,&xc,&yc) < NhlNOERROR) {
			return NhlFATAL;
		}
		lastx = xd;
		lasty = yd;
		xdist = x - lastx;
		ydist = y - lasty;
		_NhlWorkstationLineTo(llinst->trobj.wkptr,
				      c_cufx(xc),c_cufy(yc),1);
	}
	else if (call_frstd == 1) {
		_NhlWorkstationLineTo(llinst->trobj.wkptr,
				      xpoints[0],ypoints[0],1);
	}
	call_frstd = 2;

	for (i = 0; i< npoints; i++) {
		xd = lastx + xdist *(i+1)/(float)npoints;
		yd = lasty + ydist *(i+1)/(float)npoints;
		LlDataToWin(instance,&xd,&yd,1,&xc,&yc,NULL,NULL,&status);
		if (status) {
			if (npoints == 1) {
				if (LogAdjustToEdge 
				    (llinst,x,y,holdx,holdy,
				     &xd,&yd,&xc,&yc) < NhlNOERROR)
					return NhlFATAL;
			}
			else if (AdjustToEdge(llinst,x,y,holdx,holdy,
					      &xd,&yd,&xc,&yc) < NhlNOERROR) {
				return NhlFATAL;
			}
		}
		_NhlWorkstationLineTo(llinst->trobj.wkptr,
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
static NhlErrorTypes LlDataLineTo
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
	NhlErrorTypes ret = NhlNOERROR,subret;
	NhlLogLinTransObjLayer llinst = (NhlLogLinTransObjLayer)instance;
	NhlLogLinTransObjLayerPart *ltp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float holdx,holdy;

	if (ltp->x_log || ltp->y_log) {
		if ((ltp->x_log && ! x > 0.0) || 
		    (ltp->y_log && ! y > 0.0)) {
			char e_text[] =
				"%s: point %f,%f outside data domain";
			NhlPError(NhlWARNING,
				  NhlEUNKNOWN,e_text,"LlDataLineTo",x,y);
			ret = MIN(ret,NhlWARNING);
		}
		subret = LogDataLineTo(instance,x,y,upordown);
		return MIN(ret,subret);
	}
/*
* if true the moveto is being performed
*/
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd =1;
	} else {
		currentx = x;
		currenty = y;
		holdx = lastx;
		holdy = lasty;
		_NhlTransClipLine(ltp->x_min,ltp->x_max,
				  ltp->y_min,ltp->y_max,
				  &lastx,&lasty,&currentx,&currenty,
				  llinst->trobj.out_of_range);
		if((lastx == llinst->trobj.out_of_range)
		   ||(lasty == llinst->trobj.out_of_range)
		   ||(currentx == llinst->trobj.out_of_range)
		   ||(currenty == llinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
			return NhlNOERROR;
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
			if(call_frstd == 1) {
				_NhlWorkstationLineTo(llinst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);
				call_frstd = 2;
			}
			_NhlWorkstationLineTo(llinst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);
			lastx = x;
			lasty = y;
			return(NhlNOERROR);
		}
			
			
	}
	return NhlNOERROR;
	
}

/*ARGSUSED*/
static NhlErrorTypes LlNDCLineTo
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
	NhlLogLinTransObjLayer	llinst = (NhlLogLinTransObjLayer)instance;
	NhlTransObjLayerPart	*tp = &llinst->trobj;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	NhlErrorTypes ret = NhlNOERROR,ret1 = NhlNOERROR;
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
		_NhlTransClipLine(tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			&lastx, &lasty, &currentx, &currenty,
			llinst->trobj.out_of_range);
		if((lastx == llinst->trobj.out_of_range)
			||(lasty == llinst->trobj.out_of_range)
			||(currentx == llinst->trobj.out_of_range)
			||(currenty == llinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx  = x;
			lasty  = y;
			call_frstd = 1;
			return(_NhlWorkstationLineTo(llinst->trobj.wkptr,x,y,1));
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
			if(call_frstd == 1) {
				ret1 = _NhlWorkstationLineTo(llinst->trobj.wkptr,lastx,lasty,1);
				call_frstd = 2;
			}
			ret = _NhlWorkstationLineTo(llinst->trobj.wkptr,currentx,currenty,0);
			lastx = x;
			lasty = y;			
			return(MIN(ret1,ret));
		}
	}
}


/*ARGSUSED*/
static NhlErrorTypes LogDataPolygon
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
	NhlErrorTypes ret = NhlNOERROR;
	NhlLogLinTransObjLayer llinst = (NhlLogLinTransObjLayer)instance;
	NhlLogLinTransObjLayerPart *llp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	NhlString e_text;
	NhlString entry_name = "LogDataPolygon";
	float out_of_range = llinst->trobj.out_of_range;
	int i,j,ixout;
	float px,py,cx,cy,dx,dy,tx,ty;
	float *xbuf,*ybuf,*dbuf,*xout,*yout;
	int *ixbuf;
	NhlBoolean open, done = False, first, firstpoint;
	int count, pcount, cix, pix, status = 0, npoints = 256;
	float xdist,ydist,tdist;

	open = (x[0] != x[n-1] || y[0] != y[n-1]) ?  True : False;
	count = pcount = open ? n + 1 : n; 
	npoints = llinst->trobj.point_count;

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
		if ((llp->x_log && ! x[i] > 0.0) || 
		    (llp->y_log && ! y[i] > 0.0)) {
			char e_text[] = "%s: point %f,%f outside data domain";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,"LogDataPolygon",x[i],y[i]);
			ret = MIN(ret,NhlWARNING);
		}
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
		_NhlTransClipLine(llp->x_min,llp->x_max,
				  llp->y_min,llp->y_max,
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
	ret = LlDataToWin(instance,xbuf,ybuf,count,
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
		npoints *= (tdist);
		/* include some extra space for safety */
		xout = NhlMalloc((npoints+count)*sizeof(float));
		yout = NhlMalloc((npoints+count)*sizeof(float));
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
				LlDataToWin(instance,&cx,&cy,1,
				      &cx,&cy,NULL,NULL,&status);
				if (! status) {
					ixout++;
					xout[ixout] = c_cufx(cx);
					yout[ixout] = c_cufy(cy);
				}
			}
		}
				
#if 0
		printf("count,pcount,npoints,ixout+1,%d,%d,%d,%d\n",
		       count,pcount,npoints,ixout+1);
#endif
		if (npoints+count < ixout+1) {
			e_text = "%s: internal error: memory overrun";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}	
		ret = MIN(ret,_NhlWorkstationFill(llinst->trobj.wkptr,
						  xout,yout,ixout+1));

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
			xorange = cx < llp->x_min || cx >llp->x_max ? 
				True : False;
			yorange = cy < llp->y_min || cy >llp->y_max ? 
				True : False;
			status = 0;
			if (xorange && ! yorange) {
				if (cx < llp->x_min) {
					cx = llp->x_min;
					xbuf[i] = llp->x_reverse ?
						1.1 : -.1;
				}
				else {
					cx = llp->x_max;
					xbuf[i] = llp->x_reverse ?
						-.1 : 1.1;
				}
				LlDataToWin(instance,&cx,&cy,1,
					    &cx,&cy,
					    NULL,NULL,&status);
				ybuf[i] = c_cufy(cy);
			}
			else if (yorange && ! xorange) {
				if (cy < llp->y_min) {
					cy = llp->y_min;
					ybuf[i] = llp->y_reverse ?
						1.1 : -.1;
				}
				else {
					cy = llp->y_max;
					ybuf[i] = llp->y_reverse ?
						-.1 : 1.1;
				}
				LlDataToWin(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				xbuf[i] = c_cufx(cx);
			}
			else {
				if (llp->x_reverse)
					xbuf[i] = cx < llp->x_min ? 
						1.1 : -.1;
				else
					xbuf[i] = cx < llp->x_min ? 
						-.1 : 1.1;

				if (llp->y_reverse)
					ybuf[i] = cy < llp->y_min ? 
						1.1 : -.1;
				else
					ybuf[i] = cy < llp->y_min ? 
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
	npoints *= tdist;
	/* include some extra space for safety */
	xout = NhlMalloc((npoints+count)*sizeof(float));
	yout = NhlMalloc((npoints+count)*sizeof(float));
	if (xout == NULL || yout == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
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
			LlDataToWin(instance,&dx,&dy,1,
				    &tx,&ty,NULL,NULL,&status);
			if (! status) {
				if (! started) {
					started = True;
					if (lcount == 1) j--;
					if (AdjustToEdge(llinst,px,py,cx,cy,
							 &dx,&dy,&tx,&ty) 
					    < NhlNOERROR)
						return NhlFATAL;
				}
				ixout++;
				xout[ixout] = c_cufx(tx);
				yout[ixout] = c_cufy(ty);
			}
			else if (started) break;
		}
		if (status) {
			if (started) {
				if (AdjustToEdge(llinst,cx,cy,px,py,
						 &dx,&dy,&tx,&ty) < NhlNOERROR)
					return NhlFATAL;
				ixout++;
				xout[ixout] = c_cufx(tx);
				yout[ixout] = c_cufy(ty);
			}
			ixout++;
			xout[ixout] = xbuf[i];
			yout[ixout] = ybuf[i];
		}
		px = x[ixbuf[i]];
		py = y[ixbuf[i]];
	}
	if (xout[ixout] != xout[0] || yout[ixout] != yout[0]) {
		ixout++;
		xout[ixout] = xout[0];
		yout[ixout] = yout[0];
	}

#if 0
	printf("count,pcount,npoints,ixout+1,%d,%d,%d,%d\n",
	       count,pcount,npoints,ixout+1);
#endif
	if (npoints+count < ixout+1) {
		e_text = "%s: internal error: memory overrun";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}	
	ret = MIN(ret,_NhlWorkstationFill
		  (llinst->trobj.wkptr,xout,yout,ixout+1));

	NhlFree(xbuf);
	NhlFree(ybuf);
	NhlFree(dbuf);
	NhlFree(xout);
	NhlFree(yout);
	return ret;
	
}

/*ARGSUSED*/
static NhlErrorTypes LlDataPolygon
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
	NhlErrorTypes ret = NhlNOERROR;
	NhlLogLinTransObjLayer llinst = (NhlLogLinTransObjLayer)instance;
	NhlLogLinTransObjLayerPart *ltp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	NhlString e_text;
	NhlString entry_name = "LlDataPolygon";
	int i;
	float *xbuf,*ybuf;
	NhlBoolean open;
	int count;
	
	if ((ltp->x_log || ltp->y_log) && 
	    llinst->trobj.line_interpolation_on) {
		return LogDataPolygon(instance,x,y,n);
	}
				
	open = (x[0] != x[n-1] || y[0] != y[n-1]) ?  True : False;
	count = open ? n + 1 : n; 

	xbuf = NhlMalloc(count * sizeof(float));
	ybuf = NhlMalloc(count * sizeof(float));
	
	if (xbuf == NULL || ybuf == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (ltp->x_log || ltp->y_log) {
		for (i=0; i<n; i++) {
			if ((ltp->x_log && ! x[i] > 0.0) || 
			    (ltp->y_log && ! y[i] > 0.0)) {
				char e_text[] = 
					"%s: point %f,%f outside data domain";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  "LlDataPolygon",x[i],y[i]);
				ret = MIN(ret,NhlWARNING);
			}
			xbuf[i] = c_cufx(x[i]);
			ybuf[i] = c_cufy(y[i]);
		}
		
	}
	else {
		for (i=0; i<n; i++) {
			xbuf[i] = c_cufx(x[i]);
			ybuf[i] = c_cufy(y[i]);
		}
	}
	if (open) {
		xbuf[n] = c_cufx(x[0]);
		ybuf[n] = c_cufy(y[0]);
	}

	ret = MIN(ret,_NhlWorkstationFill
		  (llinst->trobj.wkptr,xbuf,ybuf,count));

	NhlFree(xbuf);
	NhlFree(ybuf);
	
	return ret;
	
}
