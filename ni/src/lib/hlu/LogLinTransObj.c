/*
 *      $Id: LogLinTransObj.c,v 1.25 1996-05-16 23:46:24 dbrown Exp $
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


static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.x_min),
		NhlTString,_NhlUSET("0.0"),0,NULL},
	{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.x_max),
		NhlTString,_NhlUSET("1.0"),0,NULL},
	{ NhlNtrXLog,NhlCtrXLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.x_log),
		NhlTImmediate,_NhlUSET(False),0,NULL},
	{ NhlNtrXReverse,NhlCtrXReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.x_reverse),
		NhlTImmediate,_NhlUSET(False),0,NULL},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.y_min),
		NhlTString,_NhlUSET("0.0"),0,NULL},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.y_max),
		NhlTString,_NhlUSET("1.0"),0,NULL},
	{ NhlNtrYLog,NhlCtrYLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.y_log),
		NhlTImmediate,_NhlUSET(False),0,NULL},
	{ NhlNtrYReverse,NhlCtrYReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlLogLinTransObjLayerRec,lltrans.y_reverse),
		NhlTImmediate,_NhlUSET(False),0,NULL}

/* End-documented-resources */

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
/* class_name			*/	"logLinTransObjClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLogLinTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhltransObjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

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
	NhlTransObjLayerPart	*otp = &lold->trobj;

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
#if 0
	if (tp->x != otp->x) {
		free(lnew->lltrans.xmin_ndc_dat);
		if ((lnew->lltrans.xmin_ndc_dat = 
		     _NhlCmpFSetup(tp->x,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (tp->x != otp->x || tp->width != otp->width) {
		free(lnew->lltrans.xmax_ndc_dat);
		if ((lnew->lltrans.xmax_ndc_dat = 
		     _NhlCmpFSetup(tp->x + tp->width,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (tp->y != otp->y || tp->height != otp->height) {
		free(lnew->lltrans.ymin_ndc_dat);
		if ((lnew->lltrans.ymin_ndc_dat =
		     _NhlCmpFSetup(tp->y - tp->height,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
	if (tp->y != otp->y) {
		free(lnew->lltrans.ymax_ndc_dat);
		if ((lnew->lltrans.ymax_ndc_dat = 
		     _NhlCmpFSetup(tp->y,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  entry_name);
			return(NhlFATAL);
		}
	}
#endif
	if (lnew->lltrans.x_log) {
		if (lnew->lltrans.x_min != lold->lltrans.x_min) {
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
		if (lnew->lltrans.x_max != lold->lltrans.x_max) {
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
		if (lnew->lltrans.y_min != lold->lltrans.y_min) {
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
		if (lnew->lltrans.y_max != lold->lltrans.y_max) {
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
	
	return(NhlNOERROR);

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

	lnew->trobj.change_count++;
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

#if 0

	if ((lnew->lltrans.xmin_ndc_dat = 
	     _NhlCmpFSetup(tp->x,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if ((lnew->lltrans.xmax_ndc_dat = 
	     _NhlCmpFSetup(tp->x + tp->width,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if ((lnew->lltrans.ymin_ndc_dat =
	     _NhlCmpFSetup(tp->y - tp->height,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
	if ((lnew->lltrans.ymax_ndc_dat = 
	     _NhlCmpFSetup(tp->y,5)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: error setting up compare information",
			  entry_name);
		return(NhlFATAL);
	}
#endif
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

	return(NhlNOERROR);

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
	NhlString		e_text;
	NhlErrorTypes		ret;
	float xr, yb;
	
	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	xr = tp->x + tp->width;
	yb = tp->y - tp->height;
	if (tp->x < 0.0 || tp->y > 1.0 || xr > 1.0 || yb < 0.0) {
		e_text = "%s: View extent is outside NDC range: constraining";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		tp->x = MAX(tp->x,0.0);
		xr = MIN(xr,1.0);
		tp->y = MIN(tp->y,1.0);
		yb = MAX(yb,0.0);
		tp->width = xr - tp->x;
		tp->height = tp->y - yb;
	}
	c_set(tp->x,xr,yb,tp->y,
	      linstance->lltrans.ul,linstance->lltrans.ur,
	      linstance->lltrans.ub,linstance->lltrans.ut,
	      linstance->lltrans.log_lin_value);

	
	return(NhlNOERROR);
	
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
	NhlLogLinTransObjLayer llinst = (NhlLogLinTransObjLayer)instance;
	static float lastx,lasty;
	static call_frstd = 1;
	float currentx,currenty;
	float holdx,holdy;

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
		_NhlTransClipLine(llinst->lltrans.x_min,
			llinst->lltrans.x_max,
			llinst->lltrans.y_min,
			llinst->lltrans.y_max,
			&lastx,
			&lasty,
			&currentx,
			&currenty,
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
	static call_frstd = 1;
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
	NhlErrorTypes ret;
	NhlLogLinTransObjLayer llinst = (NhlLogLinTransObjLayer)instance;
	NhlLogLinTransObjLayerPart *ltp = 
		(NhlLogLinTransObjLayerPart *) &llinst->lltrans;
	NhlString e_text;
	NhlString entry_name = "LlDataPolygon";
	float out_of_range = llinst->trobj.out_of_range;
	int i,j;
	float px,py,cx,cy;
	float *xbuf,*ybuf;
	int *ixbuf;
	NhlBoolean open;
	NhlBoolean status = False, log_mode = False,done = False;
	int count;
	

	if (ltp->x_log) {
		for (i = 0; i < n; i++) {
			if (x[i] <= 0.0) {
				log_mode = True;
				break;
			}
		}
	}
	if (ltp->y_log && ! log_mode) {
		for (i = 0; i < n; i++) {
			if (y[i] <= 0.0) {
				log_mode = True;
				break;
			}
		}
	}
				
	open = (x[0] != x[n-1] || y[0] != y[n-1]) ?  True : False;
	count = open ? n + 1 : n; 

	if (! log_mode) {
		xbuf = NhlMalloc(count * sizeof(float));
		ybuf = NhlMalloc(count * sizeof(float));
	
		if (xbuf == NULL || ybuf == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		for (i=0; i<n; i++) {
			xbuf[i] = c_cufx(x[i]);
			ybuf[i] = c_cufy(y[i]);
		}
		if (open) {
			xbuf[n] = c_cufx(x[0]);
			ybuf[n] = c_cufy(y[0]);
		}

		ret = _NhlWorkstationFill(llinst->trobj.wkptr,xbuf,ybuf,count);

		NhlFree(xbuf);
		NhlFree(ybuf);
	
		return ret;
	}
/*
 * "log-mode", allows primitives that contain points outside the log domain
 */
	xbuf = NhlMalloc(2 * count * sizeof(float));
	ybuf = NhlMalloc(2 * count * sizeof(float));
	ixbuf = NhlMalloc(2 * count * sizeof(int));
	
	if (xbuf == NULL || ybuf == NULL || ixbuf == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (x[0] < ltp->x_min || x[0] >ltp->x_max ||  
	    y[0] < ltp->y_min || y[0] >ltp->y_max) {
		xbuf[0] = out_of_range;
		ybuf[0] = out_of_range;
		ixbuf[0] = 0;
	}
	else {
		xbuf[0] = c_cufx(x[0]);
		ybuf[0] = c_cufy(y[0]);
		ixbuf[0] = -1;
	}

	j = 1;
	i = 0;
	while (! done) {
		if (i < n - 1) {
			px = x[i];
			py = y[i];
			i++;
			cx = x[i];
			cy = y[i];
		}
		else {
			if (! open) 
				break;
			else {
				px = x[i];
				py = y[i];
				cx = x[0];
				cy = y[0];
				done = True;
			}
		}
		_NhlTransClipLine(ltp->x_min,ltp->x_max,
				  ltp->y_min,ltp->y_max,
				  &px,&py,&cx,&cy,out_of_range);

		if (px == out_of_range) {
			xbuf[j] = out_of_range;
			ybuf[j] = out_of_range;
			ixbuf[j] = i;
			j++;
		}
		else {
			if (px != x[i-1] || py != y[i-1]) {
				xbuf[j] = c_cufx(px);
				ybuf[j] = c_cufy(py);
				ixbuf[j] = -1;
				j++;
			}
			xbuf[j] = c_cufx(cx);
			ybuf[j] = c_cufy(cy);
			ixbuf[j] = -1;
			j++;
			if (cx != x[i] || cy != y[i]) {
				xbuf[j] = out_of_range;
				ybuf[j] = out_of_range;
				ixbuf[j] = i;
				j++;
			}
		}
	}
	if (j > count) status = True;
	count = j;
/*
 * Replace out of bounds point with extrapolated points in the ndc
 * space. Since they will be clipped, these points need only be 
 * topologically correct with respect to the window coordinate boundaries.
 */
	if (status) { 
		float xrat,yrat;
		float vl,vr,vb,vt,ul,ur,ub,ut;
		int ll;

		count = j;
		c_getset(&vl,&vr,&vb,&vt,&ul,&ur,&ub,&ut,&ll);

		xrat = (vr-vl) / (ur-ul);
		yrat = (vt-vb) / (ut-ub);

		for (j = 0; j < count; j++) {
			if (xbuf[j] == out_of_range) {
				if (ixbuf[j] == -1) {
					e_text = "%s: internal error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				cx = x[ixbuf[j]];
				cy = y[ixbuf[j]];
				xbuf[j] = vl + xrat * (cx - ul);
				ybuf[j] = vb + yrat * (cy - ub);
			}
		}
	}
		
	ret = _NhlWorkstationFill(llinst->trobj.wkptr,xbuf,ybuf,count);

	NhlFree(xbuf);
	NhlFree(ybuf);
	
	return ret;
	
}
