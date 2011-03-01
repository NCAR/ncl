/*
 *      $Id: IrregularTransObj.c,v 1.46.4.1 2008-03-28 20:37:35 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularTransObj.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 19 09:33:44 MDT 1992
 *
 *	Description:	Takes one coordinate system and maps it into another
 *			Both coordinate systems must be monotonic. This will
 *			be used by contour, etc to map integer grid point
 *			indexes to irregularly spaced points so they can then
 *			be drawn on the linear viewport. Traditionally a
 *			funtion CPSPS2 has been used to interpolate the 
 *			irregular grids onto regular grids. This is not needed 
 *			since conpack can contour using the integer grids and 
 *			use this irregular transformation object to map from the
 *			integer coordinate system to the irregular one.
 *
 *			Eventually the methods here will be called from within
 *			the CPMPXY function used by CONPACK and eventually by
 *			the rest of the 2D utilities.
 *
 *			This version of irregular grid transformations
 *			differs from the the type 2 trans in that it sets 
 *			the window
 *			to be identical to the computational coordinates. This
 *			has the effect of placing the irregularly spaced 
 *			coordinates onto the frame and even spacings, thus 
 *			streching or deforming the grid so that all data can
 *			be view. The original returns points back to their
 *			data values before rendering. For some grids this has
 *			the effect of "crowding" several grid points in to one
 *			small screen location.
 *
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/ConvertersP.h>
#include <math.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		  NhlOffset(NhlIrregularTransObjLayerRec,
                            irtrans.x_axis_type_set),NhlTImmediate,
         	  _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNtrXAxisType,NhlCtrXAxisType,NhlTAxisType,sizeof(NhlAxisType),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.x_axis_type),NhlTProcedure,
		_NhlUSET((NhlPointer)_NhlResUnset),0,NULL },
	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.x_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrXInterPoints,NhlCtrXInterPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.x_inter_points_ga),NhlTImmediate,
		  _NhlUSET(NULL) ,0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrXTensionF, NhlCtrXTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_tension),
		NhlTString,_NhlUSET("2.0") ,0,NULL},
	{ NhlNtrXSamples, NhlCtrXSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_samples),
		NhlTImmediate,_NhlUSET((NhlPointer)9) ,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		  NhlOffset(NhlIrregularTransObjLayerRec,
                            irtrans.y_axis_type_set),NhlTImmediate,
         	  _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNtrYAxisType,NhlCtrYAxisType,NhlTAxisType,sizeof(NhlAxisType),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.y_axis_type),NhlTProcedure,
		_NhlUSET((NhlPointer)_NhlResUnset),0,NULL },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.y_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL) ,0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrYInterPoints,NhlCtrYInterPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.y_inter_points_ga),NhlTImmediate,
		  _NhlUSET(NULL) ,0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrYTensionF, NhlCtrYTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_tension),
		NhlTString,_NhlUSET("2.0") ,0,NULL},
	{ NhlNtrYSamples, NhlCtrYSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_samples),
		NhlTImmediate,_NhlUSET((NhlPointer)9) ,0,NULL},

/* End-documented-resources */
        
	{ NhlNtrLowLevelLogOn,NhlCtrLowLevelLogOn, NhlTBoolean, 
		sizeof(NhlBoolean),
		NhlOffset(NhlIrregularTransObjLayerRec,
			  irtrans.low_level_log_on),
		NhlTImmediate,_NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL}
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  IrTransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes IrTransInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);


static NhlErrorTypes IrTransDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);


/*
* TransObjClass Methods defined
*/

static NhlErrorTypes IrSetTrans(
#if	NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);


static NhlErrorTypes IrWinToNDC(
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


static NhlErrorTypes IrNDCToWin(
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


static NhlErrorTypes IrDataToCompc(
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

static NhlErrorTypes IrCompcToData(
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

static NhlErrorTypes IrWinToCompc(
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



static NhlErrorTypes IrNDCLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes IrDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrWinLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrDataPolygon(
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
	NhlIrregularTransObjLayerPart *irp,
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

static NhlErrorTypes IrTransGetValues(
#if NhlNeedProto
	NhlLayer /* l */,
	_NhlArgList /*args */,
	int	/*nargs*/
#endif
);

static NhlErrorTypes 	IrTransClassInitialize(
#if NhlNeedProto
	void
#endif
);

#define CREATE  1
#define SET 0

#define NhlirDATA 0
#define NhlirCOMPC 1

static NrmQuark QtrXCoordPoints;
static NrmQuark QtrYCoordPoints;
static NrmQuark QtrXInterPoints;
static NrmQuark QtrYInterPoints;

NhlIrregularTransObjClassRec NhlirregularTransObjClassRec = {
        {
/* class_name			*/	"irregularTransformationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlIrregularTransObjLayerRec),
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
/* class_initialize		*/	IrTransClassInitialize,
/* layer_initialize		*/	IrTransInitialize,
/* layer_set_values		*/	IrTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	IrTransGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	IrTransDestroy
        },
        {
/* set_trans		*/	IrSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	IrWinToNDC,
/* ndc_to_win		*/	IrNDCToWin,
/* data_to_win		*/	IrDataToCompc, 
/* win_to_data		*/	IrCompcToData, 
/* data_to_compc	*/	IrDataToCompc,
/* compc_to_data	*/	IrCompcToData,
/* win_to_compc		*/	IrWinToCompc,
/* compc_to_win		*/	IrWinToCompc,
/* data_lineto 		*/      IrDataLineTo,
/* compc_lineto		*/      IrWinLineTo,
/* win_lineto 		*/      IrWinLineTo,
/* NDC_lineto 		*/      IrNDCLineTo,
/* data_polygon		*/      IrDataPolygon

        }
};

NhlClass NhlirregularTransObjClass =
			(NhlClass)&NhlirregularTransObjClassRec;


#define INCREASING 0
#define DECREASING 1
#define NONMONOTONIC 2
#define NOSPAN 3

static int GetOrdering
#if	NhlNeedProto
(float *v,int nv,float* min, float*max) 
#else
(v,nv,min,max)
	float *v;
	int   nv;
	float	*min;
	float	*max;
#endif
{
	int i;

	i = 1;
	while((i<nv)&&(v[i-1] <= v[i])) {
		i++;
	}
	if(i == nv) {
		*min = v[0];
		*max = v[nv-1];
                if (_NhlCmpFAny2(*min,*max,5,_NhlMIN_NONZERO) == 0.0)
                        return(NOSPAN);
		return(INCREASING);
	}
	i = 1;
	while((i<nv)&&(v[i-1] >= v[i])) {
		i++;
	}
	if(i==nv){
		*max = v[0];
		*min = v[nv-1];
                if (_NhlCmpFAny2(*min,*max,5,_NhlMIN_NONZERO) == 0.0)
                        return(NOSPAN);
		return(DECREASING);
	} else {
		return(NONMONOTONIC);
	}
}

/*
 * Function:	IrTransSetValues
 *
 * Description:	SetValues method for IrregularTrans Objects
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
static NhlErrorTypes IrTransSetValues
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
	NhlIrregularTransObjLayer inew = (NhlIrregularTransObjLayer) new;
	NhlIrregularTransObjLayerPart *irp = &inew->irtrans;

	if (_NhlArgIsSet(args,num_args,NhlNtrLowLevelLogOn)) {
		/* assumed to be the only arg set */

		if (irp->low_level_log_on) {
			if (irp->x_axis_type == NhlLOGAXIS) {
				if (irp->x_reverse) {
					irp->ul = irp->x_max;
					irp->ur = irp->x_min;
				}
				else {
					irp->ul = irp->x_min;
					irp->ur = irp->x_max;
				}
				irp->compc_x_min = irp->x_min;
				irp->compc_x_max = irp->x_max;
                                irp->compc_xmin_dat = irp->xmin_dat;
                                irp->compc_xmax_dat = irp->xmax_dat;
                        }
			if (irp->y_axis_type == NhlLOGAXIS) {
				if (irp->y_reverse) {
					irp->ub = irp->y_max;
					irp->ut = irp->y_min;
				}
				else {
					irp->ub = irp->y_min;
					irp->ut = irp->y_max;
				}
				irp->compc_y_min = irp->y_min;
				irp->compc_y_max = irp->y_max;
                                irp->compc_ymin_dat = irp->ymin_dat;
                                irp->compc_ymax_dat = irp->ymax_dat;
			}
			if (irp->x_axis_type == NhlLOGAXIS && 
			    irp->y_axis_type == NhlLOGAXIS)
				irp->log_lin_value = 4;
			else if (irp->x_axis_type == NhlLOGAXIS)
				irp->log_lin_value = 3;
			else if (irp->y_axis_type == NhlLOGAXIS)
				irp->log_lin_value = 2;
		}
		else {
			irp->log_lin_value = 1;
			irp->ul = irp->ul_save;
			irp->ur = irp->ur_save;
			irp->ub = irp->ub_save;
			irp->ut = irp->ut_save;
			irp->compc_x_min = MIN(irp->ul,irp->ur);
			irp->compc_x_max = MAX(irp->ul,irp->ur);
			irp->compc_y_min = MIN(irp->ut,irp->ub);
			irp->compc_y_max = MAX(irp->ut,irp->ub);
                        irp->compc_xmin_dat = irp->save_compc_xmin_dat;
                        irp->compc_xmax_dat = irp->save_compc_xmax_dat;
                        irp->compc_ymin_dat = irp->save_compc_ymin_dat;
                        irp->compc_ymax_dat = irp->save_compc_ymax_dat;
		}
		return NhlNOERROR;
	}

	return(SetUpTrans(new,old,SET,args,num_args));	
}


/*
 * Function:	IrTransInitialize
 *
 * Description: Initialize function for IrregularTransObjs. Performs same
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
static NhlErrorTypes IrTransInitialize
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
 * Function:	IrTransDestroy
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
static NhlErrorTypes IrTransDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlIrregularTransObjLayer ir = 
		(NhlIrregularTransObjLayer)inst;

	if (ir->irtrans.x_irr_points != NULL)
		NhlFree(ir->irtrans.x_irr_points);
	if (ir->irtrans.x_inter_points != NULL)
		NhlFree(ir->irtrans.x_inter_points);
	if (ir->irtrans.y_irr_points != NULL)
		NhlFree(ir->irtrans.y_irr_points);
	if (ir->irtrans.y_inter_points != NULL)
		NhlFree(ir->irtrans.y_inter_points);
 	free(ir->irtrans.xmin_dat);
	free(ir->irtrans.xmax_dat);
	free(ir->irtrans.ymin_dat);
	free(ir->irtrans.ymax_dat);
 	free(ir->irtrans.compc_xmin_dat);
	free(ir->irtrans.compc_xmax_dat);
	free(ir->irtrans.compc_ymin_dat);
	free(ir->irtrans.compc_ymax_dat);
	_NhlDestroySplineCoordApprox(&(ir->irtrans.thecoord));

	return NhlNOERROR;
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
	NhlIrregularTransObjLayer inew = (NhlIrregularTransObjLayer)new;
	NhlIrregularTransObjLayer iold = (NhlIrregularTransObjLayer)old;
	NhlIrregularTransObjLayerPart *irp = &inew->irtrans;
	NhlIrregularTransObjLayerPart *oirp = &iold->irtrans;
	NhlTransObjLayerPart	*tp = &inew->trobj;
	NhlTransObjLayerPart	*otp = &iold->trobj;
	char *error_lead,*e_text;
	NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
	NhlStatus xstatus,ystatus;
	NhlBoolean call_spline_create = False;
	NhlBoolean new_x_extent = False, new_y_extent = False;
        NhlBoolean x_irr_coords_set = False, y_irr_coords_set = False;
        NhlBoolean x_inter_points_set = False, y_inter_points_set = False;
        NhlBoolean xirr_ll_change = False, yirr_ll_change = False;
        NhlBoolean data_x_extent_def, data_y_extent_def;
        NhlBoolean new_x_data_extent,new_y_data_extent;

	tp->change_count++;

        data_x_extent_def = (tp->data_xstart == 0.0 && tp->data_xend == 0.0) ?
                False : True;
        data_y_extent_def = (tp->data_ystart == 0.0 && tp->data_yend == 0.0) ?
                False : True;

	if(c_or_s == SET) {

		error_lead = "IrTransSetValues";

                if (_NhlArgIsSet(args,nargs,NhlNtrXAxisType))
                        irp->x_axis_type_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrXCoordPoints))
                        x_irr_coords_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrXInterPoints))
                        x_inter_points_set = True;
                new_x_data_extent =
                        tp->data_xstart != otp->data_xstart ||
                        tp->data_xend != otp->data_xend;
		if (x_irr_coords_set ||
                    irp->x_axis_type_set ||
                    new_x_data_extent)
			new_x_extent = True;
                else if (! data_x_extent_def &&
                    (tp->x_min != otp->x_min ||
                     tp->x_max != otp->x_max))
                        new_x_extent = True;
                if (new_x_extent || x_inter_points_set ||
                    irp->x_tension != oirp->x_tension ||
                    irp->x_samples != oirp->x_samples)
                        call_spline_create = True;
                if ((oirp->x_axis_type == NhlIRREGULARAXIS &&
                    irp->x_axis_type != NhlIRREGULARAXIS) ||
                    (oirp->x_axis_type != NhlIRREGULARAXIS &&
                    irp->x_axis_type == NhlIRREGULARAXIS))
                        xirr_ll_change = True;
                        
                if (_NhlArgIsSet(args,nargs,NhlNtrYAxisType))
                        irp->y_axis_type_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrYCoordPoints)) {
                        y_irr_coords_set = True;
		}
		if (_NhlArgIsSet(args,nargs,NhlNtrYInterPoints))
                        y_inter_points_set = True;
                new_y_data_extent =
                        tp->data_ystart != otp->data_ystart ||
                        tp->data_yend != otp->data_yend;
		if (y_irr_coords_set ||
                    irp->y_axis_type_set ||
                    new_y_data_extent)
			new_y_extent = True;
                else if (! data_y_extent_def &&
                    (tp->y_min != otp->y_min ||
                     tp->y_max != otp->y_max))
                        new_y_extent = True;
                if (new_y_extent || y_inter_points_set ||
                    irp->y_tension != oirp->y_tension ||
                    irp->y_samples != oirp->y_samples)
                        call_spline_create = True;
                if ((oirp->y_axis_type == NhlIRREGULARAXIS &&
                    irp->y_axis_type != NhlIRREGULARAXIS) ||
                    (oirp->y_axis_type != NhlIRREGULARAXIS &&
                    irp->y_axis_type == NhlIRREGULARAXIS))
                        yirr_ll_change = True;
	}
	else {
		error_lead = "IrTransInitialize";
		call_spline_create = True;
		new_x_extent = True;
		new_y_extent = True;
                new_x_data_extent = True;
                new_y_data_extent = True;
                if (irp->x_coord_points_ga)
                       x_irr_coords_set = True; 
                if (irp->y_coord_points_ga)
                       y_irr_coords_set = True; 
                if (irp->x_inter_points_ga)
                       x_inter_points_set = True; 
                if (irp->y_inter_points_ga)
                       y_inter_points_set = True; 
		irp->x_coord_points = NULL;
		irp->y_coord_points = NULL; 
		irp->x_inter_points = NULL; 
		irp->y_inter_points = NULL;
                irp->x_irr_points = NULL;
                irp->y_irr_points = NULL;
                irp->x_irr_min = 0.0;
                irp->x_irr_max = 0.0;
		irp->x_num_points = 0;
                irp->y_num_points = 0;
                irp->y_irr_min = 0.0;
                irp->y_irr_max = 0.0;
                
		if (! irp->x_axis_type_set) irp->x_axis_type = NhlLINEARAXIS;
		if (! irp->y_axis_type_set) irp->y_axis_type = NhlLINEARAXIS;
                irp->ul = irp->ub = 0.0;
                irp->ur = irp->ut = 1.0;
	}

/*
 * Handle X Axis
 */
	if (irp->x_irr_points && ! x_irr_coords_set) {
		irp->x_irr_min = MIN(irp->x_irr_points[0],
                                    irp->x_irr_points[irp->x_irr_count-1]);
		irp->x_irr_max = MAX(irp->x_irr_points[0],
                                    irp->x_irr_points[irp->x_irr_count-1]);
	}
        if (x_irr_coords_set) {
                new_x_extent = True;
                if (! irp->x_axis_type_set)
                        irp->x_axis_type = NhlIRREGULARAXIS;
                if (irp->x_irr_points) {
                        NhlFree(irp->x_irr_points);
                        irp->x_irr_points = NULL;
                        irp->x_irr_count = 0;
                }
                if (irp->x_coord_points_ga != NULL &&
                    irp->x_coord_points_ga->num_elements > 0) {
                        int ordering;
                        float min,max;
                        
                        irp->x_irr_count =
                                irp->x_coord_points_ga->num_elements;
                        irp->x_irr_points  = NhlMalloc
                                (irp->x_irr_count * sizeof(float));
                        if (! irp->x_irr_points) {
                                e_text = "%s: dynamic memory allocation error";
                                NHLPERROR((NhlFATAL,ENOMEM,e_text,error_lead));
                                return NhlFATAL;
                        }
                        memcpy(irp->x_irr_points,
                               irp->x_coord_points_ga->data,
                               irp->x_irr_count * sizeof(float));
                        ordering = GetOrdering(irp->x_irr_points,
					       irp->x_irr_count,
					       &min,&max);

                        if (! (ordering == INCREASING || 
			       ordering == DECREASING)) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
      "%s: %s contains invalid coordinate array: defaulting %s to LinearAxis",
                                          error_lead,NhlNtrXCoordPoints,
					  NhlNtrXAxisType);
                                ret = MIN(ret,NhlWARNING);
                                irp->x_axis_type = NhlLINEARAXIS;
                        }
                        else {
                                irp->x_irr_min = min;
                                irp->x_irr_max = max;
                        }
                }
        }
        
        
	if (new_x_extent) {
	        if (!data_x_extent_def) {
        	        irp->x_log_lin_points[0] = tp->x_min;
			irp->x_log_lin_points[1] =
				(tp->x_min + tp->x_max) / 2.0;
			irp->x_log_lin_points[2] = tp->x_max;
		}
		else {
                	irp->x_log_lin_points[0] = tp->data_xstart;
			irp->x_log_lin_points[1] =
                        	(tp->data_xstart + tp->data_xend) / 2.0;
			irp->x_log_lin_points[2] = tp->data_xend;
		}
                irp->x_coord_points = irp->x_log_lin_points;
                irp->x_num_points = 3;
		irp->x_use_log = False;
                
		switch (irp->x_axis_type) {
		case NhlLINEARAXIS:
			break;
                case NhlIRREGULARAXIS:
                        if (! irp->x_irr_points) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
 "%s: Must specify %s resource for IrregularAxis: defaulting %s to LinearAxis",
                                          error_lead, NhlNtrXCoordPoints,
					  NhlNtrXAxisType);
                                ret = MIN(ret,NhlWARNING);
                                irp->x_axis_type = NhlLINEARAXIS;
                                break;
                        }
                        irp->x_coord_points  = irp->x_irr_points;
                        irp->x_num_points = irp->x_irr_count;
                        if (data_x_extent_def) {
                                if (new_x_data_extent) {
                                        if (! tp->x_min_set)
                                                tp->x_min = irp->x_irr_min;
                                        if (! tp->x_max_set)
                                                tp->x_max = irp->x_irr_max;
                                }
                        }
                        else if (c_or_s == CREATE ||
                                 irp->x_irr_min != oirp->x_irr_min ||
                                 irp->x_irr_max != oirp->x_irr_max) {
                                if (! tp->x_min_set)
                                        tp->x_min = irp->x_irr_min;
                                if (! tp->x_max_set)
                                        tp->x_max = irp->x_irr_max;
                        }
                        break;
		case NhlLOGAXIS:
			if (tp->x_min <= 0.0 ||
                            (data_x_extent_def && tp->data_xstart <= 0.0)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: LogAxis requires all positive data extent: defaulting %s to LinearAxis",
                                          error_lead,NhlNtrXAxisType);
				ret = MIN(ret,NhlWARNING);
				irp->x_axis_type = NhlLINEARAXIS;
                                break;
			}
                        irp->x_use_log = True;
                        if (! data_x_extent_def)
                                irp->x_log_lin_points[1] =
                                        pow(10.0,(log10(tp->x_min)+
                                                  log10(tp->x_max))/2.0);
                        else
                                irp->x_log_lin_points[1] =
                                        pow(10.0,(log10(tp->data_xstart)+
                                                  log10(tp->data_xend))/2.0);
			break;
		}
                if (xirr_ll_change && ! tp->x_reverse_set)
                        tp->x_reverse = False;
	}
        
        if (x_inter_points_set) {
                if (irp->x_inter_points) {
                        NhlFree(irp->x_inter_points);
                        irp->x_inter_points = NULL;
                }
                if (irp->x_inter_points_ga != NULL &&
                    irp->x_inter_points_ga->num_elements != irp->x_irr_count) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
           "%s: %s resource must have same number of elements as %s: ignoring",
				  error_lead,
				  NhlNtrXInterPoints,NhlNtrXCoordPoints);
			ret = MIN(ret,NhlWARNING);
		}
		irp->x_inter_points = (float*)
                        NhlMalloc(sizeof(float) * (irp->x_irr_count));
		if (irp->x_inter_points == NULL) {
                        e_text = "%s: dynamic memory allocation error";
                        NHLPERROR((NhlFATAL,ENOMEM,e_text,error_lead));
                        return NhlFATAL;
                }
		memcpy((char*)irp->x_inter_points,
                       irp->x_inter_points_ga->data,
                       sizeof(float)*irp->x_irr_count);
	}


/*
 * Handle Y Axis
 */
	if (irp->y_irr_points && ! y_irr_coords_set) {
		irp->y_irr_min = MIN(irp->y_irr_points[0],
				irp->y_irr_points[irp->y_irr_count-1]);
		irp->y_irr_max = MAX(irp->y_irr_points[0],
				irp->y_irr_points[irp->y_irr_count-1]);
	}
        if (y_irr_coords_set) {
                new_y_extent = True;
                if (! irp->y_axis_type_set)
                        irp->y_axis_type = NhlIRREGULARAXIS;
                if (irp->y_irr_points) {
                        NhlFree(irp->y_irr_points);
                        irp->y_irr_points = NULL;
                        irp->y_irr_count = 0;
                }
                if (irp->y_coord_points_ga != NULL &&
                    irp->y_coord_points_ga->num_elements > 0) {
                        int ordering;
                        float min,max;
                        
                        irp->y_irr_count =
                                irp->y_coord_points_ga->num_elements;
                        irp->y_irr_points  = NhlMalloc
                                (irp->y_irr_count * sizeof(float));
                        if (! irp->y_irr_points) {
                                e_text = "%s: dynamic memory allocation error";
                                NHLPERROR((NhlFATAL,ENOMEM,e_text,error_lead));
                                return NhlFATAL;
                        }
                        memcpy(irp->y_irr_points,
                               irp->y_coord_points_ga->data,
                               irp->y_irr_count * sizeof(float));
                        ordering = GetOrdering(irp->y_irr_points,
                                               irp->y_irr_count,
                                               &min,&max);

                        if (! (ordering == INCREASING || 
			       ordering == DECREASING)) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
      "%s: %s contains invalid coordinate array: defaulting %s to LinearAxis",
                                          error_lead,NhlNtrYCoordPoints,
					  NhlNtrYAxisType);
                                ret = MIN(ret,NhlWARNING);
                                irp->y_axis_type = NhlLINEARAXIS;
                        }
                        else {
                                irp->y_irr_min = min;
                                irp->y_irr_max = max;
                        }
                }
        }
        
	if (new_y_extent) {
		if (! data_y_extent_def) {
                	irp->y_log_lin_points[0] = tp->y_min;
			irp->y_log_lin_points[1] =
                        	(tp->y_min + tp->y_max) / 2.0;
			irp->y_log_lin_points[2] = tp->y_max;
		}
		else {
                	irp->y_log_lin_points[0] = tp->data_ystart;
			irp->y_log_lin_points[1] =
                        	(tp->data_ystart + tp->data_yend) / 2.0;
			irp->y_log_lin_points[2] = tp->data_yend;
		}
                irp->y_coord_points = irp->y_log_lin_points;
                irp->y_num_points = 3;
		irp->y_use_log = False;
                
		switch (irp->y_axis_type) {
		case NhlLINEARAXIS:
			break;
                case NhlIRREGULARAXIS:
                        if (! irp->y_irr_points) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
 "%s: Must specify %s resource for IrregularAxis: defaulting %s to LinearAxis",
                                          error_lead,NhlNtrYCoordPoints,
					  NhlNtrYAxisType);
                                ret = MIN(ret,NhlWARNING);
                                irp->y_axis_type = NhlLINEARAXIS;
                                break;
                        }
                        irp->y_coord_points  = irp->y_irr_points;
                        irp->y_num_points = irp->y_irr_count;
                        if (data_y_extent_def) {
                                if (new_y_data_extent) {
                                        if (! tp->y_min_set)
                                                tp->y_min = irp->y_irr_min;
                                        if (! tp->y_max_set)
                                                tp->y_max = irp->y_irr_max;
                                }
                        }
                        else if (c_or_s == CREATE ||
                                 irp->y_irr_min != oirp->y_irr_min ||
                                 irp->y_irr_max != oirp->y_irr_max) {
                                if (! tp->y_min_set)
                                        tp->y_min = irp->y_irr_min;
                                if (! tp->y_max_set)
                                        tp->y_max = irp->y_irr_max;
                        }
                        break;
		case NhlLOGAXIS:
			if (tp->y_min <= 0.0 ||
                            (data_y_extent_def && tp->data_ystart <= 0.0)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: LogAxis requires all positive data extent: defaulting %s to LinearAxis",
                                          error_lead,NhlNtrYAxisType);
				ret = MIN(ret,NhlWARNING);
				irp->y_axis_type = NhlLINEARAXIS;
                                break;
			}
                        irp->y_use_log = True;
                        if (! data_y_extent_def)
                                irp->y_log_lin_points[1] =
                                        pow(10.0,(log10(tp->y_min)+
                                                  log10(tp->y_max))/2.0);
                        else
                                irp->y_log_lin_points[1] =
                                        pow(10.0,(log10(tp->data_ystart)+
                                                  log10(tp->data_yend))/2.0);
			break;
		}
                if (yirr_ll_change && ! tp->y_reverse_set)
                        tp->y_reverse = False;
	}
        
        if (y_inter_points_set) {
                if (irp->y_inter_points) {
                        NhlFree(irp->y_inter_points);
                        irp->y_inter_points = NULL;
                }
                if (irp->y_inter_points_ga != NULL &&
                    irp->y_inter_points_ga->num_elements != irp->y_irr_count) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
           "%s: %s resource must have same number of elements as %s: ignoring",
				  error_lead,
				  NhlNtrYInterPoints,NhlNtrYCoordPoints);
			ret = MIN(ret,NhlWARNING);
		}
		irp->y_inter_points = (float*)
                        NhlMalloc(sizeof(float) * (irp->y_irr_count));
		if (irp->y_inter_points == NULL) {
                        e_text = "%s: dynamic memory allocation error";
                        NHLPERROR((NhlFATAL,ENOMEM,e_text,error_lead));
                        return NhlFATAL;
                }
		memcpy((char*)irp->y_inter_points,
                       irp->y_inter_points_ga->data,
                       sizeof(float)*irp->y_irr_count);
	}

	if(call_spline_create) {
                NhlBoolean created = (c_or_s == SET);
                int tries = 0;
                
                while (1) {
                        if (created)
                                _NhlDestroySplineCoordApprox(&(irp->thecoord));
                        subret = _NhlCreateSplineCoordApprox
                                (&(irp->thecoord),
                                 irp->x_use_log,irp->x_coord_points,
                                 irp->x_inter_points,irp->x_num_points,
                                 irp->y_use_log,irp->y_coord_points,
                                 irp->y_inter_points,irp->y_num_points,
                                 irp->x_tension,irp->y_tension,
                                 irp->x_samples,irp->y_samples,
                                 &xstatus,&ystatus);
                        ret = MIN(ret,subret);
                        created = True;
                        tries++;
                        if (xstatus == NhlBOTHTRANS && ystatus == NhlBOTHTRANS)
                                break;
                        if (tries == 3) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                           "%s: spline coordinate approximation not possible:",
                                          error_lead);
                                ret = MIN(NhlFATAL,ret);
                                return NhlFATAL;
                        }
                        if (irp->x_axis_type == NhlIRREGULARAXIS &&
                            xstatus != NhlBOTHTRANS) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
     "%s: error creating spline approximation for %s; defaulting to linear",
                                          error_lead,NhlNtrXCoordPoints);
                                ret = MIN(ret,NhlWARNING);
                                irp->x_coord_points = irp->x_log_lin_points;
                                irp->x_num_points = 3;
                                irp->x_axis_type = NhlLINEARAXIS;
                        }
                        if (irp->y_axis_type == NhlIRREGULARAXIS &&
                            ystatus != NhlBOTHTRANS) {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
     "%s: error creating spline approximation for %s; defaulting to linear",
                                          error_lead,NhlNtrYCoordPoints);
                                ret = MIN(ret,NhlWARNING);
                                irp->y_coord_points = irp->y_log_lin_points;
                                irp->y_num_points = 3;
                                irp->y_axis_type = NhlLINEARAXIS;
                        }
                }
	}

	if (irp->x_axis_type == NhlIRREGULARAXIS) {

		if (tp->x_min < irp->x_irr_min) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: X minimum less than minimum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			tp->x_min = irp->x_irr_min;
		}
		if (tp->x_max > irp->x_irr_max) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: X maximum greater than maximum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			tp->x_max = irp->x_irr_max;
		}
	}
	if (irp->y_axis_type == NhlIRREGULARAXIS) {

		if (tp->y_min < irp->y_irr_min) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: Y minimum less than minimum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			tp->y_min = irp->y_irr_min;
		}
		if (tp->y_max > irp->y_irr_max) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: Y maximum greater than maximum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			tp->y_max = irp->y_irr_max;
		}
	}
        if (ret > NhlFATAL) {
                _NhlEvalSplineCoordForward(&irp->thecoord,
                                           tp->x_min,
                                           tp->y_min,
                                           &(irp->ul),
                                           &(irp->ub),
                                           NULL,NULL);
                _NhlEvalSplineCoordForward(&irp->thecoord,
                                           tp->x_max,
                                           tp->y_max,
                                           &(irp->ur),
                                           &(irp->ut),
                                           NULL,NULL);
        }
        
	if(tp->x_reverse) {
		float tmpf = irp->ur;
		irp->ur = irp->ul;
		irp->ul = tmpf;
	}
	if(tp->y_reverse) {
		float tmpf = irp->ut;
		irp->ut = irp->ub;
		irp->ub = tmpf;
	}
	irp->compc_x_min = MIN(irp->ul,irp->ur);
	irp->compc_x_max = MAX(irp->ul,irp->ur);
	irp->compc_y_min = MIN(irp->ut,irp->ub);
	irp->compc_y_max = MAX(irp->ut,irp->ub);

	irp->ur_save = irp->ur;
	irp->ul_save = irp->ul;
	irp->ut_save = irp->ut;
	irp->ub_save = irp->ub;
	irp->log_lin_value = 1;

	if (c_or_s == CREATE) {
		if ((irp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->ymin_dat =_NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->compc_xmin_dat =
		     _NhlCmpFSetup(irp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_xmin_dat = irp->compc_xmin_dat;
		if ((irp->compc_xmax_dat = 
		     _NhlCmpFSetup(irp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_xmax_dat = irp->compc_xmax_dat;
		if ((irp->compc_ymin_dat = 
		     _NhlCmpFSetup(irp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_ymin_dat = irp->compc_ymin_dat;
		if ((irp->compc_ymax_dat = 
		     _NhlCmpFSetup(irp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_ymax_dat = irp->compc_ymax_dat;

                tp->x_min_set = tp->x_max_set = False;
                tp->x_reverse_set = False;
                irp->x_axis_type_set = False;
                tp->y_min_set = tp->y_max_set = False;
                tp->y_reverse_set = False;
                irp->y_axis_type_set = False;
        
                irp->x_min = tp->x_min;
                irp->y_min = tp->y_min;
                irp->x_max = tp->x_max;
                irp->y_max = tp->y_max;
                irp->x_reverse = tp->x_reverse;
                irp->y_reverse = tp->y_reverse;
                
		return(ret);
	}
        if (tp->x_min != iold->irtrans.x_min) {
                free(irp->xmin_dat);
                if ((irp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->x_max != iold->irtrans.x_max) {
		free(irp->xmax_dat);
		if ((irp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_min != iold->irtrans.y_min) {
		free(irp->ymin_dat);
		if ((irp->ymin_dat = _NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_max != iold->irtrans.y_max) {
		free(irp->ymax_dat);
		if ((irp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (irp->compc_x_min != iold->irtrans.compc_x_min) {
		free(irp->compc_xmin_dat);
		if ((irp->compc_xmin_dat =
		     _NhlCmpFSetup(irp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_xmin_dat = irp->compc_xmin_dat;
	}
	if (irp->compc_x_max != iold->irtrans.compc_x_max) {
		free(irp->compc_xmax_dat);
		if ((irp->compc_xmax_dat = 
		     _NhlCmpFSetup(irp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_xmax_dat = irp->compc_xmax_dat;
	}
	if (irp->compc_y_min != iold->irtrans.compc_y_min) {
		free(irp->compc_ymin_dat);
		if ((irp->compc_ymin_dat = 
		     _NhlCmpFSetup(irp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_ymin_dat = irp->compc_ymin_dat;
	}
	if (irp->compc_y_max != iold->irtrans.compc_y_max) {
		free(irp->compc_ymax_dat);
		if ((irp->compc_ymax_dat = 
		     _NhlCmpFSetup(irp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                irp->save_compc_ymax_dat = irp->compc_ymax_dat;
	}
        
        tp->x_min_set = tp->x_max_set = False;
        tp->x_reverse_set = False;
        irp->x_axis_type_set = False;
        tp->y_min_set = tp->y_max_set = False;
        tp->y_reverse_set = False;
        irp->y_axis_type_set = False;
        
        irp->x_min = tp->x_min;
        irp->y_min = tp->y_min;
        irp->x_max = tp->x_max;
        irp->y_max = tp->y_max;
        irp->x_reverse = tp->x_reverse;
        irp->y_reverse = tp->y_reverse;
        
	return(ret);

}

/*
 * Function:	IrSetTrans
 *
 * Description: set_trans method for IrregularTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the IrregularTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes IrSetTrans
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
	NhlIrregularTransObjLayer	to=(NhlIrregularTransObjLayer)tobj;
	NhlTransObjLayerPart		*top = &to->trobj;
	NhlIrregularTransObjLayerPart	*tp = &to->irtrans;
	NhlErrorTypes ret;

	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	return(_NhlTransLLUSet(top->x,top->x+top->width,
			       top->y-top->height,top->y,
			       tp->ul,tp->ur,tp->ub,tp->ut,
			       tp->log_lin_value,
                               &top->off_screen,
                               "IrSetTrans"));

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
	NhlIrregularTransObjLayerPart *irp,
	float	*x,
 	float	*y,
	int	type /* data 0, compc 1 */
)
#else
(irp,x,y,type)
	NhlIrregularTransObjLayerPart *irp;
	float	*x;
	float	*y;
	int	type;
#endif
{
	int xmndif,xmxdif,ymndif,ymxdif;

	if (type == NhlirDATA) {
		if ((xmndif = _NhlCmpF(*x,irp->xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,irp->xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,irp->ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,irp->ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = irp->x_min;
		}
		else if (xmxdif == 0) {
			*x = irp->x_max;
		}
		if (ymndif == 0) {
			*y = irp->y_min;
		}
		else if (ymxdif == 0) {
			*y = irp->y_max;
		}
	}
	else {
		if ((xmndif = _NhlCmpF(*x,irp->compc_xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,irp->compc_xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,irp->compc_ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,irp->compc_ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = irp->compc_x_min;
		}
		else if (xmxdif == 0) {
			*x = irp->compc_x_max;
		}
		if (ymndif == 0) {
			*y = irp->compc_y_min;
		}
		else if (ymxdif == 0) {
			*y = irp->compc_y_max;
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
 * Function:	IrWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the IrregularTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes IrWinToNDC
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
	NhlIrregularTransObjLayer	iinstance =
				(NhlIrregularTransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;
	int i;
	
	*status = 0;	
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n ; i++) {
/*
* Compc and Window are identical coordinates in this object
*/
			if((x[i] > iinstance->irtrans.compc_x_max)
			   ||(x[i] < iinstance->irtrans.compc_x_min)
			   ||(y[i] > iinstance->irtrans.compc_y_max)
			   ||(y[i] < iinstance->irtrans.compc_y_min)) {
				if (! compare_check(&iinstance->irtrans,
						    &x[i],&y[i],NhlirCOMPC)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}

			} 
			strans(iinstance->irtrans.ul,
			       iinstance->irtrans.ur,
			       iinstance->irtrans.ub,
			       iinstance->irtrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n ; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
			   ||((ymissing != NULL)&&(*ymissing == y[i]))
			   ||(x[i] > iinstance->irtrans.compc_x_max)
			   ||(x[i] < iinstance->irtrans.compc_x_min)
			   ||(y[i] > iinstance->irtrans.compc_y_max)
			   ||(y[i] < iinstance->irtrans.compc_y_min)) {
				if (! compare_check(&iinstance->irtrans,
						    &x[i],&y[i],NhlirCOMPC)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}
			}
			strans(iinstance->irtrans.ul,
			       iinstance->irtrans.ur,
			       iinstance->irtrans.ub,	
			       iinstance->irtrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	}

	return(NhlNOERROR);
}


/*
 * Function:	IrNDCToWin
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
static NhlErrorTypes IrNDCToWin
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
	NhlIrregularTransObjLayer	iinstance = 
				(NhlIrregularTransObjLayer)instance;
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
			strans(tp->x,x1,y1,tp->y,iinstance->irtrans.ul,
			       iinstance->irtrans.ur, iinstance->irtrans.ub,
			       iinstance->irtrans.ut, x[i],y[i],
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
			       iinstance->irtrans.ul,
			       iinstance->irtrans.ur, 
			       iinstance->irtrans.ub,
			       iinstance->irtrans.ut, x[i],y[i],
			       &(xout[i]),&(yout[i]));
		}
	}
	return(NhlNOERROR);
}

/*
 * Function:	LLLogForward
 *
 * Description: Handles the forward spline approximation for the special
 * 		low-level-log case.
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
static NhlErrorTypes LLLogForward
#if	NhlNeedProto
(
        NhlIrregularTransObjLayerPart *irp,
        float *xin,
        float *yin,
        float *xout,
        float *yout
 )
#else
(irp,xin,yin,xout,yout)
        NhlIrregularTransObjLayerPart *irp;
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
        float tmp;
/*
 * if low level log is on, the spline coordinate approximation is only used
 * for the non-log irregular axis. Since the spline has been set up with
 * 3 points in the range 0.0 - 2.0 for the log axis, just pass a dummy value
 * in that range to the spline routine.
 */
        if (irp->x_use_log && irp->y_use_log) {
                *xout = *xin;
                *yout = *yin;

        }
        else if (irp->x_use_log) {
                tmp = *xin;
		ret = _NhlEvalSplineCoordForward(
                        &irp->thecoord,1.0,*yin,xout,yout,NULL,NULL);
                *xout = tmp;
        }
        else {
                tmp = *yin;
		ret = _NhlEvalSplineCoordForward(
                        &irp->thecoord,*xin,1.0,xout,yout,NULL,NULL);
                *yout = tmp;
        }
        return ret;
}


/*
 * Function:	IrDataToCompc
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
static NhlErrorTypes IrDataToCompc
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing, int *status)
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
	NhlIrregularTransObjLayer iinstance =
                (NhlIrregularTransObjLayer)instance;
	int i;

	*status = 0;
	for(i=0; i< n;i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < iinstance->irtrans.x_min)	
			||(x[i] > iinstance->irtrans.x_max)
			||(y[i] < iinstance->irtrans.y_min)
			||(y[i] > iinstance->irtrans.y_max)) {
		
			if (! compare_check(&iinstance->irtrans,
					    &x[i],&y[i],NhlirDATA)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
                if (iinstance->irtrans.low_level_log_on) {
                        ret = LLLogForward(&iinstance->irtrans,
                                           &(x[i]),&(y[i]),
                                           &(xout[i]),&(yout[i]));
                        continue;
                }
		ret = _NhlEvalSplineCoordForward(
					   &(iinstance->irtrans.thecoord),
					   x[i],y[i],&(xout[i]),&(yout[i]),
					   NULL,NULL);
	}
	return(ret);
}


/*
 * Function:	LLLogInverse
 *
 * Description: Handles the inverse spline approximation for the special
 * 		low-level-log case.
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
static NhlErrorTypes LLLogInverse
#if	NhlNeedProto
(
        NhlIrregularTransObjLayerPart *irp,
        float *xin,
        float *yin,
        float *xout,
        float *yout
 )
#else
(irp,xin,yin,xout,yout)
        NhlIrregularTransObjLayerPart *irp;
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
        float tmp;
/*
 * if low level log is on, the spline coordinate approximation is only used
 * for the non-log irregular axis. 
 */
        if (irp->x_use_log && irp->y_use_log) {
                *xout = *xin;
                *yout = *yin;

        }
        else if (irp->x_use_log) {
                tmp = *xin;
		ret = _NhlEvalSplineCoordInverse(
                        &irp->thecoord,*xin,*yin,xout,yout,NULL,NULL);
                *xout = tmp;
        }
        else {
                tmp = *yin;
		ret = _NhlEvalSplineCoordInverse(
                        &irp->thecoord,*xin,*yin,xout,yout,NULL,NULL);
                *yout = tmp;
        }
        return ret;
}

/*
 * Function:	IrCompcToData
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
static NhlErrorTypes IrCompcToData
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
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
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	int i;

	*status = 0;
	for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] > iinstance->irtrans.compc_x_max)
			||(x[i] < iinstance->irtrans.compc_x_min)
			||(y[i] > iinstance->irtrans.compc_y_max)
			||(y[i] < iinstance->irtrans.compc_y_min)) {

			if (! compare_check(&iinstance->irtrans,
					    &x[i],&y[i],NhlirCOMPC)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
                if (iinstance->irtrans.low_level_log_on) {
                        ret = LLLogInverse(&iinstance->irtrans,
                                           &(x[i]),&(y[i]),
                                           &(xout[i]),&(yout[i]));
                        continue;
                }
		ret = _NhlEvalSplineCoordInverse(
					     &(iinstance->irtrans.thecoord),
					     x[i],y[i],&(xout[i]),&(yout[i]),
					     NULL,NULL);	
	}
	return(ret);
}

/*ARGSUSED*/
static NhlErrorTypes IrWinToCompc
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
        NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
        int i;

        *status = 0;
        for(i = 0 ; i< n; i++) {
                if(((xmissing != NULL)&&(*xmissing == x[i]))
                        || ((ymissing != NULL)&&(*ymissing == y[i]))
                        ||(x[i] < iinstance->irtrans.compc_x_min)
                        ||(x[i] > iinstance->irtrans.compc_x_max)
                        ||(y[i] < iinstance->irtrans.compc_y_min)
                        ||(y[i] > iinstance->irtrans.compc_y_max)) {

			if (! compare_check(&iinstance->irtrans,
					    &x[i],&y[i],NhlirCOMPC)) {
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
(NhlIrregularTransObjLayer irinst, 
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
NhlIrregularTransObjLayer irinst;
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
	NhlIrregularTransObjLayerPart *irp = 
		(NhlIrregularTransObjLayerPart *) &irinst->irtrans;
	float xt,yt;
	int i,status = 1;

	xt = xclip;
	yt = yclip;

	for (i=0; i < 2; i++) {

		if (x != xclip) {
			if (_NhlCmpF(xt,irp->xmin_dat) < 0.0) {
				*xd = irp->x_min;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
			else if (_NhlCmpF(xt,irp->xmax_dat) > 0.0) {
				*xd = irp->x_max;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
		}
		if (y != yclip) {
			if (_NhlCmpF(yt,irp->ymin_dat) < 0.0) {
				*yd = irp->y_min;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
			else if (_NhlCmpF(yt,irp->ymax_dat) > 0.0) {
				*yd = irp->y_max;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
		}
		IrDataToCompc((NhlLayer)irinst,xd,yd,1,
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


static NhlErrorTypes IrDataLineTo
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
	NhlIrregularTransObjLayer irinst = (NhlIrregularTransObjLayer)instance;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	float holdx,holdy;
	int status;
	int i,npoints = 256;
	float xdist,ydist,xc,yc,xd,yd;

	npoints = irinst->trobj.point_count;
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
	_NhlTransClipLine(irinst->irtrans.x_min,
			  irinst->irtrans.x_max,
			  irinst->irtrans.y_min,
			  irinst->irtrans.y_max,
			  &lastx,
			  &lasty,
			  &currentx,
			  &currenty,
			  irinst->trobj.out_of_range);

	if((lastx == irinst->trobj.out_of_range)
	   ||(lasty == irinst->trobj.out_of_range)
	   ||(currentx == irinst->trobj.out_of_range)
	   ||(currenty == irinst->trobj.out_of_range)){
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
	IrDataToCompc(instance,xpoints,ypoints,2,
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
			_NhlWorkstationLineTo(irinst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
			call_frstd = 2;
		}
		xdist = currentx - lastx;
		ydist = currenty - lasty;
		for (i = 0; i<npoints; i++) {
			xd = lastx + xdist *(i+1)/ (float)npoints;
			yd = lasty + ydist *(i+1)/ (float)npoints;
			IrDataToCompc(instance,&xd,&yd,1,
				      &xc,&yc,NULL,NULL,&status);
			if (! status)
				_NhlWorkstationLineTo(irinst->trobj.wkptr,
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
		if (AdjustToEdge(irinst,holdx,holdy,x,y,&xd,&yd,&xc,&yc)
			< NhlNOERROR)
			return NhlFATAL;
		lastx = xd;
		lasty = yd;
		xdist = x - lastx;
		ydist = y - lasty;

		_NhlWorkstationLineTo(irinst->trobj.wkptr,
				      c_cufx(xc),c_cufy(yc),1);
	}
	else if (call_frstd == 1) {
		_NhlWorkstationLineTo(irinst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
	}
	call_frstd = 2;

	for (i = 0; i< npoints; i++) {
		xd = lastx + xdist *(i+1)/(float)npoints;
		yd = lasty + ydist *(i+1)/(float)npoints;
		IrDataToCompc(instance,&xd,&yd,1,&xc,&yc,NULL,NULL,&status);
		if (status) {
			if (AdjustToEdge(irinst,x,y,holdx,holdy,
					 &xd,&yd,&xc,&yc) < NhlNOERROR)
				return NhlFATAL;
		}
		_NhlWorkstationLineTo(irinst->trobj.wkptr,
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
static NhlErrorTypes IrWinLineTo
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
	NhlIrregularTransObjLayer irinst = (NhlIrregularTransObjLayer)instance;
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
			irinst->irtrans.compc_x_min, 
			irinst->irtrans.compc_x_max, 
			irinst->irtrans.compc_y_min, 
			irinst->irtrans.compc_y_max,
			&lastx, &lasty, &currentx, &currenty,
			irinst->trobj.out_of_range);
		if((lastx == irinst->trobj.out_of_range)
			||(lasty == irinst->trobj.out_of_range)
			||(currentx == irinst->trobj.out_of_range)
			||(currenty == irinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
			return(_NhlWorkstationLineTo(irinst->trobj.wkptr,c_cufx(x),c_cufy(y),1));
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }

			if(call_frstd == 1) {
				_NhlWorkstationLineTo(irinst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);

				call_frstd = 2;
			}
			_NhlWorkstationLineTo(irinst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);

			lastx = x;
			lasty = y;
			return(NhlNOERROR);
		}
			
			
	}
	
}


/*ARGSUSED*/
static NhlErrorTypes IrNDCLineTo
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
	NhlIrregularTransObjLayer iinstance= (NhlIrregularTransObjLayer)instance;
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

/*ARGSUSED*/
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
static NhlErrorTypes IrDataPolygon
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
	NhlIrregularTransObjLayer irinst = (NhlIrregularTransObjLayer)instance;
	NhlIrregularTransObjLayerPart *irtp = 
		(NhlIrregularTransObjLayerPart *) &irinst->irtrans;
	NhlString e_text;
	NhlString entry_name = "IrDataPolygon";
	float out_of_range = irinst->trobj.out_of_range;
	int i,j,ixout;
	float px,py,cx,cy,dx,dy,tx,ty;
	float *xbuf,*ybuf,*dbuf,*xout,*yout;
	int *ixbuf;
	NhlBoolean open, done = False, first, firstpoint;
	int count, pcount, cix, pix, status = 0, npoints = 256;
	float xdist,ydist,tdist;
	int outcount;

	npoints = irinst->trobj.point_count;
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
		_NhlTransClipLine(irtp->x_min,irtp->x_max,
				  irtp->y_min,irtp->y_max,
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
	ret = IrDataToCompc(instance,xbuf,ybuf,count,
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
		if (irinst->trobj.point_count > 1)
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
				IrDataToCompc(instance,&cx,&cy,1,
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
		ret = _NhlWorkstationFill(irinst->trobj.wkptr,
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
			xorange = cx < irtp->x_min || cx >irtp->x_max ? 
				True : False;
			yorange = cy < irtp->y_min || cy >irtp->y_max ? 
				True : False;
			status = 0;
			if (xorange && ! yorange) {
				if (cx < irtp->x_min) {
					cx = irtp->x_min;
					xbuf[i] = irtp->x_reverse ?
						1.1 : -.1;
				}
				else {
					cx = irtp->x_max;
					xbuf[i] = irtp->x_reverse ?
						-.1 : 1.1;
				}
				IrDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				ybuf[i] = c_cufy(cy);
			}
			else if (yorange && ! xorange) {
				if (cy < irtp->y_min) {
					cy = irtp->y_min;
					ybuf[i] = irtp->y_reverse ?
						1.1 : -.1;
				}
				else {
					cy = irtp->y_max;
					ybuf[i] = irtp->y_reverse ?
						-.1 : 1.1;
				}
				IrDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				xbuf[i] = c_cufx(cx);
			}
			else {
				if (irtp->x_reverse)
					xbuf[i] = cx < irtp->x_min ? 
						1.1 : -.1;
				else
					xbuf[i] = cx < irtp->x_min ? 
						-.1 : 1.1;

				if (irtp->y_reverse)
					ybuf[i] = cy < irtp->y_min ? 
						1.1 : -.1;
				else
					ybuf[i] = cy < irtp->y_min ? 
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
	if (irinst->trobj.point_count > 1)
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
			IrDataToCompc(instance,&dx,&dy,1,
				      &tx,&ty,NULL,NULL,&status);
			if (! status) {
				if (! started) {
					started = True;
					if (lcount == 1) j--;
					if (AdjustToEdge(irinst,px,py,cx,cy,
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
				if (AdjustToEdge(irinst,cx,cy,px,py,
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
	ret = _NhlWorkstationFill(irinst->trobj.wkptr,xout,yout,ixout+1);

	NhlFree(xbuf);
	NhlFree(ybuf);
	NhlFree(dbuf);
	NhlFree(xout);
	NhlFree(yout);
	return ret;
	
}
static NhlErrorTypes IrTransGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int nargs)
#else
(l, args, nargs)
NhlLayer l;
_NhlArgList args;
int nargs;
#endif
{
	NhlIrregularTransObjLayerPart* irp = 
		(&((NhlIrregularTransObjLayer)l)->irtrans);
	int i;
	ng_size_t count;
	float *fp;
	char *e_text;
	NhlString entry_name = "IrTransGetValues";


	for( i = 0; i < nargs ; i++) {
		fp = NULL;
		if(args[i].quark == QtrXCoordPoints) {
			if(irp->x_irr_points != NULL &&
			   irp->x_num_points > 0) {		
				fp = irp->x_irr_points;
				count = irp->x_irr_count;
			}
		}
		if(args[i].quark == QtrXInterPoints) {
			if(irp->x_inter_points != NULL &&
			   irp->x_irr_count > 0) {		
				fp = irp->x_inter_points;
				count = irp->x_irr_count;
			}
		}
		if(args[i].quark == QtrYCoordPoints) {
			if(irp->y_irr_points != NULL &&
			   irp->y_irr_count > 0) {
				fp = irp->y_irr_points;
				count = irp->y_irr_count;
			}
		}
		if(args[i].quark == QtrYInterPoints) {
			if(irp->y_inter_points != NULL 
			   && irp->y_irr_count > 0) {
				fp = irp->y_inter_points;
				count = irp->y_irr_count;
			}
		}
		if (fp != NULL) {
			NhlGenArray ga;
			float *fdata;

			fdata = NhlMalloc(count * sizeof(float));
			if (fdata == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			memcpy(fdata,fp,count * sizeof(float));

			if ((ga = NhlCreateGenArray((NhlPointer)fdata,
                                                    NhlTFloat,sizeof(float),
                                                    1,&count))
                            == NULL) {
                                e_text = "%s: error creating %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                          e_text,entry_name,
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
                        ga->my_data = True;
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
	}
	return(NhlNOERROR);
}

/*
 * Function:	IrTransClassInitialize
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
static NhlErrorTypes    IrTransClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{

	QtrXCoordPoints = NrmStringToQuark(NhlNtrXCoordPoints);
	QtrYCoordPoints = NrmStringToQuark(NhlNtrYCoordPoints);
	QtrXInterPoints = NrmStringToQuark(NhlNtrXInterPoints);
	QtrYInterPoints = NrmStringToQuark(NhlNtrYInterPoints);

	return(NhlNOERROR);	
}
