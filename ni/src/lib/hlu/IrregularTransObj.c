/*
 *      $Id: IrregularTransObj.c,v 1.26 1996-09-14 17:06:16 boote Exp $
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

/*
 * Function:	ResourceUnset
 *
 * Description:	This function can be used to determine if a resource has
 *		been set at initialize time either in the Create call or
 *		from a resource data base. In order to use it the Boolean
 *		'..resource_set' variable MUST directly proceed the name
 *		of the resource variable it refers to in the LayerPart
 *		struct. Also a .nores Resource for the resource_set variable
 *		must directly preceed the Resource of interest in the 
 *		Resource initialization list in this module.
 *
 * In Args:	
 *		NrmName		name,
 *		NrmClass	class,
 *		NhlPointer	base,
 *		unsigned int	offset
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

/*ARGSUSED*/
static NhlErrorTypes
ResourceUnset
#if	NhlNeedProto
(
	NrmName		name,
	NrmClass	class,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,class,base,offset)
	NrmName		name;
	NrmClass	class;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	char *cl = (char *) base;
	NhlBoolean *set = (NhlBoolean *)(cl + offset - sizeof(NhlBoolean));

	*set = False;

	return NhlNOERROR;
}

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtrXAxisType,NhlCtrXAxisType,NhlTAxisType,
		  sizeof(NhlAxisType),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.x_axis_type),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlLINEARAXIS),0,NULL },
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
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_max_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtrXMaxF, NhlCtrXMaxF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_max),
		  NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_min_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtrXMinF, NhlCtrXMinF, NhlTFloat, sizeof(float),
		  NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_min),
		  NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{ NhlNtrXReverse, NhlCtrXReverse, NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_reverse),
		NhlTImmediate,_NhlUSET(False) ,0,NULL},
	{ NhlNtrXTensionF, NhlCtrXTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_tension),
		NhlTString,_NhlUSET("2.0") ,0,NULL},
	{ NhlNtrXSamples, NhlCtrXSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_samples),
		NhlTImmediate,_NhlUSET((NhlPointer)9) ,0,NULL},
	{ NhlNtrYAxisType,NhlCtrYAxisType,NhlTAxisType,
		  sizeof(NhlAxisType),
		  NhlOffset(NhlIrregularTransObjLayerRec,
			    irtrans.y_axis_type),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlLINEARAXIS),0,NULL },
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
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_max_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtrYMaxF, NhlCtrYMaxF, NhlTFloat, sizeof(float),
		  NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_max),
		  NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_min_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtrYMinF, NhlCtrYMinF, NhlTFloat, sizeof(float),
		  NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_min),
		  NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{ NhlNtrYReverse, NhlCtrYReverse, NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_reverse),
		NhlTImmediate,_NhlUSET(0) ,0,NULL},
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
		NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL}
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
/* class_name			*/	"irregularTransObjClass",
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

	if (ir->irtrans.x_coord_points != NULL)
		NhlFree(ir->irtrans.x_coord_points);
	if (ir->irtrans.x_inter_points != NULL)
		NhlFree(ir->irtrans.x_inter_points);
	if (ir->irtrans.y_coord_points != NULL)
		NhlFree(ir->irtrans.y_coord_points);
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
	char *error_lead;
	float *tmp;
	float tmpf;
	int call_spline_create;
	NhlErrorTypes ret = NhlNOERROR;
	NhlStatus xstatus,ystatus;
	NhlBoolean tmpb, new_x_coords = False, new_y_coords = False;

	if(c_or_s == SET) {
		error_lead = "IrTransSetValues";
		call_spline_create = 0;
		if (_NhlArgIsSet(args,nargs,NhlNtrXMinF))
			irp->x_min_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrXMaxF))
			irp->x_max_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrYMinF))
			irp->y_min_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrYMaxF))
			irp->y_max_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrXCoordPoints) ||
		    _NhlArgIsSet(args,nargs,NhlNtrXAxisType) ||
		    irp->x_min_set || irp->x_max_set) {
			new_x_coords = True;
		}
		
		if (_NhlArgIsSet(args,nargs,NhlNtrYCoordPoints) ||
		    _NhlArgIsSet(args,nargs,NhlNtrYAxisType) ||
		    irp->y_min_set || irp->y_max_set) {
			new_y_coords = True;
		}
	}
	else {
		error_lead = "IrTransInitialize";
		call_spline_create = 1;
		new_x_coords = True;
		new_y_coords = True;
		irp->x_coord_points = NULL; 
		irp->y_coord_points = NULL; 
		irp->x_inter_points = NULL; 
		irp->y_inter_points = NULL; 
		irp->x_num_points = 0;
		irp->y_num_points = 0;
		if (! irp->x_min_set) irp->x_min = 0.0;
		if (! irp->x_max_set) irp->x_max = 1.0;
		if (! irp->y_min_set) irp->y_min = 0.0;
		if (! irp->y_max_set) irp->y_max = 1.0;
	}

		    
	if (new_x_coords) {
		irp->x_use_log = False;
		if (irp->x_min > irp->x_max) {
			tmpf = irp->x_min;
			irp->x_min = irp->x_max;
			irp->x_max = tmpf;
			tmpb = irp->x_min_set;
			irp->x_min_set = irp->x_max_set;
			irp->x_max_set = tmpb;
		}
		switch (irp->x_axis_type) {
		case NhlIRREGULARAXIS:
			if (irp->x_coord_points_ga != NULL ||
			  irp->x_coord_points_ga->num_elements > 0) {
				irp->x_coord_points  = 
					(float *) irp->x_coord_points_ga->data;
				irp->x_num_points =
					irp->x_coord_points_ga->num_elements;
				break;
			}
			NhlPError(NhlWARNING,NhlEUNKNOWN,
      "%s: Must specify %s resource for irregular axis: defaulting to linear",
				  error_lead,NhlNtrXCoordPoints);
			ret = MIN(ret,NhlWARNING);
			irp->x_axis_type = NhlLINEARAXIS;
			/* fall through */
		case NhlLINEARAXIS:
			irp->x_log_lin_points[0] = irp->x_min;
			irp->x_log_lin_points[1] = 
				(irp->x_min + irp->x_max) / 2.0;
			irp->x_log_lin_points[2] = irp->x_max;
			irp->x_coord_points = irp->x_log_lin_points;
			irp->x_num_points = 3;
			break;
		case NhlLOGAXIS:
			if (irp->x_min <= 0.0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: Logarithmic X axis requires minimum X value greater than 0.0: defaulting to linear",
				  error_lead);
				ret = MIN(ret,NhlWARNING);
				irp->x_axis_type = NhlLINEARAXIS;
				irp->x_log_lin_points[1] = 
					(irp->x_min + irp->x_max) / 2.0;
			}
			else {
				irp->x_use_log = True;
				irp->x_log_lin_points[1] = pow(10.0,
				  (log10(irp->x_min)+log10(irp->x_max)) / 2.0);
			}
			irp->x_log_lin_points[0] = irp->x_min;
			irp->x_log_lin_points[2] = irp->x_max;
			irp->x_coord_points = irp->x_log_lin_points;
			irp->x_num_points = 3;
			break;
		}
	}
		    
	if (new_y_coords) {
		irp->y_use_log = False;
		if (irp->y_min > irp->y_max) {
			tmpf = irp->y_min;
			irp->y_min = irp->y_max;
			irp->y_max = tmpf;
			tmpb = irp->y_min_set;
			irp->y_min_set = irp->y_max_set;
			irp->y_max_set = tmpb;
		}
		switch (irp->y_axis_type) {
		case NhlIRREGULARAXIS:
			if (irp->y_coord_points_ga != NULL ||
			    irp->y_coord_points_ga->num_elements > 0) {
				irp->y_coord_points  = 
					(float *) irp->y_coord_points_ga->data;
				irp->y_num_points =
					irp->y_coord_points_ga->num_elements;
				break;
			}
			NhlPError(NhlWARNING,NhlEUNKNOWN,
      "%s: Must specify %s resource for irregular axis: defaulting to linear",
				  error_lead,NhlNtrYCoordPoints);
			ret = MIN(ret,NhlWARNING);
			irp->y_axis_type = NhlLINEARAXIS;
			/* fall through */
		case NhlLINEARAXIS:
			irp->y_log_lin_points[0] = irp->y_min;
			irp->y_log_lin_points[1] = 
				(irp->y_min + irp->y_max) / 2.0;
			irp->y_log_lin_points[2] = irp->y_max;
			irp->y_coord_points = irp->y_log_lin_points;
			irp->y_num_points = 3;
			break;
		case NhlLOGAXIS:
			if (irp->y_min <= 0.0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: Logarithmic Y axis requires minimum Y value greater than 0.0: defaulting to linear",
				  error_lead);
				ret = MIN(ret,NhlWARNING);
				irp->y_axis_type = NhlLINEARAXIS;
				irp->y_log_lin_points[1] = 
					(irp->y_min + irp->y_max) / 2.0;
			}
			else {
				irp->y_use_log = True;
				irp->y_log_lin_points[1] = pow(10.0,
				  (log10(irp->y_min)+log10(irp->y_max)) / 2.0);
			}
			irp->y_log_lin_points[0] = irp->y_min;
			irp->y_log_lin_points[2] = irp->y_max;
			irp->y_coord_points = irp->y_log_lin_points;
			irp->y_num_points = 3;
			break;
		}
	}
	if (irp->x_inter_points_ga != NULL) {
		if (irp->x_inter_points_ga->num_elements != 
		    irp->x_num_points) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
      "%s: %s resource must have same number of elements as %s: ignoring",
				  error_lead,
				  NhlNtrXInterPoints,NhlNtrXCoordPoints);
			ret = MIN(ret,NhlWARNING);
			irp->x_inter_points = NULL;
		}
		else {
			irp->x_inter_points  = 
				(float *) irp->x_inter_points_ga->data;
		}
	}
	if (irp->y_inter_points_ga != NULL) {
		if (irp->y_inter_points_ga->num_elements != 
		    irp->y_num_points) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
      "%s: %s resource must have same number of elements as %s: ignoring",
				  error_lead,
				  NhlNtrYInterPoints,NhlNtrYCoordPoints);
			ret = MIN(ret,NhlWARNING);
			irp->y_inter_points = NULL;
		}
		else {
			irp->y_inter_points  = 
				(float *) irp->y_inter_points_ga->data;
		}
	}

	if(c_or_s == SET){
		if(new_x_coords) {
			call_spline_create = 1;
			if(iold->irtrans.x_coord_points != NULL) {
				NhlFree(iold->irtrans.x_coord_points);
			}
			tmp = irp->x_coord_points;
			irp->x_coord_points 
				= (float*)NhlMalloc((unsigned)
				    sizeof(float) *(irp->x_num_points));
			if (irp->x_coord_points == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s: dynamic memory allocation error",
					  error_lead);
				return NhlFATAL;
			}
			memcpy((char*)irp->x_coord_points,
			       (char*)tmp,sizeof(float) * irp->x_num_points);
		}
		if(new_y_coords) {
			call_spline_create = 1;
			if(iold->irtrans.y_coord_points != NULL) {
				NhlFree(iold->irtrans.y_coord_points);
			}
			tmp = irp->y_coord_points;
			irp->y_coord_points 
				= (float*)NhlMalloc((unsigned)
				    sizeof(float) *(irp->y_num_points));
			if (irp->y_coord_points == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s: dynamic memory allocation error",
					  error_lead);
				return NhlFATAL;
			}
			memcpy((char*)irp->y_coord_points,
			       (char*)tmp,sizeof(float) * irp->y_num_points);
		}
			
	} else {
		tmp = irp->x_coord_points;
		irp->x_coord_points = (float*)NhlMalloc((unsigned)
					sizeof(float) *(irp->x_num_points));
		if (irp->x_coord_points == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: dynamic memory allocation error",
				  error_lead);
			return NhlFATAL;
		}
		memcpy((char*)irp->x_coord_points,(char*)tmp,
		       sizeof(float)*irp->x_num_points);
		
		tmp = irp->y_coord_points;
		irp->y_coord_points = (float*)NhlMalloc((unsigned)
					sizeof(float) *(irp->y_num_points));
		if (irp->y_coord_points == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: dynamic memory allocation error",
				  error_lead);
			return NhlFATAL;
		}
		memcpy((char*)irp->y_coord_points,(char*)tmp,
		       sizeof(float)*irp->y_num_points);
	}
	
	if (new_x_coords && irp->x_axis_type == NhlIRREGULARAXIS) {
		float tmin,tmax;
		tmin = MIN(irp->x_coord_points[0],
			   irp->x_coord_points[irp->x_num_points-1]);
		tmax = MAX(irp->x_coord_points[0],
			   irp->x_coord_points[irp->x_num_points-1]);

		if (! irp->x_min_set)
			irp->x_min = tmin;
		if (! irp->x_max_set)
			irp->x_max = tmax;
		if (irp->x_min < tmin) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: X minimum less than minimum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			irp->x_min = tmin;
		}
		if (irp->x_max > tmax) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: X maximum greater than maximum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			irp->x_max = tmax;
		}
	}
	if (new_y_coords && irp->y_axis_type == NhlIRREGULARAXIS) {
		float tmin,tmax;
		tmin = MIN(irp->y_coord_points[0],
			   irp->y_coord_points[irp->y_num_points-1]);
		tmax = MAX(irp->y_coord_points[0],
			   irp->y_coord_points[irp->y_num_points-1]);

		if (! irp->y_min_set)
			irp->y_min = tmin;
		if (! irp->y_max_set)
			irp->y_max = tmax;
		if (irp->y_min < tmin) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: Y minimum less than minimum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			irp->y_min = tmin;
		}
		if (irp->y_max > tmax) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: Y maximum greater than maximum value of coordinate points array, defaulting",
				  error_lead);
			ret = MIN(ret,NhlWARNING);
			irp->y_max = tmax;
		}
	}

	if((c_or_s == CREATE && irp->x_inter_points != NULL) ||
	   _NhlArgIsSet(args,nargs,NhlNtrXInterPoints)) {
		if((c_or_s == SET)&&(iold->irtrans.x_inter_points != NULL)) {
			NhlFree(iold->irtrans.x_inter_points);
		}
		tmp = irp->x_inter_points;
		irp->x_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (irp->x_num_points));
		if (irp->x_inter_points == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: dynamic memory allocation error",
				  error_lead);
			return NhlFATAL;
		}
		memcpy((char*)irp->x_inter_points,(char*)tmp,
			sizeof(float)*irp->x_num_points);
		call_spline_create = 1;
	}
	
	if((c_or_s == CREATE && irp->y_inter_points != NULL) ||
	   _NhlArgIsSet(args,nargs,NhlNtrYInterPoints)) {
		if((c_or_s == SET)&&(iold->irtrans.y_inter_points != NULL)) {
			NhlFree(iold->irtrans.y_inter_points);
		}
		tmp = irp->y_inter_points;
		irp->y_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (irp->y_num_points));
		if (irp->y_inter_points == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: dynamic memory allocation error",
				  error_lead);
			return NhlFATAL;
		}
		memcpy((char*)irp->y_inter_points,(char*)tmp,
			sizeof(float)*irp->y_num_points);
		call_spline_create = 1;
	}
	if((c_or_s == SET)&&(irp->y_tension != iold->irtrans.y_tension)) {
		call_spline_create = 1;
	}
	if((c_or_s == SET)&&(irp->x_tension !=iold->irtrans.x_tension)) {
		call_spline_create = 1;
	}

	if(call_spline_create) {
		if (c_or_s == SET) {
			ret = _NhlDestroySplineCoordApprox(&(irp->thecoord));
		}
		inew->trobj.change_count++;
		ret = _NhlCreateSplineCoordApprox(&(irp->thecoord),
			irp->x_use_log,
			irp->x_coord_points,
			irp->x_inter_points,
			irp->x_num_points,
			irp->y_use_log,
			irp->y_coord_points,
			irp->y_inter_points,
			irp->y_num_points,
			irp->x_tension,irp->y_tension,
			irp->x_samples,irp->y_samples,
			&xstatus,&ystatus);	
	}
	_NhlEvalSplineCoordForward(&irp->thecoord,
				   irp->x_min,
				   irp->y_min,
				   &(irp->ul),
				   &(irp->ub),
				   NULL,NULL);
	_NhlEvalSplineCoordForward(&irp->thecoord,
				   irp->x_max,
				   irp->y_max,
				   &(irp->ur),
				   &(irp->ut),
				   NULL,NULL);

	if(irp->x_reverse) {
		tmpf = irp->ur;
		irp->ur = irp->ul;
		irp->ul = tmpf;
	}
	if(irp->y_reverse) {
		tmpf = irp->ut;
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

	irp->x_min_set = irp->y_min_set = 
		irp->x_max_set = irp->y_max_set = False;

	if (c_or_s == CREATE) {
		if ((irp->xmin_dat = _NhlCmpFSetup(irp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->xmax_dat = _NhlCmpFSetup(irp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->ymin_dat =_NhlCmpFSetup(irp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->ymax_dat = _NhlCmpFSetup(irp->y_max,5)) == NULL) {
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
		if ((irp->compc_xmax_dat = 
		     _NhlCmpFSetup(irp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->compc_ymin_dat = 
		     _NhlCmpFSetup(irp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((irp->compc_ymax_dat = 
		     _NhlCmpFSetup(irp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		return(ret);
	}
	if (irp->x_min != iold->irtrans.x_min) {
		free(irp->xmin_dat);
		if ((irp->xmin_dat = _NhlCmpFSetup(irp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (irp->x_max != iold->irtrans.x_max) {
		free(irp->xmax_dat);
		if ((irp->xmax_dat = _NhlCmpFSetup(irp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (irp->y_min != iold->irtrans.y_min) {
		free(irp->ymin_dat);
		if ((irp->ymin_dat = _NhlCmpFSetup(irp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (irp->y_max != iold->irtrans.y_max) {
		free(irp->ymax_dat);
		if ((irp->ymax_dat = _NhlCmpFSetup(irp->y_max,5)) == NULL) {
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
	}

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

	c_set(top->x,top->x+top->width,top->y-top->height,top->y,
		tp->ul,tp->ur,tp->ub,tp->ut,tp->log_lin_value);
	
	return ret;
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

        if ((xmndif = _NhlCmpFAny(*x,xmin,5)) < 0 ||
            (xmxdif = _NhlCmpFAny(*x,xmax,5)) > 0 ||
            (ymndif = _NhlCmpFAny(*y,ymin,5)) < 0 ||
            (ymxdif = _NhlCmpFAny(*y,ymax,5)) > 0) {
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
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
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
		ret = _NhlEvalSplineCoordForward(
					   &(iinstance->irtrans.thecoord),
					   x[i],y[i],&(xout[i]),&(yout[i]),
					   NULL,NULL);
	}
	return(ret);
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
		ret = _NhlEvalSplineCoordInverse(
					     &(iinstance->irtrans.thecoord),
					     x[i],y[i],&(xout[i]),&(yout[i]),
					     NULL,NULL);	
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
	static call_frstd = 1;
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
	static call_frstd = 1;
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
	static call_frstd = 1;
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
				IrDataToCompc(instance,&cx,&cy,1,
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
	ret = _NhlWorkstationFill(irinst->trobj.wkptr,xout,yout,ixout+1);

	NhlFree(xbuf);
	NhlFree(ybuf);
	NhlFree(dbuf);
	NhlFree(xout);
	NhlFree(yout);
	return ret;
	
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
	int i, count;
	float *fp;
	char *e_text;
	NhlString entry_name = "IrTransGetValues";


	for( i = 0; i < nargs ; i++) {
		fp = NULL;
		if(args[i].quark == QtrXCoordPoints) {
			if(irp->x_coord_points != NULL &&
			   irp->x_num_points > 0) {		
				fp = irp->x_coord_points;
				count = irp->x_num_points;
			}
		}
		if(args[i].quark == QtrXInterPoints) {
			if(irp->x_inter_points != NULL &&
			   irp->x_num_points > 0) {		
				fp = irp->x_inter_points;
				count = irp->x_num_points;
			}
		}
		if(args[i].quark == QtrYCoordPoints) {
			if(irp->y_coord_points != NULL &&
			   irp->y_num_points > 0) {
				fp = irp->y_coord_points;
				count = irp->y_num_points;
			}
		}
		if(args[i].quark == QtrYInterPoints) {
			if(irp->y_inter_points != NULL 
			   && irp->y_num_points > 0) {
				fp = irp->y_inter_points;
				count = irp->y_num_points;
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
        _NhlEnumVals   axistypelist[] = {
	{NhlIRREGULARAXIS,	"irregularaxis"},
	{NhlLINEARAXIS,		"linearaxis"},
	{NhlLOGAXIS,		"logaxis"}
        };

	_NhlRegisterEnumType(NhlirregularTransObjClass,
			NhlTAxisType,axistypelist,NhlNumber(axistypelist));

	QtrXCoordPoints = NrmStringToQuark(NhlNtrXCoordPoints);
	QtrYCoordPoints = NrmStringToQuark(NhlNtrYCoordPoints);
	QtrXInterPoints = NrmStringToQuark(NhlNtrXInterPoints);
	QtrYInterPoints = NrmStringToQuark(NhlNtrYInterPoints);

	return(NhlNOERROR);	
}
