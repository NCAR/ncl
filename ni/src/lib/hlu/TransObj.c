/*
 *      $Id: TransObj.c,v 1.37 2004-10-05 22:50:34 dbrown Exp $
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
 *	Date:		Fri Oct 16 11:46:05 MDT 1992
 *
 *	Description:	
 */

#include <ncarg/hlu/View.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <math.h>

static NhlResource resources[] =  {

/* Begin-documented-resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.x_min_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTransObjLayerRec,trobj.x_min),
          	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.x_max_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTransObjLayerRec,trobj.x_max),
		NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.x_reverse_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtrXReverse,NhlCtrXReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlTransObjLayerRec,trobj.x_reverse),
		NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.y_min_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTransObjLayerRec,trobj.y_min),
		NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.y_max_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlTransObjLayerRec,trobj.y_max),
		NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.y_reverse_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNtrYReverse,NhlCtrYReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlTransObjLayerRec,trobj.y_reverse),
		NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNtrOutOfRangeF, NhlCtrOutOfRangeF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec,trobj.out_of_range),
		NhlTString, _NhlUSET("1.0e30"),_NhlRES_PRIVATE,NULL },
	{ NhlNtrLineInterpolationOn,NhlCtrLineInterpolationOn,
		NhlTBoolean,sizeof(NhlBoolean),
	        NhlOffset(NhlTransObjLayerRec,trobj.line_interpolation_on),
		NhlTImmediate,
	  	_NhlUSET((NhlPointer)False),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTransObjLayerRec,trobj.grid_type_set),
	         NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtrGridType,NhlCtrGridType,
		NhlTGridType,sizeof(NhlGridType),
	        NhlOffset(NhlTransObjLayerRec,trobj.grid_type),NhlTProcedure,
		_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

/* End-documented-resources */

	{ NhlNtrResolutionF, NhlCtrResolutionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.resolution),
		NhlTString, _NhlUSET("0.002"),_NhlRES_PRIVATE,NULL },
	{ NhlNtrChangeCount, NhlCtrChangeCount, NhlTInteger, sizeof(int),
		NhlOffset(NhlTransObjLayerRec, trobj.change_count),
		NhlTImmediate, _NhlUSET((NhlPointer) 0),
          	_NhlRES_GONLY|_NhlRES_PRIVATE,NULL },
	{ NhlNtrDataXStartF, NhlCtrDataXStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_xstart),
		NhlTString, _NhlUSET("0.0"),0,NULL },
	{ NhlNtrDataXEndF, NhlCtrDataXEndF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_xend),
		NhlTString, _NhlUSET("0.0"),0,NULL },
	{ NhlNtrDataYStartF, NhlCtrDataYStartF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_ystart),
		NhlTString, _NhlUSET("0.0"),0,NULL },
	{ NhlNtrDataYEndF, NhlCtrDataYEndF, NhlTFloat, sizeof(float),
	  NhlOffset(NhlTransObjLayerRec, trobj.data_yend),
	  NhlTString, _NhlUSET("0.0"),0,NULL },
/*
 * these are not yet in use -- they will indicate to the transobj whether
 * cell-centered coordinates must be interpolated when cell bounds are
 * given -- but the rendering method only understands cell centered coordinates
 */
	{ NhlNtrXCIsBounds, NhlCtrXCIsBounds, NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlTransObjLayerRec, trobj.xc_isbounds),
	  NhlTImmediate, _NhlUSET((NhlPointer)False),0,NULL },
	{ NhlNtrYCIsBounds, NhlCtrYCIsBounds, NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlTransObjLayerRec, trobj.yc_isbounds),
	  NhlTImmediate, _NhlUSET((NhlPointer)False),0,NULL },
	{ NhlNtrDoBounds, NhlCtrDoBounds, NhlTBoolean, sizeof(NhlBoolean),
	  NhlOffset(NhlTransObjLayerRec, trobj.do_bounds),
	  NhlTImmediate, _NhlUSET((NhlPointer)True),0,NULL },

};
static NhlErrorTypes TransInitialize(
#if	NhlNeedProto
        NhlClass,     	/* class */
        NhlLayer,       /* req */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes  TransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes TransSetTrans(
#if	NhlNeedProto
	NhlLayer	tobj,
	NhlLayer	vobj
#endif
);

static NhlErrorTypes TransCopyPoints(
#if	NhlNeedProto
	NhlLayer	tl,
	float		*x,
	float		*y,
	int		n,
	float		*xout,
	float		*yout,
	float		*xmissing,
	float		*ymissing,
	int		*status
#endif
);

static NhlErrorTypes TransLineTo(
#if	NhlNeedProto
	NhlLayer	tl,
	float		x,
	float		y,
	int		upordown
#endif
);

static NhlErrorTypes TransDataPolygon(
#if	NhlNeedProto
	NhlLayer	tl,
	float		*x,
	float		*y,
	int		n
#endif
);

static NhlErrorTypes TransObjClassPartInit(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

NhlTransObjClassRec NhltransObjClassRec = {
	{
/* class_name */        "transformationClass",
/* nrm_class */         NrmNULLQUARK,
/* layer_size */        sizeof(NhlTransObjLayerRec),
/* class_inited */      False,
/* superclass*/         (NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources */   resources,
/* num_resources */     NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize */     TransObjClassPartInit,
/* class_initialize */  NULL,
/* layer_initialize */  TransInitialize,
/* layer_set_values */  TransSetValues,
/* layer_set_values_hook */  NULL,
/* layer_get_values */  NULL,
/* layer_reparent */  NULL,
/* layer_destroy */    NULL,
	},
	{
/* set_trans */		TransSetTrans,
/* trans_type */	NULL,
/* win_to_ndc */	TransCopyPoints,
/* ndc_to_win */	TransCopyPoints,
/* data_to_win */	TransCopyPoints,
/* win_to_data */	TransCopyPoints,
/* data_to_compc */	TransCopyPoints,
/* compc_to_data */	TransCopyPoints,
/* win_to_compc */	TransCopyPoints,
/* compc_to_win */	TransCopyPoints,
/* data_lineto */	TransLineTo,
/* compc_lineto */	TransLineTo,
/* win_lineto */	TransLineTo,
/* NDC_lineto */	TransLineTo,
/* data_polygon */      TransDataPolygon
	}
};

NhlClass NhltransObjClass = (NhlClass)&NhltransObjClassRec;

static NhlErrorTypes
TransObjClassPartInit
#if	NhlNeedProto
(
	NhlClass	lc
)
#else
(lc)
	NhlClass	lc;
#endif
{
	NhlTransObjClass	tlc = (NhlTransObjClass)lc;
	NhlTransObjClass	sc = (NhlTransObjClass)
						lc->base_class.superclass;

/*
 * Note: axis type and transformation type are TransObj class
 * resources. However, the must be initialized both by the Transform 
 * and the TransObj classes, because they are intercepted by Transform
 * class objects. If changes are made be sure to duplicate in both
 * classes.
 */
        _NhlEnumVals   axistypelist[] = {
	{NhlIRREGULARAXIS,	"IrregularAxis"},
	{NhlLINEARAXIS,		"LinearAxis"},
	{NhlLOGAXIS,		"LogAxis"}
        };


        _NhlEnumVals   gridtypelist[] = {
		{NhltrMAP, "Map"},
		{NhltrLOGLIN, "LogLin"},
		{NhltrIRREGULAR, "Irregular"},
		{NhltrCURVILINEAR, "Curvilinear"},
		{NhltrSPHERICAL, "Spherical"},
		{NhltrTRIANGULARMESH, "TriangularMesh"},
        };

	_NhlRegisterEnumType(NhltransObjClass,
			NhlTAxisType,axistypelist,NhlNumber(axistypelist));

	_NhlRegisterEnumType(NhltransObjClass,
			     NhlTGridType,
			     gridtypelist,
			     NhlNumber(gridtypelist));


	if(tlc->trobj_class.win_to_ndc == NhlInheritTransPoint)
		tlc->trobj_class.win_to_ndc = sc->trobj_class.win_to_ndc;
	if(tlc->trobj_class.ndc_to_win == NhlInheritTransPoint)
		tlc->trobj_class.ndc_to_win = sc->trobj_class.ndc_to_win;

	if(tlc->trobj_class.data_to_win == NhlInheritTransPoint)
		tlc->trobj_class.data_to_win = sc->trobj_class.data_to_win;
	if(tlc->trobj_class.win_to_data == NhlInheritTransPoint)
		tlc->trobj_class.win_to_data = sc->trobj_class.win_to_data;

	if(tlc->trobj_class.data_to_compc == NhlInheritTransPoint)
		tlc->trobj_class.data_to_compc = sc->trobj_class.data_to_compc;
	if(tlc->trobj_class.compc_to_data == NhlInheritTransPoint)
		tlc->trobj_class.compc_to_data = sc->trobj_class.compc_to_data;
	if(tlc->trobj_class.win_to_compc == NhlInheritTransPoint)
		tlc->trobj_class.win_to_compc = sc->trobj_class.win_to_compc;
	if(tlc->trobj_class.compc_to_win == NhlInheritTransPoint)
		tlc->trobj_class.compc_to_win = sc->trobj_class.compc_to_win;
	return NhlNOERROR;
}


/*
 * Function:	TransInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	
 */
/*ARGSUSED*/
static NhlErrorTypes
TransInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass   class;
        NhlLayer        req;
        NhlLayer        new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name = "TransInitialize";
	char			*e_text;
	NhlTransObjLayer	tnew = (NhlTransObjLayer) new;
	NhlTransObjLayerPart	*tp = &tnew->trobj;
        float			ftmp;
        NhlBoolean		btmp;

        if (! tp->x_min_set) {
                tp->x_min = 0.0;
        }
        if (! tp->x_max_set) {
                tp->x_max = 1.0;
        }
        if (tp->x_min == tp->x_max) {
		e_text = "%s: Zero X coordinate span: defaulting";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEZEROSPAN,e_text,entry_name);
		tp->x_min = 0.0;
                tp->x_min_set = False;
		tp->x_max = 1.0;
                tp->x_max_set = False;
	}
	else if (tp->x_min > tp->x_max) {
		e_text = "%s: min X coordinate exceeds max: exchanging";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ftmp = tp->x_min;
		tp->x_min = tp->x_max;
		tp->x_max = ftmp;
                btmp = tp->x_min_set;
                tp->x_min_set = tp->x_max_set;
                tp->x_max_set = btmp;
	}
                        
        if (! tp->x_reverse_set) {
                tp->x_reverse = False;
        }
        if (! tp->y_min_set) {
                tp->y_min = 0.0;
        }
        if (! tp->y_max_set) {
                tp->y_max = 1.0;
        }
        if (tp->y_min == tp->y_max) {
		e_text = "%s: Zero Y coordinate span: defaulting";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEZEROSPAN,e_text,entry_name);
		tp->y_min = 0.0;
                tp->y_min_set = False;
		tp->y_max = 1.0;
                tp->y_max_set = False;
	}
	else if (tp->y_min > tp->y_max) {
		e_text = "%s: min Y coordinate exceeds max: exchanging";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ftmp = tp->y_min;
		tp->y_min = tp->y_max;
		tp->y_max = ftmp;
                btmp = tp->y_min_set;
                tp->y_min_set = tp->y_max_set;
                tp->y_max_set = btmp;
	}
        if (! tp->y_reverse_set) {
                tp->y_reverse = False;
        }
        tp->off_screen = False;

	if (!tp->grid_type_set)
		tp->grid_type = NhltrLOGLIN;

	return ret;
}

/*
 * Function:	TransSetValues
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
static NhlErrorTypes TransSetValues
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
	NhlTransObjLayer tnew = (NhlTransObjLayer) new;
	NhlTransObjLayerPart *tp = &tnew->trobj;

        if (_NhlArgIsSet(args,num_args,NhlNtrXMinF))
                tp->x_min_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNtrXMaxF))
                tp->x_max_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNtrXReverse))
                tp->x_reverse_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNtrYMinF))
                tp->y_min_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNtrYMaxF))
                tp->y_max_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNtrYReverse))
                tp->y_reverse_set = True;
        if (_NhlArgIsSet(args,num_args,NhlNtrGridType))
                tp->grid_type_set = True;
                        
        return NhlNOERROR;
}

static NhlErrorTypes
TransSetTrans
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
	NhlErrorTypes ret;
	NhlTransObjLayerPart	*tp = &((NhlTransObjLayer)tobj)->trobj;

	tp->wkptr = vobj->base.wkptr;

	ret = NhlVAGetValues(vobj->base.id,
			     NhlNvpXF,	&tp->x,
			     NhlNvpYF,	&tp->y,
			     NhlNvpWidthF,	&tp->width,
			     NhlNvpHeightF,	&tp->height,
			     NULL);
	if(ret < NhlWARNING)
		return ret;
	if (! tp->line_interpolation_on)
		tp->point_count = 1;
	else {
		if (tp->resolution <= 0.0) tp->resolution = 0.002;
		tp->point_count = (int) 
			MAX(1.0,(tp->width + tp->height) / 
			    (2.0 * tp->resolution));
	}

	return NhlNOERROR;
}

static NhlErrorTypes
TransCopyPoints
#if	NhlNeedProto
(
	NhlLayer	tl,
	float		*x,
	float		*y,
	int		n,
	float		*xout,
	float		*yout,
	float		*xmissing,
	float		*ymissing,
	int		*status
)
#else
(tl,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer	tl;
	float		*x;
	float		*y;
	int		n;
	float		*xout;
	float		*yout;
	float		*xmissing;
	float		*ymissing;
	int		*status;
#endif
{
	if(x != xout)
		memcpy((char*)xout,(Const char*)x,n*sizeof(float));
	if(y != yout)
		memcpy((char*)yout,(Const char*)y,n*sizeof(float));

	return NhlNOERROR;
}

static NhlErrorTypes
TransLineTo
#if	NhlNeedProto
(
	NhlLayer	tl,
	float		x,
	float		y,
	int		upordown
)
#else
(tl,x,y,upordown)
	NhlLayer	tl;
	float		x;
	float		y;
	int		upordown;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"LineTo Function not defined for (%s) class",
		tl->base.layer_class->base_class.class_name);
	return NhlFATAL;
}

static NhlErrorTypes
TransDataPolygon
#if	NhlNeedProto
(
	NhlLayer	tl,
	float		*x,
	float		*y,
	int		n
)
#else
(tl,x,y,upordown)
	NhlLayer	tl;
	float		*x;
	float		*y;
	int		n;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"DataPolygon Function not defined for (%s) class",
		tl->base.layer_class->base_class.class_name);
	return NhlFATAL;
}

NhlErrorTypes _NhlDataLineTo 
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance,x,y,upordown)
NhlLayer instance;
float	x;
float y;
int upordown;
#endif
{
	NhlTransObjClass tlc = (NhlTransObjClass)
						instance->base.layer_class;

	return((*tlc->trobj_class.data_lineto)(instance,x,y,upordown));
}

NhlErrorTypes _NhlWinLineTo 
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance,x,y,upordown)
NhlLayer instance;
float	x;
float y;
int upordown;
#endif
{
	NhlTransObjClass tlc = (NhlTransObjClass)
						instance->base.layer_class;

	return((*tlc->trobj_class.win_lineto)(instance,x,y,upordown));
}

NhlErrorTypes _NhlCompcLineTo 
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance,x,y,upordown)
NhlLayer instance;
float	x;
float y;
int upordown;
#endif
{
	NhlTransObjClass tlc = (NhlTransObjClass)
						instance->base.layer_class;

	return((*tlc->trobj_class.compc_lineto)(instance,x,y,upordown));
}

NhlErrorTypes _NhlNDCLineTo 
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance,x,y,upordown)
NhlLayer instance;
float	x;
float y;
int upordown;
#endif
{
	NhlTransObjClass tlc = (NhlTransObjClass)
						instance->base.layer_class;

	return((*tlc->trobj_class.NDC_lineto)(instance,x,y,upordown));
}

NhlErrorTypes _NhlDataPolygon 
#if	NhlNeedProto
(NhlLayer instance, float *x, float *y, int n)
#else
(instance,x,y,n)
NhlLayer instance;
float	*x;
float *y;
int n;
#endif
{
	NhlTransObjClass tlc = (NhlTransObjClass)
						instance->base.layer_class;

	return((*tlc->trobj_class.data_polygon)(instance,x,y,n));
}

#define CTOP 010
#define CBOTTOM 04
#define CRIGHT 02
#define CLEFT  01
#define C0TOP 0200
#define C0BOTTOM 0100
#define C0RIGHT 040
#define C0LEFT 020

/*ARGSUSED*/
void _NhlTransClipLine
#if	NhlNeedProto
(float xmin, float xmax, float ymin, float ymax, float *x0, float *y0, float *x1, float *y1,float missing)
#else
(xmin, xmax, ymin, ymax, x0, y0, x1, y1,missing)
float xmin;
float xmax;
float ymin;
float ymax;
float *x0;
float *y0;
float *x1;
float *y1;
float missing;
#endif
{

	int ready = 0;
        unsigned int outcodea = 0,outcodeb = 0;
	float dx,dy;


	        while (!ready) {


                /*
                * These set the outcodes as discussed in class
                */

                if( *x1 > xmax ) 
			outcodea |= CRIGHT;
                else if( *x1 < xmin ) 
			outcodea |= CLEFT;
                if( *y1 > ymax) 
			outcodea |= CTOP;
                else if( *y1 < ymin ) 
			outcodea |= CBOTTOM;


                if( *x0 > xmax ) 
			outcodeb |= CRIGHT;
                else if( *x0 < xmin ) 
			outcodeb |= CLEFT;
                if( *y0 > ymax) 
			outcodeb |= CTOP;
                else if( *y0 < ymin ) 
			outcodeb |= CBOTTOM;

                /*
                * The following determines what case is cxrrent
                */
                /*
                * Completely out
                */
                if(outcodeb & outcodea){
			*x0 = *x1 = *y0 = *y1 = missing;
                        outcodeb = 0; outcodea = 0;
                        break;
                }else if((outcodea == 0)&& (outcodeb == 0)) 
			ready = 1;
                else {
                        /*
                        * Choose one of the endpoints and procede to trae
                        * port back into yiewport The slope is determined
                        * to figxre out direction of trace
                        */
                        dx = *x1 - *x0;
                        dy = *y1 - *y0;
                        if(dx == 0) {
                        /*
                        * Case of straight line xp and down
                        */
                                if( outcodea & CTOP) *y1 = ymax;
                                else if( outcodeb & CTOP) *y0 = ymax;
                                if( outcodea & CBOTTOM) *y1 = ymin;
                                else if(outcodeb & CBOTTOM) *y0 = ymin;
                        } else if( dy == 0) {
                        /*
                        * case of straight line right left
                        */
                                if( outcodea & CRIGHT) *x1 = xmax;
                                else if( outcodeb & CRIGHT) *x0 = xmax;
                                if( outcodea & CLEFT) *x1 = xmin;
                                else if(outcodeb & CLEFT) *x0 = xmin;
                        } else {

                                /*
                                * Each of these figxre out intersection with
                                * yiewport only one is processed at atime to
                                * ayoid cycles.
                                */

                                if(outcodea & CTOP) {
                                        *x1 = *x1 - dx/dy * (*y1- ymax);
                                        *y1 = ymax;
                                } else if( outcodea & CBOTTOM) {
                                        *x1 = *x1 + dx/dy * (ymin - *y1);
                                        *y1 = ymin;
                                } else if( outcodea & CRIGHT) {
                                        *y1 = *y1 - dy/dx * (*x1 - xmax);
                                        *x1 = xmax;
                                } else if( outcodea & CLEFT) {
                                        *y1 = *y1 + dy/dx * (xmin - *x1);
                                        *x1 = xmin;
                                } else if(outcodeb & CTOP) {
                                        *x0 = *x0 - dx/dy * (*y0- ymax);
                                        *y0 = ymax;
                                } else if( outcodeb & CBOTTOM) {
                                        *x0 = *x0 + dx/dy * (ymin - *y0);
                                        *y0 = ymin;
                                } else if( outcodeb & CRIGHT) {
                                        *y0 = *y0 - dy/dx * (*x0 - xmax);
                                        *x0 = xmax;
                                } else if( outcodeb & CLEFT) {
                                        *y0 = *y0 + dy/dx * (xmin - *x0);
                                        *x0 = xmin;
                                }
                        }
                }
                outcodea = outcodeb = 0;
        }

}

NhlErrorTypes _NhlTransLLUSet
#if NhlNeedProto
(
    float vl,
    float vr,
    float vb,
    float vt,
    float wl,
    float wr,
    float wb,
    float wt,
    int lf,
    NhlBoolean *off_screen,
    NhlString entry
)
#else
(vl,vr,vb,vt,wl,wr,wb,wt,lf,off_screen,entry)
    float vl;
    float vr;
    float vb;
    float vt;
    float wl;
    float wr;
    float wb;
    float wt;
    int lf;
    NhlBoolean *off_screen;
    NhlString entry;
#endif
{
        float fl,fr,fb,ft,ul,ur,ub,ut;
        float fwidth,fheight,uwidth,uheight;
        *off_screen = False;

        if (vl >= 0.0 && vr <= 1.0 && vb >=0.0 && vt <= 1.0) {
                _NHLCALLF(set,SET) (&vl,&vr,&vb,&vt,&wl,&wr,&wb,&wt,&lf);
                return NhlNOERROR;
        }

        if (vl >= 1.0 || vr <= 0.0 || vb >= 1.0 || vt <= 0.0) {
                *off_screen = True;
                NHLPERROR((NhlINFO,NhlEUNKNOWN,
                    "%s: plot entirely outside viewspace; cannot draw",entry));
                return NhlINFO;
        }

        fwidth = vr-vl;
        fheight = vt-vb;

        fl = MAX(0.0,vl);
        fr = MIN(1.0,vr);
        fb = MAX(0.0,vb);
        ft = MIN(1.0,vt);

        switch (lf) {
            case 1:
            default:
                    uwidth = wr-wl;
                    ul = (vl < 0.0) ? wl + uwidth * (-vl / fwidth) : wl;
                    ur = (vr > 1.0) ? wr - uwidth * ((vr-1.0) / fwidth) : wr;
                    uheight = wt-wb;
                    ub = (vb < 0.0) ? wb + uheight * (-vb / fheight) : wb;
                    ut = (vt > 1.0) ? wt - uheight * ((vt-1.0) / fheight) : wt;
                    break;
            case 2:
                    uwidth = wr-wl;
                    ul = (vl < 0.0) ?  wl + uwidth * (-vl / fwidth) : wl;
                    ur = (vr > 1.0) ?  wr - uwidth * ((vr-1.0) / fwidth) : wr;
                    uheight = log10(wt)-log10(wb);
                    ub = (vb < 0.0) ?
                            pow(10,log10(wb) + uheight * (-vb / fheight)) : wb;
                    ut = (vt > 1.0) ?
                            pow(10,log10(wt)
                                - uheight * ((vt-1.0) / fheight)) : wt;
                    break;
            case 3:
                    uwidth = log10(wr)-log10(wl);
                    ul = (vl < 0.0) ?
                            pow(10,log10(wl) + uwidth * (-vl / fwidth)) : wl;
                    ur = (vr > 1.0) ?
                            pow(10,log10(wr)
                                - uwidth * ((vr-1.0) / fwidth)) : wr;
                    uheight = wt-wb;
                    ub = (vb < 0.0) ? wb + uheight * (-vb / fheight) : wb;
                    ut = (vt > 1.0) ? wt - uheight * ((vt-1.0) / fheight) : wt;
                    break;
            case 4:
                    uwidth = log10(wr)-log10(wl);
                    ul = (vl < 0.0) ?
                            pow(10,log10(wl) + uwidth * (-vl / fwidth)) : wl;
                    ur = (vr > 1.0) ?
                            pow(10,log10(wr)
                                - uwidth * ((vr-1.0) / fwidth)) : wr;
                    uheight = log10(wt)-log10(wb);
                    ub = (vb < 0.0) ?
                            pow(10,log10(wb) + uheight * (-vb / fheight)) : wb;
                    ut = (vt > 1.0) ?
                            pow(10,log10(wt) -
                                uheight * ((vt-1.0) / fheight)) : wt;
                    break;
        }

        _NHLCALLF(set,SET) (&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&lf);
        return NhlNOERROR;
}
