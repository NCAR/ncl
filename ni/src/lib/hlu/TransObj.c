/*
 *      $Id: TransObj.c,v 1.23 1997-07-25 21:12:48 dbrown Exp $
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

static NhlResource resources[] =  {

/* Begin-documented-resources */

	{ NhlNtrOutOfRangeF, NhlCtrOutOfRangeF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.out_of_range),
		NhlTString, _NhlUSET("1.0e12"),0,NULL },
	{ NhlNtrResolutionF, NhlCtrResolutionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.resolution),
		NhlTString, _NhlUSET("0.002"),0,NULL },

/* End-documented-resources */

	{ NhlNtrChangeCount, NhlCtrChangeCount, NhlTInteger, sizeof(int),
		NhlOffset(NhlTransObjLayerRec, trobj.change_count),
		NhlTImmediate, _NhlUSET((NhlPointer) 0),
          	_NhlRES_GONLY|_NhlRES_PRIVATE,NULL },
	{ NhlNtrDataXMinF, NhlCtrDataXMinF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_xmin),
		NhlTString, _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL },
	{ NhlNtrDataXMaxF, NhlCtrDataXMaxF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_xmax),
		NhlTString, _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL },
	{ NhlNtrDataYMinF, NhlCtrDataYMinF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_ymin),
		NhlTString, _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL },
	{ NhlNtrDataYMaxF, NhlCtrDataYMaxF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.data_ymax),
		NhlTString, _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL }

};

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
/* class_name */        "transObjClass",
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
/* layer_initialize */  NULL,
/* layer_set_values */  NULL,
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
	if (tp->resolution <= 0.0) tp->resolution = 0.002;
	tp->point_count = (int) 
		MAX(1.0,(tp->width + tp->height) / (2.0 * tp->resolution));

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
                                        *x0 = *x0 + dx/dy * (ymin - *y0)
;
                                        *y0 = ymin;
                                } else if( outcodeb & CRIGHT) {
                                        *y0 = *y0 - dy/dx * (*x0 - xmax);
                                        *x0 = xmax;
                                } else if( outcodeb & CLEFT) {
                                        *y0 = *y0 + dy/dx * (xmin - *x0)
;
                                        *x0 = xmin;
                                }
                        }
                }
                outcodea = outcodeb = 0;
        }

}

