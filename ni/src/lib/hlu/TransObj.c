/*
 *      $Id: TransObj.c,v 1.9 1994-12-16 20:04:51 boote Exp $
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

#include <ncarg/hlu/hluP.h>

#include <ncarg/hlu/TransObjP.h>

static NhlResource resources[] =  {

/* Begin-documented-resources */

	{ NhlNtrOutOfRangeF, NhlCtrOutOfRangeF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTransObjLayerRec, trobj.out_of_range),
		NhlTString, "-9999.0",0,NULL }

/* End-documented-resources */

};

NhlTransObjLayerClassRec NhltransObjLayerClassRec = {
	{
/* class_name */        "transObjLayerClass",
/* nrm_class */         NrmNULLQUARK,
/* layer_size */        sizeof(NhlTransObjLayerRec),
/* class_inited */      False,
/* superclass*/         (NhlLayerClass)&NhlobjLayerClassRec,

/* layer_resources */   resources,
/* num_resources */     NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize */     NULL,
/* class_initialize */  NULL,
/* layer_initialize */  NULL,
/* layer_set_values */  NULL,
/* layer_set_values_hook */  NULL,
/* layer_get_values */  NULL,
/* layer_reparent */  NULL,
/* layer_destroy */    NULL,
	},
	{
/* set_trans */		NULL,
/* trans_type */	NULL,
/* win_to_ndc */	NULL,
/* ndc_to_win */	NULL,
/* data_to_win */	NULL,
/* win_to_data */	NULL,
/* data_to_compc */	NULL,
/* compc_to_data */	NULL,
/* win_to_compc */	NULL,
/* compc_to_win */	NULL,
/* data_lineto */	NULL,
/* compc_lineto */	NULL,
/* win_lineto */	NULL,
/* NDC_lineto */	NULL
	}
};

NhlLayerClass NhltransObjLayerClass = (NhlLayerClass)&NhltransObjLayerClassRec;


#define CTOP 010
#define CBOTTOM 04
#define CRIGHT 02
#define CLEFT  01
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
                * These set the outcodes as discxssed in class
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

static NhlErrorTypes CallDataLineTo 
#if	NhlNeedProto
(NhlLayerClass lc, NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
NhlLayerClass lc;
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
	NhlTransObjLayerClass tlc = (NhlTransObjLayerClass)lc;

	if(tlc->trobj_class.data_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallDataLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlDataLineTo: Transformation object of type (%s) does not have data_lineto function",tlc->base_class.class_name);
			return(NhlWARNING);
		}
	} else {
		return((*tlc->trobj_class.data_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlDataLineTo 
#if	NhlNeedProto
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
NhlLayer instance;
NhlLayer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallDataLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

static NhlErrorTypes CallWinLineTo 
#if	NhlNeedProto
(NhlLayerClass lc, NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
NhlLayerClass lc;
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
	NhlTransObjLayerClass tlc = (NhlTransObjLayerClass)lc;

	if(tlc->trobj_class.win_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallWinLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlWinLineTo: Transformation object of type (%s) does not have win_lineto function",tlc->base_class.class_name);
			return(NhlWARNING);
		}
	} else {
		return((*tlc->trobj_class.win_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlWinLineTo 
#if	NhlNeedProto
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
NhlLayer instance;
NhlLayer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallWinLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

static NhlErrorTypes CallCompcLineTo 
#if	NhlNeedProto
(NhlLayerClass lc, NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
NhlLayerClass lc;
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
	NhlTransObjLayerClass tlc = (NhlTransObjLayerClass)lc;

	if(tlc->trobj_class.compc_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallCompcLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlCompcLineTo: Transformation object of type (%s) does not have compc_lineto function",tlc->base_class.class_name);
			return(NhlWARNING);
		}
	} else {
		return((*tlc->trobj_class.compc_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlCompcLineTo 
#if	NhlNeedProto
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
NhlLayer instance;
NhlLayer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallCompcLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

static NhlErrorTypes CallNDCLineTo 
#if	NhlNeedProto
(NhlLayerClass lc, NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
NhlLayerClass lc;
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
	NhlTransObjLayerClass tlc = (NhlTransObjLayerClass)lc;

	if(tlc->trobj_class.NDC_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallNDCLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlNDCLineTo: Transformation object of type (%s) does not have NDC_lineto function",tlc->base_class.class_name);
			return(NhlWARNING);
		}
	} else {
		return((*tlc->trobj_class.NDC_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlNDCLineTo 
#if	NhlNeedProto
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
NhlLayer instance;
NhlLayer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallNDCLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

