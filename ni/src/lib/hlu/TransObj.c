
/*
 *      $Id: TransObj.c,v 1.2 1993-05-27 19:11:25 ethan Exp $
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

#include <stdio.h>
#include <ncarg/hlu/hluP.h>

#include <ncarg/hlu/TransObjP.h>

static NhlErrorTypes TrSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes TrInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);


static NhlResource resources[] = {
	{ NhlNtrDashPattern, NhlCtrDashPattern, NhlTInteger, sizeof(int),
		NhlOffset(TransObjLayerRec,trobj.dash_pattern),NhlTString,"0"},
	{ NhlNtrLineLabel, NhlCtrLineLabel, NhlTString, sizeof(char*),
		NhlOffset(TransObjLayerRec,trobj.line_label),
		NhlTImmediate,(NhlPointer)NULL},
	{ NhlNtrLineThicknessF, NhlCtrLineThicknessF, NhlTFloat, sizeof(float),
		NhlOffset(TransObjLayerRec,trobj.line_thickness),
		NhlTString,"1.0"},
	{ NhlNtrLineLabelFontHeightF, NhlCtrLineLabelFontHeightF, NhlTFloat, 
		sizeof(float), 
		NhlOffset(TransObjLayerRec,trobj.line_label_font_height),
		NhlTString,"0.0125" },
	{ NhlNtrLineDashSegLenF, NhlCtrLineDashSegLenF,NhlTFloat, sizeof(float),
		NhlOffset(TransObjLayerRec,trobj.line_dash_seglen),NhlTString,
		".15" },
	{ NhlNtrLineColor, NhlCtrLineColor,NhlTInteger, sizeof(int),
		NhlOffset(TransObjLayerRec,trobj.line_color),NhlTString,
		"1" }
};
TransObjLayerClassRec transObjLayerClassRec = {
	{
/* superclass*/         (LayerClass)&baseLayerClassRec,
/* class_name */        "TransObj",
/* nrm_class */         NrmNULLQUARK,
/* layer_size */        sizeof(TransObjLayerRec),
/* layer_resources */   resources,
/* num_resources */     NhlNumber(resources),
/* child_resources */	NULL,
/* all_resources */     NULL,
/* class_part_initialize */     NULL,
/* class_inited */      False,
/* class_initialize */  NULL,
/* layer_initialize */  TrInitialize,
/* layer_set_values */  TrSetValues,
/* layer_set_values_not */  NULL,
/* layer_get_values */  NULL,
/* layer_pre_draw */    NULL,
/* layer_draw */        NULL,
/* layer_draw_segonly */NULL,
/* layer_post_draw */   NULL,
/* layer_clear */       NULL,
/* layer_destroy */    NULL 
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

LayerClass transObjLayerClass = (LayerClass)&transObjLayerClassRec;


char *dash_patterns[] = { "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
                 "$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'",
                 "$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'",
                 "$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'",
                 "$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'",
                 "$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'",
                 "$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'",
                 "$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'",
                 "$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'",
                 "$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''",
                 "$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''",
                 "$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''",
                 "$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''",
                 "$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''",
                 "$$$$$'$'$$$$$'$'$$$$$'$'$$$$$'$$$$$'$'$$$$$'$'",
                 "$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'",
};
/*ARGSUSED*/
static NhlErrorTypes TrInitialize
#if   __STDC__
(LayerClass class, Layer req, Layer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        LayerClass      class;
        Layer           req;
        Layer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
        TransObjLayer tnew = (TransObjLayer)new;
	char * tmp;

	if(tnew->trobj.line_label != NULL) {
		tmp = (char*)NhlMalloc((unsigned)strlen(tnew->trobj.line_label)+1);
		strcpy(tmp,tnew->trobj.line_label);
		tnew->trobj.line_label = tmp;
	}
	return(NOERROR);
}
/*ARGSUSED*/
static NhlErrorTypes TrSetValues
#if __STDC__
(Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        Layer   old;
        Layer   reference;
        Layer   new;
        _NhlArgList     args;
        int     num_args;
#endif
{
        TransObjLayer tnew = (TransObjLayer)new;
        TransObjLayer told = (TransObjLayer)old;
	char *tmp;


	if( (told->trobj.line_label != tnew->trobj.line_label)) {

		if(told->trobj.line_label != NULL){
			NhlFree(told->trobj.line_label);
			told->trobj.line_label = NULL;
		}
		if(tnew->trobj.line_label != NULL) {
			tmp = (char*)NhlMalloc((unsigned)strlen(tnew->trobj.line_label)+1);
			strcpy(tmp,tnew->trobj.line_label);
			tnew->trobj.line_label = tmp;
		}
	}
	return(NOERROR);
}
/*ARGSUSED*/
void _NhlSetLineInfo
#if  __STDC__
(Layer instance,Layer plot)
#else
(instance,plot)
	Layer instance;
	Layer plot;
#endif
{
	TransObjLayer tinst = (TransObjLayer)instance;
	float	fl,fr,fb,ft,ul,ur,ub,ut;
	float  y0,y1,x0,x1;
	int ll,i;
	char buffer[80];

        for(i = 0; i< 80; i++)
                buffer[i] = '\0';

	c_sflush();

	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

	y0 = fb;
        y1 = fb + tinst->trobj.line_label_font_height;
        y0 = (float)c_kfpy(y0);
        y1 = (float)c_kfpy(y1);

	tinst->trobj.char_size = (int) (y1 - y0);
	if(tinst->trobj.char_size < 4) {
		tinst->trobj.char_size = 4;
	}
        x0 = fl;
        x1 = fl + tinst->trobj.line_dash_seglen;
        x0 = (float)c_kfpy(x0);
        x1 = (float)c_kfpy(x1);
	
	tinst->trobj.dash_dollar_size = (int)((x1-x0)/
		strlen(dash_patterns[(tinst->trobj.dash_pattern-1)%16])+.5);
	if(tinst->trobj.dash_dollar_size < 1) 
			tinst->trobj.dash_dollar_size = 1;

	strcpy(buffer,dash_patterns[(tinst->trobj.dash_pattern-1)%16]);
	if(tinst->trobj.line_label != NULL) {
		  strcpy(&(buffer[strlen(dash_patterns[(tinst->trobj.dash_pattern-1)%16])
			- strlen(tinst->trobj.line_label)]) ,
			tinst->trobj.line_label);
	}
	gset_line_colr_ind((Gint)_NhlGetGksCi(plot->base.wkptr,tinst->trobj.line_color));
	gset_linewidth(tinst->trobj.line_thickness);
	c_dashdc(buffer,tinst->trobj.dash_dollar_size,tinst->trobj.char_size);
	return;
}
#define CTOP 010
#define CBOTTOM 04
#define CRIGHT 02
#define CLEFT  01
/*ARGSUSED*/
void _NhlTransClipLine
#if __STDC__
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

NhlErrorTypes CallDataLineTo 
#if  __STDC__
(LayerClass lc, Layer instance, Layer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
LayerClass lc;
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	TransObjLayerClass tlc = (TransObjLayerClass)lc;

	if(tlc->trobj_class.data_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallDataLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(WARNING,E_UNKNOWN,"_NhlDataLineTo: Transformation object of type (%s) does not have data_lineto function",tlc->base_class.class_name);
			return(WARNING);
		}
	} else {
		return((*tlc->trobj_class.data_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlDataLineTo 
#if __STDC__
(Layer instance, Layer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
Layer instance;
Layer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallDataLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

NhlErrorTypes CallWinLineTo 
#if  __STDC__
(LayerClass lc, Layer instance, Layer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
LayerClass lc;
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	TransObjLayerClass tlc = (TransObjLayerClass)lc;

	if(tlc->trobj_class.win_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallWinLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(WARNING,E_UNKNOWN,"_NhlWinLineTo: Transformation object of type (%s) does not have win_lineto function",tlc->base_class.class_name);
			return(WARNING);
		}
	} else {
		return((*tlc->trobj_class.win_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlWinLineTo 
#if __STDC__
(Layer instance, Layer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
Layer instance;
Layer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallWinLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

NhlErrorTypes CallCompcLineTo 
#if  __STDC__
(LayerClass lc, Layer instance, Layer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
LayerClass lc;
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	TransObjLayerClass tlc = (TransObjLayerClass)lc;

	if(tlc->trobj_class.compc_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallCompcLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(WARNING,E_UNKNOWN,"_NhlCompcLineTo: Transformation object of type (%s) does not have compc_lineto function",tlc->base_class.class_name);
			return(WARNING);
		}
	} else {
		return((*tlc->trobj_class.compc_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlCompcLineTo 
#if __STDC__
(Layer instance, Layer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
Layer instance;
Layer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallCompcLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

NhlErrorTypes CallNDCLineTo 
#if  __STDC__
(LayerClass lc, Layer instance, Layer parent, float x, float y, int upordown)
#else
(lc, instance, parent, x, y, upordown)
LayerClass lc;
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	TransObjLayerClass tlc = (TransObjLayerClass)lc;

	if(tlc->trobj_class.NDC_lineto == NULL){
		if(tlc->base_class.superclass != NULL) {
			return(CallNDCLineTo(lc->base_class.superclass,instance,parent,x,y,upordown));
		} else {
			NhlPError(WARNING,E_UNKNOWN,"_NhlNDCLineTo: Transformation object of type (%s) does not have NDC_lineto function",tlc->base_class.class_name);
			return(WARNING);
		}
	} else {
		return((*tlc->trobj_class.NDC_lineto)(instance,parent,x,y,upordown));
	}
}


NhlErrorTypes _NhlNDCLineTo 
#if __STDC__
(Layer instance, Layer parent, float x, float y, int upordown)
#else
(instance,parent,x,y,upordown)
Layer instance;
Layer parent;
float	x;
float y;
int upordown;
#endif
{
	return(CallNDCLineTo(instance->base.layer_class,instance,parent,x,y,upordown));
}

