/*
 *      $Id: SubStyles.c,v 1.7.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SubStyles.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	This file contains the implementation of each
 *                      of the individual subclasses of Style.
 */

#include <math.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/SubStylesP.h>

#define Oset(field)     NhlOffset(NhlLineStyleLayerRec,linestyle.ls.field)
static NhlResource lineresources[] = {

/* Begin-documented-resources */

	{NhlNgsLineDashPattern,NhlCLineDashPattern,NhlTDashIndex,
		 sizeof(NhlDashIndex),Oset(dash_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer)0),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineDashSegLenF,NhlCLineDashSegLenF,NhlTFloat,sizeof(float),
		Oset(line_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_DEFAULT,NULL},
	{NhlNgsLineColor,NhlCLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
		Oset(line_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineOpacityF,NhlCLineOpacityF,NhlTFloat,
		sizeof(float),Oset(line_opacity),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineThicknessF,NhlCLineThicknessF,NhlTFloat,sizeof(float),
		Oset(line_thickness),NhlTString,_NhlUSET("1.0"),
		_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelString,NhlCLineLabelString,NhlTString,
		sizeof(NhlString),Oset(line_label_string),NhlTImmediate,
		 _NhlUSET((NhlPointer)NULL),
		 _NhlRES_DEFAULT,(NhlFreeFunc)NhlFree},
	{NhlNgsLineLabelFont,NhlCFont,NhlTFont,sizeof(NhlFont),
		Oset(line_label_font),NhlTImmediate,
		 _NhlUSET((NhlPointer)21),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(line_label_font_color),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),
		_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelFontHeightF,NhlCFontHeightF,NhlTFloat,
		sizeof(float),Oset(line_label_font_height),NhlTString,
		_NhlUSET("0.0125"),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelFontAspectF,NhlCFontAspectF,NhlTFloat,
		sizeof(float),Oset(line_label_font_aspect),NhlTString,
		_NhlUSET("1.3125"),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		sizeof(float),Oset(line_label_font_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelFontQuality,NhlCFontQuality,NhlTFontQuality,
		sizeof(NhlFontQuality),Oset(line_label_font_quality),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlHIGH),_NhlRES_DEFAULT,
		NULL},
	{NhlNgsLineLabelConstantSpacingF,NhlCTextConstantSpacingF,
		NhlTFloat,sizeof(float),Oset(line_label_const_spacing),
		NhlTString,_NhlUSET("0.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsLineLabelFuncCode,NhlCTextFuncCode,NhlTCharacter,
		sizeof(char),Oset(line_label_func_code),NhlTString,
		_NhlUSET("~"),_NhlRES_DEFAULT,NULL},

/* End-documented-resources */
		
	{_NhlNgsLineStyleInfo,_NhlCgsLineStyleInfo,NhlTPointer,
		 sizeof(NhlPointer),
		 NhlOffset(NhlLineStyleLayerRec,linestyle.lsp),
		 NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		 _NhlRES_GONLY|_NhlRES_PRIVATE,NULL},
};
#undef Oset

/*
* Base Methods used
*/

static NhlErrorTypes    LineStyleInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes LineStyleSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);


static NhlErrorTypes    LineStyleGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes	LineStyleDestroy(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);


NhlLineStyleClassRec NhllineStyleClassRec = {
	{
/* class_name			*/	"lineStyleClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLineStyleLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlstyleClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	lineresources,
/* num_resources		*/	NhlNumber(lineresources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	LineStyleInitialize,
/* layer_set_values		*/	LineStyleSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	LineStyleGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LineStyleDestroy,

/* child_resources		*/	NULL,
/* layer_draw			*/      NULL,
/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL
	},
	{
					NULL
	},
	{
					NULL
	}
};

NhlClass NhllineStyleClass = 
			(NhlClass)&NhllineStyleClassRec;

static NrmQuark	Qline_label_string = NrmNULLQUARK; 

/*
 * Function:	nhlflineStyleclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlflineStyleclass,NHLFLINESTYLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhllineStyleClass;
}

/*
 * Function:	LineStyleInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes
LineStyleInitialize
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
	NhlClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlString		e_text, entry_name = "LineStyleInitialize";
	NhlLineStyleLayer	lsnew = (NhlLineStyleLayer) new;
	NhlLineStyleLayerPart	*lsp = &(lsnew->linestyle);
	NhlString		tstring;

	if (Qline_label_string == NrmNULLQUARK) {
		Qline_label_string = NrmStringToQuark(NhlNgsLineLabelString);
	}

	if (lsp->ls.line_label_string != NULL) {
		tstring = NhlMalloc(strlen(lsp->ls.line_label_string) + 1);
		if (tstring == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(tstring,lsp->ls.line_label_string);
		lsp->ls.line_label_string = tstring;
	}
	lsp->lsp = &lsp->ls;
	return ret;
}

/*
 * Function:	LineStyleSetValues
 *
 * Description: 
 *
 * In Args:	Standard SetValues args
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: GKS and plotchar state changes.
 */
/*ARGSUSED*/
static NhlErrorTypes 
LineStyleSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{

	NhlErrorTypes 		ret = NhlNOERROR;
	NhlString		e_text, entry_name = "LineStyleSetValues";
	NhlLineStyleLayer	lsnew = (NhlLineStyleLayer) new;
	NhlLineStyleLayer	lsold = (NhlLineStyleLayer) old;
	NhlLineStyleLayerPart	*lsp = &(lsnew->linestyle);
	NhlString		tstring;

	if (_NhlArgIsSet(args,num_args,NhlNgsLineLabelString)) {
		NhlFree(lsold->linestyle.ls.line_label_string);
		if (lsp->ls.line_label_string != NULL) {
			tstring = NhlMalloc(
				     strlen(lsp->ls.line_label_string) + 1);
			if (tstring == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(tstring,lsp->ls.line_label_string);
			lsp->ls.line_label_string = tstring;
		}
	}
	return ret;

}

/*
 * Function:    LineStyleGetValues
 *
 * Description: Retrieves the current setting of LineStyle resources.
 *      This routine only retrieves resources that require special methods
 *      that the generic GetValues method cannot handle. For now this means
 *      all the GenArray resources. Note that space is allocated; the user
 *      is responsible for freeing this space.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when the following resource is retrieved:
 *		NhlNgsLineLabelString
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    LineStyleGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
	NhlLineStyleLayer ls = (NhlLineStyleLayer)l;
        NhlLineStyleLayerPart *lsp = &(ls->linestyle);
	NhlString	ts;
        char		*e_text;
	int		i;

        for( i = 0; i< num_args; i++ ) {

		ts = NULL;
		if (args[i].quark == Qline_label_string){
			ts = lsp->ls.line_label_string;
		}
                if (ts != NULL) {
			*((NhlString*)(args[i].value.ptrval)) =
				NhlMalloc(strlen(ts)+1);
			if(!*((NhlString*)(args[i].value.ptrval))){
                                e_text = "%s: error copying String";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  "LineStyleGetValues");
                                return NhlFATAL;
                        }
			strcpy(*((NhlString*)(args[i].value.ptrval)),ts);
			continue;
                }
        }

        return(NhlNOERROR);
}


/*
 * Function:	LineStyleDestroy
 *
 * Description: 
 *
 * In Args:	NhlLayer inst	instance of LineStyle
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes
LineStyleDestroy
#if	NhlNeedProto
(
	NhlLayer	layer)
#else
(layer)
	NhlLayer	layer;
#endif
{
	NhlLineStyleLayer 	ls = (NhlLineStyleLayer) layer;
	NhlLineStyleLayerPart	*lsp = &(ls->linestyle);

	if (lsp->ls.line_label_string != NULL)
		NhlFree(lsp->ls.line_label_string);

	return NhlNOERROR;
}


#define Oset(field)     NhlOffset(NhlFillStyleLayerRec,fillstyle.fs.field)
static NhlResource fillresources[] = {

/* Begin-documented-resources */

	{NhlNgsFillIndex,NhlCFillPattern,NhlTFillIndex,sizeof(NhlFillIndex),
		Oset(fill_index),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_DEFAULT,NULL},
	{NhlNgsFillColor,NhlCFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(fill_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_DEFAULT,NULL},
	{NhlNgsFillOpacityF,NhlCFillOpacityF,NhlTFloat,
		 sizeof(float),Oset(fill_opacity),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsFillBackgroundColor,NhlCFillBackgroundColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(fill_background),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlTRANSPARENT),_NhlRES_DEFAULT,NULL},
	{NhlNgsFillScaleF,NhlCFillScaleF,NhlTFloat,
		sizeof(float),Oset(fill_scale_factor),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsFillLineThicknessF,NhlCFillLineThicknessF,NhlTFloat,
		sizeof(float),Oset(fill_line_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsFillDotSizeF,NhlCFillDotSizeF,NhlTFloat,
		sizeof(float),Oset(fill_dot_size),NhlTString,
		_NhlUSET("0.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsEdgesOn,NhlCEdgesOn,NhlTBoolean,sizeof(NhlBoolean),
		Oset(edges_on),NhlTImmediate,_NhlUSET(False),_NhlRES_DEFAULT,
		NULL},
	{NhlNgsEdgeDashPattern,NhlCEdgeDashPattern,NhlTDashIndex,
		sizeof(NhlDashIndex),Oset(edge_dash_pattern),NhlTImmediate,
		_NhlUSET(0),_NhlRES_DEFAULT,NULL},
	{NhlNgsEdgeThicknessF,NhlCEdgeThicknessF,NhlTFloat,sizeof(float),
		Oset(edge_thickness),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsEdgeDashSegLenF,NhlCEdgeDashSegLenF,NhlTFloat,sizeof(float),
		Oset(edge_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_DEFAULT,NULL},
	{NhlNgsEdgeColor,NhlCEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(edge_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_DEFAULT,NULL},

/* End-documented-resources */

	{_NhlNgsFillStyleInfo,_NhlCgsFillStyleInfo,NhlTPointer,
		 sizeof(NhlPointer),
		 NhlOffset(NhlFillStyleLayerRec,fillstyle.fsp),
		 NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		 _NhlRES_GONLY|_NhlRES_PRIVATE,NULL},
};
#undef Oset

static NhlErrorTypes    FillStyleInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

NhlFillStyleClassRec NhlfillStyleClassRec = {
	{
/* class_name			*/	"fillStyleClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlFillStyleLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlstyleClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	fillresources,
/* num_resources		*/	NhlNumber(fillresources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	FillStyleInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,
/* layer_draw			*/      NULL,
/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

	},
	{
					NULL
	},
	{
					NULL
	}
};

NhlClass NhlfillStyleClass = 
			(NhlClass)&NhlfillStyleClassRec;

/*
 * Function:	nhlffillStyleclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlffillStyleclass,NHLFFILLSTYLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlfillStyleClass;
}



/*
 * Function:	FillStyleInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes
FillStyleInitialize
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
	NhlClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{

	NhlFillStyleLayer	fsnew = (NhlFillStyleLayer) new;
	NhlFillStyleLayerPart	*fsp = &(fsnew->fillstyle);

	fsp->fsp = &fsp->fs;

	return NhlNOERROR;
}

#define Oset(field)     NhlOffset(NhlMarkerStyleLayerRec,markerstyle.ms.field)
static NhlResource markerresources[] = {

/* Begin-documented-resources */

	{NhlNgsMarkerIndex,NhlCMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),Oset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),_NhlRES_DEFAULT,NULL},
	{NhlNgsMarkerColor,NhlCMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_DEFAULT,NULL},
	{NhlNgsMarkerOpacityF,NhlCMarkerOpacityF,NhlTFloat,
		sizeof(float),Oset(marker_opacity),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsMarkerSizeF,NhlCMarkerSizeF,NhlTFloat,sizeof(float),
		Oset(marker_size),NhlTString,
		 _NhlUSET("0.007"),_NhlRES_DEFAULT,NULL},
	{NhlNgsMarkerThicknessF,NhlCMarkerThicknessF,NhlTFloat,sizeof(float),
		Oset(marker_thickness),NhlTString,_NhlUSET("1.0"),
		_NhlRES_DEFAULT,NULL},

/* End-documented-resources */

	{_NhlNgsMarkerStyleInfo,_NhlCgsMarkerStyleInfo,NhlTPointer,
		 sizeof(NhlPointer),
		 NhlOffset(NhlMarkerStyleLayerRec,markerstyle.msp),
		 NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		 _NhlRES_GONLY|_NhlRES_PRIVATE,NULL},
};
#undef Oset

static NhlErrorTypes    MarkerStyleInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

NhlMarkerStyleClassRec NhlmarkerStyleClassRec = {
	{
/* class_name			*/	"markerStyleClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlMarkerStyleLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlstyleClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	markerresources,
/* num_resources		*/	NhlNumber(markerresources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	MarkerStyleInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,
/* layer_draw			*/      NULL,
/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

	},
	{
					NULL
	},
	{
					NULL
	}
};

NhlClass NhlmarkerStyleClass = 
			(NhlClass)&NhlmarkerStyleClassRec;

/*
 * Function:	nhlfmarkerStyleclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlfmarkerStyleclass,NHLFMARKERSTYLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlmarkerStyleClass;
}


/*
 * Function:	MarkerStyleInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes
MarkerStyleInitialize
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
	NhlClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlMarkerStyleLayer	msnew = (NhlMarkerStyleLayer) new;
	NhlMarkerStyleLayerPart	*msp = &(msnew->markerstyle);

	msp->msp = &msp->ms;

	return NhlNOERROR;
}


#define Oset(field)     NhlOffset(NhlTextStyleLayerRec,textstyle.ts.field)
static NhlResource textresources[] = {

/* Begin-documented-resources */

	{NhlNgsTextAngleF, NhlCTextAngleF, NhlTFloat, sizeof(float),
		Oset(angle),
		NhlTString,_NhlUSET("0.0"),0,NULL },
	{NhlNgsFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		Oset(font),
		NhlTImmediate,_NhlUSET(21),0,NULL },
	{NhlNgsTextJustification, NhlCTextJustification,
		 NhlTJustification, sizeof(int),Oset(just),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNgsFontQuality, NhlCFontQuality, NhlTFontQuality, 
		sizeof(NhlFontQuality),
		Oset(font_quality),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlHIGH),0,NULL},
	{NhlNgsFontColor,NhlCFontColor,NhlTColorIndex,sizeof(NhlColorIndex),
		Oset(font_color),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNgsFontOpacityF,NhlCFontOpacityF,NhlTFloat,
		sizeof(float),Oset(font_opacity),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNgsFontHeightF, NhlCFontHeightF, NhlTFloat, sizeof(float),
		Oset(font_height),
		NhlTString,_NhlUSET("0.015") ,0,NULL},
	{NhlNgsFontAspectF, NhlCFontAspectF, NhlTFloat, sizeof(float),
		Oset(font_aspect),
		NhlTString, _NhlUSET("1.3125"),0,NULL },
	{NhlNgsFontThicknessF, NhlCFontThicknessF, NhlTFloat, sizeof(float),
		Oset(font_thickness),
		NhlTString,_NhlUSET("1.0") ,0,NULL},
	{NhlNgsTextConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(constant_spacing),
		NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNgsTextDirection,NhlCTextDirection,NhlTTextDirection,
		sizeof(NhlTextDirection),
		Oset(direction),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNgsTextFuncCode, NhlCTextFuncCode, NhlTCharacter, 
		sizeof(char),
		Oset(func_code),
		NhlTString,_NhlUSET("~"),0,NULL},

/* End-documented-resources */

	{_NhlNgsTextStyleInfo,_NhlCgsTextStyleInfo,NhlTPointer,
		 sizeof(NhlPointer),
		 NhlOffset(NhlTextStyleLayerRec,textstyle.tsp),
		 NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		 _NhlRES_GONLY|_NhlRES_PRIVATE,NULL},

};
#undef Oset

static NhlErrorTypes    TextStyleInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

NhlTextStyleClassRec NhltextStyleClassRec = {
	{
/* class_name			*/	"textStyleClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlTextStyleLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlstyleClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	textresources,
/* num_resources		*/	NhlNumber(textresources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	TextStyleInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,
/* layer_draw			*/      NULL,
/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

	},
	{
					NULL
	},
	{
					NULL
	}
};

NhlClass NhltextStyleClass = 
			(NhlClass)&NhltextStyleClassRec;

/*
 * Function:	nhlftextStyleclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlftextStyleclass,NHLFTEXTSTYLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhltextStyleClass;
}


/*
 * Function:	TextStyleInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes
TextStyleInitialize
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
	NhlClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlTextStyleLayer	tsnew = (NhlTextStyleLayer) new;
	NhlTextStyleLayerPart	*tsp = &(tsnew->textstyle);

	tsp->tsp = &tsp->ts;

	return ret;
}


