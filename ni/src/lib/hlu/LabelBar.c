
/*
 *      $Id: LabelBar.c,v 1.1 1993-07-27 18:02:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LabelBar.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	LabelBar source for drawing text. 
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LabelBarP.h>
#include <ncarg/hlu/Workstation.h>

/* default pattern list */

static int def_patterns[] = { 
	1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
static int def_colors[] = { 
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
static float def_values[] = { 
	1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

/* SUPPRESS 112 */

#define DEFSTRING "NOTHING"
#define DEGTORAD 0.017453293
static NhlResource resources[] = { 

{NhlNlbLabelBar, NhlClbLabelBar, NhlTInteger,
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.labelbar_on),
	 NhlTImmediate,(NhlPointer) 1}, 
{NhlNlbOrientation, NhlClbOrientation, NhlTOrientation,
	 sizeof(NhlOrientation), NhlOffset(LabelBarLayerRec,labelbar.orient),
	 NhlTImmediate,(NhlPointer) NhlVERTICAL},
{NhlNlbJustification, NhlClbJustification, NhlTJustification,
	 sizeof(NhlJustification), NhlOffset(LabelBarLayerRec,labelbar.just),
	 NhlTImmediate,(NhlPointer) NhlCENTERCENTER},
{NhlNlbXF, NhlClbXF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.lb_x),
	 NhlTString,"0.2"},
{NhlNlbYF, NhlClbYF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.lb_y),
	 NhlTString,"0.8"},
{NhlNlbWidthF, NhlClbWidthF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.lb_width),
	 NhlTString,"0.3"},
{NhlNlbHeightF, NhlClbHeightF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.lb_height),
	 NhlTString,"0.6"},
{NhlNlbBoxMajorExtentF, NhlClbBoxMajorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.box_major_ext),
	 NhlTString,"1.0"},
{NhlNlbBoxMinorExtentF, NhlClbBoxMinorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.box_minor_ext),
	 NhlTString,"0.33"},
{NhlNlbAlignment, NhlClbAlignment, NhlTInteger,
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.alignment),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlbBoxCount, NhlClbBoxCount, NhlTInteger,
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.box_count),
	 NhlTImmediate,(NhlPointer) 16},
{NhlNlbBoxMode, NhlClbBoxMode, NhlTInteger,
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.box_mode),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlbBoxSizing, NhlClbBoxSizing, NhlTInteger,
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.box_sizing),
	 NhlTImmediate,(NhlPointer) NhlLB_UNIFORMSIZING},
	
{NhlNlbFillPatterns, NhlClbFillPatterns, NhlTIntegerPtr,
	 sizeof(int *), NhlOffset(LabelBarLayerRec,labelbar.fill_patterns),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlbColors, NhlClbColors, NhlTIntegerPtr,
	 sizeof(int *), NhlOffset(LabelBarLayerRec,labelbar.colors),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlbValues, NhlClbValues, NhlTFloatPtr,
	 sizeof(float *), NhlOffset(LabelBarLayerRec,labelbar.values),
	 NhlTImmediate,(NhlPointer) NULL },
	
{NhlNlbLabelStrings, NhlClbLabelStrings, NhlTStringPtr,
	 sizeof(NhlString *), 
	 NhlOffset(LabelBarLayerRec,labelbar.label_strings),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlbBoxFractions, NhlClbBoxFractions, NhlTFloatPtr,
	 sizeof(float *), NhlOffset(LabelBarLayerRec,labelbar.box_fractions),
	 NhlTImmediate,(NhlPointer) NULL },
	
{NhlNlbDrawLabels, NhlClbDrawLabels, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.labels_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbLabelPosition, NhlClbLabelPosition, NhlTPosition, 
	 sizeof(NhlPosition), NhlOffset(LabelBarLayerRec,labelbar.label_pos),
	 NhlTImmediate,(NhlPointer) NhlRIGHT},
{NhlNlbLabelAngleF, NhlClbLabelAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.label_angle),
	 NhlTString,"0.0"},
{NhlNlbLabelAlignment, NhlClbLabelAlignment, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.label_alignment),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlbLabelDirection,NhlClbLabelDirection,NhlTTextDirection,
	 sizeof(TextDirection),
	 NhlOffset(LabelBarLayerRec,labelbar.label_direction),
	 NhlTImmediate,(NhlPointer)ACROSS},
{NhlNlbLabelJust, NhlClbLabelJust, NhlTJustification, sizeof(NhlJustification),
	 NhlOffset(LabelBarLayerRec,labelbar.label_just),
	 NhlTImmediate,(NhlPointer)NhlCENTERCENTER},
{NhlNlbLabelFont, NhlClbLabelFont, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.label_font),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbLabelFontColor, NhlClbLabelFontColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.label_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbLabelFontHeightF, NhlClbLabelFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.label_height),
	 NhlTString,"0.07"},
{NhlNlbLabelFontAspectF, NhlClbLabelFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.label_aspect),
	 NhlTString,"1.0"},
{NhlNlbLabelFontThicknessF, NhlClbLabelFontThicknessF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.label_thickness),
	 NhlTString,"1.0"},
{NhlNlbLabelFontQuality, NhlClbLabelFontQuality, NhlTFQuality, 
	 sizeof(FontQuality), 
	 NhlOffset(LabelBarLayerRec,labelbar.label_quality),
	 NhlTImmediate,(NhlPointer) HIGH},
{NhlNlbLabelConstantSpacingF, NhlClbLabelConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.label_const_spacing),
	 NhlTString,"0.0"},
{NhlNlbLabelFuncCode, NhlClbLabelFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(LabelBarLayerRec,labelbar.label_func_code),
	 NhlTString,":"},
{NhlNlbLabelStride, NhlClbLabelStride, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.label_stride),
	 NhlTImmediate,(NhlPointer) 1},

{NhlNlbTitleString, NhlClbTitleString, NhlTString, 
	 sizeof(char *), NhlOffset(LabelBarLayerRec,labelbar.title_string),
	 NhlTImmediate,DEFSTRING},
{NhlNlbDrawTitle, NhlClbDrawTitle, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.title_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbTitlePosition, NhlClbTitlePosition, NhlTInteger, 
	 sizeof(NhlPosition), NhlOffset(LabelBarLayerRec,labelbar.title_pos),
	 NhlTImmediate,(NhlPointer) NhlTOP},
{NhlNlbTitleExtentF, NhlClbTitleExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.title_ext),
	 NhlTString,"0.15"},
{NhlNlbTitleAngleF, NhlClbTitleAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.title_angle),
	 NhlTString,"0.0"},
{NhlNlbTitleDirection,NhlClbTitleDirection,NhlTTextDirection,
	 sizeof(TextDirection),
	 NhlOffset(LabelBarLayerRec,labelbar.title_direction),
	 NhlTImmediate,(NhlPointer)ACROSS},
{NhlNlbTitleFont, NhlClbTitleFont, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.title_font),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbTitleFontColor, NhlClbTitleFontColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.title_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbTitleJust, NhlClbTitleJust, NhlTJustification, sizeof(NhlJustification),
	 NhlOffset(LabelBarLayerRec,labelbar.title_just),
	 NhlTImmediate,(NhlPointer)NhlCENTERCENTER},
{NhlNlbTitleFontHeightF, NhlClbTitleFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.title_height),
	 NhlTString,"0.025"},
{NhlNlbTitleFontAspectF, NhlClbTitleFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.title_aspect),
	 NhlTString,"1.0"},
{NhlNlbTitleFontThicknessF, NhlClbTitleFontThicknessF, NhlTFloat, 
	 sizeof(float), NhlOffset(LabelBarLayerRec,labelbar.title_thickness),
	 NhlTString,"1.0"},
{NhlNlbTitleFontQuality, NhlClbTitleFontQuality, NhlTFQuality, 
	 sizeof(FontQuality), 
	 NhlOffset(LabelBarLayerRec,labelbar.title_quality),
	 NhlTImmediate,(NhlPointer) HIGH},
{NhlNlbTitleConstantSpacingF, NhlClbTitleConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.title_const_spacing),
	 NhlTString,"0.0"},
{NhlNlbTitleFuncCode, NhlClbTitleFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(LabelBarLayerRec,labelbar.title_func_code),
	 NhlTString,":"},
	
{NhlNlbDrawBoxLines, NhlClbDrawBoxLines, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.box_line_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbBoxLineColor, NhlClbBoxLineColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.box_line_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbBoxLineThicknessF, NhlClbBoxLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.box_line_thickness),
	 NhlTString,"1.0"},
{NhlNlbBoxLineDashPattern, NhlClbBoxLineDashPattern, NhlTInteger, 
	 sizeof(int), 
	 NhlOffset(LabelBarLayerRec,labelbar.box_line_dash_pattern),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlbBoxLineDashLengthF, NhlClbBoxLineDashLengthF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.box_line_dash_length),
	 NhlTString,"0.15"},

{NhlNlbDrawPerim, NhlClbDrawPerim, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.perim_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbPerimColor, NhlClbPerimColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.perim_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbPerimThicknessF, NhlClbPerimThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.perim_thickness),
	 NhlTString,"1.0"},
{NhlNlbPerimDashPattern, NhlClbPerimDashPattern, NhlTInteger, 
	 sizeof(int), 
	 NhlOffset(LabelBarLayerRec,labelbar.perim_dash_pattern),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlbPerimDashLengthF, NhlClbPerimDashLengthF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.perim_dash_length),
	 NhlTString,"0.15"},

{NhlNlbFillLineColor, NhlClbFillLineColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LabelBarLayerRec,labelbar.fill_line_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlbFillLineThicknessF, NhlClbFillLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LabelBarLayerRec,labelbar.fill_line_thickness),
	 NhlTString,"1.0"},
};

/*
* Base Methods used
*/

static NhlErrorTypes LabelBarSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes    LabelBarInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	LabelBarDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes	LabelBarDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes 	LabelBarClassInitialize();

/*
* Private functions
*/

static NhlErrorTypes    SetLabelBarGeometry(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetTitle(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetBoxLocations(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    ValidateBoxFractions(
#ifdef NhlNeedProto
	LabelBarLayerPart, /* *lb_p */
#endif
);

static void CreateIntermediates(
#ifdef NhlNeedProto
	float,		/* *flist */
	int,		/* start  */
	int		/* end    */
#endif
);

static NhlErrorTypes    SetLabels(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    AdjustGeometry(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlBoolean    	InCriticalZone(
#ifdef NhlNeedProto
	LabelBarLayerPart,	/* *lb_p */
	float,			/* height */
	float *,		/* critical_angle */
#endif
);

static NhlBoolean    	LabelBarChanged(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
#endif
);

LabelBarLayerClassRec labelBarLayerClassRec = {
	{
/* superclass			*/	(LayerClass)&viewLayerClassRec,
/* class_name			*/	"LabelBar",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(LabelBarLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	LabelBarClassInitialize,
/* layer_initialize		*/	LabelBarInitialize,
/* layer_set_values		*/	LabelBarSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	LabelBarDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	LabelBarDestroy
	},
	{
/* segment_workstation */ -1,
/* get_bb */		NULL, /* ---------> Do I need one?<---------*/
	},
	{
			NULL
	}
};

LayerClass labelBarLayerClass = (LayerClass)&labelBarLayerClassRec;


/*
 * Function:	LabelBarSetValues
 *
 * Description: Performs same operations as LabelBarInitialize except if 
 *		a move or resize has ocurred font_height is automatically 
 *		scaled.
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
static NhlErrorTypes LabelBarSetValues
#if __STDC__
	(Layer old,
	Layer reference,
	Layer new,
	_NhlArgList args,
	int num_args)
#else
(old,reference,new,args,num_args)
	Layer	old;
	Layer	reference;
	Layer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	LabelBarLayer told = (LabelBarLayer) old;
	LabelBarLayer tnew = (LabelBarLayer) new;
	LabelBarLayerPart *olb_p = &(told->labelbar);
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NOERROR,ret1 = NOERROR;
	float fl,fr,ft,fb,ul,ur,ut,ub;
	int i, ll;
	char number[10];
	char **cold_pp, **cnew_pp;
	float ftmp;

/*
 * Since the color array is allocated only once copy the new information 
 * into the old array then change the new pointer to the old one. Otherwise
 * if the number of label boxes has increased, find the GKS color index
 * for the workstation indices (which if not otherwise defined are set to
 * the default value 0).
 */

	if (lb_p->colors != olb_p->colors) {

		memcpy((void *) olb_p->colors,(Const void *) lb_p->colors,
		       lb_p->box_count * sizeof(int));
		lb_p->colors = olb_p->colors;

		for (i=0; i<lb_p->box_count; i++) {
			lb_p->gks_colors[i] = _NhlGetGksCi(tnew->base.wkptr,
							    lb_p->colors[i]);
		}
	}
	else if (lb_p->box_count > olb_p->box_count) {

		for (i=olb_p->box_count;i<lb_p->box_count;i++) {
			lb_p->gks_colors[i] =_NhlGetGksCi(tnew->base.wkptr,
							   lb_p->colors[i]);
		}
	}

/*
 * Since the pattern array is allocated only once copy the new information 
 * into the old array then change the new pointer to the old one. If the
 * number of label boxes has increased, no action is necessary, since all
 * members of the array contain the default pattern index, if they have not
 * otherwise been set.
 */

	if (lb_p->fill_patterns != olb_p->fill_patterns) {

		memcpy((void *) olb_p->fill_patterns,
		       (Const void *) lb_p->fill_patterns,
		       lb_p->box_count * sizeof(int));
		lb_p->fill_patterns = olb_p->fill_patterns;
	}

/* 
 * The pointer array is only allocated once, but each string is allocated
 * as needed. Stuff from the new string array is copied into the old
 * array. If the new string array contains a NULL pointer a default string
 * is created. If there are no explicit changes to the label array, but the
 * number of boxes has increased, default strings are created for all
 * currently NULL pointers.
 */

	cold_pp = olb_p->label_strings;
	cnew_pp = lb_p->label_strings;
	if (cnew_pp != cold_pp) {
		
		for (i=0;i<lb_p->box_count+1;i++) {

			if (cold_pp[i] != NULL)
				NhlFree(cold_pp[i]);
			if (cnew_pp[i] != NULL) {
				cold_pp[i] = (char *)
					NhlMalloc((strlen(cnew_pp[i])+1));
				strcpy(cold_pp[i],(Const char *)cnew_pp[i]);
			} 
			else {
				sprintf(number,"%d",i);
				cold_pp[i] = (char *)
					NhlMalloc((strlen(NhlLB_DEF_STRING) +
						   strlen(number) + 1));
				strcpy(cold_pp[i],
				       (Const char *)NhlLB_DEF_STRING);
				strcat(cold_pp[i],number);
				strcat(cold_pp[i],"M");
			}
		}
		cnew_pp = cold_pp;
	}
	else if (lb_p->box_count > olb_p->box_count) {

		for (i=olb_p->box_count+1;i<lb_p->box_count+1;i++) {

			if (cnew_pp[i] == NULL) { 
				sprintf(number,"%d",i);
				cnew_pp[i] = (char *)
					NhlMalloc((strlen(NhlLB_DEF_STRING) +
						   strlen(number) + 2));
				strcpy(cnew_pp[i],
				       (Const char *)NhlLB_DEF_STRING);
				strcat(cnew_pp[i],number);
				strcat(cnew_pp[i],"M");
			}

		}
	}

	if ((tnew->view.x != told->view.x)
	   ||(tnew->view.width != told->view.width)
	   ||(tnew->view.y != told->view.y)
	   ||(tnew->view.height != told->view.height)){
#if 0		

		if (LabelBarChanged(tnew, told, 0, args, num_args)) {
			NhlPError(WARNING,E_UNKNOWN,"LabelBarSetValues: Can not change x,y,width,and height when other labelbar attribute changes have been requested also, proceding with other text attribute requests");
                  ret1 = WARNING;
			
		}
	}
	else {

		ret1 = TransformPoints(tnew);
#endif
		_NhlEvalTrans(tnew->view.trans_children,
			      lb_p->perim.l,lb_p->perim.b,
			      &lb_p->perim.l,&lb_p->perim.b);

		_NhlEvalTrans(tnew->view.trans_children,
			      lb_p->perim.r,lb_p->perim.t,
			      &lb_p->perim.r,&lb_p->perim.t);
	}

#if 0
/*
 * Adjust the size
 */

	ftmp = lb_p->lb_x;
	lb_p->lb_x -= lb_p->adjust_frac.l * lb_p->lb_width;
	lb_p->lb_width -= lb_p->adjust_frac.r * lb_p->lb_width +
		ftmp - lb_p->lb_x;
	ftmp = lb_p->lb_y;
	lb_p->lb_y -= lb_p->adjust_frac.t * lb_p->lb_height;
	lb_p->lb_height += lb_p->adjust_frac.b * lb_p->lb_height +
		lb_p->lb_y - ftmp;
#endif

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlbTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlbTitleDirection)) {
		switch (lb_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lb_p->title_direction = ACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lb_p->title_direction = DOWN;
			break;
		}
	}

/*
 * Calculate labelbar geometry
 */

	ret1 = SetLabelBarGeometry(new,args,num_args);
	ret = MIN(ret1,ret);
	ret1 = SetTitle(new,old,0,args,num_args);
	ret = MIN(ret1,ret);
/*
 * Set the box locations
 */
	ret1 = SetBoxLocations(new,old,0,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = SetLabels(new,old,0,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = AdjustGeometry(new,old,0,args,num_args);
	ret = MIN(ret1,ret);
		     
	lb_p->last_box_count = lb_p->box_count;
#if 0
        c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
        ret1 = FigureAndSetTextBBInfo(tnew);
        c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
#endif
        return(MIN(ret,ret1));
}

/*
 * Function:	LabelBarInitialize
 *
 * Description:	Performs initilization of LabelBar. This involves copying
 *		user data into internal space and then makes calls to 
 *		plotchar to set plotchars space so text extent computations 
 *		can be done.
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: Plotchar state affected
 */
/*ARGSUSED*/
static NhlErrorTypes    LabelBarInitialize
#if __STDC__
	(LayerClass class, 
	 Layer req, 
	 Layer new, 
	 _NhlArgList args,
	 int num_args)
#else
(class,req,new,args,num_args)
	LayerClass	class;
	Layer		req;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	char *c_p;
	int  *ifrom_p;
	float *ffrom_p;
	char **cfrom_pp;
	NhlErrorTypes	ret=NOERROR,ret1 = NOERROR;
	int i,count;
	float fr,fl,ft,fb,ur,ul,ut,ub;
	char number[10];
	float ftmp;
	int fill_len;

	lb_p->last_box_count = 0;
	lb_p->labels_id = -1;
	lb_p->title_id = -1;
	lb_p->title_string = NULL;
	lb_p->stride_labels = NULL;
	lb_p->box_locs = NULL;
	lb_p->label_locs = NULL;

/* temporary */

	lb_p->margin.l = 0.05;
	lb_p->margin.r = 0.05;
	lb_p->margin.b = 0.05;
	lb_p->margin.t = 0.05;

/* 
 * There seems to be no way to ensure that a SetValues array contains
 * enough elements, so there is always a core dump possibility here.
 */

/* The box colors array */


	if ( _NhlArgIsSet(args,num_args,NhlNlbColors)) {
		ifrom_p = lb_p->colors;
		count = lb_p->box_count;
	}
	else {
		ifrom_p = def_colors;
		count = NhlLB_DEF_BOX_COUNT;
	}
	lb_p->colors = (int *) NhlMalloc(NhlLB_MAX_BOXES * sizeof(int));
	memset((void *) lb_p->colors,NhlLB_DEF_COLOR,
	       NhlLB_MAX_BOXES * sizeof(int));

	memcpy((void *)lb_p->colors,(Const void *) ifrom_p, 
	       count * sizeof(int));
	lb_p->gks_colors = (int *) NhlMalloc(NhlLB_MAX_BOXES * sizeof(int));
	for (i=0; i<lb_p->box_count; i++) {
		lb_p->gks_colors[i] =
			_NhlGetGksCi(tnew->base.wkptr,lb_p->colors[i]);
	}

/* The fill patterns array, first retrieve the number of defined patterns */

	NhlGetValues(tnew->base.wkptr->base.id,
		     NhlNwkFillTableLength, &fill_len, NULL);
	
	if (_NhlArgIsSet(args,num_args,NhlNlbFillPatterns)) {
		ifrom_p = lb_p->fill_patterns;
		count = lb_p->box_count;
	}
	else {
		ifrom_p = def_patterns;
		count = NhlLB_DEF_BOX_COUNT;
	}
	lb_p->fill_patterns = (int *) NhlMalloc(NhlLB_MAX_BOXES * sizeof(int));
	memset((void *) lb_p->fill_patterns,
	       NhlLB_DEF_PATTERN,NhlLB_MAX_BOXES * sizeof(int));
	for (i=0; i<count; i++) {
		if (ifrom_p[i] > fill_len)
			lb_p->fill_patterns[i] = NhlLB_DEF_PATTERN;
		else
			lb_p->fill_patterns[i] = ifrom_p[i];
	}

/* The values array */

	if (_NhlArgIsSet(args,num_args,NhlNlbValues)) {
		ffrom_p = lb_p->values;
		count = lb_p->box_count;
	}
	else {
		ffrom_p = def_values;
		count = NhlLB_DEF_BOX_COUNT;
	}
	lb_p->values = (float *) NhlMalloc(NhlLB_MAX_LBL_STRINGS * 
					   sizeof(float));
	memset((void *) lb_p->values,NhlLB_DEF_VALUE,
	       NhlLB_MAX_LBL_STRINGS * sizeof(float));
	memcpy((void *)lb_p->values,(Const void *) ifrom_p, 
	       count * sizeof(float));

/* 
 * The label strings array: note that if the label position mode is
 * external edges, then there is one more label than the number of boxes
 * For this reason, for simplicity always prepare one more label string
 * than the box count
 */

	if (_NhlArgIsSet(args,num_args,NhlNlbLabelStrings))
		cfrom_pp = lb_p->label_strings;
	else
		cfrom_pp = NULL;
	lb_p->label_strings =
		(char **) NhlMalloc(NhlLB_MAX_LBL_STRINGS * sizeof(char *));
	memset((void *)lb_p->label_strings,0,
	       NhlLB_MAX_LBL_STRINGS * sizeof(char *));
	for (i=0;i<lb_p->box_count+1;i++) {
		if (cfrom_pp != NULL && cfrom_pp[i] != NULL) {
			c_p = (char *)NhlMalloc(strlen(cfrom_pp[i])+1); 
			strcpy(c_p, cfrom_pp[i]);
		} else {
			sprintf(number,"%d",i);
			c_p = (char *)NhlMalloc(strlen(NhlLB_DEF_STRING) + 
						strlen(number) + 2); 
			strcpy(c_p, NhlLB_DEF_STRING);
			strcat(c_p, number);
			strcat(c_p, "M");
		}
		lb_p->label_strings[i] = c_p;
	}

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlbTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlbTitleDirection)) {
		switch (lb_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lb_p->title_direction = ACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lb_p->title_direction = DOWN;
			break;
		}
	}

/* planning to eliminate */

	lb_p->lb_x = tnew->view.x;
	lb_p->lb_y = tnew->view.y;
	lb_p->lb_width = tnew->view.width;
	lb_p->lb_height = tnew->view.height;
/* */
	lb_p->perim.l = tnew->view.x;
	lb_p->perim.r = tnew->view.x + tnew->view.width;
	lb_p->perim.b = tnew->view.y - tnew->view.height;
	lb_p->perim.t = tnew->view.y;

/*
 * Calculate labelbar geometry
 */

	ret1 = SetLabelBarGeometry(new,args,num_args);
	ret = MIN(ret1,ret);
/*
 * Set up the title using a text object
 */

	ret1 = SetTitle(new,req,1,args,num_args);
	ret = MIN(ret1,ret);

/*
 * Set the box locations
 */
	ret1 = SetBoxLocations(new,req,1,args,num_args);
	ret = MIN(ret1,ret);

/*
 * Set up the labels using a multitext object
 */

	ret1 = SetLabels(new,req,1,args,num_args);
	ret = MIN(ret1,ret);

/*
  Adjust the geometry
  */

	ret1 = AdjustGeometry(new,req,1,args,num_args);
	ret = MIN(ret1,ret);
	
	lb_p->last_box_count = lb_p->box_count;
#if 0
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	ret1 = FigureAndSetTextBBInfo(tnew);
	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
#endif
	return(MIN(ret,ret1));
}


/*ARGSUSED*/
static NhlBoolean    	LabelBarChanged
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayer	told = (LabelBarLayer) old;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	LabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;

	if (lb_p->orient != olb_p->orient    ||
	    lb_p->just != olb_p->just    ||
	    lb_p->box_major_ext != olb_p->box_major_ext    ||
	    lb_p->box_minor_ext != olb_p->box_minor_ext    ||
	    lb_p->alignment != olb_p->alignment    ||
	    lb_p->box_count != olb_p->box_count    ||
	    lb_p->box_mode != olb_p->box_mode    ||
	    lb_p->box_sizing != olb_p->box_sizing    ||
	    lb_p->label_strings != olb_p->label_strings    ||
	    lb_p->box_fractions != olb_p->box_fractions    ||
	    lb_p->labels_on != olb_p->labels_on    ||
	    lb_p->label_angle != olb_p->label_angle    ||
	    lb_p->label_font != olb_p->label_font    ||
	    lb_p->label_color != olb_p->label_color    ||
	    lb_p->label_height != olb_p->label_height    ||
	    lb_p->label_aspect != olb_p->label_aspect    ||
	    lb_p->label_thickness != olb_p->label_thickness    ||
	    lb_p->label_quality != olb_p->label_quality    ||
	    lb_p->label_const_spacing != olb_p->label_const_spacing    ||
	    lb_p->label_func_code != olb_p->label_func_code    ||
	    lb_p->label_direction != olb_p->label_direction    ||
	    lb_p->title_ext != olb_p->title_ext    ||
	    lb_p->title_string != olb_p->title_string    ||
	    lb_p->title_on != olb_p->title_on    ||
	    lb_p->title_pos != olb_p->title_pos    ||
	    lb_p->title_just != olb_p->title_just    ||
	    lb_p->title_direction != olb_p->title_direction    ||
	    lb_p->title_angle != olb_p->title_angle    ||
	    lb_p->title_font != olb_p->title_font    ||
	    lb_p->title_color != olb_p->title_color    ||
	    lb_p->title_height != olb_p->title_height    ||
	    lb_p->title_aspect != olb_p->title_aspect    ||
	    lb_p->title_thickness != olb_p->title_thickness    ||
	    lb_p->title_quality != olb_p->title_quality    ||
	    lb_p->title_const_spacing != olb_p->title_const_spacing ||
	    lb_p->title_func_code != olb_p->title_func_code) {
		
		return True;
	}

	return False;

		
}
/*ARGSUSED*/
static NhlErrorTypes    AdjustGeometry
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayer	told = (LabelBarLayer) old;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	LabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	NhlBoundingBox titleBB;
	NhlBoundingBox labelsBB;
	float x_off, y_off;
	float title_x = lb_p->title_x;
	float title_y = lb_p->title_y;
	NhlBoundingBox labelbarBB;

	if ((ret1 = NhlGetBB(lb_p->title_id, &titleBB)) < WARNING)
		return FATAL;
	ret = MIN(ret1,ret);

	if ((ret1 = NhlGetBB(lb_p->labels_id, &labelsBB)) < WARNING)
		return FATAL;
	ret = MIN(ret1,ret);

	lb_p->expand_perim.l = lb_p->perim.l;
	lb_p->expand_perim.r = lb_p->perim.r;
	lb_p->expand_perim.b = lb_p->perim.b;
	lb_p->expand_perim.t = lb_p->perim.t;
	if (lb_p->orient == NhlHORIZONTAL) {

		if (labelsBB.l < lb_p->labels.l) {
			x_off = lb_p->labels.l - labelsBB.l;
			lb_p->expand_perim.l -= x_off; 
			if (lb_p->title_on && lb_p->title_pos == NhlLEFT)
				lb_p->title_x -= x_off;
		}
		if (labelsBB.r > lb_p->labels.r) {
			x_off = labelsBB.r - lb_p->labels.r;
			lb_p->expand_perim.r += x_off; 
			if (lb_p->title_on && lb_p->title_pos == NhlRIGHT)
				lb_p->title_x += x_off;
		}
		if (lb_p->label_pos != NhlTOP) {
			if (labelsBB.b < lb_p->labels.b) {
				y_off = lb_p->labels.b - labelsBB.b;
				lb_p->expand_perim.b -= y_off; 
				if (lb_p->title_on && 
				    lb_p->title_pos == NhlBOTTOM)
					lb_p->title_y -= y_off;
			}
		}
		if (lb_p->label_pos != NhlBOTTOM) {
			if (labelsBB.t > lb_p->labels.t) {
				y_off = labelsBB.t - lb_p->labels.t;
				lb_p->expand_perim.t += y_off; 
				if (lb_p->title_on && 
				    lb_p->title_pos == NhlTOP)
					lb_p->title_y += y_off;
			}
		}
	}
	else {

		if (labelsBB.b < lb_p->labels.b) {
			y_off = lb_p->labels.b - labelsBB.b;
			lb_p->expand_perim.b -= y_off; 
			if (lb_p->title_on && lb_p->title_pos == NhlBOTTOM)
				lb_p->title_y -= y_off;
		}
		if (labelsBB.t > lb_p->labels.t) {
			y_off = labelsBB.t - lb_p->labels.t;
			lb_p->expand_perim.t += y_off; 
			if (lb_p->title_on && lb_p->title_pos == NhlTOP)
				lb_p->title_y += y_off;
		}
		if (lb_p->label_pos != NhlRIGHT) {
			if (labelsBB.l < lb_p->labels.l) {
				x_off = lb_p->labels.l - labelsBB.l;
				lb_p->expand_perim.l -= x_off; 
				if (lb_p->title_on && 
				    lb_p->title_pos == NhlLEFT)
					lb_p->title_x -= x_off;
			}
		}
		if (lb_p->label_pos != NhlLEFT) {
			if (labelsBB.r > lb_p->labels.r) {
				x_off = labelsBB.r - lb_p->labels.r;
				lb_p->expand_perim.r += x_off; 
				if (lb_p->title_on && 
				    lb_p->title_pos == NhlRIGHT)
					lb_p->title_x += x_off;
			}
		}
	}
	labelbarBB.l = MIN(labelsBB.l, lb_p->adj_bar.l);
	labelbarBB.r = MAX(labelsBB.r, lb_p->adj_bar.r);
	labelbarBB.b = MIN(labelsBB.b, lb_p->adj_bar.b);
	labelbarBB.t = MAX(labelsBB.t, lb_p->adj_bar.t);

	if (title_x != lb_p->title_x) {
		title_x = lb_p->title_x - title_x;
	}
	if (title_y != lb_p->title_y) {
		title_y = lb_p->title_y - title_y;
	}

#if 0
	lb_p->adjust_frac.l = (lb_p->perim.l - lb_p->lb_x) /
		(lb_p->perim.r - lb_p->perim.l);
	lb_p->adjust_frac.r = (lb_p->perim.r - lb_p->lb_x - lb_p->lb_width) /
		(lb_p->perim.r - lb_p->perim.l);
	lb_p->adjust_frac.b = (lb_p->perim.b - lb_p->lb_y + lb_p->lb_height) /
		(lb_p->perim.r - lb_p->perim.l);
	lb_p->adjust_frac.t = (lb_p->perim.t - lb_p->lb_y) /
		(lb_p->perim.r - lb_p->perim.l);
	lb_p->lb_x = lb_p->perim.l;
	lb_p->lb_y = lb_p->perim.t;
	lb_p->lb_width = lb_p->perim.r - lb_p->perim.l;
	lb_p->lb_height = lb_p->perim.t - lb_p->perim.b;
#endif

	ret1 = NhlSetValues(lb_p->title_id,
			    NhlNtxPosXF,lb_p->title_x,
			    NhlNtxPosYF,lb_p->title_y,
			    NULL);

	_NhlInternalSetView((ViewLayer)tnew,
			    lb_p->expand_perim.l, lb_p->expand_perim.t,
			    lb_p->expand_perim.r - lb_p->expand_perim.l,
			    lb_p->expand_perim.t - lb_p->expand_perim.b
			    ,False);

}
/*ARGSUSED*/
static NhlErrorTypes    SetBoxLocations
#if __STDC__
	Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayer	told = (LabelBarLayer) old;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	LabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	float *f_p;
	int first_neg, last_neg;
	float bar_len;
	int i;

/* 
 * Set up the box_fraction array
 */
	if (init && lb_p->box_sizing == NhlLB_UNIFORMSIZING) {
		lb_p->box_fractions = NULL;
	}
	else if (lb_p->box_sizing == NhlLB_EXPLICITSIZING) {
		if (init && !_NhlArgIsSet(args,num_args,NhlNlbBoxFractions)) {
			/* WARNING */
			printf("WARNING, defaulting to UNIFORM SIZING\n");
			lb_p->box_sizing = NhlLB_UNIFORMSIZING;
			lb_p->box_fractions = NULL;
		}
		else if (init) {
			f_p = (float *)
				NhlMalloc(lb_p->box_count * sizeof(float));
			memcpy((void *)f_p,(Const void *) lb_p->box_fractions,
			       lb_p->box_count * sizeof(float));
			lb_p->box_fractions = f_p;
		}
		else if (lb_p->box_fractions == NULL) {
			/* WARNING */
			printf("WARNING, defaulting to UNIFORM SIZING\n");
			lb_p->box_sizing = NhlLB_UNIFORMSIZING;
		}
		else if (lb_p->box_fractions != olb_p->box_fractions) {
			NhlFree(olb_p->box_fractions);
			f_p = (float *) 
				NhlMalloc(lb_p->box_count * sizeof(float));
			memcpy((void *)f_p,(Const void *) lb_p->box_fractions,
			       lb_p->box_count * sizeof(float));
			lb_p->box_fractions = f_p;
		}
		else if (lb_p->box_count > lb_p->last_box_count) {
			f_p = (float *) 
				NhlMalloc(lb_p->box_count * sizeof(float));
			memcpy((void *)f_p,(Const void *) lb_p->box_fractions,
			       lb_p->box_count * sizeof(float));
			NhlFree(lb_p->box_fractions);
			lb_p->box_fractions = f_p;
			for (i=lb_p->last_box_count; i < lb_p->box_count; i++)
				lb_p->box_fractions[i] = -1.0;
		}

		if (lb_p->box_sizing == NhlLB_EXPLICITSIZING) {
			ret1 = ValidateBoxFractions(lb_p);
			if (ret1 <= WARNING) {
				printf("invalid box fraction array\n");
				lb_p->box_sizing = NhlLB_UNIFORMSIZING;
			}
		}
					
	}
	
	     
/*
 * Allocate or reallocate the location arrays: use one more than
 * the current box count so that both ends of the labelbar can be stored
 */

	if (lb_p->box_count > lb_p->last_box_count) {
		if (lb_p->box_locs == NULL) {
			lb_p->box_locs = (float *) 
				NhlMalloc((lb_p->box_count+1) * sizeof(float));
		}
		else {
			lb_p->box_locs = (float *) 
				NhlRealloc(lb_p->box_locs, 
					   (lb_p->box_count+1) *
					   sizeof(float));
		}
	}

/* 
 * Adjust boundary of bar area, depending on the label state.
 */

	memcpy((void *)&lb_p->adj_bar, (Const void *)&lb_p->bar, 
	       sizeof(NhlBoundingBox));
	memcpy((void *)&lb_p->adj_box_size, (Const void *)&lb_p->box_size,
	       sizeof(NhlCoord));
	if (lb_p->label_alignment == NhlLB_EXTERNALEDGES) {
		if (lb_p->orient == NhlHORIZONTAL) {
			lb_p->adj_box_size.x = lb_p->box_size.x * 
				lb_p->box_count / (lb_p->box_count + 1);
			lb_p->adj_bar.l = lb_p->bar.l + lb_p->box_size.x / 2.0;
			lb_p->adj_bar.r = lb_p->bar.r - lb_p->box_size.x / 2.0;
		}
		else {
			lb_p->adj_box_size.y = lb_p->box_size.y * 
				lb_p->box_count / (lb_p->box_count + 1);
			lb_p->adj_bar.b = lb_p->bar.b + lb_p->box_size.y / 2.0;
			lb_p->adj_bar.t = lb_p->bar.t - lb_p->box_size.y / 2.0;
		}
	}
/*
 * still need an angle adjustment
 */

/*
 * create an array of the left or bottom varying coordinates -- 
 */
		
	if (lb_p->orient == NhlHORIZONTAL &&
	    lb_p->box_sizing == NhlLB_UNIFORMSIZING) {
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.l + 
				(float) i * lb_p->adj_box_size.x;
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.r;
	}
	if (lb_p->orient == NhlVERTICAL &&
	    lb_p->box_sizing == NhlLB_UNIFORMSIZING) {
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.b + 
				(float) i * lb_p->adj_box_size.y;
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.t;
	}
	if (lb_p->orient == NhlHORIZONTAL &&
	    lb_p->box_sizing == NhlLB_EXPLICITSIZING) {
		bar_len = lb_p->adj_bar.r - lb_p->adj_bar.l;
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.l + 
				bar_len * lb_p->box_fractions[i];
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.r;
	}
	if (lb_p->orient == NhlVERTICAL &&
	    lb_p->box_sizing == NhlLB_EXPLICITSIZING) {
		bar_len = lb_p->adj_bar.t - lb_p->adj_bar.b;
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.b + 
				bar_len * lb_p->box_fractions[i];
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.t;
	}

}

/*ARGSUSED*/
static NhlErrorTypes    ValidateBoxFractions
#if __STDC__
	(LabelBarLayerPart *lb_p) 
#else
(lb_p)
	LabelBarLayerPart *lb_p;
#endif

{
	int i, first_neg;
			
/*
 * At this point it should be guaranteed that the box_fraction array exists
 * and it is of the correct length.
 * Now check the validity of the explicit sizing fractions. Negative values
 * are not an error, but cause equal divisions between the two surrounding
 * specified values. Non-monotonically increasing positive values, or
 * values greater than 1.0 are an error causing a warning and a mode
 * change to uniform spacing.
 */
	/* deal with first element manually, since in the loop the
	   previous element must be compared */

	if (lb_p->box_fractions[0] < 0.0)
		lb_p->box_fractions[0] = 0.0;
	else if (lb_p->box_fractions[0] > 1.0)
		return (WARNING);

	first_neg = -1;
	for (i=1; i<lb_p->box_count;i++) {
		if (lb_p->box_fractions[i] < 0.0) {
			if (first_neg == -1)
				first_neg = i;
			else
				continue;
		}
		else if (first_neg != -1) {
			if (lb_p->box_fractions[i] > 1.0 ||
			    lb_p->box_fractions[i] <
			    lb_p->box_fractions[first_neg-1]) {
				return (WARNING);
			}
			CreateIntermediates(lb_p->box_fractions, 
					    first_neg, i);
			first_neg = -1;
		}
		else if (lb_p->box_fractions[i] > 1.0 ||
			 (lb_p->box_fractions[i] < lb_p->box_fractions[i-1])) {
			return (WARNING);
		}
	}
	return (NOERROR);

}

/*
 * Creates evenly spaced values to replace negative values in the
 * box fractions array. The start value is the index of the first
 * negative value in the chain; the end value is the index of the
 * first non-negative value. Since the zeroth value has been dealt with
 * separately, it is not necessary to test for it here.
 */

static void CreateIntermediates
#if __STDC__
	(float		*flist,
	 int		start,
	 int		end)
#else
(flist,start,end)
	float		*flist;
	int		start;
	int		end;
#endif
{
	float	spacing;
	int	count;
	int	i;

	count = end - start + 1;
	spacing = (flist[end] - flist[start - 1]) / count;
	for (i=0; i<count-1; i++) {
		flist[start+i] = flist[start-1] + (float) i * spacing;
	}
}
#define DEGTORAD 0.017453293
/*ARGSUSED*/
static NhlErrorTypes    SetLabels
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayer	told = (LabelBarLayer) old;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	LabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	char buffer[MAXRESNAMLEN];
	static int label_draw_count = 0;
	char **labels_p;
	int count; 
	int itmp, max_strlen = 0;
	int i,j,ix;
	NhlMTextOrientatonType mtext_orient;
	float label_height, char_space, avail_char_space;
	float const_pos;
	float base_pos, offset, increment;
	NhlCoord larea;
	NhlBoundingBox stringBB;
	float w, h, wta, hta, factor1, factor2;
	float wb, wt, hb, ht;
	float tol_angle = 0.5;
	float max_char_size;
	float c_angle;

	if (! lb_p->labels_on)
		return;

/*
 * Determine the multitext orientation
 */

	if (lb_p->orient == NhlHORIZONTAL)
		mtext_orient = MTEXT_Y_CONST;
	else
		mtext_orient = MTEXT_X_CONST;
/*
 * The number of labels varies depending on the label alignment
 */

	count = lb_p->box_count;
	if (lb_p->label_alignment == NhlLB_INTERIOREDGES)
		count--;
	else if (lb_p->label_alignment == NhlLB_EXTERNALEDGES) 
		count++;
		
/*
 * If the label stride is greater than 1, find the number of
 * labels to use. If greater than the previous size, reallocate
 * the array used to point to the correct labels.
 */
	
	labels_p = lb_p->label_strings;
	if (lb_p->label_stride > 1) {
		count /= lb_p->label_stride;
		if (count > label_draw_count) {
			if (lb_p->stride_labels == NULL) {
				lb_p->stride_labels = (char **) 
					NhlMalloc(count * sizeof(char *));
			}
			else {
				lb_p->stride_labels = (char **)
					NhlRealloc(lb_p->stride_labels,
						   count * sizeof(char *));
			}
		}
		labels_p = lb_p->stride_labels;
		for (i=0,j=0; i<count; i++,j+=lb_p->label_stride) {
			labels_p[i] = lb_p->label_strings[j];
		}
	}

/*
 * Now allocate the location array
 */
	if (count > label_draw_count) {
		if (lb_p->label_locs == NULL) {
			lb_p->label_locs = (float *) 
				NhlMalloc(count * sizeof(float));
		}
		else {
			lb_p->label_locs = (float *)
				NhlRealloc(lb_p->label_locs,
					   count * sizeof(float));
		}
	}

	label_draw_count = count;
	
/*
 * Find the size of the longest text string, 
 * then determine a suitable text size -
 * if the text angle is not 0, there will be extra complications
 * don't deal with that issue yet.
 */
	for (i=0; i<count; i++) {
		if ((itmp = strlen(labels_p[i])) > max_strlen) {
			max_strlen = itmp;
		}
	}
	
	if (lb_p->label_pos == NhlCENTER) {
		larea.x = lb_p->adj_bar.r - lb_p->adj_bar.l;
		larea.y = lb_p->adj_bar.t - lb_p->adj_bar.b;
	}
	else {
		larea.x = lb_p->labels.r - lb_p->labels.l;
		larea.y = lb_p->labels.t - lb_p->labels.b;
	}
	lb_p->label_angle = fmod(lb_p->label_angle,360.0);
		
	if (lb_p->orient == NhlHORIZONTAL && 
	    lb_p->label_direction == ACROSS) {

		char_space = 0.8 * larea.y / (max_strlen + 1.0);
		avail_char_space = 0.5 * larea.x / label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		if (lb_p->label_pos == NhlBOTTOM) {
				
			const_pos = lb_p->labels.t - 
				label_height;
		}
		else if (lb_p->label_pos == NhlCENTER) {
			const_pos = lb_p->adj_bar.b + 
				lb_p->adj_box_size.y / 2.0;
		}
		else if (lb_p->label_pos == NhlTOP) {
			const_pos = lb_p->labels.b + 
				label_height;
		}
		else if (lb_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}
	else if (lb_p->orient == NhlHORIZONTAL){ /* DOWN or UP */

		/* Set the font height */

		char_space = 0.8 * larea.y / (max_strlen + 1.0);
		avail_char_space = 0.5 * larea.x / label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		if (lb_p->label_pos == NhlBOTTOM) {
			const_pos = lb_p->labels.t - 
				label_height;
		}
		else if (lb_p->label_pos == NhlCENTER) {
			const_pos = lb_p->adj_bar.b + 
				lb_p->adj_box_size.y / 2.0;
		}
		else if (lb_p->label_pos == NhlTOP) {
			const_pos = lb_p->labels.b + 
				label_height;
		}
		else if (lb_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}
	else if (lb_p->orient == NhlVERTICAL && 
		 lb_p->label_direction == ACROSS) {

		char_space = 0.8 * larea.x / (max_strlen + 1.0);
		avail_char_space = 0.5 * larea.y / label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		if (lb_p->label_pos == NhlLEFT) {
			const_pos = lb_p->labels.r - 
				label_height;
		}
		else if (lb_p->label_pos == NhlCENTER) {
			const_pos = lb_p->adj_bar.l + 
				lb_p->adj_box_size.x / 2.0;
		}
		else if (lb_p->label_pos == NhlRIGHT) {
			const_pos = lb_p->labels.l + 
				label_height;
		}
		else if (lb_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}
	else { /* NhlVERTICAL DOWN or UP */
		char_space = 0.8 * larea.x / (max_strlen + 1.0);
		avail_char_space = 0.5 * larea.y / label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		if (lb_p->label_pos == NhlLEFT) {
			const_pos = lb_p->labels.r - 
				label_height;
		}
		else if (lb_p->label_pos == NhlCENTER) {
			const_pos = lb_p->adj_bar.l + 
				lb_p->adj_box_size.x / 2.0;
		}
		else if (lb_p->label_pos == NhlRIGHT) {
			const_pos = lb_p->labels.l + 
				label_height;
		}
		else if (lb_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}

/*
 * Now find the variable label positions
 */

	if (lb_p->box_sizing == NhlLB_UNIFORMSIZING) {

		if (lb_p->orient == NhlHORIZONTAL) {
		
			/* position array contains X values */

			base_pos = lb_p->labels.l;
		       
			/* determine offset */
			if (lb_p->label_alignment == NhlLB_BOXCENTERS)
				offset = lb_p->adj_box_size.x / 2.0;
			else if (lb_p->label_alignment == NhlLB_INTERIOREDGES)
				offset = lb_p->adj_box_size.x;
			else
				offset = 0;

			increment = lb_p->adj_box_size.x * lb_p->label_stride;
		}
		else if (lb_p->orient == NhlVERTICAL) {
		
			/* position array contains Y values */

			base_pos = lb_p->labels.b;
		       
			/* determine offset */
			if (lb_p->label_alignment == NhlLB_BOXCENTERS)
				offset = lb_p->adj_box_size.y / 2.0;
			else if (lb_p->label_alignment == NhlLB_INTERIOREDGES)
				offset = lb_p->adj_box_size.y;
			else
				offset = 0;

			increment = lb_p->adj_box_size.y * lb_p->label_stride;
		}
		for (i=0; i<label_draw_count; i++) 
			lb_p->label_locs[i] = base_pos + offset + 
				(float) i * increment;
	}
	else {
		for (i=0; i < label_draw_count; i++) {

			ix = i * lb_p->label_stride;
			if (lb_p->label_alignment == NhlLB_BOXCENTERS)
				lb_p->label_locs[i] = lb_p->box_locs[ix] +
					(lb_p->box_locs[ix+1] -
					  lb_p->box_locs[ix]) / 2.0;
			else if (lb_p->label_alignment == NhlLB_INTERIOREDGES)
				lb_p->label_locs[i] = lb_p->box_locs[ix+1];
			else
				lb_p->label_locs[i] = lb_p->box_locs[ix];

		}
	}

	if (init) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Labels");
		ret1 = NhlCreate(&(lb_p->labels_id),buffer,
				 multiTextLayerClass,tnew->base.id,
				 NhlNMtextNumStrings,label_draw_count,
				 NhlNMtextStrings,labels_p,
				 NhlNMtextOrientation,mtext_orient,
				 NhlNMtextConstPosF,const_pos,
				 NhlNMtextPosArray,lb_p->label_locs,
				 NhlNtxAngleF,lb_p->label_angle,
				 NhlNtxFont,lb_p->label_font,
				 NhlNtxJust,lb_p->label_just,
				 NhlNtxFontHeightF,label_height,
				 NhlNtxFontAspectF,lb_p->label_aspect,
				 NhlNtxDirection,lb_p->label_direction,
				 NhlNtxConstantSpacingF,
				 lb_p->label_const_spacing,
				 NhlNtxFontColor,lb_p->label_color,
				 NULL);
		if(ret1 < WARNING) {
			NhlPError(FATAL,E_UNKNOWN,"MultiText create error");
			return(FATAL);
		}
		ret = MIN(ret,ret1);

	} 
	else {
		ret1 = NhlSetValues(lb_p->labels_id,
				    NhlNMtextNumStrings,label_draw_count,
				    NhlNMtextStrings,labels_p,
				    NhlNMtextOrientation,mtext_orient,
				    NhlNMtextConstPosF,const_pos,
				    NhlNMtextPosArray,lb_p->label_locs,
				    NhlNtxAngleF,lb_p->label_angle,
				    NhlNtxFont,lb_p->label_font,
				    NhlNtxJust,lb_p->label_just,
				    NhlNtxFontHeightF,label_height,
				    NhlNtxFontAspectF,lb_p->label_aspect,
				    NhlNtxDirection,lb_p->label_direction,
				    NhlNtxFontColor,lb_p->label_color,
				    NULL);
		if(ret1 < WARNING) {
			NhlPError(FATAL,E_UNKNOWN,
				  "Multitext SetValues error");
			return(FATAL);
		}
		ret = MIN(ret,ret1);

	}

/*
 * Unless the label height has been set explicitly adjust it to fit
 * the size it has available
 */


	ret = NhlGetBB(lb_p->labels_id, &stringBB);
	if ((w=stringBB.r-stringBB.l) <= 0.0 ||
	    (h=stringBB.t-stringBB.b) <= 0.0) {
		printf("error getting BB\n");
	}
	if (lb_p->orient == NhlHORIZONTAL) {
		wb = larea.x / count;
		wt = wb - larea.x + w;
		hb = larea.y;
		ht = h;
	}
	else {
		wb = larea.x;
		wt = w;
		hb = larea.y / count;
		ht = hb - larea.y + h;
	}
		
	{
		float theta = DEGTORAD * lb_p->label_angle;
		float ct = fabs(cos(theta));
		float st = fabs(sin(theta));
		factor1 = (ct*larea.x + st*larea.y) /
			(ct*w+st*h);
		if (i == 0) {
			factor2 = wb / wt < hb / ht ?  
				wb / wt  : hb / ht;
			factor1 = factor1 > factor2 ? 
				factor1 : factor2;
		}
		if (lb_p->orient == NhlHORIZONTAL) {
			if (ct == 0.0) printf ("whoops\n");
			else 
				label_height /= st;
		}
		else {
			if (ct == 0.0) printf ("whoops\n");
			else 
				label_height /= ct;
		}
		label_height = label_height < avail_char_space ?
			label_height : avail_char_space;
		if (InCriticalZone(lb_p, label_height, &c_angle)) {
			if (lb_p->orient == NhlHORIZONTAL) {
				factor1 = 0.8 * wb / (max_strlen +2);
				factor2 = factor1 +
					(st / fabs(sin(DEGTORAD * c_angle))) *
#if 0
					(ct * ct /
					 fabs(cos(DEGTORAD * c_angle)) *
					 fabs(cos(DEGTORAD * c_angle))) *
#endif
					fabs(0.8 *avail_char_space - factor1);
				label_height = label_height < factor2 ?
					label_height : factor2;
			}
			else {
				factor1 = 0.8 * hb / (max_strlen +2);
				factor2 = factor1 + 
					(ct / fabs(cos(DEGTORAD * c_angle))) *
#if 0
					(ct * ct /
					 fabs(cos(DEGTORAD * c_angle)) *
					 fabs(cos(DEGTORAD * c_angle))) *
#endif
					fabs(0.8 * avail_char_space - factor1);
				label_height = label_height < factor2 ?
					label_height : factor2;
			}
		}
	}
#if 0
			factor1 = wb / wt < hb / ht ?  wb / wt  : hb / ht;
			label_height *= factor1 * 0.8;
		}
		label_height = label_height < avail_char_space ?
			label_height : avail_char_space;
#endif
	ret = NhlSetValues(lb_p->labels_id,
			   NhlNtxFontHeightF,label_height,
			   NhlNtxJust,lb_p->label_just,
			   NULL);

	return (ret);
}

#define NhlLB_TRANSITIONANGLE 7.5
static NhlBoolean    	InCriticalZone
#if __STDC__
	(LabelBarLayerPart *lb_p,
	float		height,
	float		*critical_angle)
#else
(lb_p, height, critical_angle)
	LabelBarLayerPart *lb_p;
	float		height;
	float		*critical_angle;
#endif
{
	float theta1, theta2, theta3, theta4;
	float w_weight;
	int yes_or_no = 0;

	if (lb_p->label_pos == NhlCENTER) {
		return 1;
	}
	
	theta1 = lb_p->label_angle < 360.0 - lb_p->label_angle ?
		lb_p->label_angle : 360.0 - lb_p->label_angle;
	theta2 = fabs(90 - lb_p->label_angle);
	theta3 = fabs(180 - lb_p->label_angle);
	theta4 = fabs(270 - lb_p->label_angle);

	if (lb_p->orient == NhlHORIZONTAL && 
	    lb_p->label_direction == ACROSS) {

 		*critical_angle = asin(1.5 * height / 
				       lb_p->box_size.x) / DEGTORAD;
		if (theta1 <= *critical_angle ||
		    theta3 <= *critical_angle)
			yes_or_no = 1;

		if (theta1 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (theta3 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else if (lb_p->label_angle < 90.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPRIGHT;
			}
			else {
				lb_p->label_just = NhlBOTTOMLEFT;
			}
		}
		else if (lb_p->label_angle < 180.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
			else {
				lb_p->label_just = NhlTOPLEFT;
			}
		}
		else if (lb_p->label_angle < 270.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMLEFT;
			}
			else {
				lb_p->label_just = NhlTOPRIGHT;
			}
		}
		else {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPLEFT;
			}
			else {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
		}
	}
	else if (lb_p->orient == NhlHORIZONTAL){ /* DOWN or UP */

 		*critical_angle = asin(height / lb_p->box_size.x) / DEGTORAD;
		if (theta2 <= *critical_angle ||
		    theta4 <= *critical_angle)
			yes_or_no = 1;


		if (theta2 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta4 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (lb_p->label_angle > 270.0 ||
			 lb_p->label_angle < 90.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
			 
	}
	else if (lb_p->orient == NhlVERTICAL && 
		 lb_p->label_direction == ACROSS) {

 		*critical_angle = asin(height / lb_p->box_size.y) / DEGTORAD;
		if (theta2 <= *critical_angle ||
		    theta4 <= *critical_angle)
			yes_or_no = 1;


		if (theta2 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else if (theta4 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (lb_p->label_angle > 270.0  ||
			 lb_p->label_angle < 90.0) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
			 
	}
	else { /* NhlVERTICAL DOWN or UP */

 		*critical_angle = asin(height / lb_p->box_size.y) / DEGTORAD;
		if (theta1 <= *critical_angle ||
		    theta3 <= *critical_angle)
			yes_or_no = 1;



		if (theta1 <=NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta3 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (lb_p->label_angle < 180.0) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
			else {
				lb_p->label_just = NhlTOPLEFT;
			}
		}
		else {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlTOPLEFT;
			}
			else {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
		}
	}

	return (yes_or_no);
}
/*ARGSUSED*/
static NhlErrorTypes    SetTitle
#if __STDC__
	Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		 num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayer	told = (LabelBarLayer) old;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	LabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	char buffer[MAXRESNAMLEN];
	char *c_p;
	NhlBoundingBox stringBB;
	float w, h, wta, hta, factor;

/*
 * Only initialize a text item for the title if it is turned on
 */
	if (!lb_p->title_on)
		return ret;

/*
 * If the title string is NULL, create a default string.
 * The default string is the name of the label bar object
 */
	if (lb_p->title_string == NULL) {
                lb_p->title_string = (char*)
                        NhlMalloc((unsigned)strlen(tnew->base.name) +1);
                strcpy(lb_p->title_string,tnew->base.name);
        } 
	else if (init) {
                c_p = lb_p->title_string;
                lb_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
                strcpy(lb_p->title_string,c_p);
        }
	else if (lb_p->title_string != olb_p->title_string) {
		NhlFree(olb_p->title_string);
		c_p = lb_p->title_string;
		lb_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
		strcpy(lb_p->title_string, c_p);
	}

	switch (lb_p->title_just) {
	case NhlBOTTOMLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlBOTTOMCENTER:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlBOTTOMRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlCENTERLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlCENTERCENTER:
	default:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlCENTERRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlTOPLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.t;
		break;
	case NhlTOPCENTER:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.t;
		break;
	case NhlTOPRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.t;
		break;
	}


	if (init || lb_p->title_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Title");
		ret1 = NhlCreate(&lb_p->title_id,
				 buffer,textItemLayerClass,
				 tnew->base.id,
				 NhlNtxFont,lb_p->title_font,
				 NhlNtxString,lb_p->title_string,
				 NhlNtxPosXF,lb_p->title_x,
				 NhlNtxPosYF,lb_p->title_y,
				 NhlNtxDirection,lb_p->title_direction,
				 NhlNtxAngleF,lb_p->title_angle,
				 NhlNtxJust,(int)lb_p->title_just,
				 NhlNtxFontColor,lb_p->title_color,
				 NhlNtxFontHeightF,lb_p->title_height,
				 NhlNtxFontAspectF,lb_p->title_aspect,
				 NhlNtxConstantSpacingF,
				 	lb_p->title_const_spacing,
				 NhlNtxFontQuality,lb_p->title_quality,
				 NhlNtxFuncCode,lb_p->title_func_code,
				 NhlNtxFontThicknessF,lb_p->title_thickness,
				 NULL);
		
	}
	else {
		ret1 = NhlSetValues(lb_p->title_id,
				    NhlNtxFont,lb_p->title_font,
				    NhlNtxString,lb_p->title_string,
				    NhlNtxPosXF,lb_p->title_x,
				    NhlNtxPosYF,lb_p->title_y,
				    NhlNtxDirection,lb_p->title_direction,
				    NhlNtxAngleF,lb_p->title_angle,
				    NhlNtxJust,(int)lb_p->title_just,
				    NhlNtxFontColor,lb_p->title_color,
				    NhlNtxFontHeightF,lb_p->title_height,
				    NhlNtxFontAspectF,lb_p->title_aspect,
				    NhlNtxConstantSpacingF,
				    	lb_p->title_const_spacing,
				    NhlNtxFontQuality,lb_p->title_quality,
				    NhlNtxFuncCode,lb_p->title_func_code,
				    NhlNtxFontThicknessF,lb_p->title_thickness,
				    NULL);
	}

/*
 * Unless the title height has been set explicitly adjust it to fit
 * the size it has available
 */

	ret = NhlGetBB(lb_p->title_id, &stringBB);
	if ((w=stringBB.r-stringBB.l) <= 0.0 ||
	    (h=stringBB.t-stringBB.b) <= 0.0) {
		printf("error getting BB\n");
	}
	if ((wta=lb_p->title.r-lb_p->title.l) <= 0.0 ||
	    (hta=lb_p->title.t-lb_p->title.b) <= 0.0) {
		printf("error in title area\n");
	}
	factor = wta / w < hta / h ? wta / w : hta / h;
	lb_p->title_height *= factor * 0.8;
#if 0
	switch (lb_p->title_direction) {
	case ACROSS:
		lb_p->title_height *= (wta / w);
		lb_p->title_height = (lb_p->title_height < hta) ?
			lb_p->title_height : hta;
		break;
	case DOWN:
	case UP:
		lb_p->title_height *= (hta / h);
		lb_p->title_height = (lb_p->title_height < wta) ?
			lb_p->title_height : wta;
		break;
	default:
		/* fatal */
		break;
	}
#endif
	ret = NhlSetValues(lb_p->title_id,
			   NhlNtxFontHeightF,lb_p->title_height,
			   NULL);

	return ret;
}

/*ARGSUSED*/
static NhlErrorTypes    SetLabelBarGeometry
#if __STDC__
	 Layer new, 
	 _NhlArgList args,
	 int num_args)
#else
(new,args,num_args)
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LabelBarLayer	tnew = (LabelBarLayer) new;
	LabelBarLayerPart *lb_p = &(tnew->labelbar);
	int max_str_adjust = False;
	enum {NO_TITLE, MINOR_AXIS, MAJOR_AXIS} title_loc;
	float bar_ext, title_ext, adj_perim_width, adj_perim_height;

#if 0
/* Calculate the perimeter */

	lb_p->perim.l = lb_p->lb_x;
	lb_p->perim.t = lb_p->lb_y;
	lb_p->perim.r = lb_p->lb_x + lb_p->lb_width;
	lb_p->perim.b = lb_p->lb_y - lb_p->lb_height;
#endif

/* Calculate the ndc margin from the fractional margin */

	lb_p->adj_perim.l = lb_p->perim.l + lb_p->margin.l * lb_p->lb_width;
	lb_p->adj_perim.r = lb_p->perim.r - lb_p->margin.r * lb_p->lb_width;
	lb_p->adj_perim.b = lb_p->perim.b + lb_p->margin.b * lb_p->lb_height;
	lb_p->adj_perim.t = lb_p->perim.t - lb_p->margin.t * lb_p->lb_height;
	adj_perim_width = lb_p->adj_perim.r - lb_p->adj_perim.l;
	adj_perim_height = lb_p->adj_perim.t - lb_p->adj_perim.b;
		
/*
 * Locate the title
 */
	if (!lb_p->title_on)
		title_loc = NO_TITLE;
	else if (lb_p->orient == NhlHORIZONTAL) {
		if (lb_p->title_pos == NhlRIGHT ||
		    lb_p->title_pos == NhlLEFT)
			title_loc = MAJOR_AXIS;
		else 
			title_loc = MINOR_AXIS;
	}
	else {
		if (lb_p->title_pos == NhlRIGHT ||
		    lb_p->title_pos == NhlLEFT)
			title_loc = MINOR_AXIS;
		else 
			title_loc = MAJOR_AXIS;
	}

/*
 * Fix the label position if necessary
 */

	if (lb_p->orient == NhlHORIZONTAL) {
		switch (lb_p->label_pos) {
		case NhlLEFT:
			lb_p->label_pos = NhlBOTTOM;
			break;
		case NhlRIGHT:
			lb_p->label_pos = NhlTOP;
			break;
		}
	}
	else {
		switch (lb_p->label_pos) {
		case NhlBOTTOM:
			lb_p->label_pos = NhlLEFT;
			break;
		case NhlTOP:
			lb_p->label_pos = NhlRIGHT;
			break;
		}
	}
/*
 * Determine dimensions: first pass determines basic position for
 * title, bar and labels. Bar dimensions may be adjusted later to 
 * ensure that labels are visible.
 */
	if (lb_p->orient == NhlHORIZONTAL) {
		bar_ext = adj_perim_height * lb_p->box_minor_ext;
		switch (title_loc) {

		case NO_TITLE:
			lb_p->bar.l = lb_p->adj_perim.l;
			lb_p->bar.r = lb_p->adj_perim.r;
			lb_p->labels.l = lb_p->bar.l;
			lb_p->labels.r = lb_p->bar.r;
					
			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = lb_p->adj_perim.b +
					(adj_perim_height - bar_ext) / 2.0;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
				lb_p->labels.t = lb_p->adj_perim.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.t - bar_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->labels.b = lb_p->adj_perim.b;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;

		case MAJOR_AXIS:
			if (lb_p->title_ext > 0.5) {
				/* need a WARNING */
				lb_p->title_ext = 0.5;
			}
			title_ext = adj_perim_width * lb_p->title_ext;
			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.l + title_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->title.l = lb_p->adj_perim.l;
				lb_p->title.r = lb_p->bar.l;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->adj_perim.r - title_ext;
				lb_p->title.l = lb_p->bar.r;
				lb_p->title.r = lb_p->adj_perim.r;
			}
			lb_p->labels.l = lb_p->bar.l;
			lb_p->labels.r = lb_p->bar.r;

			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = lb_p->adj_perim.b +
					(adj_perim_height - bar_ext) / 2.0;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
				lb_p->labels.t = lb_p->adj_perim.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.t - bar_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->labels.b = lb_p->adj_perim.b;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;

		case MINOR_AXIS:
			if (lb_p->title_ext > 0.5) {
				/* need a WARNING */
				lb_p->title_ext = 0.5;
			}
			if (lb_p->title_ext + lb_p->box_minor_ext > 1.0) {
				/* need a WARNING */
				lb_p->box_minor_ext = 
					1.0 - lb_p->title_ext;
				bar_ext = adj_perim_height * 
					lb_p->box_minor_ext;
			}
			title_ext = adj_perim_height * lb_p->title_ext;

			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
			lb_p->bar.l = lb_p->adj_perim.l;
			lb_p->bar.r = lb_p->adj_perim.r;
			lb_p->labels.l = lb_p->adj_perim.l;
			lb_p->labels.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->title.b = lb_p->adj_perim.b;
				lb_p->title.t = lb_p->adj_perim.b + 
					title_ext;
				lb_p->bar.b = lb_p->title.t;
				lb_p->bar.t = lb_p->adj_perim.t;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->adj_perim.t - title_ext;
				lb_p->title.b = lb_p->bar.t;
				lb_p->title.t = lb_p->adj_perim.t;
			}

			
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = (lb_p->bar.t - lb_p->bar.b - 
					       bar_ext) / 2.0 + lb_p->bar.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->labels.t = lb_p->bar.t;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->labels.b = lb_p->bar.b;
				lb_p->bar.b = lb_p->bar.t - bar_ext;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;
		default:
			/* FATAL */
			break;
		}
		/* get the box size */
		
		lb_p->box_size.x = 
			(lb_p->bar.r - lb_p->bar.l) / lb_p->box_count;
		lb_p->box_size.y = lb_p->bar.t - lb_p->bar.b;
	}

	else { /* NhlVERTICAL */

		bar_ext = adj_perim_width * lb_p->box_minor_ext;

		switch (title_loc) {

		case NO_TITLE:
			lb_p->bar.b = lb_p->adj_perim.b;
			lb_p->bar.t = lb_p->adj_perim.t;
			lb_p->labels.b = lb_p->adj_perim.b;
			lb_p->labels.t = lb_p->adj_perim.t;
					
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = lb_p->adj_perim.l +
					(adj_perim_width - bar_ext) / 2.0;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
				lb_p->labels.r = lb_p->adj_perim.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.r - bar_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->labels.l = lb_p->adj_perim.l;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;

		case MAJOR_AXIS:
			if (lb_p->title_ext > 0.5) {
				/* need a WARNING */
				lb_p->title_ext = 0.5;
			}
			title_ext = adj_perim_height * lb_p->title_ext;
			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.b + title_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->title.b = lb_p->adj_perim.b;
				lb_p->title.t = lb_p->bar.b;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->adj_perim.t - title_ext;
				lb_p->title.b = lb_p->bar.t;
				lb_p->title.t = lb_p->adj_perim.t;
			}
			lb_p->labels.b = lb_p->bar.b;
			lb_p->labels.t = lb_p->bar.t;

			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = lb_p->adj_perim.l +
					(adj_perim_width - bar_ext) / 2.0;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
				lb_p->labels.r = lb_p->adj_perim.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.r - bar_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->labels.l = lb_p->adj_perim.l;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;

		case MINOR_AXIS:
			if (lb_p->title_ext > 0.5) {
				/* need a WARNING */
				lb_p->title_ext = 0.5;
			}
			if (lb_p->title_ext + lb_p->box_minor_ext > 1.0) {
				/* need a WARNING */
				lb_p->box_minor_ext = 
					1.0 - lb_p->title_ext;
				bar_ext = adj_perim_width * 
					lb_p->box_minor_ext;
			}
			title_ext = adj_perim_width * lb_p->title_ext;

			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
			lb_p->bar.b = lb_p->adj_perim.b;
			lb_p->bar.t = lb_p->adj_perim.t;
			lb_p->labels.b = lb_p->adj_perim.b;
			lb_p->labels.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->title.l = lb_p->adj_perim.l;
				lb_p->title.r = lb_p->adj_perim.l + 
					title_ext;
				lb_p->bar.l = lb_p->title.r;
				lb_p->bar.r = lb_p->adj_perim.r;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->adj_perim.r - title_ext;
				lb_p->title.l = lb_p->bar.r;
				lb_p->title.r = lb_p->adj_perim.r;
			}

			
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = (lb_p->bar.r - lb_p->bar.l - 
					       bar_ext) / 2.0 + lb_p->bar.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->labels.r = lb_p->bar.r;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->labels.l = lb_p->bar.l;
				lb_p->bar.l = lb_p->bar.r - bar_ext;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;
		default:
			/* FATAL */
			break;
		}
		lb_p->box_size.x = lb_p->bar.r - lb_p->bar.l;
		lb_p->box_size.y = 
			(lb_p->bar.t - lb_p->bar.b) / lb_p->box_count;
	}
				
}

/*
 * Function:	LabelBarDraw
 *
 * Description:  Activates parent workstation, calls plotchar parameter 
 *		setting functions and then GKS attributes for linewidth, 
 *		fill styles, fill colors and line colors.
 *
 * In Args: the instance to be drawn
 *
 * Out Args: NONE
 *
 * Return Values: Error conditions
 *
 * Side Effects: GKS and plotchar state affected. 
 *		Does do a get_set before makeing internal set.
 */
static NhlErrorTypes    LabelBarDraw
#if  __STDC__
(Layer layer)
#else
(layer)
	Layer 	layer;
#endif
{
	LabelBarLayer tlayer = (LabelBarLayer) layer;
	LabelBarLayerPart *lb_p = &(tlayer->labelbar);
	NhlErrorTypes ret = NOERROR;

	float fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;
	int lbab;

	Gfill_int_style save_fillstyle;
	int save_txfont;
	Gint save_linecolor;
	Gint save_linetype;
	Gdouble save_linewidth;
	Gint err_ind;
	Gpoint points[5];
	Gpoint_list box;
	float xpoints[5];
	float ypoints[5];
	float frac_width, frac_height;
	int i;
	float rwrk[6];
	int iwrk[8];

	if (! lb_p->labelbar_on)
		return(ret);

	lb_p->fill_line_color = 1;

	_NhlActivateWorkstation(tlayer->base.wkptr);

#if 0	
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	c_lbseti("CBL - box line color", 
		 _NhlGetGksCi(tlayer->base.wkptr,lb_p->box_line_color));
	c_lbseti("CFL - fill line color", 
		 _NhlGetGksCi(tlayer->base.wkptr,lb_p->fill_line_color));

	c_lbsetr("WBL - box line width", lb_p->box_line_thickness);
	c_lbsetr("WFL - fill line width", lb_p->fill_line_thickness);
	c_sfseti("type of fill", 0);
	gset_fill_int_style(1);

	if (lb_p->orient == NhlVERTICAL) {
		frac_width = lb_p->box_minor_ext;
		frac_height = lb_p->box_major_ext;
	}
	else {
		frac_width = lb_p->box_major_ext;
		frac_height = lb_p->box_minor_ext;
	}
	frac_width = 1.0;
	frac_height = 1.0;
#endif

/*
 * Set the values that remain constant for all boxes
 */

	NhlSetValues(tlayer->base.wkptr->base.id,
		     NhlNwkDrawEdges, lb_p->box_line_on,
		     NhlNwkEdgeDashPattern, lb_p->box_line_dash_pattern,
		     NhlNwkEdgeThicknessF, lb_p->box_line_thickness,
		     NhlNwkEdgeDashSegLenF, lb_p->box_line_dash_length,
		     NhlNwkEdgeColor, lb_p->box_line_color,
		     NhlNwkFillLineThickness, lb_p->fill_line_thickness,
		     NULL);
				     
/* 
 * Draw the boxes
 */
	if (lb_p->orient == NhlHORIZONTAL) {

		ypoints[0] = lb_p->adj_bar.b;
		ypoints[1] = lb_p->adj_bar.b;
		ypoints[2] = lb_p->adj_bar.t;
		ypoints[3] = lb_p->adj_bar.t;
		ypoints[4] = lb_p->adj_bar.b;
		for (i=0; i<lb_p->box_count; i++) {
			xpoints[0] = lb_p->box_locs[i];
			xpoints[1] = lb_p->box_locs[i+1];
			xpoints[2] = lb_p->box_locs[i+1];
			xpoints[3] = lb_p->box_locs[i];
			xpoints[4] = lb_p->box_locs[i];
			
			NhlSetValues(tlayer->base.wkptr->base.id,
				     NhlNwkFillIndex, lb_p->fill_patterns[i],
				     NhlNwkFillColor, lb_p->fill_line_color,
				     NULL);
			
			_NhlSetFillInfo(tlayer->base.wkptr, layer);
			_NhlWorkstationFill(tlayer->base.wkptr,
					    xpoints,ypoints,5);
			
		}
	}
	else {
		xpoints[0] = lb_p->adj_bar.l;
		xpoints[1] = lb_p->adj_bar.r;
		xpoints[2] = lb_p->adj_bar.r;
		xpoints[3] = lb_p->adj_bar.l;
		xpoints[4] = lb_p->adj_bar.l;
		for (i=0; i< lb_p->box_count; i++) {
			ypoints[0] = lb_p->box_locs[i];
			ypoints[1] = lb_p->box_locs[i];
			ypoints[2] = lb_p->box_locs[i+1];
			ypoints[3] = lb_p->box_locs[i+1];
			ypoints[4] = lb_p->box_locs[i];

			NhlSetValues(tlayer->base.wkptr->base.id,
				     NhlNwkFillIndex, lb_p->fill_patterns[i],
				     NhlNwkFillColor, lb_p->fill_line_color,
				     NULL);
			
			_NhlSetFillInfo(tlayer->base.wkptr, layer);
			_NhlWorkstationFill(tlayer->base.wkptr,
					    xpoints,ypoints,5);
			
		}
	}

	if (lb_p->perim_on) {
#if 0		
		xpoints[0] = lb_p->lb_x;
		ypoints[0] = lb_p->lb_y - lb_p->lb_height;
		xpoints[1] = lb_p->lb_x + lb_p->lb_width;
		ypoints[1] = lb_p->lb_y - lb_p->lb_height;
		xpoints[2] = lb_p->lb_x + lb_p->lb_width;
		ypoints[2] = lb_p->lb_y;
		xpoints[3] = lb_p->lb_x;
		ypoints[3] = lb_p->lb_y;
		xpoints[4] = lb_p->lb_x;
		ypoints[4] = lb_p->lb_y - lb_p->lb_height;
#endif

		xpoints[0] = lb_p->expand_perim.l;
		ypoints[0] = lb_p->expand_perim.b;
		xpoints[1] = lb_p->expand_perim.r;;
		ypoints[1] = lb_p->expand_perim.b;
		xpoints[2] = lb_p->expand_perim.r;
		ypoints[2] = lb_p->expand_perim.t;
		xpoints[3] = lb_p->expand_perim.l;
		ypoints[3] = lb_p->expand_perim.t;
		xpoints[4] = lb_p->expand_perim.l;
		ypoints[4] = lb_p->expand_perim.b;


		NhlSetValues(tlayer->base.wkptr->base.id,
			     NhlNwkDrawEdges, 1,
			     NhlNwkEdgeDashPattern, lb_p->perim_dash_pattern,
			     NhlNwkEdgeThicknessF, lb_p->perim_thickness,
			     NhlNwkEdgeDashSegLenF, lb_p->perim_dash_length,
			     NhlNwkEdgeColor, lb_p->perim_color,
			     NhlNwkFillIndex, NhlHOLLOWFILL,
			     NULL);
			
		_NhlSetFillInfo(tlayer->base.wkptr, layer);
		_NhlWorkstationFill(tlayer->base.wkptr,
				    xpoints,ypoints,5);
			
	}

#if 0	
/* for debugging */

	xpoints[0] = lb_p->bar.l;
	ypoints[0] = lb_p->bar.b;
	xpoints[1] = lb_p->bar.r;
	ypoints[1] = lb_p->bar.b;
	xpoints[2] = lb_p->bar.r;
	ypoints[2] = lb_p->bar.t;
	xpoints[3] = lb_p->bar.l;
	ypoints[3] = lb_p->bar.t;
	xpoints[4] = lb_p->bar.l;
	ypoints[4] = lb_p->bar.b;


#endif
	_NhlDeactivateWorkstation(tlayer->base.wkptr);

/*
 * Draw the child objects
 */
	if (lb_p->title_on)
		NhlDraw(lb_p->title_id);

	if (lb_p->labels_on)
		NhlDraw(lb_p->labels_id);

	return(ret);
}

/*
 * Function:	LabelBarClassInitialize
 *
 * Description: Just calls StringToQuark to register new types
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    LabelBarClassInitialize
#if  __STDC__
(void)
#else
()
#endif
{
	_NhlInitializeLayerClass(textItemLayerClass);
	_NhlInitializeLayerClass(multiTextLayerClass);
	return(NOERROR);	
}


/*
 * Function:	LabelBarDestroy
 *
 * Description: Frees all dynamically allocated memory
 *
 * In Args:	Layer inst	instance of LabelBar
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes    LabelBarDestroy
#if  __STDC__
(Layer  inst)
#else
(inst)
	Layer	inst;
#endif
{
	LabelBarLayer tinst = (LabelBarLayer) inst;
	LabelBarLayerPart *lb_p = &(tinst->labelbar);
	int i;
	
	NhlFree(lb_p->colors);
	NhlFree(lb_p->gks_colors);
	NhlFree(lb_p->fill_patterns);
	NhlFree(lb_p->values);
	for (i=0;i<NhlLB_MAX_LBL_STRINGS;i++) {
		if (lb_p->label_strings[i] != NULL) {
			NhlFree(lb_p->label_strings[i]);
		}
	}
	if (lb_p->box_fractions != NULL)
		NhlFree(lb_p->box_fractions);
	if (lb_p->stride_labels != NULL)
		NhlFree(lb_p->stride_labels);
	if (lb_p->label_locs != NULL)
		NhlFree(lb_p->label_locs);
	if (lb_p->box_locs != NULL)
		NhlFree(lb_p->box_locs);
			

	if (lb_p->labels_id >=0)
		NhlDestroy(lb_p->labels_id);

	if (lb_p->title_string != NULL) {
		NhlFree(lb_p->title_string);
		NhlDestroy(lb_p->title_id);
	}

	return(NOERROR);
}

