/*
 *      $Id: CnStdRenderer.c,v 1.15.4.1 2010-03-17 20:47:07 brownrig Exp $
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
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 23 12:01:04 MDT 1998
 *
 *	Description:	
 */
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/CnStdRendererP.h>
#include <ncarg/hlu/WorkstationI.h>

static NhlErrorTypes CnStdRendererInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes CnStdRender(
#if     NhlNeedProto
        NhlLayer                instance,
        NhlContourPlotLayer     cnl,
	NhlDrawOrder            order,
	NhlString		entry_name
#endif
);


extern int (_NHLCALLF(cpdrpl,CPDRPL))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs,
	int *ncs,
	int *iai,
	int *iag,
	int *nai
#endif
);

extern int (_NHLCALLF(hlucpfill,HLUCPFILL))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
#endif
);

extern void  (_NHLCALLF(hlucpscae,HLUCPSCAE))(
#if	NhlNeedProto
	int		*icra,
	int		*ica1,
	int		*icam,
	int		*ican,
	float		*xcpf,
	float		*ycpf,
	float		*xcqf,
	float		*ycqf,
	int		*ind1,
	int		*ind2,
	int		*icaf,
	int		*iaid		      
#endif
);

extern void   (_NHLCALLF(hlucpchcl,HLUCPCHCL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hlucpchhl,HLUCPCHHL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hlucpchll,HLUCPCHLL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hlucpmpxy,HLUCPMPXY))(
#if	NhlNeedProto
	int	*imap,
	float	*xinp,
	float	*yinp,
	float	*xotp,
	float	*yotp
#endif
);

static void   load_hlucp_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);

extern void (_NHLCALLF(cpscae,CPSCAE))(
	int		*icra,
	int		*ica1,
	int		*icam,
	int		*ican,
	float		*xcpf,
	float		*ycpf,
	float		*xcqf,
	float		*ycqf,
	int		*ind1,
	int		*ind2,
	int		*icaf,
	int		*iaid
);

extern void (_NHLCALLF(cpchcl,CPCHCL))(
	int *iflg
);

extern void (_NHLCALLF(dprset,DPRSET))(
	void
);

extern void (_NHLCALLF(cpchhl,CPCHHL))(
	int *iflg
);

extern void (_NHLCALLF(cpchll,CPCHLL))(
	int *iflg
);

extern void (_NHLCALLF(cpmpxy,CPMPXY))(
	int *imap,
	float *xinp,
	float *yinp,
	float *xotp,
	float *yotp
);

NhlCnStdRendererClassRec NhlcnStdRendererClassRec = {
	{
/* class_name 		*/      "cnStdRendererClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlCnStdRendererLayerRec),
/* class_inited 	*/	False,
/* superclass		*/      (NhlClass)&NhlobjClassRec,
/* cvt_table		*/	NULL,

/* layer_resources 	*/   	NULL,
/* num_resources 	*/     	0,
/* all_resources 	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize */     NULL,
/* class_initialize 	*/  	NULL,
/* layer_initialize 	*/  	CnStdRendererInitialize,
/* layer_set_values 	*/  	NULL,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	NULL,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	NULL,
	},
	{
/* render */		        CnStdRender
	},
	{
/* foo */		        0
	}
};

NhlClass NhlcnStdRendererClass = (NhlClass)&NhlcnStdRendererClassRec;

typedef enum { 
	cnInt,
	cnFloat,
	cnString 
} _cnParamType;

typedef struct _cnCp_Params {
	NhlString	name;
	_cnParamType	type;
} cnCp_Params;

static cnCp_Params Cp_Params[] = {
{"HCL", cnFloat},
{"HCS", cnFloat}, 
{"HCF", cnInt}, 
{"HLX", cnInt}, 
{"HLY", cnInt}, 
{"IWM", cnInt}, 
{"PC1", cnFloat},
{"PC2", cnFloat}, 
{"PC3", cnFloat}, 
{"PC4", cnFloat}, 
{"PC5", cnFloat}, 
{"PC6", cnFloat},
{"PIC", cnInt}, 
{"PIE", cnInt},
{"PW1", cnFloat}, 
{"PW2", cnFloat}, 
{"PW3", cnFloat},
{"PW4", cnFloat}, 
{"RC1", cnFloat}, 
{"RC2", cnFloat}, 
{"RC3", cnFloat}, 
{"RWC", cnInt}, 
{"RWG", cnInt}, 
{"RWM", cnInt},
};

static float LowLabelFactor = 1.0;
#define NhlDASHBUFSIZE	128

static NhlContourPlotLayer	Cnl = NULL;
static NhlContourPlotLayerPart	*Cnp = NULL;

/*
 * Function:	CnStdRendererInitialize
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
CnStdRendererInitialize
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

	load_hlucp_routines(False);
	

        return ret;
}


/*
 * Function:	SetCpParams
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes SetCpParams
#if	NhlNeedProto
(NhlContourPlotLayer cnl,NhlString entry_name)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	NhlString		*sp;
	int			i,j;
	char			param[4];
	float			value;

	if (cnp->conpack_params == NULL)
		return NhlNOERROR;

	sp = (NhlString *) cnp->conpack_params->data;

	for (i = 0; i < cnp->conpack_params->num_elements; i++) {
		NhlBoolean matched = False;
		_cnParamType type;
		if (sp[i] != NULL && sp[i][0] != '\0') {
			value = 0.0;
			sscanf(sp[i],"%3s:%f",&param[0],&value);
			for (j = 0; j < NhlNumber(Cp_Params); j ++) {
				if (! strcmp(Cp_Params[j].name,param)) {
					matched = True;
					type = Cp_Params[j].type;
					break;
				}
			}
			if (matched && type == cnInt) {
				c_cpseti(param,(int) value);
			}
			else if (matched && type == cnFloat) {
				c_cpsetr(param,value);
			}
			else {
				char * e_text = 
              "%s: %s is invalid Conpack param or cannot be from HLU library";
				NhlPError(NhlWARNING,
					  NhlEUNKNOWN,e_text,entry_name,param);
				ret = MIN(ret,NhlWARNING);
			}
		}
	}
	return ret;
}

/*
 * Function:	SetRegionAttrs
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Updates various internal parameters in Conpack,Plotchar,
 *		 etc.
 *		 
 */	

static void SetRegionAttrs
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlcnRegionAttrs *reg_attrs, 
	int cpix
)
#else
(cl,reg_attrs,cpix)
	NhlContourPlotLayer	cl;
	NhlcnRegionAttrs *reg_attrs;
	int		 cpix;
#endif
{
	reg_attrs->gks_pcolor = reg_attrs->perim_color == NhlTRANSPARENT ?
                NhlTRANSPARENT : _NhlGetGksCi(cl->base.wkptr,
                                              reg_attrs->perim_color);
	reg_attrs->gks_fcolor = reg_attrs->fill_color  == NhlTRANSPARENT ?
		NhlTRANSPARENT : _NhlGetGksCi(cl->base.wkptr,
                                              reg_attrs->fill_color);
	
	c_cpseti("PAI",cpix);
	if (! reg_attrs->perim_on)
		c_cpseti("CLU",0);
	else if (cpix == -2 && cl->contourplot.missing_val_perim_grid_bound_on)
		c_cpseti("CLU",2);
	else
		c_cpseti("CLU",1);

	if (cpix == -1)
		c_cpseti("AIA",99);
	else if (cpix == -2)
		c_cpseti("AIA",98);
	else if (cpix == -3)
		c_cpseti("AIA",97);
	else
		c_cpseti("AIA",-1);

	return;

}

/*
 * Function:	UpdateLineAndLabelParams
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: Updates various internal parameters in Conpack,Plotchar,
 *		 etc.
 *		 
 */	

static NhlErrorTypes UpdateLineAndLabelParams
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlBoolean	*do_lines,
	NhlBoolean	*do_labels
)
#else
(cl,do_lines,do_labels)
        NhlContourPlotLayer	cl;
	NhlBoolean	*do_lines;
	NhlBoolean	*do_labels;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cl->contourplot);
	float			*clvp;
	int			*clup;
	int			i,j;
	float			height;

	cnp->line_lbls.text = (NhlString *) cnp->llabel_strings->data;
	if (cnp->line_lbls.mono_color) {
                if (cnp->line_lbls.color == NhlTRANSPARENT)
                        cnp->line_lbls.gks_color =  NhlTRANSPARENT;
                else
                        cnp->line_lbls.gks_color =
                                 _NhlGetGksCi(cl->base.wkptr,
                                                     cnp->line_lbls.color);
        }
	else
		cnp->line_lbls.colors =  cnp->gks_llabel_colors;
        if (cnp->line_lbls.back_color == NhlTRANSPARENT)
                cnp->line_lbls.gks_bcolor = NhlTRANSPARENT;
        else
                cnp->line_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->line_lbls.back_color);
	/* 
	 * If the perim color is transparent the line will not be
	 * drawn, but just in case, set the gks color to the foreground
	 */
        if (cnp->line_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->line_lbls.gks_plcolor = 
                        _NhlGetGksCi(cl->base.wkptr,NhlFOREGROUND);
        else
                cnp->line_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->line_lbls.perim_lcolor);

        if (cnp->high_lbls.color == NhlTRANSPARENT)
                cnp->high_lbls.gks_color =  NhlTRANSPARENT;
        else
                cnp->high_lbls.gks_color =
                         _NhlGetGksCi(cl->base.wkptr,
                                             cnp->high_lbls.color);
        if (cnp->high_lbls.back_color == NhlTRANSPARENT)
                cnp->high_lbls.gks_bcolor = NhlTRANSPARENT;
        else
                cnp->high_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->high_lbls.back_color);
        if (cnp->high_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->high_lbls.gks_plcolor = 
                        _NhlGetGksCi(cl->base.wkptr,NhlFOREGROUND);
        else
                cnp->high_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->high_lbls.perim_lcolor);


        if (cnp->low_lbls.color == NhlTRANSPARENT)
                cnp->low_lbls.gks_color =  NhlTRANSPARENT;
        else
                cnp->low_lbls.gks_color =
                         _NhlGetGksCi(cl->base.wkptr,
                                             cnp->low_lbls.color);
/* 
 * the low label background can be transparent iff (and only if) the high
 * label background is transparent, but otherwise the color can be 
 * different. If the low label background is set transparent when
 * the high label is not transparent, default to the background color.
 */
        if (cnp->low_lbls.back_color == NhlTRANSPARENT)
                cnp->low_lbls.gks_bcolor =  
			_NhlGetGksCi(cl->base.wkptr,NhlBACKGROUND);
        else
                cnp->low_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->low_lbls.back_color);
        if (cnp->low_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->low_lbls.gks_plcolor = 
                        _NhlGetGksCi(cl->base.wkptr,NhlFOREGROUND);
        else
                cnp->low_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->low_lbls.perim_lcolor);

	SetRegionAttrs(cl,&cnp->grid_bound,-1);
	SetRegionAttrs(cl,&cnp->missing_val,-2);
	SetRegionAttrs(cl,&cnp->out_of_range,-3);

	*do_lines = True;
	*do_labels = False;

	gset_line_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_text_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_linewidth(1.0);

	c_cpseti("CLS",0);		/* Conpack not to select levels */
	c_cpseti("NCL",cnp->level_count); 
	clvp = (float *) cnp->levels->data;
	clup = (int *) cnp->level_flags->data;
	c_cpseti("DPU",-1); /* dash pattern use flag */

	if (cnp->mono_line_color) {
		cnp->gks_line_colors[0] = cnp->line_color == NhlTRANSPARENT ?
			NhlTRANSPARENT :
			_NhlGetGksCi(cl->base.wkptr,cnp->line_color);
	} else {
		cnp->gks_line_colors[0] =
			((int *)cnp->line_colors->data)[0] == NhlTRANSPARENT ?
			NhlTRANSPARENT :
			_NhlGetGksCi(cl->base.wkptr,
				     ((int *)cnp->line_colors->data)[0]);
	}
	if (cnp->mono_line_color && cnp->gks_line_colors[0] == NhlTRANSPARENT)
		*do_lines = False;
	if (! cnp->lines_on)
		*do_lines = False;

	for (i=0; i<cnp->level_count; i++) {
		int pai,aia,aib;
		NhlcnLevelUseMode flag;
		NhlBoolean blank = True;
		char *cp;

		pai = i+1;
		aib = NhlcnAREAID_OFFSET+i;
		aia = NhlcnAREAID_OFFSET+i+1;
		c_cpseti("PAI",pai);
		c_cpsetr("CLV",(float)clvp[i]);
		c_cpseti("AIB",aib);
		c_cpseti("AIA",aia);

		flag = cnp->mono_level_flag ? 
			cnp->level_flag : (NhlcnLevelUseMode) clup[i];

		if (! *do_lines) {
			switch (flag) {
			case NhlNOLINE:
			case NhlLINEONLY:
			default:
				flag = NhlNOLINE;
				break;
			case NhlLABELONLY:
			case NhlLINEANDLABEL:
				flag = NhlLABELONLY;
				break;
			}
		}

#if 0
		printf("pai %d,clv %f,aib %d,aia %d\n",pai,clvp[i],aib,aia);
#endif
		cp = ((NhlString*)cnp->line_lbls.text)[i];
		if (cp) {
			for (j = 0; j < strlen(cp); j++) {
				if (! isgraph(cp[j]))
					continue;
				blank = False;
			}
		}
		if (blank) {
			switch (flag) {
			case NhlNOLINE:
			case NhlLABELONLY:
			default:
				flag = NhlNOLINE;
				break;
			case NhlLINEONLY:
			case NhlLINEANDLABEL:
				flag = NhlLINEONLY;
				break;
			}
		}
		c_cpseti("CLU",flag);
		c_cpsetc("LLT",cp);
	}
	if (cnp->level_selection_mode != NhlEXPLICITLEVELS)
		c_cpsetr("CIU",(float)cnp->level_spacing);
 
/* Set up for labels */

/* Conpack not to render the Informational label */
	c_cpsetc("ILT"," ");

/* Line labels */
	if (! cnp->line_lbls.on) {
		c_cpseti("LLP",0); 
	}
	else if (cnp->llabel_placement == NhlCONSTANT) {
		*do_labels = True;
		c_cpseti("LLP",1);
#if 0
		c_cpsetr("DPS",
			 (float)(cnp->line_lbls.real_height / cl->view.width));
		c_cpsetr("DPV",(float).015);
#endif
#if 0
		c_cpsetr("RC3",(float)0.0);
		c_cpseti("LLP",2);
		if (cnp->line_lbls.angle < 0.0) 
			c_cpseti("LLO",1); /* angle to contour direction */
		else {
			c_cpseti("LLO",0); /* fixed angle  */
			c_cpsetr("LLA",(float)cnp->line_lbls.angle);
		}
#endif

	}
	else if (cnp->llabel_placement == NhlRANDOMIZED) {
		*do_labels = True;
		c_cpseti("LLP",2);
		if (cnp->line_lbls.angle < 0.0) 
			c_cpseti("LLO",1); /* angle to contour direction */
		else {
			c_cpseti("LLO",0); /* fixed angle  */
			c_cpsetr("LLA",(float)cnp->line_lbls.angle);
		}
		if (cnp->llabel_density > 0.0) {
			float rc1 = 0.25 / cnp->llabel_density;
			float rc2 = 0.25 / cnp->llabel_density;
			float rc3 = 0.05 / cnp->llabel_density;	
			c_cpsetr("RC1",rc1);
			c_cpsetr("RC2",rc2);
			c_cpsetr("RC3",rc3);
		}
	}
	else {
		*do_labels = True;
		c_cpseti("LLP",3);
		if (cnp->line_lbls.angle < 0.0) 
			c_cpseti("LLO",1); /* angle to contour direction */
		else {
			c_cpseti("LLO",0); /* fixed angle  */
			c_cpsetr("LLA",(float)cnp->line_lbls.angle);
		}
		if (cnp->llabel_density > 0.0) {
			float pc1 = 1.0;
			float pc2 = 5.0;
			float pc3 = 60.0;
                        float pc4 = 0.05;
			float pc5 = 0.15;
                        float pc6 = 0.30;
			float pw1 = 2.0;
                        float pw2 = 0.0;
                        float pw3 = 1.0;
                        float pw4 = 1.0;

			pc6 /= cnp->llabel_density;
			pc3 = pc3 + 30 * (cnp->llabel_density - 1);
			pc1 *= cnp->llabel_density;
			pc5 *= cnp->llabel_density;

			c_cpsetr("PC1",pc1);
			c_cpsetr("PC2",pc2);
			c_cpsetr("PC3",pc3);
			c_cpsetr("PC4",pc4);
			c_cpsetr("PC5",pc5);
			c_cpsetr("PC6",pc6);
			c_cpsetr("PW1",pw1);
			c_cpsetr("PW2",pw2);
			c_cpsetr("PW3",pw3);
			c_cpsetr("PW4",pw4);
		}
	}

	if (*do_labels) {
		height = cnp->line_lbls.real_height / cl->view.width;
		c_cpsetr("LLS",(float)height);
		c_cpsetr("LLW", 
			 (float) (height * cnp->line_lbls.perim_space));
		if (cnp->line_lbls.back_color == NhlTRANSPARENT) {
			if (cnp->line_lbls.perim_lcolor == NhlTRANSPARENT ||
			    ! cnp->line_lbls.perim_on) 
				c_cpseti("LLB",0); 
			else
				c_cpseti("LLB",1);
		}
		else {
			c_cpseti("LBC",cnp->line_lbls.back_color);
			if (cnp->line_lbls.perim_lcolor == NhlTRANSPARENT ||
			    ! cnp->line_lbls.perim_on) 
				c_cpseti("LLB",2);
			else
				c_cpseti("LLB",3);
		}
	}

/*
 * In order to allow user control of the high and low attributes 
 * individually set the appropriate part of the flag on if either 
 * the high or the low is on. Further distinguishing between high and low
 * occurs in the low level routine cpchhl_
 */
	if (! cnp->high_lbls.on)
		c_cpsetc("HIT"," ");
	else 
		c_cpsetc("HIT",(NhlString)cnp->high_lbls.text);

	if (! cnp->low_lbls.on)
		c_cpsetc("LOT"," ");
	else
		c_cpsetc("LOT",(NhlString)cnp->low_lbls.text);

/*
 * Due to the way Conpack works it is not possible to have different text
 * sizes, white space, background and perim on/off settings for the high
 * and low labels. The high labels take precedence, so set up accordingly.
 * Background and perim can have different colors, except that if the
 * high background or perim is transparent (emulated by turning these
 * features off) then the corresponding low feature must also become 
 * transparent. 
 * This means that 
 * cnLowLabelFontHeightF
 * cnLowLabelAngleF
 * cnLowLabelPerimSpacingF
 * cnLowLabelPerimOn
 * are always ignored.
 * cnLowLabelBackgroundColor and cnLowLabelPerimColor can be set independently
 * of the corresponding HighLabel resource if that resource is not set to 
 * transparent. However, if the low label resource is set to transparent in
 * this case, it will be coerced to transparent.
 * 
 * It could be possible to set the low label font height independently of
 * the high label font height, but it will require a more sophisticated
 * method than the LowLabelFactor which is commented out below. 
 */

	if (cnp->high_lbls.on || cnp->low_lbls.on) {
		*do_labels = True;
		height = cnp->high_lbls.real_height / cl->view.width;
#if 0
		LowLabelFactor =  cnp->high_lbls.real_height /
			cnp->low_lbls.real_height;
#endif
		c_cpsetr("HLS",(float)height);
		c_cpsetr("HLW",(float)(cnp->high_lbls.perim_space  * height));
		c_cpsetr("HLA",(float)cnp->high_lbls.angle);
		c_cpseti("HLO", (int) cnp->high_low_overlap);

		if (cnp->high_lbls.back_color == NhlTRANSPARENT) {
			if (cnp->high_lbls.perim_lcolor == NhlTRANSPARENT ||
			    ! cnp->high_lbls.perim_on) 
				c_cpseti("HLB",0); 
			else
				c_cpseti("HLB",1);
		}
		else {
			if (cnp->high_lbls.perim_lcolor == NhlTRANSPARENT ||
			    ! cnp->high_lbls.perim_on)
				c_cpseti("HLB",2);
			else
				c_cpseti("HLB",3);
		}
	}

	c_pcsetc("FC",":");
	return ret;

}

/*
 * Function:	UpdateFillInfo
 *
 * Description:	
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: sets the do_fill Boolean flag depending on whether 
 *		 fill is to be done.
 *		 
 */	

static NhlErrorTypes UpdateFillInfo
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlBoolean	*do_fill
)
#else
(cl,do_fill)
        NhlContourPlotLayer	cl;
	NhlBoolean	*do_fill;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cl->contourplot);
#if 0
 	NhlBoolean	color_fill, pattern_fill;

	color_fill = (cnp->mono_fill_color && 
		      cnp->fill_color == NhlTRANSPARENT) ? False : True;
	pattern_fill = (cnp->mono_fill_pattern && 
			cnp->fill_pattern == NhlHOLLOWFILL) ? False : True;

	if (color_fill &&  pattern_fill &&  
	    (cnp->fill_on || cnp->raster_mode_on)) {
	}
#endif 
/*
 * Since the missing value fill resources are not supposed to be affected
 * by the mono flags, you cannot optimize the fill away if mono fill color is
 * true and fill color is transparent or mono fill pattern is true and the
 * fill pattern is hollow. So just keep it simple.
 * 
 */
	if (cnp->fill_on) {
		*do_fill = True;
		return ret;
	}

	*do_fill = False;
	return ret;
}


/*
 * Function:	ContourAbortDraw
 *
 * Description:	cleans up if a fatal error occurs while drawing
 *
 * In Args:	layer	ContourPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static void ContourAbortDraw
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl
)
#else
(cnl)
	NhlContourPlotLayer	cnl;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	NhlTransformLayerPart	*tfp = &(cnl->trans);
	char *e_text;

	Cnp = NULL;
	Cnl = NULL;

	if (cnp->aws != NULL) {
		_NhlIdleWorkspace(cnp->aws);
		cnp->aws = NULL;
	}
	if (cnp->cws != NULL) {
		_NhlIdleWorkspace(cnp->cws);
		cnp->cws = NULL;
	}
	if (cnp->fws != NULL) {
		_NhlIdleWorkspace(cnp->fws);
		cnp->fws = NULL;
	}
	if (cnp->iws != NULL) {
		_NhlIdleWorkspace(cnp->iws);
		cnp->iws = NULL;
	}

	if (cnl->view.use_segments && cnp->current_trans_dat) {
		_NhlEndSegment(cnp->current_trans_dat);
		cnp->current_trans_dat = NULL;
	}

	if (cnp->wk_active) {
		_NhlDeactivateWorkstation(cnl->base.wkptr);
		cnp->wk_active = False;
	}

	if (cnp->low_level_log_on) {
		NhlVASetValues(tfp->trans_obj->base.id,
			       NhlNtrLowLevelLogOn,False,NULL);
                cnp->low_level_log_on = False;
        }

	e_text = "%s: draw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,"ContourPlotDraw");
}

/*
 * Function:	AddDataBoundToAreamap
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: 
 *		 
 */	

static NhlErrorTypes AddDataBoundToAreamap
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlString	entry_name
)
#else
(cl,entry_name)
	NhlContourPlotLayer	cl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = 
		(NhlContourPlotLayerPart *) &cl->contourplot;
	int			i;
	int			status;
	NhlBoolean		ezmap = False;
	int			xrev,yrev;
	float			xa[5],ya[5];

#define _cnBBOXGID 3
#if 0
#define _cnMAPBOUNDINC	3700
#endif
#define _cnMAPBOUNDINC	100

	if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlmapTransObjClass->base_class.class_name) {
		ezmap = True;
	}


#if 0
	gset_linewidth(4.0);
	gset_line_colr_ind(30);
	c_arseti("RC(1)",1);
	c_arseti("RC(3)",2);
#endif
	c_arseti("RC",1);
	if (! ezmap) {
		float twlx,twrx,twby,twuy;
		float gwlx,gwrx,gwby,gwuy;
		float txmin,txmax,tymin,tymax;
		float gxmin,gxmax,gymin,gymax;
		NhlBoolean lbox, rbox, bbox, tbox;
		float	   xeps,yeps;

		ret = NhlVAGetValues(cnp->trans_obj->base.id,
				     NhlNtrXMinF,&txmin,
				     NhlNtrXMaxF,&txmax,
				     NhlNtrYMinF,&tymin,
				     NhlNtrYMaxF,&tymax,
				     NULL);

		_NhlDataToWin(cnp->trans_obj,&txmin,&tymin,
			      1,&twlx,&twby,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		_NhlDataToWin(cnp->trans_obj,&txmax,&tymax,
			      1,&twrx,&twuy,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		gxmin = MAX(txmin,cnp->xlb);
		gxmax = MIN(txmax,cnp->xub);
		gymin = MAX(tymin,cnp->ylb);
		gymax = MIN(tymax,cnp->yub);

		_NhlDataToWin(cnp->trans_obj,&gxmin,&gymin,
			      1,&gwlx,&gwby,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		_NhlDataToWin(cnp->trans_obj,&gxmax,&gymax,
			      1,&gwrx,&gwuy,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		xrev = twlx > twrx;
		yrev = twby > twuy;
/*
 * added a hack to prevent fill dropout in certain cases, where because
 * of floating point precision issues in the mapping routines, contour
 * lines were being removed because they didn't quite touch the viewport
 * edge. Now a very thin rectangle is drawn just to the inside of each
 * viewport edge in this situation.
 */
		xeps = 1e-5 * fabs(twrx-twlx);
		yeps = 1e-5 * fabs(twuy-twby);

		if (! xrev) {
			if (gwlx >= twlx && gwlx - twlx < xeps)
				gwlx = twlx + xeps;
			if (gwrx <= twrx && twrx - gwrx < xeps)
				gwrx = twrx - xeps;
			lbox = gwlx > twlx;
			rbox = gwrx < twrx;
		}
		else {
			if (gwrx >= twrx && gwrx - twrx < xeps)
				gwrx = twrx + xeps;
			if (gwlx <= twlx && twlx - gwlx < xeps)
				gwlx = twlx - xeps;
			lbox = gwlx < twlx;
			rbox = gwrx > twrx;
		}
		if (! yrev) {
			if (gwby >= twby && gwby - twby < xeps)
				gwby = twby + yeps;
			if (gwuy <= twuy && twuy - gwuy < yeps)
				gwuy = twuy - yeps;
			bbox = gwby > twby;
			tbox = gwuy < twuy;
		}
		else {
			if (gwuy >= twuy && gwuy - twuy < yeps)
				gwuy = twuy + yeps;
			if (gwby <= twby && twby - gwby < yeps)
				gwby = twby - yeps;
			bbox = gwby > twby;
			tbox = gwuy < twuy;
		}

/*
 * This code from 'added a hack' above to the end of 'if (! ezmap)' below was not working properly
 * for log plots where the lower values get stretched by log scaling. It sometime resulted in 
 * boxes that were big enough to be visible. The solution is to start with NDC values and convert them
 * to data values using the current transformation. That is what the code below does. If it succeeds
 * (st is 0) then the inside coordinates of the skinny boxes are replaced. (Are there situations where
 * it would fail? -- it seems safest to allow for that possibility.
 */
		{
			float xn[4],yn[4];
			float xe,ye;
			int st;
			float oor;
			float x,y,w,h;
			x = cl->view.x;
			y = cl->view.y;
			w = cl->view.width;
			h = cl->view.height;

			xe = 1e-5 * w;
			ye = 1e-5 * h;
			xn[0] = x + xe;
			xn[1] = x + w - xe;
			xn[2] = xn[1];
			xn[3] = xn[0];
			yn[0] = y - h + ye;
			yn[1] = yn[0];
			yn[2] = y - ye;
			yn[3] = yn[2];

			NhlNDCToData(cl->base.id,xn,yn,4,xn,yn,NULL,NULL,&st,&oor);
			if (! st) {
				_NhlDataToWin(cnp->trans_obj,xn,yn,
					      4,xn,yn,&st,NULL,NULL);
			}
			if (! st) {
				gwlx = xn[0];
				gwrx = xn[1];
				gwby = yn[0];
				gwuy = yn[2];
			}
		}

		if (lbox) {
				
			xa[0] = xa[1] = xa[4] = twlx;
			xa[2] = xa[3] = gwlx;
			ya[0] = ya[3] = ya[4] = twuy;
			ya[1] = ya[2] = twby;

			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,0,9999,entry_name);
		}
		if (rbox) {
			xa[0] = xa[1] = xa[4] = gwrx;
			xa[2] = xa[3] = twrx;
			ya[0] = ya[3] = ya[4] = twuy;
			ya[1] = ya[2] = twby;
			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,0,9999,entry_name);
		}
		if (bbox) {
			xa[0] = xa[1] = xa[4] = gwlx;
			xa[2] = xa[3] = gwrx;
			ya[0] = ya[3] = ya[4] = gwby;
			ya[1] = ya[2] = twby;
			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,0,9999,entry_name);
		}
		if (tbox) {
			xa[0] = xa[1] = xa[4] = gwlx;
			xa[2] = xa[3] = gwrx;
			ya[0] = ya[3] = ya[4] = twuy;
			ya[1] = ya[2] = gwuy;
			if (! (xrev || yrev) || (xrev && yrev)) 
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,9999,0,entry_name);
			else
				_NhlAredam(cnp->aws,xa,ya,
					   5,_cnBBOXGID,0,9999,entry_name);
		}
	}
	else {
		NhlBoolean	started = False;
		float		xinc,yinc; 
		int		j;
		char		cval[4];

		if (! cnp->fix_fill_bleed)
			return NhlNOERROR;
		xa[0] = xa[3] = xa[4] = cnp->xlb;
		xa[1] = xa[2] = cnp->xub;
		ya[0] = ya[1] = ya[4] = cnp->ylb;
		ya[2] = ya[3] = cnp->yub;

		for (i=0;  i < 4; i++) {
			xinc = (xa[i+1] - xa[i]) / _cnMAPBOUNDINC;
			yinc = (ya[i+1] - ya[i]) / _cnMAPBOUNDINC;
			if (! started) {
				_NhlMapita(cnp->aws,ya[i],xa[i],
					   0,3,0,-1,entry_name);
				started = True;
			}
			for (j = 0; j < _cnMAPBOUNDINC + 1; j++) {
				_NhlMapita(cnp->aws,ya[i]+j*yinc,xa[i]+j*xinc,
					   1,3,0,-1,entry_name);
			}
		}
		_NhlMapiqa(cnp->aws,3,0,-1,entry_name);

		c_mpgetc("OU",cval,3);
		c_mpsetc("OU","NO");
		c_mpseti("G2",3);
		c_mpseti("VS",1);
		_NhlMapbla(cnp->aws,entry_name);
		c_mpsetc("OU",cval);
	}
	return NhlNOERROR;
}

/*
 * Function:	cnInitAreamap
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnInitAreamap
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
	NhlString	entry_name
)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);

	if (cnp->aws_id < 1) {
		cnp->aws_id = 
			_NhlNewWorkspace(NhlwsAREAMAP,
					 NhlwsNONE,800000*sizeof(int));
		if (cnp->aws_id < 1) 
			return MIN(ret,(NhlErrorTypes)cnp->aws_id);
	}
	if ((cnp->aws = _NhlUseWorkspace(cnp->aws_id)) == NULL) {
		e_text = 
			"%s: error reserving label area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

#if 0
	c_arseti("lc",(int) (cnp->amap_crange * 
		 MIN(cnl->view.width,cnl->view.height)));
#endif
	subret = _NhlArinam(cnp->aws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	return ret;
}

static float Xsoff,Xeoff,Ysoff,Yeoff;
/*
 * Function:	cnInitCellArray
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes cnInitCellArray
#if	NhlNeedProto
(
	NhlContourPlotLayer	cnl,
	int		*msize,
	int		*nsize,
	NhlBoundingBox	*bbox,
	float		*min_cell_size,
	NhlString	entry_name
)
#else
(cnl,entry_name)
        NhlContourPlotLayer cnl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = &(cnl->contourplot);
        int dunits,dwidth,dheight;
        int max_msize, max_nsize;
        NhlBoolean xlinear,ylinear;
	int mcount,ncount;

	c_cpseti("CAF", -1);
	
	xlinear = True;
        ylinear = True;
	mcount = cnp->sfp->fast_len;
	ncount = cnp->sfp->slow_len;

	subret = CnGetDataBound(cnl,bbox,&xlinear,&ylinear,
			      &mcount,&ncount,&Xsoff,&Xeoff,&Ysoff,&Yeoff,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
        
        max_msize = (int) ((bbox->r - bbox->l) / cnp->min_cell_size);
        max_nsize = (int) ((bbox->t - bbox->b) / cnp->min_cell_size);

        subret = NhlVAGetValues(cnl->base.wkptr->base.id,
                                NhlNwkVSWidthDevUnits,&dunits,
                                NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

        dwidth = dunits * (bbox->r - bbox->l);
        dheight = dunits * (bbox->t - bbox->b);
	*min_cell_size = MAX(1.0/dunits,cnp->min_cell_size);

        if (cnp->sticky_cell_size_set) {
                if ((bbox->r - bbox->l) / cnp->cell_size <= 1.0 ||
                    (bbox->t - bbox->b) / cnp->cell_size <= 1.0) {
                        e_text = 
                                "%s: invalid value for %s: defaulting";
                        NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
                                  entry_name,NhlNcnRasterCellSizeF);
                        ret = NhlWARNING;
                        cnp->sticky_cell_size_set = False;
                }
        }
                
	if (cnp->sticky_cell_size_set) {
		*msize = (int) ((bbox->r - bbox->l) / cnp->cell_size + 0.5);
		*nsize = (int) ((bbox->t - bbox->b) / cnp->cell_size + 0.5);
	}
        else if (cnp->raster_sample_factor <= 0.0) {
                *msize = mcount;
                *nsize = ncount;
        }
        else if (cnp->raster_smoothing_on) {
                *msize = dwidth * cnp->raster_sample_factor;
                *nsize = dheight * cnp->raster_sample_factor;
        }
        else {
                if (! xlinear)
                        *msize = dwidth * cnp->raster_sample_factor;
                else
                        *msize = MIN(dwidth,mcount)
				* cnp->raster_sample_factor;
                if (! ylinear)
                        *nsize = dheight * cnp->raster_sample_factor;
                else
                        *nsize = MIN(dheight,ncount)
                                * cnp->raster_sample_factor;
        }
        
        if (!cnp->sticky_cell_size_set && cnp->raster_sample_factor > 0.0) {
                *msize = MIN(*msize,max_msize);
                *nsize = MIN(*nsize,max_nsize);
                cnp->cell_size = (bbox->r - bbox->l) / (float) *msize;
        }
	
	if (cnp->cws_id < 1) {
		cnp->cws_id = 
			_NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
					 (*msize * *nsize) * sizeof(int));
		if (cnp->cws_id < 1) 
			return MIN(ret,(NhlErrorTypes)cnp->cws_id);
	}
	if ((cnp->cws = _NhlUseWorkspace(cnp->cws_id)) == NULL) {
		e_text = 
			"%s: error reserving cell array workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	return ret;
}

#if 0
/* 
 * not fully implemented -- leave out for now.
 * The idea of this routine is to manage data specified using 
 * cell boundaries for the conpack routines that only handle
 * cell-centered data. 
 */
static NhlErrorTypes SetTransBoundsState
#if	NhlNeedProto
(
        NhlContourPlotLayer     cnl,
	NhlBoolean              do_bounds,
	NhlBoolean              *do_bounds_ret
        )
#else
(cnl,do_bounds)
        NhlContourPlotLayer     cnl;
	NhlBoolean              do_bounds;
	NhlBoolean              *do_bounds_ret;
#endif
{
        NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
	NhlContourPlotLayerPart 	  *cnp = &cnl->contourplot;
	NhlTransformLayerPart 		  *tfp = &cnl->trans;

	if (do_bounds == *do_bounds_ret)
		return ret;

	if (! cnp->sfp->xc_is_bounds && ! cnp->sfp->yc_is_bounds)
		return ret;

	ret = NhlVASetValues(tfp->trans_obj->base.id, 
			     NhlNtrDoBounds, do_bounds,
			     NULL);

	if (ret > NhlFATAL)
		*do_bounds_ret = do_bounds;

	return ret;
}
#endif

static NhlErrorTypes CnStdRender
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlContourPlotLayer     cnl,
	NhlDrawOrder            order,
	NhlString		entry_name
        )
#else
(instance,cnl,order,entry_name)
	NhlLayer		instance;
        NhlContourPlotLayer     cnl;
	NhlDrawOrder            order;
	NhlString		entry_name;
#endif
{
        NhlCnStdRendererLayer csrl = (NhlCnStdRendererLayer) instance;
	NhlCnStdRendererLayerPart	  *csrp = &csrl->cnstdrenderer;
	NhlContourPlotLayerPart 	  *cnp = &cnl->contourplot;
	NhlTransformLayerPart 		  *tfp = &cnl->trans;
	NhlString e_text;
        NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
        Gint            err_ind;
        Gclip           clip_ind_rect;

	Cnl = cnl;
	Cnp = cnp;
	
	ginq_clip(&err_ind,&clip_ind_rect);
        gset_clip_ind(GIND_CLIP);

	c_cprset();
	SetCpParams(cnl,entry_name);

/*
 * Get the current bounds handling state
 */
	NhlVAGetValues(tfp->trans_obj->base.id, 
		       NhlNtrDoBounds, &csrp->do_bounds,
		       NULL);
/*
 * Only set the ORV parameter if overlaying on EZMAP. It can cause
 * problems otherwise. (Not sure yet whether it is needed in some cases
 * though, and perhaps not needed in certain Ezmap cases.
 */
	if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlmapTransObjClass->base_class.class_name) {
		NhlVAGetValues(cnp->trans_obj->base.id, 
			       NhlNtrOutOfRangeF, &cnp->out_of_range_val,
			       NULL);
		c_cpsetr("ORV",cnp->out_of_range_val);
	}

	if (cnp->sfp->missing_value_set)
		c_cpsetr("SPV",cnp->sfp->missing_value);
	else
		c_cpsetr("SPV",(float)0.0);

	if (cnp->low_level_log_on && cnl->trans.x_log) {
		c_cpsetr("XC1",(float)tfp->data_xstart);
		c_cpsetr("XCM",(float)tfp->data_xend);
	}
	else {
		c_cpsetr("XC1",(float)cnp->xc1);
		c_cpsetr("XCM",(float)cnp->xcm);
	}
	if (cnp->low_level_log_on && cnl->trans.y_log) {
		c_cpsetr("YC1",(float)tfp->data_ystart);
		c_cpsetr("YCN",(float)tfp->data_yend);
	}
	else {
		c_cpsetr("YC1",(float)cnp->yc1);
		c_cpsetr("YCN",(float)cnp->ycn);
	}
	c_cpseti("WSO", 3);		/* error recovery on */
	c_cpseti("NVS",0);		/* no vertical strips */
	c_cpseti("HLE",1);              /* search for equal high/lows */
        c_cpseti("SET",0);
        c_cpseti("RWC",500);
        c_cpseti("RWG",1500);
        c_cpseti("MAP",NhlcnMAPVAL);

	c_cpsetr("PIT",MAX(0.0,cnp->max_point_distance));
	
        if (cnp->smoothing_on) {
                c_cpsetr("T2D",cnp->smoothing_tension);
                c_cpsetr("SSL",cnp->smoothing_distance);
        }
        else {
                c_cpsetr("T2D",(float)0.0);
        }
	gset_fill_colr_ind((Gint)_NhlGetGksCi(cnl->base.wkptr,0));

	subret = UpdateLineAndLabelParams(cnl,&cnp->do_lines,&cnp->do_labels);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return ret;
	}

	subret = UpdateFillInfo(cnl, &cnp->do_fill);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return ret;
	}


/* Retrieve workspace pointers */

	if ((cnp->fws = _NhlUseWorkspace(cnp->fws_id)) == NULL) {
		e_text = "%s: error reserving float workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return(ret);
	}
	if ((cnp->iws = _NhlUseWorkspace(cnp->iws_id)) == NULL) {
		e_text = "%s: error reserving integer workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return(ret);
	}
	/* Draw the contours */

	subret = _NhlCprect(cnp->data,cnp->sfp->fast_dim,cnp->sfp->fast_len,
			    cnp->sfp->slow_len,cnp->fws,cnp->iws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return ret;
	}
#if 0
	{ /* for debugging */
		float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
		c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
		printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
		       flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
  	}
#endif


	if (cnp->do_fill && cnp->fill_order == order) {

		if (cnp->fill_mode == NhlAREAFILL) {
#if 0
			subret = SetTransBoundsState
				(cnl,False,&csrp->do_bounds);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
#endif
			if (cnp->aws == NULL) {
				subret = cnInitAreamap(cnl,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					ContourAbortDraw(cnl);
					gset_clip_ind(clip_ind_rect.clip_ind);
					return ret;
				}
			}
			if (! cnp->aws) {
				e_text = "%s: Error reserving workspace";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return NhlFATAL;
			}
			subret = AddDataBoundToAreamap(cnl,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}

			subret = _NhlCpclam(cnp->data,cnp->fws,cnp->iws,
					    cnp->aws,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}

			if (cnp->dump_area_map)
				_NhlDumpAreaMap(cnp->aws,entry_name);

			subret = _NhlArscam(cnp->aws,
					    (_NHLCALLF(hlucpfill,HLUCPFILL)),
					    entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			subret = _NhlIdleWorkspace(cnp->aws);
			ret = MIN(subret,ret);
			cnp->aws = NULL;
		}
		else if (cnp->fill_mode == NhlCELLFILL) {
#if 0
			subret = SetTransBoundsState
				(cnl,True,&csrp->do_bounds);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
#endif
			subret = _NhlCellFill((NhlLayer)cnl,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				return ret;
			}
		}
		else { /* NhlRASTERFILL */
			int msize,nsize;
			float min_cell_size;
			NhlBoundingBox bbox;
#if 0
			if (! cnp->raster_smoothing_on) {
				subret = SetTransBoundsState
					(cnl,True,&csrp->do_bounds);
			}
			else {
				subret = SetTransBoundsState
					(cnl,False,&csrp->do_bounds);
			}
#endif
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			subret = cnInitCellArray(cnl,&msize,&nsize,&bbox,
						 &min_cell_size,entry_name);
 			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			subret = _NhlCpcica(cnp->data,
					    cnp->fws,cnp->iws,cnp->cws,
					    msize,msize,nsize,
					    bbox.l,bbox.b,bbox.r,bbox.t,
					    min_cell_size,
					    cnp->raster_smoothing_on,
					    entry_name);
 			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			if (cnp->cws != NULL) {
				subret = _NhlIdleWorkspace(cnp->cws);
				ret = MIN(subret,ret);
				cnp->cws = NULL;
			}
		}
	}

	if (cnp->line_order == order &&
	    (cnp->do_lines || cnp->missing_val.perim_on ||
	     cnp->grid_bound.perim_on || cnp->out_of_range.perim_on)) {
#if 0
		subret = SetTransBoundsState
			(cnl,False,&csrp->do_bounds);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			gset_clip_ind(clip_ind_rect.clip_ind);
			return ret;
		}
#endif
		if (cnp->do_labels && cnp->label_masking) {
			c_cpseti("GIL",5);
			if (cnp->aws == NULL) {
				subret = cnInitAreamap(cnl,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					ContourAbortDraw(cnl);
					gset_clip_ind(clip_ind_rect.clip_ind);
					return ret;
				}
			}
			if (! cnp->aws) {
				e_text = "%s: Error reserving workspace";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return NhlFATAL;
			}
			c_pcsetr("PH",(float)cnp->line_lbls.pheight);
			c_pcsetr("PW",(float)cnp->line_lbls.pwidth);
			c_pcsetr("CS",(float)cnp->line_lbls.cspacing);
			c_pcseti("FN",cnp->line_lbls.font);
			c_pcseti("QU",cnp->line_lbls.quality);
			c_pcsetc("FC",cnp->line_lbls.fcode);
			subret = _NhlCplbam(cnp->data,cnp->fws,cnp->iws,
					    cnp->aws,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			subret = _NhlCpcldm(cnp->data,
					    cnp->fws,cnp->iws,cnp->aws,
					    (_NHLCALLF(cpdrpl,CPDRPL)),
					    entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			subret = _NhlIdleWorkspace(cnp->aws);
			ret = MIN(subret,ret);
			cnp->aws = NULL;
		}
		else {
			subret = _NhlCpcldr(cnp->data,
					    cnp->fws,cnp->iws,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
		}
	}
	
	if (cnp->do_labels && cnp->label_order == order) {
#if 0
		subret = SetTransBoundsState
			(cnl,False,&csrp->do_bounds);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			gset_clip_ind(clip_ind_rect.clip_ind);
			return ret;
		}
#endif
		cnp->line_lbls.count = 0;
		cnp->high_lbls.count = 0;
		cnp->low_lbls.count = 0;
		gset_fill_int_style(GSTYLE_SOLID);

		c_pcsetr("PH",(float)cnp->line_lbls.pheight);
		c_pcsetr("PW",(float)cnp->line_lbls.pwidth);
		c_pcsetr("CS",(float)cnp->line_lbls.cspacing);
		c_pcseti("FN",cnp->line_lbls.font);
		c_pcseti("QU",cnp->line_lbls.quality);
		c_pcsetc("FC",cnp->line_lbls.fcode);
		subret = _NhlCplbdr(cnp->data,cnp->fws,cnp->iws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			gset_clip_ind(clip_ind_rect.clip_ind);
			return ret;
		}
	}

	if (cnp->fws != NULL) {
		subret = _NhlIdleWorkspace(cnp->fws);
		ret = MIN(subret,ret);
		cnp->fws = NULL;
	}
	if (cnp->iws != NULL) {
		subret = _NhlIdleWorkspace(cnp->iws);
		cnp->iws = NULL;
		ret = MIN(subret,ret);
	}

	return MIN(subret,ret);
}

/*
 * Function:  hlucpfill
 *
 * Description: C version of APR user routine called from within ARSCAM 
 *		to fill areas based on the area ID.
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
int (_NHLCALLF(hlucpfill,HLUCPFILL))
#if	NhlNeedProto
(
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcs,ycs,ncs,iai,iag,nai)
	float *xcs; 
	float *ycs; 
	int *ncs; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{
	int i;
	int pat_ix, col_ix;
	float fscale;
	int *colp, *patp;
	float *sclp;

	if (Cnp == NULL) return 0;

	for (i = 0; i < *nai; i++) {
#if 0
		printf("i %d iai %d iag %d\n",i,iai[i],iag[i]);
#endif
		if (iai[i] == 9999) {
			return 0;
		}
		if (iag[i] == 5 && iai[i] == -1) {
			return 0;
		}
	}

	colp = (int *) Cnp->fill_colors->data;
	patp = (int *) Cnp->fill_patterns->data;
	sclp = (float *) Cnp->fill_scales->data;
	for (i = 0; i < *nai; i++) {
		if (iag[i] == 3) {
			if (iai[i] > 99 && 
			    iai[i] < 100 + Cnp->fill_count) {
				int ix = iai[i] - 100;
				col_ix = Cnp->mono_fill_color ? 
					Cnp->fill_color : colp[ix];
				pat_ix = Cnp->mono_fill_pattern ?
					Cnp->fill_pattern : patp[ix];
				fscale = Cnp->mono_fill_scale ?
					Cnp->fill_scale : sclp[ix];
			}
			else {
				NhlcnRegionAttrs *reg_attrs;

				switch (iai[i]) {
				case 99:
					reg_attrs = &Cnp->grid_bound;
					break;
				case 98:
					reg_attrs = &Cnp->missing_val;
					break;
				case 97:
					reg_attrs = &Cnp->out_of_range;
					break;
				default:
					return 0;
				}
				col_ix = reg_attrs->gks_fcolor;
				pat_ix = reg_attrs->fill_pat;
				fscale = reg_attrs->fill_scale;
			}
			NhlVASetValues(Cnl->base.wkptr->base.id,
				       _NhlNwkFillIndex, pat_ix,
				       _NhlNwkFillColor, col_ix,
				       _NhlNwkFillOpacityF, Cnp->fill_opacity,
				       _NhlNwkFillScaleFactorF,fscale,
				       _NhlNwkFillBackground,
				       Cnp->fill_background_color,
				       _NhlNwkFillDotSizeF,Cnp->fill_dot_size,
				       _NhlNwkEdgesOn,0,
				       NULL);
			
			_NhlSetFillInfo(Cnl->base.wkptr,(NhlLayer) Cnl);
			_NhlWorkstationFill(Cnl->base.wkptr,xcs,ycs,*ncs);
		}
	}
	return 0;
}


/*
 * Function:  hlucpscae
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

/*ARGSUSED*/
void  (_NHLCALLF(hlucpscae,HLUCPSCAE))
#if	NhlNeedProto
(
	int		*icra,
	int		*ica1,
	int		*icam,
	int		*ican,
	float		*xcpf,
	float		*ycpf,
	float		*xcqf,
	float		*ycqf,
	int		*ind1,
	int		*ind2,
	int		*icaf,
	int		*iaid		      
)
#else
(icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf,ind1,ind2,icaf,iaid)
	int		*icra;
	int		*ica1;
	int		*icam;
	int		*ican;
	float		*xcpf;
	float		*ycpf;
	float		*xcqf;
	float		*ycqf;
	int		*ind1;
	int		*ind2;
	int		*icaf;
	int		*iaid;
#endif
{
	int col_ix;

	if (Cnp == NULL) {
		_NHLCALLF(cpscae,CPSCAE)
			(icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf,
			 ind1,ind2,icaf,iaid);
		return;
	}
	/* no support in cell arrays for transparent, so it's necessary
	   to reset transparent color indexes to background */
	   

	if (*iaid > 99 && *iaid < 100 + Cnp->fill_count) {
		col_ix = Cnp->gks_fill_colors[*iaid - 100];
		if (col_ix < 0) col_ix = NhlBACKGROUND;
	}
	else if (*iaid == 99) {
		col_ix = Cnp->grid_bound.gks_fcolor;
		if (col_ix < 0) col_ix = NhlBACKGROUND;
	}
	else if (*iaid == 98) {
		col_ix = Cnp->missing_val.gks_fcolor;
		if (col_ix < 0) col_ix = NhlBACKGROUND;
	}
	else if (*iaid == 97) {
		col_ix = Cnp->out_of_range.gks_fcolor;
		if (col_ix < 0) col_ix = NhlBACKGROUND;
	}
	else {
		col_ix = NhlBACKGROUND;
	}
	*(icra + ((*ind2 - 1) * *ica1 + (*ind1 - 1))) = col_ix;

	return;
}

/*
 * Function:  hlucpchcl
 *
 * Description: C version of the CPCHCL function that is called from
 *              the Conpack CPCLDR and CPCLDM functions. 
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
void   (_NHLCALLF(hlucpchcl,HLUCPCHCL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{

	char func[] = "HLUCPCHCL";
	int i, pai, dpix;
	char buffer[NhlDASHBUFSIZE];
	int lcol;
	float thickness, tf;
	float *thp;
	int   *dpp;

	if (Cnp == NULL) {
		_NHLCALLF(cpchcl,CPCHCL)(iflg);
		return;
	}

	dpp = (int *) Cnp->line_dash_patterns->data;
	thp = (float *) Cnp->line_thicknesses->data;

	if (*iflg != 1) return;

	c_cpgeti("PAI", &pai);
	if (pai > 0 && pai < 256) {
		if (! Cnp->do_lines) return;
		thickness = Cnp->mono_line_thickness ? 
			Cnp->line_thickness : thp[pai-1];
		lcol = Cnp->mono_line_color ? 
			Cnp->gks_line_colors[0] : Cnp->gks_line_colors[pai-1];
		dpix = Cnp->mono_line_dash_pattern ? 
			Cnp->line_dash_pattern : dpp[pai-1];
	}
	else {
		NhlcnRegionAttrs *reg_attrs;

		switch (pai) {
		case -1:
			reg_attrs = &Cnp->grid_bound;
			break;
		case -2:
			reg_attrs = &Cnp->missing_val;
			break;
		case -3:
			reg_attrs = &Cnp->out_of_range;
			break;
		default:
			return;
		}
		thickness = reg_attrs->perim_thick;
		lcol = reg_attrs->gks_pcolor;
		dpix = reg_attrs->perim_dpat;
	}
		
	memset((void *) buffer,'\0', sizeof(buffer)*sizeof(char));

	c_pcseti("FN",0);
	c_pcseti("CL",1);
 	c_pcseti("CC",-1);
	c_pcseti("OC",-1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	
	/*
	 * Reset DashPack so we know what state we are starting from.
	 */
	_NHLCALLF(dprset,DPRSET)();
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if (dpix < 0) 
		dpix = NhlSOLIDLINE;
	else if (dpix > Cnp->dtable_len)
	    dpix = 1 + (dpix - 1) % Cnp->dtable_len;
	strncpy(buffer,Cnp->dtable[dpix],sizeof(buffer) - 1);

	tf = Cnp->line_dash_seglen / (strlen(buffer)+.5);
	c_dpsetr("WOG",(float)tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOS",(float)tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if (lcol == NhlTRANSPARENT) {
		for (i = 0; i < strlen(buffer); i++)
			buffer[i] = '_';
	}
	else{
	        gset_line_colr_ind((Gint)lcol);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

        gset_linewidth(thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);


	if (pai > 0 && Cnp->llabel_placement == NhlCONSTANT) {
		int buff_size = sizeof(buffer) - strlen(buffer) - 1;
		char *tchar = &buffer[strlen(buffer)];
		char *ts = ((NhlString *) Cnp->line_lbls.text)[pai-1];
		NhlcnLevelUseMode *lup = 
			(NhlcnLevelUseMode *) Cnp->level_flags->data;
		NhlcnLevelUseMode flag;
		NhlColorIndex llcol;
		int j;
		NhlBoolean do_label;

		llcol = Cnp->line_lbls.mono_color ?
			 Cnp->line_lbls.gks_color : 
				Cnp->line_lbls.colors[pai-1];
		
		flag = Cnp->mono_level_flag ? 
			Cnp->level_flag : lup[pai-1];

		do_label = Cnp->line_lbls.on && flag > NhlLINEONLY;

		if (llcol == NhlTRANSPARENT && do_label) {
			/*
			 * Put spaces in for label.
			 */
			j = MIN(strlen(ts) * 2 + 1,buff_size);
			for(i=0;i < j-1;i+=2){
				tchar[i] = ' ';
				tchar[i+1] = '|';
			}
		}
		else if (do_label) {
			/*
			 * Add breaks in at each space of the label.
			 */
			i=0;
			j=0;
			while (i < buff_size && ts[j] != '\0'){
				if (ts[j] == ' ')
					tchar[i++] = '|';
				tchar[i++] = ts[j++];
			}
			c_pcseti("OC",llcol);
			c_pcseti("CC",llcol);
		}
		c_pcsetr("PH",(float)Cnp->line_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->line_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->line_lbls.cspacing);
		c_pcseti("FN",Cnp->line_lbls.font);
		c_pcseti("QU",Cnp->line_lbls.quality);
		c_pcsetc("FC",Cnp->line_lbls.fcode);
		c_pcsetr("CL",(float)Cnp->line_lbls.thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

	c_dpsetc("DPT",buffer);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOC",(float)Cnp->line_lbls.real_height);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	return;
}


/*
 * Function:	Substitute
 *
 * Description: substitutes a string for a Conpack substitution sequence.
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static void Substitute
#if	NhlNeedProto
(
	char		*buf,
	int		replace_count,
	char		*subst
)
#else 
(buf,replace_count,subst)
	char		*buf;
	int		replace_count;
	char		*subst;
#endif
{
	int subst_count,add,buflen;
	char *from, *to;

	buflen = strlen(buf);
	subst_count = strlen(subst);
	if (subst_count - replace_count < 0) {
		for (from = buf+replace_count,to = buf+subst_count; ;
		     to++,from++) { 
			*to = *from;
			if (*from == '\0')
				break;
		}
	}
	else if ((add = subst_count - replace_count) > 0) {
		for (from = buf + buflen,to = buf + buflen + add; 
		     from >= buf + replace_count;)
			*to-- = *from--;
	}
	strncpy(buf,subst,subst_count);
}

/*
 * Function:  hlucpchhl
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

/*ARGSUSED*/
void   (_NHLCALLF(hlucpchhl,HLUCPCHHL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{
	char buf[128];
	char *fstr,*sub;
	float zdv;
	NhlFormatRec *frec;
	int *fwidth, *sig_digits, *left_sig_digit, *point_pos, *exp_switch_len, *exp_field_width;

	if (Cnp == NULL) {
		_NHLCALLF(cpchhl,CPCHHL)(iflg);
		return;
	}

#if 0
	{ /* for debugging */
		float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
		c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
		printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
		       flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
  	}
#endif
	switch (*iflg) {
	case 1:
	case 3:
	case 5:
	case 7:
		frec = &Cnp->max_data_format;
		fwidth = frec->field_width_flag == NhlffUNSPECED ? NULL : &frec->field_width;
		sig_digits = frec->sig_digits_flag == NhlffUNSPECED ? NULL : &frec->sig_digits;
		left_sig_digit = frec->left_sig_digit_flag == NhlffUNSPECED ? NULL : &frec->left_sig_digit;
		point_pos =  frec->point_position_flag == NhlffUNSPECED ? NULL : &frec->point_position;
		exp_switch_len = frec->exp_switch_flag == NhlffUNSPECED ? NULL : &frec->exp_switch_len;
		exp_field_width = frec->exp_field_width_flag == NhlffUNSPECED ? NULL : &frec->exp_field_width;
		/* drop through */
	default:
		break;
	}
	switch (*iflg) {
	case 1:
		if (! Cnp->high_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if ( Cnp->high_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->high_lbls.gks_color);
			c_pcseti("OC", Cnp->high_lbls.gks_color);
		}
		c_pcsetr("PH",(float)Cnp->high_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->high_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->high_lbls.cspacing);
		c_pcseti("FN",Cnp->high_lbls.font);
		c_pcseti("QU",Cnp->high_lbls.quality);
		c_pcsetc("FC",Cnp->high_lbls.fcode);
		gset_linewidth(Cnp->high_lbls.thickness);

		strcpy(buf,(char *)Cnp->high_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->high_lbls.format,zdv,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->high_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 2:
		if (! Cnp->high_lbls.on) return;
		gset_fill_colr_ind(Cnp->high_lbls.gks_bcolor);
		break;
	case 3:
		if (! Cnp->high_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if ( Cnp->high_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->high_lbls.gks_color);
			c_pcseti("OC", Cnp->high_lbls.gks_color);
		}
		else {
			c_cpsetc("CTM"," ");
			return;
		}
		c_pcsetr("PH",(float)Cnp->high_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->high_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->high_lbls.cspacing);
		c_pcseti("FN",Cnp->high_lbls.font);
		c_pcseti("QU",Cnp->high_lbls.quality);
		c_pcsetc("FC",Cnp->high_lbls.fcode);
		gset_linewidth((float)Cnp->high_lbls.thickness);

		Cnp->high_lbls.count++;
		strcpy(buf,(char *)Cnp->high_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->high_lbls.format,zdv,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->high_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 4:
		gset_line_colr_ind(Cnp->high_lbls.gks_plcolor);
		gset_linewidth(Cnp->high_lbls.perim_lthick);
		break;
	case 5:
		if (! Cnp->low_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if (Cnp->low_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->low_lbls.gks_color);
			c_pcseti("OC", Cnp->low_lbls.gks_color);
		}
		c_pcsetr("PH",(float)Cnp->low_lbls.pheight * LowLabelFactor);
		c_pcsetr("PW",(float)Cnp->low_lbls.pwidth * LowLabelFactor);
		c_pcsetr("CS",(float)Cnp->low_lbls.cspacing);
		c_pcseti("FN",Cnp->low_lbls.font);
		c_pcseti("QU",Cnp->low_lbls.quality);
		c_pcsetc("FC",Cnp->low_lbls.fcode);
		gset_linewidth((float)Cnp->low_lbls.thickness);
		strcpy(buf,(char *)Cnp->low_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->low_lbls.format,zdv,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->low_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 6:
		if (! Cnp->low_lbls.on) return;
		gset_fill_colr_ind(Cnp->low_lbls.gks_bcolor);
		break;
	case 7:
		if (! Cnp->low_lbls.on) {
			c_cpsetc("CTM"," ");
			return;
		}
		if (Cnp->low_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->low_lbls.gks_color);
			c_pcseti("OC", Cnp->low_lbls.gks_color);
		}
		else {
			c_cpsetc("CTM"," ");
			return;
		}
		c_pcsetr("PH",(float)Cnp->low_lbls.pheight * LowLabelFactor);
		c_pcsetr("PW",(float)Cnp->low_lbls.pwidth * LowLabelFactor);
		c_pcsetr("CS",(float)Cnp->low_lbls.cspacing);
		c_pcseti("FN",Cnp->low_lbls.font);
		c_pcseti("QU",Cnp->low_lbls.quality);
		c_pcsetc("FC",Cnp->low_lbls.fcode);
		gset_linewidth((float)Cnp->low_lbls.thickness);

		Cnp->low_lbls.count++;
		strcpy(buf,(char *)Cnp->low_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_cpgetr("zdv",&zdv);
		zdv /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->low_lbls.format,zdv,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->low_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_cpsetc("CTM",buf);
		break;
	case 8:
		gset_line_colr_ind(Cnp->low_lbls.gks_plcolor);
		gset_linewidth(Cnp->low_lbls.perim_lthick);
		break;
	default:
		break;
	}

	return;
}

/*
 * Function:  hlucpchll
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

/*ARGSUSED*/
void   (_NHLCALLF(hlucpchll,HLUCPCHLL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{

	int pai;
	static int llcol;

	if (Cnp == NULL) {
		_NHLCALLF(cpchll,CPCHLL)(iflg);
		return;
	}
	if (Cnp->llabel_placement == NhlCONSTANT)
		return;

	if (*iflg == 1) {
		c_pcsetr("PH",(float)Cnp->line_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->line_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->line_lbls.cspacing);
		c_pcseti("FN",Cnp->line_lbls.font);
		c_pcseti("QU",Cnp->line_lbls.quality);
		c_pcsetc("FC",Cnp->line_lbls.fcode);
		gset_linewidth((float)Cnp->line_lbls.thickness);
	}
	else if (*iflg == 2) {
		if (Cnp->line_lbls.gks_bcolor > NhlTRANSPARENT)
			gset_fill_colr_ind(Cnp->line_lbls.gks_bcolor);
	}
	else if (*iflg == 3) {
		c_cpgeti("PAI", &pai);
		if (pai > 0) {
			pai -= 1;

			llcol = Cnp->line_lbls.mono_color ?
				 Cnp->line_lbls.gks_color : 
					Cnp->line_lbls.colors[pai];
			if (llcol > NhlTRANSPARENT) {
				c_pcseti("CC",llcol);
				c_pcseti("OC",llcol);
			}
			else {
				c_cpsetc("CTM"," ");
			}
			c_pcsetr("PH",(float)Cnp->line_lbls.pheight);
			c_pcsetr("PW",(float)Cnp->line_lbls.pwidth);
			c_pcsetr("CS",(float)Cnp->line_lbls.cspacing);
			c_pcseti("FN",Cnp->line_lbls.font);
			c_pcseti("QU",Cnp->line_lbls.quality);
			c_pcsetc("FC",Cnp->line_lbls.fcode);
			gset_linewidth((float)Cnp->line_lbls.thickness);
		}
		Cnp->line_lbls.count++;
	}
	else if (*iflg == 4) {
		gset_line_colr_ind(Cnp->line_lbls.gks_plcolor);
		gset_linewidth(Cnp->line_lbls.perim_lthick);
	}

	return;
}


/* low level overlay mapping functions */

static void OverlayMapXY
#if	NhlNeedProto
(
        NhlTransformLayerPart *tfp,
        float *xin,
        float *yin,
        float* xout,
        float* yout)
#else
(tfp,xin,yin,xout,yout)
	NhlTransformLayerPart *tfp;
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;

        if (! tfp->overlay_trans_obj ||
            tfp->overlay_trans_obj == tfp->trans_obj) {
		_NhlCompcToWin(tfp->trans_obj,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlCompcToData(tfp->trans_obj,xin,yin,1,xout,yout,
				&status,NULL,NULL);

		if (status) return;
#if 0
		fprintf (stderr,"OverlayMapXY: inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToWin(tfp->overlay_trans_obj,
			     xout,yout,1,xout,yout,&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"OverlayMapXY: %f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

	return;
}


static void OverlayInvMapXY
#if	NhlNeedProto
(
        NhlTransformLayerPart *tfp,
        float *xin,
        float *yin,
        float* xout,
        float* yout)
#else
(tfp,xin,yin,xout,yout)
	NhlTransformLayerPart *tfp;
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;
	float xtmp;
	float ytmp;

        if (! tfp->overlay_trans_obj ||
            tfp->overlay_trans_obj == tfp->trans_obj) {
		_NhlWinToCompc(tfp->trans_obj,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
#if 0
		if (status) {

			fprintf (stderr,
				 "OverlayInvMapXY: %f %f : %f %f \n",
				 *xin,*yin,*xout,*yout);
		}
#endif
	}
        else {
		_NhlWinToData(tfp->overlay_trans_obj,
			      xin,yin,1,&xtmp,&ytmp,
			      &status,NULL,NULL);


		if (status) {
			*xout = xtmp;
			*yout = ytmp;
#if 0
			fprintf (stderr,
				 "OverlayInvMapXY: inter: %f %f\n",
				 *xout,*yout);
#endif
			return;
		}
		_NhlDataToCompc(tfp->trans_obj,&xtmp,&ytmp,1,xout,yout,
				&status,NULL,NULL);
#if 0
		if (status) {
			fprintf (stderr,
				 "OverlayInvMapXY: %f %f : %f %f : %f %f \n",
				 *xin,*yin,xtmp,ytmp,*xout,*yout);
		}
#endif
        }

	return;
}

/*
 * Function:  hlucpmpxy
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

/*ARGSUSED*/
void   (_NHLCALLF(hlucpmpxy,HLUCPMPXY))
#if	NhlNeedProto
(
	int	*imap,
	float	*xinp,
	float	*yinp,
	float	*xotp,
	float	*yotp
)
#else
(imap,xinp,yinp,xotp,yotp)
	int	*imap;
	float	*xinp;
	float	*yinp;
	float	*xotp;
	float	*yotp;
#endif

{
	int status;

	if (Cnp == NULL) {
		_NHLCALLF(cpmpxy,CPMPXY)(imap,xinp,yinp,xotp,yotp);
		return;
	}

        if (*imap == 0) {
                if ((int) *xinp == NhlcnMAPVAL)
                        *yinp = 3.0;
                else
                        *yinp = 0.0;
        }
        else if (abs(*imap) != NhlcnMAPVAL) {
                *xotp = *xinp;
                *yotp = *yinp;
        }
	else if (Cnl->trans.overlay_status == _tfCurrentOverlayMember &&
		 ! Cnl->trans.do_ndc_overlay) { 
		if (*imap > 0)
			OverlayMapXY(&Cnl->trans,xinp,yinp,xotp,yotp);
		else
			OverlayInvMapXY(&Cnl->trans,xinp,yinp,xotp,yotp);
	}
	else {
		if (*imap > 0)
			_NhlCompcToWin((NhlLayer)Cnp->trans_obj,
				       xinp,yinp,1,xotp,yotp,
				       &status,NULL,NULL);
		else 
			_NhlWinToCompc((NhlLayer)Cnp->trans_obj,
				       xinp,yinp,1,xotp,yotp,
				       &status,NULL,NULL);
	}
	return;
}


/*
 * Function:  load_hlucp_routines
 *
 * Description: Forces the hlucp... routines to load from the HLU library
 *
 * In Args:   NhlBoolean flag - should always be False - dont actually
 *			        want to call the routines.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void   load_hlucp_routines
#if	NhlNeedProto
(
	NhlBoolean	flag
)
#else
(flag)
	NhlBoolean	flag;
#endif
{
	int idum;
	float fdum;


	if (flag) {
		_NHLCALLF(hlucpmpxy,HLUCPMPXY)(&idum,&fdum,&fdum,&fdum,&fdum);
		_NHLCALLF(hlucpchll,HLUCPCHLL)(&idum);
		_NHLCALLF(hlucpchhl,HLUCPCHHL)(&idum);
		_NHLCALLF(hlucpchcl,HLUCPCHCL)(&idum);
		_NHLCALLF(hlucpscae,HLUCPSCAE)(&idum,&idum,&idum,&idum,
					       &fdum,&fdum,&fdum,&fdum,
					       &idum,&idum,&idum,&idum);
	}
	return;
}

/*
 * Function:  _NhlRasterFill
 *
 * Description: performs a discrete raster fill - 
 * replaces Conpack routine CPCICA - Conpack must be initialized, etc.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

NhlErrorTypes _NhlRasterFill
#if	NhlNeedProto
(
	float 		*zdat,
	int		*cell,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	char		*entry_name
)
#else
(zdat,cell,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,entry_name)
	float		*zdat;
	int		*cell;
	int		ica1;
	int		icam;
	int		ican;
	float		xcpf;
	float		ycpf;
	float		xcqf;
	float		ycqf;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	float		xc1,xcm,yc1,ycn;
	float		xmn,xmx,ymn,ymx;
	int		i,j,k,izd1,izdm,izdn,indx,indy,icaf,map,iaid;
	float		xccf,xccu,xccd,xcci,yccf,yccu,yccd,ycci;
	float		zval,orv,spv;
        float		*levels;
	float		cxstep,cystep,dxstep,dystep;
	float           xsoff,xeoff,ysoff,yeoff;
	NhlBoolean      x_isbound,y_isbound;

        if (Cnp == NULL) {
		e_text = "%s: invalid call to _NhlRasterFill";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
        }
        levels = (float*) Cnp->levels->data;
	
        
/* 
 * replacement for CPCICA
 */
	c_cpgetr("XC1",&xc1);
	c_cpgetr("XCM",&xcm);
	c_cpgetr("YC1",&yc1);
	c_cpgetr("YCN",&ycn);
	c_cpgetr("ORV",&orv);
	c_cpgetr("SPV",&spv);
	c_cpgeti("ZDM",&izdm);
	c_cpgeti("ZDN",&izdn);
	c_cpgeti("ZD1",&izd1);
	c_cpgeti("CAF",&icaf);
	c_cpgeti("MAP",&map);
	xmn = MIN(xc1,xcm);
	xmx = MAX(xc1,xcm);
	ymn = MIN(yc1,ycn);
	ymx = MAX(yc1,ycn);

        map = -map;
	cxstep = (xcqf-xcpf)/(float)icam;
	cystep = (ycqf-ycpf)/(float)ican;
 	x_isbound = Cnp->sfp->xc_is_bounds;
 	y_isbound = Cnp->sfp->yc_is_bounds;
	xsoff = Xsoff + .5 * (1.0 - Xsoff);
	xeoff = Xeoff + .5 * (1.0 - Xeoff);
	ysoff = Ysoff + .5 * (1.0 - Ysoff);
	yeoff = Yeoff + .5 * (1.0 - Yeoff);

	if (x_isbound) {
		dxstep = (xcm-xc1) / (float)izdm;
	}
	else {
		dxstep = (xcm-xc1) / (float)(izdm-1);
	}
	if (y_isbound) {
		dystep = (ycn-yc1) / (float)izdn;
	}
	else {
		dystep = (ycn-yc1) / (float)( izdn - 1);
	}

	for (i = 0; i < icam; i++) {
#if 0
		int print = 1;
#endif
		int iplus = i+1;
		if (i == 0)
			xccf = xcpf + xsoff * cxstep;
		else if (i == icam - 1)
			xccf = xcpf + (icam - xeoff) * cxstep; 
		else
			xccf = xcpf + (i+xsoff) * cxstep;
		xccu = c_cfux(xccf);


		for (j = 0; j < ican; j++) {
			int jplus = j+1;
			if (j == 0)
		  		yccf = ycpf + ysoff * cystep;
			else if (j == ican - 1)
		  		yccf = ycpf + (ican - yeoff) * cystep;
			else
				yccf = ycpf + (j + ysoff) * cystep;
			yccu = c_cfuy(yccf);
			
			(_NHLCALLF(hlucpmpxy,HLUCPMPXY))
				(&map,&xccu,&yccu,&xccd,&yccd);
			zval = spv;
			if (xccd == orv) {
				iaid = 97;
			}
			else if (xccd < xmn || xccd > xmx || 
				 yccd < ymn || yccd > ymx) {
				iaid = 99;
			}
			else {
				xcci =(xccd-xc1) / dxstep;
				ycci =(yccd-yc1)/ dystep;
				indx = x_isbound ? 
					(int) xcci : (int)(xcci + 0.5);
				indy = y_isbound ? 
					(int) ycci : (int)(ycci + 0.5);
				if (indx < 0 || indx > izdm-1 ||
				    indy < 0 || indy > izdn-1)
					iaid = 99;
				else if (spv != 0.0 &&
					 *(zdat + indy * izd1 + indx) == spv)
					iaid = 98;
				else {
                                        iaid = -1;
					zval = *(zdat + indy*izd1 + indx);
                                        for (k=0; k < Cnp->level_count; k++) {
                                                if (zval < levels[k]) {
                                                   iaid = NhlcnAREAID_OFFSET+k;
                                                   break;
                                                }
                                        }
                                        if (iaid == -1) {
                                                iaid = NhlcnAREAID_OFFSET +
                                                        Cnp->level_count;
                                        }
				}
#if 0
				if (print) {
					print = 0;
					printf("%f %f %f %f %f %d %d\n",
					       xccu,yccu,xccd,yccd,
					       zval,iplus,jplus);
				}
#endif
			}
			(_NHLCALLF(hlucpscae,HLUCPSCAE))
			  (cell,&ica1,&icam,&ican,&xcpf,&ycpf,&xcqf,&ycqf,
			   &iplus,&jplus,&icaf,&iaid);
		}
	}

	return ret;
}
