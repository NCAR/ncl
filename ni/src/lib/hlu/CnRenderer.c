/*
 *      $Id: CnRenderer.c,v 1.3 2008-01-31 19:33:51 dbrown Exp $
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
 *	Date:		Tue Sep 23 17:23:23 MDT 2003
 *
 *	Description:	
 */

#include <math.h>
#include <float.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/CurvilinearTransObj.h>
#include <ncarg/hlu/SphericalTransObj.h>
#include <ncarg/hlu/TriMeshTransObjP.h>
#include <ncarg/hlu/CnRendererP.h>

static NhlErrorTypes CnRendererInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

NhlCnRendererClassRec NhlcnRendererClassRec = {
	{
/* class_name 		*/      "cnRendererClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlCnRendererLayerRec),
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
/* layer_initialize 	*/  	CnRendererInitialize,
/* layer_set_values 	*/  	NULL,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	NULL,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	NULL,
	},
	{
/* render */		        NULL,
/* get_isolines */              NULL
	}
};

NhlClass NhlcnRendererClass = (NhlClass)&NhlcnRendererClassRec;

/*
 * Function:	CnRendererInitialize
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
CnRendererInitialize
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

        return ret;
}



NhlErrorTypes _NhlContourRender
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
      NhlCnRendererClassPart *cnrcp =
              &((NhlCnRendererClass)
                instance->base.layer_class)->cnrenderer_class;
      
      return (*cnrcp->render)(instance,cnl,order,entry_name);
}

NhlIsoLine * _NhlGetIsoLines
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlContourPlotLayer     cnl,
        int			n_levels,
        float 			*levels,
	NhlString		entry_name
        )
#else
(instance,cnl,order,entry_name)
	NhlLayer		instance;
        NhlContourPlotLayer     cnl;
        int			n_levels;
        float 			*levels;
	NhlString		entry_name;
#endif
{
      NhlCnRendererClassPart *cnrcp =
              &((NhlCnRendererClass)
                instance->base.layer_class)->cnrenderer_class;
      
      return (*cnrcp->get_isolines)(instance,cnl,n_levels,levels,entry_name);
}


static void CanonicalLonBounds
#if	NhlNeedProto
(
	float lon0,
	float lon1,
	float center_lon,
	float *min_lon,
	float *max_lon
)
#else
(lon0,lon1,center_lon,min_lon,max_lon)
	float lon0;
	float lon1;
	float center_lon;
	float *min_lon;
	float *max_lon;
#endif
{
	float lmin,lmax;

	lmin = lon0;
	lmax = lon1;

	while (lmax - lmin > 360)
		lmax -= 360;

	while (_NhlCmpFAny2(lmin,lmax,6,1e-6) >= 0.0) 
		lmin -=360;

		
	if (center_lon > 360)
		center_lon -= 360;
	if (center_lon < -360)
		center_lon += 360;

	while (lmin < center_lon - 180.0) {
		lmin += 360;
		lmax += 360;
	}
	while (lmin > center_lon + 180.0) {
		lmin -= 360;
		lmax -= 360;
	}
	if (lmax > center_lon + 180.0) {
		lmin = center_lon - 180.0;
		lmax = center_lon + 180.0;
	}
	*min_lon = lmin;
	*max_lon = lmax;

	return;
}
			

static void GetCellInfo
#if	NhlNeedProto
(
	NhlContourPlotLayerPart	*cnp,
	float			*cxd,
	float			*cyd,
        NhlBoolean		xlinear,
        NhlBoolean		ylinear,
	int			*mcount,
	int			*ncount,
	float			*xsoff,
	float			*xeoff,
	float			*ysoff,
	float			*yeoff,
	float			*xexact_count,
	float			*yexact_count
)
#else
(cnp,cxd,cyd,xlinear,ylinear,mcount,ncount,
 xsoff,xeoff,ysoff,yeoff,xexact_count,yexact_count)
	float			*cxd;
	float			*cyd;
        NhlBoolean		xlinear;
        NhlBoolean		ylinear;
	NhlBoolean              ismap;
	float                   center_lon;
	int			*mcount;
	int			*ncount;
	float			*xsoff;
	float			*xeoff;
	float			*ysoff;
	float			*yeoff;
	float			*xexact_count;
	float			*yexact_count;
#endif
{
	float offset,ddiff,stepsize;
	float xmin,xmax,ymin,ymax;
	double pint;
/*
 * these are the offsets (in fraction of a cell edge) added to the 
 * data space in order to account
 * for the fact that more or less of the edge cells should show. If the
 * data window is equal to or larger than the data being shown, the offset
 * is half a cell. Under other conditions it may be more or less.
 */
	if (cnp->sfp->xc_is_bounds)
		*xsoff = *xeoff = 0.0;
	else 
		*xsoff = *xeoff = 0.5;
	if (cnp->sfp->yc_is_bounds)
		*ysoff = *yeoff = 0.0;
	else 
		*ysoff = *yeoff = 0.5;
	*xexact_count = *mcount - 1;
	*yexact_count = *ncount - 1;

	xmin = MIN(cnp->sfp->x_end,cnp->sfp->x_start);
	xmax = MAX(cnp->sfp->x_end,cnp->sfp->x_start);

	if (xlinear && (xmin < cxd[0] || xmax > cxd[1])) {
		ddiff = xmax - xmin;
		stepsize = ddiff / (*mcount - 1);
		offset = cxd[0] - xmin;
		*xsoff += modf(offset/stepsize,&pint);
		if (*xsoff >= 1.0) {
			*xsoff = *xsoff - 1;
		}
		else if (*xsoff < 0.0) {
			*xsoff = 1.0 + *xsoff;
		}
		
		offset = xmax - cxd[1];
		*xeoff += modf(offset/stepsize,&pint);
		*xeoff = (*xeoff >= 1.0) ? *xeoff - 1 : *xeoff;
		*xexact_count = (cxd[1] - cxd[0]) / stepsize;
		*mcount = (int) (0.5 + *xexact_count + *xsoff + *xeoff);
	}

	ymin = MIN(cnp->sfp->y_end,cnp->sfp->y_start);
	ymax = MAX(cnp->sfp->y_end,cnp->sfp->y_start);
	if (ylinear && (ymin < cyd[0] || ymax > cyd[1])) {
		ddiff = ymax - ymin;
		stepsize = ddiff / (*ncount - 1);
		offset = cyd[0] - ymin;
		*ysoff += modf(offset/stepsize,&pint);
		*ysoff = (*ysoff >= 1.0) ? *ysoff - 1 : *ysoff;
		offset = ymax - cyd[1];
		*yeoff += modf(offset/stepsize,&pint);
		*yeoff = (*yeoff >= 1.0) ? *yeoff - 1 : *yeoff;
		*yexact_count = (cyd[1] - cyd[0]) / stepsize;
		*ncount = (int) (0.5 + *yexact_count + *ysoff + *yeoff);
	}
}



/*
 * Function:	CnGetDataBound
 *
 * Description:	Returns the bounding box of contour data in NDC
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

NhlErrorTypes CnGetDataBound
#if	NhlNeedProto
(
	NhlContourPlotLayer	cl,
	NhlBoundingBox		*bbox,
        NhlBoolean		*xlinear,
        NhlBoolean		*ylinear,
	int			*mcount,
	int			*ncount,
	float                   *xsoff,
	float                   *xeoff,
	float                   *ysoff,
	float                   *yeoff,
	NhlString		entry_name
)
#else
(cl,bbox,xlinear,ylinear,mcount,ncount,xsoff,xeoff,ysoff,yeoff,entry_name)
	NhlContourPlotLayer	cl;
	NhlBoundingBox		*bbox;
        NhlBoolean		*xlinear;
        NhlBoolean		*ylinear;
	int			*mcount;
	int			*ncount;
	float                   *xsoff;
	float                   *xeoff;
	float                   *ysoff;
	float                   *yeoff;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlContourPlotLayerPart	*cnp = 
		(NhlContourPlotLayerPart *) &cl->contourplot;
	NhlTransformLayerPart	*tfp = 
		(NhlTransformLayerPart *) &cl->trans;
	int			status;
	NhlBoolean		x_irr = False,y_irr = False;
	float			xexact,yexact;
	float                   center_lon = 0.0;
	float			cxd[2],cyd[2];
	float 			tx[2],ty[2];
	float 			fx[2],fy[2];
	float			fstep;

#define EPSILON 1e-4


	cxd[0] = cnp->xlb;
	cxd[1] = cnp->xub;
	cyd[0] = cnp->ylb;
	cyd[1] = cnp->yub;

        if (cl->trans.grid_type == NhltrIRREGULAR) {
                int i;
                float *coords,start_diff,diff,eps,range;
                if (cnp->sfp->x_arr) {
                        coords = (float *)cnp->sfp->x_arr->data;
			range = coords[cnp->sfp->x_arr->num_elements-1] - coords[0];
			start_diff = fabs(range / (cnp->sfp->x_arr->num_elements-1));
                        eps = start_diff * EPSILON;
                        for (i = 1; i < cnp->sfp->x_arr->num_elements; i++) {
                                diff = fabs(coords[i] - coords[i-1]);
                                if (fabs(diff -  start_diff) >  eps) {
                                        x_irr = True;
                                        break;
                                }
                        }
                }
                if (cnp->sfp->y_arr) {
                        coords = (float *)cnp->sfp->y_arr->data;
			range = coords[cnp->sfp->y_arr->num_elements-1] - coords[0];
			start_diff = fabs(range / (cnp->sfp->y_arr->num_elements-1));
                        eps = start_diff * EPSILON;
                        for (i = 1; i < cnp->sfp->y_arr->num_elements; i++) {
                                diff = fabs(coords[i] - coords[i-1]);
                                if (fabs(diff -  start_diff) >  eps) {
                                        y_irr = True;
                                        break;
                                }
                        }
                }
        }
	
#undef EPSILON
                

	if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlcurvilinearTransObjClass->base_class.class_name ||
	    cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlsphericalTransObjClass->base_class.class_name) {
		*xlinear = False;
		*ylinear = False;

		tx[0] = cnp->xc1;
		tx[1] = cnp->xcm;
		ty[0] = cnp->yc1;
		ty[0] = cnp->ycn;
		_NhlWinToNDC(cnp->trans_obj,tx,ty,
			      2,fx,fy,&status,NULL,NULL);

		bbox->l = MIN(fx[0],fx[1]);
		bbox->r = MAX(fx[0],fx[1]);
		bbox->b = MIN(fy[0],fy[1]);
		bbox->t = MAX(fy[0],fy[1]);
	}
	else if (cnp->trans_obj->base.layer_class->base_class.class_name ==
		 NhltriMeshTransObjClass->base_class.class_name) {
		_NhlWinToNDC(cnp->trans_obj,cxd,cyd,
			     2,fx,fy,&status,NULL,NULL);
		bbox->l = MIN(fx[0],fx[1]);
		bbox->r = MAX(fx[0],fx[1]);
		bbox->b = MIN(fy[0],fy[1]);
		bbox->t = MAX(fy[0],fy[1]);
	}
	else if (cnp->trans_obj->base.layer_class->base_class.class_name ==
                 NhlirregularTransObjClass->base_class.class_name) {

		if (_NhlIsOverlay(cl->base.id)) {
			*xlinear = False;
			*ylinear = False;
		}


		_NhlDataToWin(cnp->trans_obj,cxd,cyd,
			      2,tx,ty,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}
		_NhlWinToNDC(cnp->trans_obj,tx,ty,
			      2,fx,fy,&status,NULL,NULL);

		bbox->l = MIN(fx[0],fx[1]);
		bbox->r = MAX(fx[0],fx[1]);
		bbox->b = MIN(fy[0],fy[1]);
		bbox->t = MAX(fy[0],fy[1]);
	}
	else if (cnp->trans_obj->base.layer_class->base_class.class_name ==
                 NhllogLinTransObjClass->base_class.class_name) {

		if (cl->trans.x_log || x_irr || 
		    cl->trans.grid_type >= NhltrCURVILINEAR)
			*xlinear = False;
		if (cl->trans.y_log || y_irr ||
		    cl->trans.grid_type >= NhltrCURVILINEAR)
			*ylinear = False;

		if (_NhlIsOverlay(cl->base.id)) {
			float xmin,xmax,ymin,ymax;
			NhlVAGetValues(cnp->trans_obj->base.id,
				       NhlNtrXMinF,&xmin,
				       NhlNtrXMaxF,&xmax,
				       NhlNtrYMinF,&ymin,
				       NhlNtrYMaxF,&ymax,
				       NULL);
			cxd[0] = MAX(xmin,cxd[0]);
			cxd[1] = MIN(xmax,cxd[1]);
			cyd[0] = MAX(ymin,cyd[0]);
			cyd[1] = MIN(ymax,cyd[1]);
		}

		_NhlDataToWin(cnp->trans_obj,cxd,cyd,
			      2,tx,ty,&status,
			      NULL,NULL);
		if (status) {
			e_text = "%s: data boundary is out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			return ret;
		}

		_NhlWinToNDC(cnp->trans_obj,tx,ty,
			      2,fx,fy,&status,NULL,NULL);

		bbox->l = MIN(fx[0],fx[1]);
		bbox->r = MAX(fx[0],fx[1]);
		bbox->b = MIN(fy[0],fy[1]);
		bbox->t = MAX(fy[0],fy[1]);
	}
	else if (cnp->trans_obj->base.layer_class->base_class.class_name ==
	    NhlmapTransObjClass->base_class.class_name) {
		
		NhlProjection projection;
		NhlMapLimitMode limit_mode;
		float center_lat,rotation;
		float min_lat,max_lat,min_lon,max_lon;
		NhlBoolean rel_center_lat,rel_center_lon;

		if (cl->trans.x_log || x_irr || 
		    cl->trans.grid_type >= NhltrCURVILINEAR)
			*xlinear = False;
		if (cl->trans.y_log || y_irr ||
		    cl->trans.grid_type >= NhltrCURVILINEAR)
			*ylinear = False;


		NhlVAGetValues(cnp->trans_obj->base.id,
			       NhlNmpLeftNDCF,&bbox->l,
			       NhlNmpRightNDCF,&bbox->r,
			       NhlNmpBottomNDCF,&bbox->b,
			       NhlNmpTopNDCF,&bbox->t,
			       NhlNmpProjection,&projection,
			       NhlNmpLimitMode,&limit_mode,
			       NhlNmpMinLatF,&min_lat,
			       NhlNmpMaxLatF,&max_lat,
			       NhlNmpMinLonF,&min_lon,
			       NhlNmpMaxLonF,&max_lon,
			       NhlNmpRelativeCenterLat,&rel_center_lat,
			       NhlNmpCenterLatF,&center_lat,
			       NhlNmpCenterLonF,&center_lon,
			       NhlNmpRelativeCenterLon,&rel_center_lon,
			       NhlNmpCenterRotF,&rotation,
			       NULL);
		if (projection != NhlCYLINDRICALEQUIDISTANT) {
			*xlinear = False;
			*ylinear = False;
		}
		else {
			if (rel_center_lat && limit_mode == NhlLATLON) {
				center_lat = center_lat + min_lat +
					(min_lat + max_lat) / 2.0;
			}
			if (rotation != 0.0 || center_lat != 0.0) {
				*xlinear = False;
				*ylinear = False;
			}
                }
		/*
		 * In order to treat the data coordinates as linear it's necessary to get the
		 * actual bounds of the data in NDC. If, because of certain boundary conditions, 
		 * this calculation fails, default to treating the coordinates as non-linear.
		 * Note there are complications caused by the cyclic nature of the longitudinal value.
		 * E.g. the boundaries of the data could be 0-360 while the map is canonically -180 - 180
		 * Furthermore if the data does not repeat the cyclic point, there may be a 1 cell gap.
                 */

		if (*xlinear || *ylinear) {
			float tx[2],ty[2];
			float fx[2],fy[2];
			float dx[2],dy[2];
			float lmin,lmax;
			
			fx[0] = bbox->l;
			fx[1] = bbox->r;
			fy[0] = bbox->b;
			fy[1] = bbox->t;
			_NhlNDCToWin(cnp->trans_obj,fx,fy,
				     2,tx,ty,&status,NULL,NULL);
			if (! status) {
				_NhlWinToData(cnp->trans_obj,tx,ty,
					      2,dx,dy,&status,NULL,NULL);
				if (! status) {
					cxd[0] = dx[0];
					cyd[0] = dy[0];
					cxd[1] = dx[1];
					cyd[1] = dy[1];

					if (limit_mode == NhlLATLON && rel_center_lon) {
						center_lon = center_lon + min_lon 
							+ (max_lon - min_lon) / 2;
					}

					CanonicalLonBounds(cxd[0],cxd[1],center_lon,
							   &cxd[0],&cxd[1]);
					CanonicalLonBounds(cnp->xlb,cnp->xub,center_lon,
							   &lmin,&lmax);
					cxd[0] = MAX(lmin,cxd[0]);
					cxd[1] = MIN(lmax,cxd[1]);
					cyd[0] = MAX(cnp->ylb,cyd[0]);
					cyd[1] = MIN(cnp->yub,cyd[1]);

					_NhlDataToWin(cnp->trans_obj,cxd,cyd,
						      2,tx,ty,&status,
						      NULL,NULL);
					if (! status) {
						_NhlWinToNDC(cnp->trans_obj,tx,ty,
							     2,fx,fy,&status,NULL,NULL);
						if (! status) {
							bbox->l = MIN(fx[0],fx[1]);
							bbox->r = MAX(fx[0],fx[1]);
							bbox->b = MIN(fy[0],fy[1]);
							bbox->t = MAX(fy[0],fy[1]);
						}
					}
				}
			}
			if (status) {
				*xlinear = *ylinear = False;
			}
		}
	}
	GetCellInfo(cnp,cxd,cyd,*xlinear,*ylinear,mcount,ncount,
		    xsoff,xeoff,ysoff,yeoff,&xexact,&yexact);

		
	fstep = (bbox->r - bbox->l) / xexact;
	if (tfp->x_reverse) {
		bbox->l -= *xeoff * fstep;
		bbox->r += *xsoff * fstep;
	}
	else {
		bbox->l -= *xsoff * fstep;
		bbox->r += *xeoff * fstep;
	}
	fstep = (bbox->t - bbox->b) / yexact;
	if (tfp->y_reverse) {
		bbox->b -= *yeoff * fstep;
		bbox->t += *ysoff * fstep;
	}
	else {
		bbox->b -= *ysoff * fstep;
		bbox->t += *yeoff * fstep;
	}

	return NhlNOERROR;
}
