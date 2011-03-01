/*
 *      $Id: CurvilinearTransObj.c,v 1.4 2004-12-23 22:42:29 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CurvilinearTransObj.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 11 15:14:27 MST 2002
 *
 *	Description:	
 *
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/CurvilinearTransObjP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/ConvertersP.h>
#include <math.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlCurvilinearTransObjLayerRec,
			    crtrans.x_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatGenArray,
		  sizeof(NhlGenArray),
		  NhlOffset(NhlCurvilinearTransObjLayerRec,
			    crtrans.y_coord_points_ga),NhlTImmediate,
		  _NhlUSET(NULL) ,0,(NhlFreeFunc)NhlFreeGenArray }

/* End-documented-resources */
        
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  CrTransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes CrTransInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);


static NhlErrorTypes CrTransDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);


/*
* TransObjClass Methods defined
*/

static NhlErrorTypes CrSetTrans(
#if	NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);


static NhlErrorTypes CrWinToNDC(
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


static NhlErrorTypes CrNDCToWin(
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


static NhlErrorTypes CrDataToCompc(
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

static NhlErrorTypes CrCompcToData(
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

static NhlErrorTypes CrWinToCompc(
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



static NhlErrorTypes CrNDCLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes CrDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes CrWinLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes CrDataPolygon(
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
	NhlCurvilinearTransObjLayerPart *crp,
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

static NhlErrorTypes CrTransGetValues(
#if NhlNeedProto
	NhlLayer /* l */,
	_NhlArgList /*args */,
	int	/*nargs*/
#endif
);

static NhlErrorTypes 	CrTransClassInitialize(
#if NhlNeedProto
	void
#endif
);

#define CREATE  1
#define SET 0

#define NhlcrDATA 0
#define NhlcrCOMPC 1

static NrmQuark QtrXCoordPoints;
static NrmQuark QtrYCoordPoints;
static NrmQuark Qdouble;

NhlCurvilinearTransObjClassRec NhlcurvilinearTransObjClassRec = {
        {
/* class_name			*/	"curvilinearTransformationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCurvilinearTransObjLayerRec),
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
/* class_initialize		*/	CrTransClassInitialize,
/* layer_initialize		*/	CrTransInitialize,
/* layer_set_values		*/	CrTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	CrTransGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CrTransDestroy
        },
        {
/* set_trans		*/	CrSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	CrWinToNDC,
/* ndc_to_win		*/	CrNDCToWin,
/* data_to_win		*/	CrDataToCompc, 
/* win_to_data		*/	CrCompcToData, 
/* data_to_compc	*/	CrDataToCompc,
/* compc_to_data	*/	CrCompcToData,
/* win_to_compc		*/	CrWinToCompc,
/* compc_to_win		*/	CrWinToCompc,
/* data_lineto 		*/      CrDataLineTo,
/* compc_lineto		*/      CrWinLineTo,
/* win_lineto 		*/      CrWinLineTo,
/* NDC_lineto 		*/      CrNDCLineTo,
/* data_polygon		*/      CrDataPolygon

        }
};

NhlClass NhlcurvilinearTransObjClass =
			(NhlClass)&NhlcurvilinearTransObjClassRec;


#define INCREASING 0
#define DECREASING 1
#define NONMONOTONIC 2
#define NOSPAN 3


/*
 * Function:	CrTransSetValues
 *
 * Description:	SetValues method for CurvilinearTrans Objects
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
static NhlErrorTypes CrTransSetValues
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
	NhlCurvilinearTransObjLayer inew = (NhlCurvilinearTransObjLayer) new;
	NhlTransObjLayerPart	*tp = &inew->trobj;

	if (_NhlArgIsSet(args,num_args,NhlNtrDoBounds)) {
		/*
		 * This arg should be set by itself
		 */
		if (! tp->xc_isbounds && ! tp->yc_isbounds)
			return NhlNOERROR;
	}

	return(SetUpTrans(new,old,SET,args,num_args));	
}


/*
 * Function:	CrTransInitialize
 *
 * Description: Initialize function for CurvilinearTransObjs. Performs same
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
static NhlErrorTypes CrTransInitialize
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
 * Function:	CrTransDestroy
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
static NhlErrorTypes CrTransDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlCurvilinearTransObjLayer cr = 
		(NhlCurvilinearTransObjLayer)inst;

	NhlFreeGenArray(cr->crtrans.x_crv_ga);
	NhlFreeGenArray(cr->crtrans.y_crv_ga);

	if (cr->crtrans.ixmin)
		NhlFree(cr->crtrans.ixmin);
	if (cr->crtrans.ixmax)
		NhlFree(cr->crtrans.ixmax);
	if (cr->crtrans.iymin)
		NhlFree(cr->crtrans.iymin);
	if (cr->crtrans.iymax)
		NhlFree(cr->crtrans.iymax);

 	free(cr->crtrans.xmin_dat);
	free(cr->crtrans.xmax_dat);
	free(cr->crtrans.ymin_dat);
	free(cr->crtrans.ymax_dat);
 	free(cr->crtrans.compc_xmin_dat);
	free(cr->crtrans.compc_xmax_dat);
	free(cr->crtrans.compc_ymin_dat);
	free(cr->crtrans.compc_ymax_dat);

	return NhlNOERROR;
}

#define MIN4(a,b,c,d) MIN(MIN((a),(b)),MIN((c),(d)))
#define MAX4(a,b,c,d) MAX(MAX((a),(b)),MAX((c),(d)))

static void SetUpIndexArrays
#if	NhlNeedProto
(
	NhlCurvilinearTransObjLayerPart *crp
)
#else
(crp)
	NhlCurvilinearTransObjLayerPart *crp;
#endif
{
	int ixbeg,ixend,iybeg,iyend;
	int ixmin,ixmax,iymin,iymax;
	double xmin,xmax,ymin,ymax;
	double *xp,*yp;
	int i,j;
	int size = crp->msize * crp->nsize;
	int xsz = crp->xaxis_size; 
	int ii,jj;
	int *ixmn,*ixmx,*iymn,*iymx;
	int msz = crp->msize;
	double dmsz = (double) crp->msize;
	double dnsz = (double) crp->nsize;

	ixmn = crp->ixmin;
	ixmx = crp->ixmax;
	iymn = crp->iymin;
	iymx = crp->iymax;

	xp = (double *) crp->x_crv_ga->data;
	yp = (double *) crp->y_crv_ga->data;
	
	ixbeg = (int)crp->compc_x_min;
	ixend = (int)crp->compc_x_max;
	iybeg = (int)crp->compc_y_min;
	iyend = (int)crp->compc_y_max;

	for (i = 0; i < size; i++) {
		*(crp->ixmin + i) = ixend;
		*(crp->ixmax + i) = ixbeg;
		*(crp->iymin + i) = iyend;
		*(crp->iymax + i) = iybeg;
	}
	
	for (j = 0; j < crp->yaxis_size - 1; j++) {
		for (i = 0; i < crp->xaxis_size - 1; i++) {
			xmin = MIN4(*(xp + j * xsz + i),
				    *(xp + j * xsz + i + 1),
				    *(xp + (j+1) * xsz + i),
				    *(xp + (j+1) * xsz + i + 1));
			xmax = MAX4(*(xp + j * xsz + i),
				    *(xp + j * xsz + i + 1),
				    *(xp + (j+1) * xsz + i),
				    *(xp + (j+1) * xsz + i + 1));
			ymin = MIN4(*(yp + j * xsz + i),
				    *(yp + j * xsz + i + 1),
				    *(yp + (j+1) * xsz + i),
				    *(yp + (j+1) * xsz + i + 1));
			ymax = MAX4(*(yp + j * xsz + i),
				    *(yp + j * xsz + i + 1),
				    *(yp + (j+1) * xsz + i),
				    *(yp + (j+1) * xsz + i + 1));
			ixmin = MAX(0,MIN(crp->msize-1,
					  (int)(dmsz * xmin)));
			ixmax = MAX(0,MIN(crp->msize-1,
					  (int)(dmsz * xmax)));
			iymin = MAX(0,MIN(crp->nsize-1,
					  (int)(dnsz * ymin)));
			iymax = MAX(0,MIN(crp->nsize-1,
					  (int)(dnsz * ymax)));
			for (jj = iymin; jj <= iymax; jj++) {
				for (ii = ixmin; ii <= ixmax; ii++) {
					*(ixmn + jj*msz + ii)
					 = MIN(*(ixmn + jj*msz + ii),i);
					*(ixmx + jj*msz + ii)
					 = MAX(*(ixmx + jj*msz + ii),i+1);
					*(iymn + jj*msz + ii)
					 = MIN(*(iymn + jj*msz + ii),j);
					*(iymx + jj*msz + ii)
					 = MAX(*(iymx + jj*msz + ii),j+1);
				}
			}
		}
	}
	return;
}

static void DetermineCoordHandedness
#if	NhlNeedProto
(NhlCurvilinearTransObjLayerPart *crp)
#else
(crp)
	NhlCurvilinearTransObjLayerPart *crp;
#endif
{
	double *xdata = (double *)crp->x_crv_ga->data;
	double *ydata = (double *)crp->y_crv_ga->data;
	int ysz = crp->x_crv_ga->len_dimensions[0];
	int xsz = crp->x_crv_ga->len_dimensions[1];
	int ydir,xdir;
	int i,j;

	for (j = 0,xdir = 0; j < ysz; j++) {
		if (*(xdata + j * xsz + xsz - 1) >= *(xdata + j * xsz))
			xdir++;
		else
			xdir--;
	}
	for (i = 0,ydir = 0; i < xsz; i++) {
		if (*(ydata + (ysz -1) * xsz + i) >= *(ydata + i))
			ydir++;
		else
			ydir--;
	}
	if ((xdir < 0 && ydir > 0) || (xdir > 0 && ydir < 0)) {
		crp->handedness_sign = -1;
	}
	else {
		crp->handedness_sign = 1;
	}
	return;
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
	NhlCurvilinearTransObjLayer inew = (NhlCurvilinearTransObjLayer)new;
	NhlCurvilinearTransObjLayer iold = (NhlCurvilinearTransObjLayer)old;
	NhlCurvilinearTransObjLayerPart *crp = &inew->crtrans;
	NhlTransObjLayerPart	*tp = &inew->trobj;
	NhlTransObjLayerPart	*otp = &iold->trobj;
	char *error_lead;
	NhlErrorTypes ret = NhlNOERROR;
	NhlBoolean new_coords = False;
        NhlBoolean x_crv_coords_set = False,y_crv_coords_set = False;
        NhlBoolean data_extent_def;
        NhlBoolean new_data_extent;
	int old_msize,old_nsize;
	
	tp->change_count++;
	
        data_extent_def = (tp->data_xstart == 0.0 && tp->data_xend == 0.0)
		|| (tp->data_ystart == 0.0 && tp->data_yend == 0.0) ?
                False : True;
	
	if(c_or_s == SET) {

		error_lead = "CrTransSetValues";

		if (_NhlArgIsSet(args,nargs,NhlNtrXCoordPoints))
                        x_crv_coords_set = True;
		if (_NhlArgIsSet(args,nargs,NhlNtrYCoordPoints))
                        y_crv_coords_set = True;
		if (x_crv_coords_set || y_crv_coords_set)
			new_coords = True;
                if (! data_extent_def &&
                    (tp->x_min != otp->x_min ||
                     tp->x_max != otp->x_max ||
		     tp->y_min != otp->y_min ||
                     tp->y_max != otp->y_max))
			new_data_extent = True;
		else {
			new_data_extent =
				tp->data_xstart != otp->data_xstart ||
				tp->data_xend != otp->data_xend ||
				tp->data_ystart != otp->data_ystart ||
				tp->data_yend != otp->data_yend;
		}
		old_msize = crp->msize;
		old_nsize = crp->nsize;
	}
	else {
		error_lead = "CrTransInitialize";
		new_coords = True;
                new_data_extent = True;
                if (crp->x_coord_points_ga)
                       x_crv_coords_set = True; 
                if (crp->y_coord_points_ga)
                       y_crv_coords_set = True; 
		if (! (x_crv_coords_set && y_crv_coords_set)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: Both %s and %s must be set",
			  error_lead,NhlNtrXCoordPoints,NhlNtrYCoordPoints);
			return NhlFATAL;
		}
		crp->x_crv_ga = NULL;
		crp->y_crv_ga = NULL; 
                crp->x_crv_min = 0.0;
                crp->x_crv_max = 0.0;
                crp->y_crv_min = 0.0;
                crp->y_crv_max = 0.0;
                crp->xaxis_size = 0;
                crp->yaxis_size = 0;

                crp->ul = crp->ub = 0.0;
                crp->ur = crp->ut = 1.0;
		crp->ixmin = crp->ixmax = NULL;
		crp->iymin = crp->iymax = NULL;
		old_msize = 0;
		old_nsize = 0;
	}

	/*
	 * To improve accuracy all internal calculation is now done
	 * in double precision. Also the coordinate data is 
	 * normalized along each axis into the range [0.0, 1.0].
	 * This prevents errors when the extent of one axis varies
	 * widely from the other, or when one or another axis has a small 
	 * extent relative to its absolute value.
	 */

	if (data_extent_def) {
		crp->x_crv_min = MIN(tp->data_xstart,tp->data_xend);
		crp->x_crv_max = MAX(tp->data_xstart,tp->data_xend);
		crp->x_crv_ext = crp->x_crv_max - crp->x_crv_min;
		crp->y_crv_min = MIN(tp->data_ystart,tp->data_yend);
		crp->y_crv_max = MAX(tp->data_ystart,tp->data_yend);
		crp->y_crv_ext = crp->y_crv_max - crp->y_crv_min;
	}
	if (x_crv_coords_set) {
		float *fdata;
		double *data;
		int i;
		if (crp->x_crv_ga)
			NhlFreeGenArray(crp->x_crv_ga);
		crp->x_crv_ga = 
			_NhlCopyGenArray(crp->x_coord_points_ga,False);
		data = NhlMalloc(crp->x_crv_ga->num_elements * sizeof(double));
		if (! (crp->x_crv_ga && data)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		/* copy to double */
		fdata = crp->x_crv_ga->data;
		for (i = 0; i < crp->x_crv_ga->num_elements; i++) 
			*(data + i) = ((double) *(fdata + i) - crp->x_crv_min)
				/ crp->x_crv_ext;
		crp->x_crv_ga->data = (void *)data;
		crp->x_crv_ga->typeQ = Qdouble;
		crp->x_crv_ga->my_data = True;
		crp->x_coord_points_ga = NULL;
	}
	if (y_crv_coords_set) {
		float *fdata;
		double *data;
		int i;
		if (crp->y_crv_ga)
			NhlFreeGenArray(crp->y_crv_ga);
		crp->y_crv_ga = 
			_NhlCopyGenArray(crp->y_coord_points_ga,False);
		data = NhlMalloc(crp->y_crv_ga->num_elements * sizeof(double));
		if (! (crp->y_crv_ga && data)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		fdata = crp->y_crv_ga->data;
		for (i = 0; i < crp->y_crv_ga->num_elements; i++) 
			*(data + i) = ((double) *(fdata + i) - crp->y_crv_min)
				/ crp->y_crv_ext;
		crp->y_crv_ga->data = (void *)data;
		crp->y_crv_ga->typeQ = Qdouble;
		crp->y_crv_ga->my_data = True;
		crp->y_coord_points_ga = NULL;
	}
	if (crp->y_crv_ga->len_dimensions[1] != 
	    crp->x_crv_ga->len_dimensions[1] ||
	    crp->x_crv_ga->len_dimensions[0] != 
	    crp->x_crv_ga->len_dimensions[0]) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: %s and %s must have equal dimension lengths",
			  error_lead,NhlNtrXCoordPoints,NhlNtrYCoordPoints);
		return NhlFATAL;
	}
	if (new_coords)
		DetermineCoordHandedness(crp);
		

	if (tp->x_min < crp->x_crv_min) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: X minimum less than minimum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->x_min = (float)crp->x_crv_min;
	}
	if (tp->x_max > crp->x_crv_max) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: X maximum greater than maximum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->x_max = (float)crp->x_crv_max;
	}
	if (tp->y_min < crp->y_crv_min) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Y minimum less than minimum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->y_min = (float)crp->y_crv_min;
	}
	if (tp->y_max > crp->y_crv_max) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: Y maximum greater than maximum value of coordinate points array, defaulting",
			  error_lead);
		ret = MIN(ret,NhlWARNING);
		tp->y_max = (float)crp->y_crv_max;
	}

/*
 * this is temporary
 * actually need to find the index points in the grid  
 * corresponding to current x/y min/max and adjust these values accordingly.
 */
	crp->ul = 0;
	crp->ur = crp->x_crv_ga->len_dimensions[1] - 1;
	crp->ub = 0;
	crp->ut = crp->x_crv_ga->len_dimensions[0] - 1;
	crp->xaxis_size = crp->x_crv_ga->len_dimensions[1];
	crp->yaxis_size = crp->x_crv_ga->len_dimensions[0];

        
	if(tp->x_reverse) {
		float tmpf = crp->ur;
		crp->ur = crp->ul;
		crp->ul = tmpf;
	}
	if(tp->y_reverse) {
		float tmpf = crp->ut;
		crp->ut = crp->ub;
		crp->ub = tmpf;
	}
	crp->compc_x_min = MIN(crp->ul,crp->ur);
	crp->compc_x_max = MAX(crp->ul,crp->ur);
	crp->compc_y_min = MIN(crp->ut,crp->ub);
	crp->compc_y_max = MAX(crp->ut,crp->ub);
	crp->ixb = crp->compc_x_min;
	crp->ixe = crp->compc_x_max;
	crp->iyb = crp->compc_y_min;
	crp->iye = crp->compc_y_max;
	

	crp->log_lin_value = 1;
	crp->x_min = tp->x_min;
	crp->y_min = tp->y_min;
	crp->x_max = tp->x_max;
	crp->y_max = tp->y_max;
	crp->x_reverse = tp->x_reverse;
	crp->y_reverse = tp->y_reverse;

	if (new_coords || new_data_extent) {
		int size;
		crp->msize = crp->xaxis_size / 2.0;
		crp->nsize = crp->yaxis_size / 2.0;

		if (crp->msize != old_msize || crp->nsize != old_nsize) {
			if (crp->ixmin)
				NhlFree(crp->ixmin);
			if (crp->ixmax)
				NhlFree(crp->ixmax);
			if (crp->iymin)
				NhlFree(crp->iymin);
			if (crp->iymax)
				NhlFree(crp->iymax);
			size = crp->msize * crp->nsize;
			crp->ixmin = NhlMalloc(sizeof(int) * size);
			crp->ixmax = NhlMalloc(sizeof(int) * size);
			crp->iymin = NhlMalloc(sizeof(int) * size);
			crp->iymax = NhlMalloc(sizeof(int) * size);
			
			if (! (crp->ixmin && crp->ixmax &&
			       crp->iymin && crp->iymax)) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
		}
		SetUpIndexArrays(crp);
	}


	if (c_or_s == CREATE) {
		if ((crp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((crp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((crp->ymin_dat =_NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((crp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
		if ((crp->compc_xmin_dat =
		     _NhlCmpFSetup(crp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_xmin_dat = crp->compc_xmin_dat;
		if ((crp->compc_xmax_dat = 
		     _NhlCmpFSetup(crp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_xmax_dat = crp->compc_xmax_dat;
		if ((crp->compc_ymin_dat = 
		     _NhlCmpFSetup(crp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_ymin_dat = crp->compc_ymin_dat;
		if ((crp->compc_ymax_dat = 
		     _NhlCmpFSetup(crp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_ymax_dat = crp->compc_ymax_dat;

                tp->x_min_set = tp->x_max_set = False;
                tp->x_reverse_set = False;
                tp->y_min_set = tp->y_max_set = False;
                tp->y_reverse_set = False;
        
		return(ret);
	}
        if (tp->x_min != iold->crtrans.x_min) {
                free(crp->xmin_dat);
                if ((crp->xmin_dat = _NhlCmpFSetup(tp->x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->x_max != iold->crtrans.x_max) {
		free(crp->xmax_dat);
		if ((crp->xmax_dat = _NhlCmpFSetup(tp->x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_min != iold->crtrans.y_min) {
		free(crp->ymin_dat);
		if ((crp->ymin_dat = _NhlCmpFSetup(tp->y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (tp->y_max != iold->crtrans.y_max) {
		free(crp->ymax_dat);
		if ((crp->ymax_dat = _NhlCmpFSetup(tp->y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
	}
	if (crp->compc_x_min != iold->crtrans.compc_x_min) {
		free(crp->compc_xmin_dat);
		if ((crp->compc_xmin_dat =
		     _NhlCmpFSetup(crp->compc_x_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_xmin_dat = crp->compc_xmin_dat;
	}
	if (crp->compc_x_max != iold->crtrans.compc_x_max) {
		free(crp->compc_xmax_dat);
		if ((crp->compc_xmax_dat = 
		     _NhlCmpFSetup(crp->compc_x_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_xmax_dat = crp->compc_xmax_dat;
	}
	if (crp->compc_y_min != iold->crtrans.compc_y_min) {
		free(crp->compc_ymin_dat);
		if ((crp->compc_ymin_dat = 
		     _NhlCmpFSetup(crp->compc_y_min,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_ymin_dat = crp->compc_ymin_dat;
	}
	if (crp->compc_y_max != iold->crtrans.compc_y_max) {
		free(crp->compc_ymax_dat);
		if ((crp->compc_ymax_dat = 
		     _NhlCmpFSetup(crp->compc_y_max,5)) == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: error setting up compare information",
				  error_lead);
			return(NhlFATAL);
		}
                crp->save_compc_ymax_dat = crp->compc_ymax_dat;
	}
        
        tp->x_min_set = tp->x_max_set = False;
        tp->x_reverse_set = False;
        tp->y_min_set = tp->y_max_set = False;
        tp->y_reverse_set = False;
        
	return(ret);

}

/*
 * Function:	CrSetTrans
 *
 * Description: set_trans method for CurvilinearTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the CurvilinearTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes CrSetTrans
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
	NhlCurvilinearTransObjLayer	to=(NhlCurvilinearTransObjLayer)tobj;
	NhlTransObjLayerPart		*top = &to->trobj;
	NhlCurvilinearTransObjLayerPart	*tp = &to->crtrans;
	NhlErrorTypes ret;

	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	return(_NhlTransLLUSet(top->x,top->x+top->width,
			       top->y-top->height,top->y,
			       tp->ul,tp->ur,tp->ub,tp->ut,
			       tp->log_lin_value,
                               &top->off_screen,
                               "CrSetTrans"));

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
	NhlCurvilinearTransObjLayerPart *crp,
	float	*x,
 	float	*y,
	int	type /* data 0, compc 1 */
)
#else
(crp,x,y,type)
	NhlCurvilinearTransObjLayerPart *crp;
	float	*x;
	float	*y;
	int	type;
#endif
{
	int xmndif,xmxdif,ymndif,ymxdif;

	if (type == NhlcrDATA) {
		if ((xmndif = _NhlCmpF(*x,crp->xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,crp->xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,crp->ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,crp->ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = crp->x_min;
		}
		else if (xmxdif == 0) {
			*x = crp->x_max;
		}
		if (ymndif == 0) {
			*y = crp->y_min;
		}
		else if (ymxdif == 0) {
			*y = crp->y_max;
		}
	}
	else {
		if ((xmndif = _NhlCmpF(*x,crp->compc_xmin_dat)) < 0 ||
		    (xmxdif = _NhlCmpF(*x,crp->compc_xmax_dat)) > 0 ||
		    (ymndif = _NhlCmpF(*y,crp->compc_ymin_dat)) < 0 ||
		    (ymxdif = _NhlCmpF(*y,crp->compc_ymax_dat)) > 0) {
			return False;
		}
		if (xmndif == 0) {
			*x = crp->compc_x_min;
		}
		else if (xmxdif == 0) {
			*x = crp->compc_x_max;
		}
		if (ymndif == 0) {
			*y = crp->compc_y_min;
		}
		else if (ymxdif == 0) {
			*y = crp->compc_y_max;
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
 * Function:	CrWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the CurvilinearTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes CrWinToNDC
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
	NhlCurvilinearTransObjLayer	iinstance =
				(NhlCurvilinearTransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;
	int i;
	
	*status = 0;	
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n ; i++) {
/*
* Compc and Window are identical coordinates in this object
*/
			if((x[i] > iinstance->crtrans.compc_x_max)
			   ||(x[i] < iinstance->crtrans.compc_x_min)
			   ||(y[i] > iinstance->crtrans.compc_y_max)
			   ||(y[i] < iinstance->crtrans.compc_y_min)) {
				if (! compare_check(&iinstance->crtrans,
						    &x[i],&y[i],NhlcrCOMPC)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}

			} 
			strans(iinstance->crtrans.ul,
			       iinstance->crtrans.ur,
			       iinstance->crtrans.ub,
			       iinstance->crtrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n ; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
			   ||((ymissing != NULL)&&(*ymissing == y[i]))
			   ||(x[i] > iinstance->crtrans.compc_x_max)
			   ||(x[i] < iinstance->crtrans.compc_x_min)
			   ||(y[i] > iinstance->crtrans.compc_y_max)
			   ||(y[i] < iinstance->crtrans.compc_y_min)) {
				if (! compare_check(&iinstance->crtrans,
						    &x[i],&y[i],NhlcrCOMPC)) {
					*status = 1;
					xout[i]=yout[i] =
						iinstance->trobj.out_of_range;
					continue;
				}
			}
			strans(iinstance->crtrans.ul,
			       iinstance->crtrans.ur,
			       iinstance->crtrans.ub,	
			       iinstance->crtrans.ut,
			       tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
			       x[i],y[i],&(xout[i]),&(yout[i]));
		}
	}

	return(NhlNOERROR);
}


/*
 * Function:	CrNDCToWin
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
static NhlErrorTypes CrNDCToWin
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
	NhlCurvilinearTransObjLayer	iinstance = 
				(NhlCurvilinearTransObjLayer)instance;
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
			strans(tp->x,x1,y1,tp->y,iinstance->crtrans.ul,
			       iinstance->crtrans.ur, iinstance->crtrans.ub,
			       iinstance->crtrans.ut, x[i],y[i],
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
			       iinstance->crtrans.ul,
			       iinstance->crtrans.ur, 
			       iinstance->crtrans.ub,
			       iinstance->crtrans.ut, x[i],y[i],
			       &(xout[i]),&(yout[i]));
		}
	}
	return(NhlNOERROR);
}

/*
 * this returns a negative value if point (xp,yp) is left of 
 * line (x0,y0),(x1,y1)
 */

#define RIGHTOF(sign,xp,yp,x0,y0,x1,y1)			\
 	(sign * ((x0)-(xp))*((yp)-(y1))-((x1)-(xp))*((yp)-(y0)))
#define RIGHTOFD(sign,xp,yp,x0,y0,x1,y1)		\
 	(sign * ((double)((x0)-(xp))*(double)((yp)-(y1))-	\
		 (double)((x1)-(xp))*(double)((yp)-(y0))))


/*
 * Function:	IsInCompcBox
 *
 * Description: return True if point (x,y) is in the box with
 * rectangular computational coordinates bounded by (ixbeg,iybeg) and
 * (ixend,iyend)   
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
static NhlBoolean IsInCompcBox
#if	NhlNeedProto
(
	NhlCurvilinearTransObjLayerPart *crp,
	double x,
	double y,
	int ixbeg,
	int iybeg,
	int ixend,
	int iyend
)
#else
(crp,x,y,ixbeg,iybeg,ixend,iyend)
	NhlCurvilinearTransObjLayerPart *crp;
	double x;
	double y;
	int ixbeg;
	int iybeg;
	int ixend;
	int iyend;

#endif
{
	int ipmn = 0;
	int ixm = ixbeg;
	int iym = iybeg;
	double *xp = (double *)crp->x_crv_ga->data;
	double *yp = (double *)crp->y_crv_ga->data;
	int xsz = crp->xaxis_size;
	double xt = *(xp + iym * xsz + ixm);
	double yt = *(yp + iym * xsz + ixm);
	double sdmn = (x - xt) * (x - xt) + (y - yt) * (y -yt);
	int ixn = ixm;
	int iyn = iym;
	int ixp, iyp;
	double sdst;
	double sideprev,sidenext,sidebend;
	int i;
	int sign = crp->handedness_sign;

/*
 * traverse the edge of the compc box counterclockwise to find
 * the compc box point closest to the point being tested. 
 * (ixm,iym) -- indices of current minimum distance grid point
 * (ixn,iyn) -- next grid point
 * (ixp,iyp) -- last grid point
 */ 
	for (i = 1; i < 2 * (ixend - ixbeg) + 2 * (iyend - iybeg); i++) {
		if (i <= ixend - ixbeg)
			ixn++;
		else if (i <= ixend - ixbeg + iyend - iybeg)
			iyn++;
		else if (i <= 2 * (ixend - ixbeg) + iyend - iybeg)
			ixn--;
		else
			iyn--;
		
		xt = *(xp + iyn * xsz + ixn);
		yt = *(yp + iyn * xsz + ixn);
		sdst = (x - xt) * (x - xt) + (y - yt) * (y - yt);
		if (sdst < sdmn) {
			sdmn = sdst;
			ipmn = i;
			ixm = ixn;
			iym = iyn;
		}
	}
/*
 * get the next point and the previous point indices
 * (picture traveling around the box counter-clockwise)
 */
	if (ipmn == 0) {
		ixp = ixm;
		iyp = iym + 1;
		ixn = ixm + 1;
		iyn = iym;
	}
	else if (ipmn < ixend - ixbeg) {
		ixp = ixm - 1;
		iyp = iym;
		ixn = ixm + 1;
		iyn = iym;
	}
	else if (ipmn == ixend - ixbeg) {
		ixp = ixm - 1;
		iyp = iym;
		ixn = ixm;
		iyn = iym + 1;
	}
	else if (ipmn < ixend - ixbeg + iyend - iybeg) {
		ixp = ixm;
		iyp = iym - 1;
		ixn = ixm;
		iyn = iym + 1;
	}
	else if (ipmn ==  ixend - ixbeg + iyend - iybeg) {
		ixp = ixm;
		iyp = iym - 1;
		ixn = ixm - 1;
		iyn = iym;
	}
	else if (ipmn < 2 * (ixend - ixbeg) + iyend - iybeg) {
		ixp = ixm + 1;
		iyp = iym;
		ixn = ixm - 1;
		iyn = iym;
	}
	else if (ipmn ==  2 * (ixend - ixbeg) + iyend - iybeg) {
		ixp = ixm + 1;
		iyp = iym;
		ixn = ixm;
		iyn = iym - 1;
	}
	else {
		ixp = ixm;
		iyp = iym + 1;
		ixn = ixm;
		iyn = iym - 1;
	}
/*
 * if (x,y) is to the left of both line segments, it's inside the box.
 * If it's to the right of both is outside. If it's to the left of one
 * and to the right of the other, it's outside if the box edge bends to
 * the left where the segments join, and inside if the bend is to the
 * right.
 */
	
	sideprev = RIGHTOFD(sign,x,y,*(xp+iyp*xsz+ixp),*(yp+iyp*xsz+ixp),
			   *(xp+iym*xsz+ixm),*(yp+iym*xsz+ixm));
	sidenext = RIGHTOFD(sign,x,y,*(xp+iym*xsz+ixm),*(yp+iym*xsz+ixm),
			   *(xp+iyn*xsz+ixn),*(yp+iyn*xsz+ixn));

	if (sideprev <= 0.0 && sidenext <= 0.0)
		return True;
	else if (sideprev >= 0.0 && sidenext >= 0.0)
		return False;

	sidebend = RIGHTOFD(sign,*(xp+iyn*xsz+ixn),*(yp+iyn*xsz+ixn),
			   *(xp+iyp*xsz+ixp),*(yp+iyp*xsz+ixp),
			   *(xp+iym*xsz+ixm),*(yp+iym*xsz+ixm));

	if (sidebend <= 0) 
		return False;

	return True;
	
}

/*
 * Function:	SetCompcBox
 *
 * Description: return True if data point (x,y) can be located in a 
 * computational coordinate gridbox. The grid box borders will be
 * specified by the integer members of the NhlCurvilinearTransObjLayerPart:
 * ixb,iyb,ixe,iyb.
 * If the point is located outside the data grid False is returned.
 *		
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: struct members ixb,iyb,ixe,iyb are set.
 */
/*ARGSUSED*/
static NhlBoolean SetCompcBox
#if	NhlNeedProto
(
	NhlCurvilinearTransObjLayerPart *crp,
	double x,
	double y
)
#else
(crp,x,y)
	NhlCurvilinearTransObjLayerPart *crp;
	double x;
	double y;

#endif
{
	int msz = crp->msize;
	int ix,iy;
	int ixb,iyb,ixe,iye,ixt,iyt;


	ix = MAX(0,MIN(crp->msize - 1,(int)
		       (((double)crp->msize * x))));
	iy = MAX(0,MIN(crp->nsize - 1,(int)
		       (((double)crp->nsize * y))));
	ixb = *(crp->ixmin + iy*msz + ix); 
	ixe = *(crp->ixmax + iy*msz + ix); 
	iyb = *(crp->iymin + iy*msz + ix); 
	iye = *(crp->iymax + iy*msz + ix); 

	if (ixe <= ixb || iye <= iyb)
		return False;

	if (! IsInCompcBox(crp,x,y,ixb,iyb,ixe,iye))
		return False;

	while (ixe != ixb+1 || iye != iyb+1) {
		if (ixe-ixb > iye-iyb) {
			ixt = (ixb + ixe) / 2;
			if (IsInCompcBox(crp,x,y,ixb,iyb,ixt,iye))
				ixe = ixt;
			else
				ixb = ixt;
		}
		else {
			iyt = (iyb + iye) / 2;
			if (IsInCompcBox(crp,x,y,ixb,iyb,ixe,iyt))
				iye = iyt;
			else
				iyb = iyt;
		}
	}
	crp->ixb = ixb;
	crp->ixe = ixe;
	crp->iyb = iyb;
	crp->iye = iye;

	return True;
}

#define EPS 1.0e-8


/*
 * Function:	GetFracCoordsD
 *
 * Description: Once it is known what compc grid box a data point lies,
 *              this routine determines the fractional component within
 *              the grid box, and sets the final data values.
 *              Note this routine assumes that 
 *              (ixe == ixb+1 && iye == iyb+1)
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
static NhlErrorTypes GetFracCoordsD
#if	NhlNeedProto
(
	NhlCurvilinearTransObjLayerPart *crp,
        double   x,
        double   y,
        double*  xout,
        double*  yout
)
#else
(crp,x,y,xout,yout)
	NhlCurvilinearTransObjLayerPart *crp;
        double   x;
        double   y;
        double*  xout;
        double*  yout;

#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	double *xp = (double *)crp->x_crv_ga->data;
	double *yp = (double *)crp->y_crv_ga->data;
	int xsz = crp->xaxis_size;
	int ixb = crp->ixb;
	int iyb = crp->iyb;
	int ixe = crp->ixe;
	int iye = crp->iye;
	double fx,fy,fx_last;
	int i;
	double xlb = *(xp + iyb * xsz + ixb);
	double xrb = *(xp + iyb * xsz + ixe);
	double xlt = *(xp + iye * xsz + ixb);
	double xrt = *(xp + iye * xsz + ixe);
	double ylb = *(yp + iyb * xsz + ixb);
	double yrb = *(yp + iyb * xsz + ixe);
	double ylt = *(yp + iye * xsz + ixb);
	double yrt = *(yp + iye * xsz + ixe);
	double sfos,sdos;
	double xtm,ytm;
	NhlBoolean do_bin = False;
	int sign = 1;
	
/*
 * first use Newton's rule (10 tries), and if that doesn't work try a binary
 * search technique
 */
	fx = 0.5;
	for (i = 0; i < 10; i++) {
		sfos = RIGHTOF(sign,(double)x,(double)y,
			       ((1.0 - fx) * xlt + fx * xrt),
			       ((1.0 - fx) * ylt + fx * yrt),
			       ((1.0 - fx) * xlb + fx * xrb),
			       ((1.0 - fx) * ylb + fx * yrb));

		sdos = 2.0 * fx * ((xrt - xlt) * (ylb - yrb) -
				   (xrb - xlb) * (ylt - yrt)) +
			(xlt - (double)x) * (ylb - yrb) +
			(xrt - xlt) * ((double)y - ylb) -
			(xlb - (double)x) * (ylt - yrt) - 
			(xrb - xlb) * ((double)y - ylt);
		if (sdos == 0.0) {
			do_bin = True;
			break;
		}
		fx_last = fx;
		fx = fx - sfos / sdos;
		if (fabs(fx - fx_last) < EPS) {
			if (fx < -EPS || fx > 1.0 + EPS) {
				do_bin = True;
			}
			break;
		}
	}

	if (do_bin) {
		double fx1 = 0.0;
		double fx2 = 1.0;

		while (fabs(fx2-fx1) > EPS) {

			fx = 0.5 * (fx1 + fx2);
			sfos =  RIGHTOF(sign,(double)x,(double)y,
					((1.0 - fx) * xlt + fx * xrt),
					((1.0 - fx) * ylt + fx * yrt),
					((1.0 - fx) * xlb + fx * xrb),
					((1.0 - fx) * ylb + fx * yrb));
			if (sfos < 0.0)
				fx1 = fx;
			else
				fx2 = fx;
		}
	}
			
	xtm = (1.0 - fx) * (xlt - xlb) + fx * (xrt - xrb);
	ytm = (1.0 - fx) * (ylt - ylb) + fx * (yrt - yrb);

	if (fabs(xtm) > fabs(ytm))
		fy = ((double)x - (1.0 - fx) * xlb - fx * xrb) / xtm;
	else
		fy = ((double)y - (1.0 - fx) * ylb - fx * yrb) / ytm;


	*xout = (double)ixb + MAX(0.0,MIN(1.0,fx));
	*yout = (double)iyb + MAX(0.0,MIN(1.0,fy));
			

	return ret;
}

/*
 * Function:	CrDataToCompc
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
static NhlErrorTypes CrDataToCompc
#if	NhlNeedProto
(
	NhlLayer instance,
	float *x,
	float *y,
	int n,
	float* xout,
	float* yout,
	float *xmissing,
	float *ymissing, 
	int *status)
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
	NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
	NhlCurvilinearTransObjLayer iinstance =
                (NhlCurvilinearTransObjLayer)instance;
	NhlCurvilinearTransObjLayerPart *crp = &iinstance->crtrans;
	int i;
	double xt,yt;
	double xo,yo;

	*status = 0;
	for(i=0; i< n;i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < crp->x_min)	
			||(x[i] > crp->x_max)
			||(y[i] < crp->y_min)
			||(y[i] > crp->y_max)) {
		
			if (! compare_check(crp,&x[i],&y[i],NhlcrDATA)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		xt = (x[i] -  crp->x_crv_min) / crp->x_crv_ext;
		yt = (y[i] -  crp->y_crv_min) / crp->y_crv_ext;
		if ((crp->ixe == crp->ixb+1 && crp->iye == crp->iyb+1) &&
		    IsInCompcBox(crp,xt,yt,crp->ixb,crp->iyb,
				 crp->ixe,crp->iye)) {
			subret = GetFracCoordsD(crp,xt,yt,
						&xo,&yo);
			xout[i] = xo;
			yout[i] = yo;
			ret = MIN(ret,subret);
			continue;
		}
		if (! SetCompcBox(crp,xt,yt)) {
			*status = 1;
			xout[i]= yout[i] = iinstance->trobj.out_of_range;
			continue;
		}
		subret = GetFracCoordsD(crp,xt,yt,&xo,&yo);

		xout[i] = xo;
		yout[i] = yo;
			
		ret = MIN(ret,subret);

	}
	return(ret);
}

/*
 * Function:	CrCompcToData
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
static NhlErrorTypes CrCompcToData
#if	NhlNeedProto
(NhlLayer instance,
 float *x,
 float *y,
 int n,
 float* xout,
 float* yout,
 float *xmissing,
 float *ymissing,
 int* status)
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
	NhlCurvilinearTransObjLayer iinstance = 
		(NhlCurvilinearTransObjLayer)instance;
	NhlCurvilinearTransObjLayerPart *crp = &iinstance->crtrans;
	int i;
	int ix,iy;
	double *xp = (double *)crp->x_crv_ga->data;
	double *yp = (double *)crp->y_crv_ga->data;
	int xsz = crp->xaxis_size;
	double x0,y0;
	double xo,yo;

	*status = 0;
	for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] > crp->compc_x_max)
			||(x[i] < crp->compc_x_min)
			||(y[i] > crp->compc_y_max)
			||(y[i] < crp->compc_y_min)) {

			if (! compare_check(crp,&x[i],&y[i],NhlcrCOMPC)) {
				*status = 1;
				xout[i]=yout[i] =
					iinstance->trobj.out_of_range;
				continue;
			}
		}
		ix = MAX(0,MIN(crp->xaxis_size-2,(int)x[i]));
		iy = MAX(0,MIN(crp->yaxis_size-2,(int)y[i]));
		
		x0 = *(xp + iy * xsz + ix);
		xo = (float)(x0 +
			(*(xp + iy * xsz + ix+1) - x0) * (x[i] - (double)ix) +
			(*(xp + (iy+1)*xsz + ix) - x0) * (y[i] - (double)iy) +
			(*(xp + (iy+1)*xsz + ix+1) - *(xp + (iy+1)*xsz + ix) -
			 *(xp + iy * xsz + ix+1) + x0) *
			   (x[i] - (double)ix) * (y[i] - (double)iy));

		y0 = *(yp + iy * xsz + ix);
		yo = (float)(y0 +
			(*(yp + iy * xsz + ix+1) - y0) * (x[i] - (double)ix) +
			(*(yp + (iy+1)*xsz + ix) - y0) * (y[i] - (double)iy) +
			(*(yp + (iy+1)*xsz + ix+1) - *(yp + (iy+1)*xsz + ix) -
			 *(yp + iy * xsz + ix+1) + y0) *
			   (x[i] - (double)ix) * (y[i] - (double)iy));
		xout[i] = xo * crp->x_crv_ext + crp->x_crv_min;
		yout[i] = yo * crp->y_crv_ext + crp->y_crv_min;
	}
	return(ret);
}

/*ARGSUSED*/
static NhlErrorTypes CrWinToCompc
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
        NhlCurvilinearTransObjLayer iinstance = (NhlCurvilinearTransObjLayer)instance;
        int i;

        *status = 0;
        for(i = 0 ; i< n; i++) {
                if(((xmissing != NULL)&&(*xmissing == x[i]))
                        || ((ymissing != NULL)&&(*ymissing == y[i]))
                        ||(x[i] < iinstance->crtrans.compc_x_min)
                        ||(x[i] > iinstance->crtrans.compc_x_max)
                        ||(y[i] < iinstance->crtrans.compc_y_min)
                        ||(y[i] > iinstance->crtrans.compc_y_max)) {

			if (! compare_check(&iinstance->crtrans,
					    &x[i],&y[i],NhlcrCOMPC)) {
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
(NhlCurvilinearTransObjLayer crinst, 
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
(crinst,xclip,yclip,x,y,xd, yd,xc,yc)
NhlCurvilinearTransObjLayer crinst;
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
	NhlCurvilinearTransObjLayerPart *crp = 
		(NhlCurvilinearTransObjLayerPart *) &crinst->crtrans;
	float xt,yt;
	int i,status = 1;

	xt = xclip;
	yt = yclip;

	for (i=0; i < 2; i++) {

		if (x != xclip) {
			if (_NhlCmpF(xt,crp->xmin_dat) < 0.0) {
				*xd = crp->x_min;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
			else if (_NhlCmpF(xt,crp->xmax_dat) > 0.0) {
				*xd = crp->x_max;
				*yd = yclip +(y-yclip) * (*xd-xclip)/(x-xclip);
			}
		}
		if (y != yclip) {
			if (_NhlCmpF(yt,crp->ymin_dat) < 0.0) {
				*yd = crp->y_min;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
			else if (_NhlCmpF(yt,crp->ymax_dat) > 0.0) {
				*yd = crp->y_max;
				*xd = xclip +(x-xclip) * (*yd-yclip)/(y-yclip);
			}
		}
		CrDataToCompc((NhlLayer)crinst,xd,yd,1,
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


static NhlErrorTypes CrDataLineTo
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
	NhlCurvilinearTransObjLayer crinst = (NhlCurvilinearTransObjLayer)instance;
	static float lastx,lasty;
	static int call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	float holdx,holdy;
	int status;
	int i,npoints = 256;
	float xdist,ydist,xc,yc,xd,yd;

	npoints = crinst->trobj.point_count;
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
 * fcrst find out whether the line is clipped
 */
	currentx = x;
	currenty = y;
	holdx = lastx;
	holdy = lasty;
	_NhlTransClipLine(crinst->crtrans.x_min,
			  crinst->crtrans.x_max,
			  crinst->crtrans.y_min,
			  crinst->crtrans.y_max,
			  &lastx,
			  &lasty,
			  &currentx,
			  &currenty,
			  crinst->trobj.out_of_range);

	if((lastx == crinst->trobj.out_of_range)
	   ||(lasty == crinst->trobj.out_of_range)
	   ||(currentx == crinst->trobj.out_of_range)
	   ||(currenty == crinst->trobj.out_of_range)){
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
	CrDataToCompc(instance,xpoints,ypoints,2,
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
			_NhlWorkstationLineTo(crinst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
			call_frstd = 2;
		}
		xdist = currentx - lastx;
		ydist = currenty - lasty;
		for (i = 0; i<npoints; i++) {
			xd = lastx + xdist *(i+1)/ (float)npoints;
			yd = lasty + ydist *(i+1)/ (float)npoints;
			CrDataToCompc(instance,&xd,&yd,1,
				      &xc,&yc,NULL,NULL,&status);
			if (! status)
				_NhlWorkstationLineTo(crinst->trobj.wkptr,
						      c_cufx(xc),c_cufy(yc),0);
		}
		lastx = x;
		lasty = y;
		return(NhlNOERROR);
	}
	xdist = x - holdx;
	ydist = y - holdy;
/*
 * If the beginning of the line is clipped find the fcrst visible point
 * and move there.
 */
	if((lastx != holdx)||(lasty!= holdy)) {
		if (AdjustToEdge(crinst,holdx,holdy,x,y,&xd,&yd,&xc,&yc)
			< NhlNOERROR)
			return NhlFATAL;
		lastx = xd;
		lasty = yd;
		xdist = x - lastx;
		ydist = y - lasty;

		_NhlWorkstationLineTo(crinst->trobj.wkptr,
				      c_cufx(xc),c_cufy(yc),1);
	}
	else if (call_frstd == 1) {
		_NhlWorkstationLineTo(crinst->trobj.wkptr,
				      c_cufx(xpoints[0]),c_cufy(ypoints[0]),1);
	}
	call_frstd = 2;

	for (i = 0; i< npoints; i++) {
		xd = lastx + xdist *(i+1)/(float)npoints;
		yd = lasty + ydist *(i+1)/(float)npoints;
		CrDataToCompc(instance,&xd,&yd,1,&xc,&yc,NULL,NULL,&status);
		if (status) {
			if (AdjustToEdge(crinst,x,y,holdx,holdy,
					 &xd,&yd,&xc,&yc) < NhlNOERROR)
				return NhlFATAL;
		}
		_NhlWorkstationLineTo(crinst->trobj.wkptr,
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
static NhlErrorTypes CrWinLineTo
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
	NhlCurvilinearTransObjLayer crinst = (NhlCurvilinearTransObjLayer)instance;
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
			crinst->crtrans.compc_x_min, 
			crinst->crtrans.compc_x_max, 
			crinst->crtrans.compc_y_min, 
			crinst->crtrans.compc_y_max,
			&lastx, &lasty, &currentx, &currenty,
			crinst->trobj.out_of_range);
		if((lastx == crinst->trobj.out_of_range)
			||(lasty == crinst->trobj.out_of_range)
			||(currentx == crinst->trobj.out_of_range)
			||(currenty == crinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
			return(_NhlWorkstationLineTo(crinst->trobj.wkptr,c_cufx(x),c_cufy(y),1));
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }

			if(call_frstd == 1) {
				_NhlWorkstationLineTo(crinst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);

				call_frstd = 2;
			}
			_NhlWorkstationLineTo(crinst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);

			lastx = x;
			lasty = y;
			return(NhlNOERROR);
		}
			
			
	}
	
}


/*ARGSUSED*/
static NhlErrorTypes CrNDCLineTo
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
	NhlCurvilinearTransObjLayer iinstance= (NhlCurvilinearTransObjLayer)instance;
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
static NhlErrorTypes CrDataPolygon
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
	NhlCurvilinearTransObjLayer crinst = (NhlCurvilinearTransObjLayer)instance;
	NhlCurvilinearTransObjLayerPart *crtp = 
		(NhlCurvilinearTransObjLayerPart *) &crinst->crtrans;
	NhlString e_text;
	NhlString entry_name = "CrDataPolygon";
	float out_of_range = crinst->trobj.out_of_range;
	int i,j,ixout;
	float px,py,cx,cy,dx,dy,tx,ty;
	float *xbuf,*ybuf,*dbuf,*xout,*yout;
	int *ixbuf;
	NhlBoolean open, done = False, fcrst, fcrstpoint;
	int count, pcount, cix, pix, status = 0, npoints = 256;
	float xdist,ydist,tdist;

	npoints = crinst->trobj.point_count;
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
		_NhlTransClipLine(crtp->x_min,crtp->x_max,
				  crtp->y_min,crtp->y_max,
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
	ret = CrDataToCompc(instance,xbuf,ybuf,count,
			    xbuf,ybuf,NULL,NULL,&status);
	tdist = 0.0;

/*
 * Fcrst handle the simpler situation where no clipping is requcred.
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
		if (crinst->trobj.point_count > 1)
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
				CrDataToCompc(instance,&cx,&cy,1,
				      &cx,&cy,NULL,NULL,&status);
				if (! status) {
					ixout++;
					xout[ixout] = c_cufx(cx);
					yout[ixout] = c_cufy(cy);
				}
			}
		}
		if (npoints+count < ixout+1) {
			e_text = "%s: internal error: memory overrun";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
			
				
#if 0
		printf("count,pcount,npoints,ixout+1,%d,%d,%d,%d\n",
		       count,pcount,npoints,ixout+1);
#endif
		ret = _NhlWorkstationFill(crinst->trobj.wkptr,
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
 * enough measure of the length to compute the number of points requcred.
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
			xorange = cx < crtp->x_min || cx >crtp->x_max ? 
				True : False;
			yorange = cy < crtp->y_min || cy >crtp->y_max ? 
				True : False;
			status = 0;
			if (xorange && ! yorange) {
				if (cx < crtp->x_min) {
					cx = crtp->x_min;
					xbuf[i] = crtp->x_reverse ?
						1.1 : -.1;
				}
				else {
					cx = crtp->x_max;
					xbuf[i] = crtp->x_reverse ?
						-.1 : 1.1;
				}
				CrDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				ybuf[i] = c_cufy(cy);
			}
			else if (yorange && ! xorange) {
				if (cy < crtp->y_min) {
					cy = crtp->y_min;
					ybuf[i] = crtp->y_reverse ?
						1.1 : -.1;
				}
				else {
					cy = crtp->y_max;
					ybuf[i] = crtp->y_reverse ?
						-.1 : 1.1;
				}
				CrDataToCompc(instance,&cx,&cy,1,
					      &cx,&cy,
					      NULL,NULL,&status);
				xbuf[i] = c_cufx(cx);
			}
			else {
				if (crtp->x_reverse)
					xbuf[i] = cx < crtp->x_min ? 
						1.1 : -.1;
				else
					xbuf[i] = cx < crtp->x_min ? 
						-.1 : 1.1;

				if (crtp->y_reverse)
					ybuf[i] = cy < crtp->y_min ? 
						1.1 : -.1;
				else
					ybuf[i] = cy < crtp->y_min ? 
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
	if (crinst->trobj.point_count > 1)
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
	fcrstpoint = True;
	fcrst = True;
	for (i=0; i< count; i++) {
		float ratio;
		int lcount;
		NhlBoolean started;
		if (ixbuf[i] < 0) {
			continue;
		}
		else if (fcrstpoint == True) {
			fcrstpoint = False;
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
			CrDataToCompc(instance,&dx,&dy,1,
				      &tx,&ty,NULL,NULL,&status);
			if (! status) {
				if (! started) {
					started = True;
					if (lcount == 1) j--;
					if (AdjustToEdge(crinst,px,py,cx,cy,
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
				if (AdjustToEdge(crinst,cx,cy,px,py,
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
	if (npoints+count < ixout+1) {
		e_text = "%s: internal error: memory overrun";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

#if 0
	printf("count,pcount,npoints,ixout+1,%d,%d,%d,%d\n",
	       count,pcount,npoints,ixout+1);
#endif
	ret = _NhlWorkstationFill(crinst->trobj.wkptr,xout,yout,ixout+1);

	NhlFree(xbuf);
	NhlFree(ybuf);
	NhlFree(dbuf);
	NhlFree(xout);
	NhlFree(yout);
	return ret;
	
}
static NhlErrorTypes CrTransGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int nargs)
#else
(l, args, nargs)
NhlLayer l;
_NhlArgList args;
int nargs;
#endif
{
	NhlCurvilinearTransObjLayerPart* crp = 
		(&((NhlCurvilinearTransObjLayer)l)->crtrans);
	int i;
	NhlGenArray ga;


	for( i = 0; i < nargs ; i++) {
		ga = NULL;
		if(args[i].quark == QtrXCoordPoints) {
			if(crp->x_crv_ga != NULL)
				ga = crp->x_crv_ga;
		}
		if(args[i].quark == QtrYCoordPoints) {
			if(crp->y_crv_ga != NULL)
				ga = crp->y_crv_ga;
		}
		if (ga) {
			NhlGenArray ga_out;

			ga_out = _NhlCopyGenArray(ga,True);

			if (! ga_out) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga_out;
		}
	}
	return(NhlNOERROR);
}

/*
 * Function:	CrTransClassInitialize
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
static NhlErrorTypes    CrTransClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{

	QtrXCoordPoints = NrmStringToQuark(NhlNtrXCoordPoints);
	QtrYCoordPoints = NrmStringToQuark(NhlNtrYCoordPoints);
	Qdouble = NrmStringToQuark(NhlTDouble);

	return(NhlNOERROR);	
}
