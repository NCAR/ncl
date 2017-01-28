/*
 *      $Id: CnTriMeshRenderer.c,v 1.17 2010-03-27 18:58:25 dbrown Exp $
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
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/CnTriMeshRendererP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/TriMeshTransObj.h>
#include <ncarg/hlu/WorkspaceP.h>
#include <ncarg/hlu/color.h>

#ifdef BuildTRIANGLE
#define REAL double
#include <ncarg/hlu/triangle.h>
#endif

#define Oset(field) \
    NhlOffset(NhlCnTriMeshRendererLayerRec,cntrimeshrenderer.field)

static NhlResource resources[] = {
	{NhlNtriMeshUpdateMode,NhlCtriMeshUpdateMode,NhlTInteger,
		 sizeof(int),Oset(update_mode),NhlTImmediate,
		 _NhlUSET((NhlPointer) TRIMESH_NEWMESH),_NhlRES_PRIVATE,NULL}
};

static NhlErrorTypes CnTriMeshRendererInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes CnTriMeshRendererDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);

static NhlErrorTypes CnTriMeshRender(
#if     NhlNeedProto
        NhlLayer                instance,
        NhlContourPlotLayer     cnl,
	NhlDrawOrder            order,
	NhlString		entry_name
#endif
);


extern int (_NHLCALLF(ctdrpl,CTDRPL))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs,
	int *ncs,
	int *iai,
	int *iag,
	int *nai
#endif
);

extern int (_NHLCALLF(hluctfill,HLUCTFILL))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
#endif
);

extern void  (_NHLCALLF(hluctscae,HLUCTSCAE))(
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

extern void   (_NHLCALLF(hluctchcl,HLUCTCHCL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hluctchhl,HLUCTCHHL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hluctchll,HLUCTCHLL))(
#if	NhlNeedProto
	int	*iflg
#endif
);

extern void   (_NHLCALLF(hluctmxyz,HLUCTMXYZ))(
#if	NhlNeedProto
	int	*imap,
	float	*xinp,
	float	*yinp,
	float   *zinp,
	float	*xotp,
	float	*yotp
#endif
);

static int   (_NHLCALLF(rtmi,RTMI))(
#if	NhlNeedProto
	int	*idim,
	int	*jdim,
	int     *iini,
	int     *jini,
	int     *iino,
	int     *jino
#endif
);

static void   load_hluct_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);


extern void (_NHLCALLF(trmrgr,TRMRGR))(
	int *idim,
	int *jdim,
	float *rlon,
	float *rlat,
	float *rdat,
	int *iscr,
	float *missing_val,
	float *rpnt,
	int *mpnt,
	int *npnt,
	int *lopn,
	int *iedg,
	int *medg,
	int *edg,
	int *loen,
	int *itri,
	int *mtri,
	int *ntri,
	int *lotn
); 

extern void (_NHLCALLF(hlucttmtl,HLUCTTMTL))(
	int *kbuf,
	float *tbuf,
	int *mbuf,
	int *nbuf,
	int *ippp,
	int *mnop,
	int *nppp,
	int *ippe,
	int *mnoe,
	int *nppe,
	float *rpnt,
	int *mpnt,
	int *npnt,
	int *lopn,
	int *iedg,
	int *medg,
	int *nedg,
	int *loen,
	int *itri,
	int *mtri,
	int *ntri,
	int *lotn
);

extern void (_NHLCALLF(ctscae,CTSCAE))(
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

extern void (_NHLCALLF(ctchcl,CTCHCL))(
	int *iflg
);

extern void (_NHLCALLF(dprset,DPRSET))(
	void
);

extern void (_NHLCALLF(ctchhl,CTCHHL))(
	int *iflg
);

extern void (_NHLCALLF(ctchll,CTCHLL))(
	int *iflg
);

extern void (_NHLCALLF(ctmxyz,CTMXYZ))(
	int *imap,
	float *xinp,
	float *yinp,
	float *zinp,
	float *xotp,
	float *yotp
);

static NhlErrorTypes CnTriMeshWriteCellData
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	char		*entry_name
#endif
	);

static NhlIsoLine *CnTriMeshGetIsoLines(
#if     NhlNeedProto
        NhlLayer                instance,
        NhlContourPlotLayer     cnl,
        int			n_levels,
        float			*levels,
	NhlString		entry_name
#endif
	);

static void FreeTriBlockContents (
	TriBlock *tri_block,
	int *count
	);

NhlCnTriMeshRendererClassRec NhlcnTriMeshRendererClassRec = {
	{
/* class_name 		*/      "cnTriMeshRendererClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlCnTriMeshRendererLayerRec),
/* class_inited 	*/	False,
/* superclass		*/      (NhlClass)&NhlobjClassRec,
/* cvt_table		*/	NULL,

/* layer_resources 	*/   	resources,
/* num_resources 	*/     	NhlNumber(resources),
/* all_resources 	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize */     NULL,
/* class_initialize 	*/  	NULL,
/* layer_initialize 	*/  	CnTriMeshRendererInitialize,
/* layer_set_values 	*/  	NULL,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	NULL,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	CnTriMeshRendererDestroy,
	},
	{
/* render */		        CnTriMeshRender,
/* get_isolines */              CnTriMeshGetIsoLines
	},
	{
/* foo */		        0
	}
};

NhlClass NhlcnTriMeshRendererClass = (NhlClass)&NhlcnTriMeshRendererClassRec;

typedef enum { 
	cnInt,
	cnFloat,
	cnString 
} _cnParamType;

typedef struct _cnCt_Params {
	NhlString	name;
	_cnParamType	type;
} cnCt_Params;

static cnCt_Params Ct_Params[] = {
{"HCL", cnFloat},
{"HCS", cnFloat}, 
{"HCF", cnInt}, 
{"HLR", cnFloat}, 
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
static NhlCnTriMeshRendererLayerPart   *Tmp = NULL;

static int Lopn = 4;
static int Loen = 5;
static int Lotn = 4;
static TriBlock *Tbp;


static NhlErrorTypes CreateTilePartitions
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	int mnop;
	float *rlat,*rlon;
	float *rdat;
	int i,j;
	int block_count = 1;
	int block_size;
	int threshold_size;
	double xs, xe, ys, ye, xstep,ystep,xspace,yspace;
	int nx_div, ny_div;
	TriBlock *tbp;
	int npnt_alloc;
	int block_ix;
	float yadd;
	float xadd;
	double xt,yt;
	int nthreads;
	double xtmp,ytmp;
	float *tlon = NULL, *tlat = NULL;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;

	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	threshold_size = 40000000;
	FreeTriBlockContents(tmp->tri_block,&(tmp->nblocks));

	rdat = (float*)cnp->sfp->d_arr->data;
	rlat = (float*)cnp->sfp->y_arr->data;
	rlon = (float*)cnp->sfp->x_arr->data;
	if (cnp->sfp->d_arr->num_dimensions == 1) {
		mnop = cnp->sfp->fast_len;
	}
	else {
		mnop = cnp->sfp->fast_len * cnp->sfp->slow_len;
		if (cnp->sfp->x_arr->num_dimensions == 2) {  
			rlat = (float*)cnp->sfp->y_arr->data;
			rlon = (float*)cnp->sfp->x_arr->data;
		}
		else {
			int isize = cnp->sfp->fast_len;
			int jsize = cnp->sfp->slow_len;
			/* need to create 2D coordinate arrays */
			tlon = NhlMalloc(mnop * sizeof(float));
			tlat = NhlMalloc(mnop * sizeof(float));
			if (cnp->sfp->y_arr) {
				float y;
				for (j = 0; j < jsize; j++) {
					y = rlat[j];
					for (i = 0; i < isize; i++) {
						*(tlat + j*isize+i) = y;
					}
				}
			}
			else {
				/* need to do something here */
				float y;
				float step = (cnp->sfp->y_end - cnp->sfp->y_start) /    
					(jsize - 1);
				for (j = 0; j < jsize; j++) {
					y = cnp->sfp->y_start + j * step;
					for (i = 0; i < isize; i++) {
						*(tlat + j*isize+i) = y;
					}
				}
			}
			if (cnp->sfp->x_arr) {
				float *x = rlon; 
				for (j = 0; j < jsize; j++) {
					memcpy(tlon + j*isize,x,isize * sizeof(float));
				}
			}
			else {
				/* need to do something here */
				float x;
				float step = (cnp->sfp->x_end - cnp->sfp->x_start) /
					(isize - 1);
				for (i = 0; i < isize; i++) {
					x = cnp->sfp->x_start + i * step;
					for (j = 0; j < jsize; j++) {
						*(tlon + j*isize + i) = x;
					}
				}
			}
			rlat = tlat;
			rlon = tlon;
		}
	}


	block_size = mnop;
        while (block_size > threshold_size) {
		block_size = block_size / 2 + block_size % 2;
		block_count *= 2;
	}
	nx_div = MAX(1,block_count / 2);
	ny_div = MAX(1,block_count / 2 + block_count % 2);

#ifdef _OPENMP
	nthreads = omp_get_num_threads();
#else
	nthreads = 1;
#endif
	/* printf("%d threads\n",nthreads);*/
	if (nthreads > block_count) {
		/* block_count = nthreads - nthreads % 2;  accept even numbers only */
		block_count = nthreads;
	}
	if (block_count >= tmp->nblocks_alloced) {
		NhlFree(tmp->tri_block);
		tmp->tri_block = (TriBlock *) NhlCalloc(block_count, sizeof(TriBlock));
		tmp->nblocks_alloced = block_count;
	}

	nx_div = MAX(1,(int) sqrt(block_count));
	ny_div = MAX(1,(int) block_count / nx_div);
	block_count = nx_div * ny_div;
	block_size = mnop / block_count + block_size % block_count;
	xspace = wrx - wlx;
	yspace = wuy - wby;

	yadd = yspace * 0.01;
	xadd = xspace * 0.01;
	ys = wby;
	ye = ys + (wuy - ys) / ny_div;
	ystep = ye - ys;

	block_ix = 0;
	npnt_alloc = (int) 1.25 * block_size;
	for (j = 0; j < ny_div; j++) {
		xs = wlx;
		xe = xs + (wrx - xs) / nx_div;
		xstep = xe - xs;
		for (i = 0; i < nx_div; i++) {
			tbp = &(tmp->tri_block[block_ix]);
			tbp->xs = MAX(wlx,xs - xadd);
			tbp->xe = MIN(wrx,xe + xadd);
			tbp->ys = MAX(wby,ys - yadd);
			tbp->ye = MIN(wuy,ye + yadd);
			tbp->xer = xe;
			tbp->yer = ye;
			tbp->xsr = xs;
			tbp->ysr = ys;
			xs = xe;
			xe = xs + xstep;
			tbp->points = (double *)NhlMalloc(2 * npnt_alloc * sizeof(double));
			tbp->dat = (float *) NhlMalloc(npnt_alloc * sizeof(float));
			tbp->npnt = 0;
			tbp->npnt_alloc = npnt_alloc;
			block_ix++;
		}
		ys = ye;
		ye = ys + ystep;
	}
		
	for (i = 0; i < mnop; i++) {
		xtmp = (double) rlon[i];
		ytmp = (double) rlat[i];
		if (tmp->ezmap) {
			NGCALLF(mdptra,MDPTRA)(&ytmp,&xtmp,&xt,&yt);
			if (xt > 1e10 || yt > 1e10)
				continue;
		}
		else {
			xt = xtmp;
			yt = ytmp;
		}
		for (block_ix = 0; block_ix < block_count; block_ix++) {
			tbp = &(tmp->tri_block[block_ix]);
			xs = tbp->xs;
			xe = tbp->xe;
			ys = tbp->ys;
			ye = tbp->ye;
			if (xt < xs || xt > xe || yt < ys || yt > ye) {
				continue;
			}
				
			if (tbp->npnt == tbp->npnt_alloc) {
				tbp->npnt_alloc *= 1.5;
				tbp->dat = (float *) NhlRealloc(tbp->dat,tbp->npnt_alloc * sizeof(float));
				tbp->points = (double *)NhlRealloc(tbp->points,2 * tbp->npnt_alloc * sizeof(double));
			}
			tbp->points[tbp->npnt * 2] = xt;
			tbp->points[tbp->npnt * 2 + 1] = yt;
			tbp->dat[tbp->npnt] = rdat[i];
			tbp->npnt++;
		}
	}
	tmp->nblocks = block_count;
	if (tlat) NhlFree(tlat);
	if (tlon) NhlFree(tlon);

	return NhlNOERROR;
}

#if 0
static NhlErrorTypes LatLon2IJ
(
	float lat,
	float lon,
	float *latc,
	float *lonc,
	int latsize,
	int lonsize,
	int *ix,   /* return i */
	int *jx   /* return j */
	)
{
	int i , j;

	if ()
		;
}
#endif
			
	

static NhlErrorTypes CreateTilePartitions2D
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	int mnop;
	float *rlat,*rlon;
	int i,j;
	int block_count = 1;
	int block_size;
	int threshold_size;
	double xs, xe, ys, ye, xstep,ystep,xspace,yspace;
	int nx_div, ny_div;
	TriBlock *tbp;
	int block_ix;
	float yadd;
	float xadd;
	double xt,yt;
	int nthreads;
	double xtmp,ytmp;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;
	int xsize, ysize;

	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	threshold_size = 40000000;
	FreeTriBlockContents(tmp->tri_block,&(tmp->nblocks));

	rlat = (float*)cnp->sfp->y_arr->data;
	rlon = (float*)cnp->sfp->x_arr->data;
	mnop = cnp->sfp->slow_len * cnp->sfp->fast_len;

	block_size = mnop;
        while (block_size > threshold_size) {
		block_size = block_size / 2 + block_size % 2;
		block_count *= 2;
	}
	nx_div = MAX(1,block_count / 2);
	ny_div = MAX(1,block_count / 2 + block_count % 2);

#ifdef _OPENMP
	nthreads = omp_get_num_threads();
#else
	nthreads = 1;
#endif

	/* printf("%d threads\n",nthreads);*/
	if (nthreads > block_count) {
		/* block_count = nthreads - nthreads % 2;  accept even numbers only */
		block_count = nthreads;
	}
	if (block_count >= tmp->nblocks_alloced) {
		NhlFree(tmp->tri_block);
		tmp->tri_block = (TriBlock *) NhlCalloc(block_count, sizeof(TriBlock));
		tmp->nblocks_alloced = block_count;
	}
	else {
		memset(tmp->tri_block,0,sizeof(TriBlock) * block_count);
	}

	nx_div = MAX(1,(int) sqrt(block_count));
	nx_div = 1;
	ny_div = MAX(1,(int) block_count / nx_div);
	block_count = nx_div * ny_div;
	block_size = mnop / block_count + block_size % block_count;
	xspace = wrx - wlx;
	yspace = wuy - wby;

	yadd = yspace * 0.01;
	xadd = xspace * 0.01;
	ys = wby;
	ye = ys + (wuy - ys) / ny_div;
	ystep = ye - ys;

	block_ix = 0;
	xsize = cnp->sfp->fast_len;
	ysize = cnp->sfp->slow_len;
	for (j = 0; j < ny_div; j++) {
		xs = wlx;
		xe = xs + (wrx - xs) / nx_div;
		xstep = xe - xs;
		for (i = 0; i < nx_div; i++) {
			tbp = &(tmp->tri_block[block_ix]);
			tbp->xs = MAX(wlx,xs - xadd);
			tbp->xe = MIN(wrx,xe + xadd);
			tbp->ys = MAX(wby,ys - yadd);
			tbp->ye = MIN(wuy,ye + yadd);
			tbp->xer = xe;
			tbp->yer = ye;
			tbp->xsr = xs;
			tbp->ysr = ys;
			tbp->ixmn = xsize;
			tbp->iymn = ysize;
			tbp->ixmx = tbp->iymx = 0;
			
			xs = xe;
			xe = xs + xstep;
			block_ix++;
		}
		ys = ye;
		ye = ys + ystep;
		/*printf("xs %f xe %f ys %f ye %f \n",tbp->xs,tbp->xe,tbp->ys,tbp->ye);*/
	}

	for (j = 0; j < ysize; j++) {
		for (i = 0; i < xsize; i++) {
			int ix = j * xsize + i;
			xtmp = (double) rlon[ix];
			ytmp = (double) rlat[ix];
			if (tmp->ezmap) {
				NGCALLF(mdptra,MDPTRA)(&ytmp,&xtmp,&xt,&yt);
				if (xt > 1e10 || yt > 1e10)
					continue;
			}
			else {
				xt = xtmp;
				yt = ytmp;
			}
			for (block_ix = 0; block_ix < block_count; block_ix++) {
				tbp = &(tmp->tri_block[block_ix]);
				xs = tbp->xs;
				xe = tbp->xe;
				ys = tbp->ys;
				ye = tbp->ye;
				if (xt < xs || xt > xe || yt < ys || yt > ye) {
					continue;
				}
				if (i < tbp->ixmn) tbp->ixmn = i;
				if (i > tbp->ixmx) tbp->ixmx = i;
				if (j < tbp->iymn) tbp->iymn = j;
				if (j > tbp->iymx) tbp->iymx = j;
			}
		}
	}

	tmp->nblocks = block_count;

	return NhlNOERROR;
}

static int IsCyclic;

static void SetCyclicFlag(NhlContourPlotLayer cnl, ng_size_t ny, ng_size_t nx, float *tlat,float *tlon)
{
	float tlat1, tlon1, tlat2, tlon2;
	int status = 1;
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	ng_size_t ix;

	/* this checks to set if the first column of longitudes is the same as the last column.
	   Actually only the first row that can project into the visible area is checked for now.
	   This may need updating if it doesn't work. Avoid latitudes within 1 degree of the poles.
	   A static variable is needed for the cyclic flag. */

	IsCyclic = 0;

	ix = 0;
	while (ix < 2 + nx * (ny-1)) {

		if (fabs(tlat[ix]) < 89 && fabs(tlat[ix + nx-1]) < 89) {
			_NhlDataToWin(cnp->trans_obj,&(tlon[ix]),&(tlat[ix]),
				      1,&tlon1,&tlat1,&status,
				      NULL,NULL);
			_NhlDataToWin(cnp->trans_obj,&(tlon[ix + nx-1]),&(tlat[ix + nx-1]),
				      1,&tlon2,&tlat2,&status,
				      NULL,NULL);
			if (! status) {
				if (_NhlCmpFAny2(tlon1,tlon2,6,1e-32) == 0.0) {
					IsCyclic = 1;
				}
				return;
			}
		}
		ix = ix + nx;
	}
}
	

/*
 * Function: rtmi_
 *
 * Description: 
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
static int (_NHLCALLF(rtmi,RTMI))
#if	NhlNeedProto
(
	int	*idim,
	int	*jdim,
	int     *iini,
	int     *jini,
	int     *iino,
	int     *jino
)
#else
(idim,jdim,iini,jini,iino,jino)
	int	*idim;
	int	*jdim;
	int     *iini;
	int     *jini;
	int     *iino;
	int     *jino;
#endif
{
	/* assume simplest situation for now */

	
	if (IsCyclic && *iini == *idim) {
		*iino = 1;
		*jino = *jini;
	}
	else {
		*jino = *jini;
		*iino = *iini;
	}
	
#if 0

	*iino = *iini;
	if (*jini == *jdim) {
		if (*iini > *idim / 2 && *iini < *idim - 1) {
			*iino = *idim - *iini - 1;
		}
		else if (*iini == *idim) {
			*iino = 1;
		}
		else {
			*iino = *iini;
		}
		printf("ini %d outi %d\n",*iini, *iino);
	}

#endif
	return 0;
}

static NhlErrorTypes BuildTriangularMesh 
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	int *iscr;
	float missing_val;
	int coords_alloced;
	int idim = cnp->sfp->fast_len;
	int jdim = cnp->sfp->slow_len;
	int idm1 = cnp->sfp->fast_len - 1;
	int jdm1 = cnp->sfp->slow_len - 1;

	int mnop = idim * jdim;
	int mnoe = 3 *idm1 * jdm1 + idm1 + jdm1;
	int mnot = 2 * idm1 * jdm1;
	int mpnt = mnop * Lopn;
	int medg = mnoe * Loen;
	int mtri = mnot * Lotn;
	float *rpnt;
	int *iedg, *itri;
	int npnt,nedg,ntri;
	float *rlat,*rlon,*rdat;
	float *tlat, *tlon, *tdat;
	int i,j;
	TriBlock *tbp;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;
	int block_ix;
	int blk_idim, blk_jdim,blk_j_off, blk_i_off;
	int tid,nthreads;
	int tsize;

	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	tid = 0;
	nthreads = 1;

	if (nthreads > 1) {
		if (tid == 0) 
			CreateTilePartitions2D(tmp,cnl,entry_name);
	}
	else {
		tbp = &(tmp->tri_block[0]);
		tbp->xs = tbp->xsr = wlx;
		tbp->xe = tbp->xer = wrx;
		tbp->ys = tbp->ysr = wby;
		tbp->ye = tbp->yer = wuy;
		tbp->ixmn = tbp->iymn = 0;
		tbp->ixmx = cnp->sfp->fast_len - 1;
		tbp->iymx = cnp->sfp->slow_len - 1;
		tbp->dat = (float*)cnp->sfp->d_arr->data;
		tbp->npnt = cnp->sfp->slow_len * cnp->sfp->fast_len;
		tmp->nblocks = 1;
	}

		    
	for (block_ix = 0; block_ix < tmp->nblocks; block_ix++) {
		    
		tbp = &(tmp->tri_block[block_ix]);
		blk_idim = tbp->ixmx - tbp->ixmn + 1;
		blk_jdim = tbp->iymx - tbp->iymn + 1;
		idm1 = blk_idim - 1;
		jdm1 = blk_jdim - 1;
		blk_j_off = tbp->iymn * cnp->sfp->fast_len;
		blk_i_off = tbp->ixmn;

		mnop = blk_idim * blk_jdim;
		mnoe = 3 *idm1 * jdm1 + idm1 + jdm1;
		mnot = 2 * idm1 * jdm1;
		mpnt = mnop * Lopn;
		medg = mnoe * Loen;
		mtri = mnot * Lotn;

		iscr = NhlMalloc(4 * blk_idim * blk_jdim * sizeof(int));
		rpnt = NhlMalloc(mpnt * sizeof(float));
		iedg = NhlMalloc(medg * sizeof(int));
		itri = NhlMalloc(mtri * sizeof(int));

		if (! iscr || ! rpnt || ! iedg || ! itri) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
	
		missing_val = cnp->sfp->missing_value_set ?
			cnp->sfp->missing_value : -FLT_MAX;

		tsize = blk_jdim * blk_idim * sizeof(float);
		coords_alloced = 0;
		if (tmp->nblocks == 1 &&  cnp->sfp->x_arr && cnp->sfp->y_arr &&
                    cnp->sfp->x_arr->num_dimensions == 2
		    && ! (cnp->sfp->xc_is_bounds || cnp->sfp->yc_is_bounds)) {
			/* in this case only there is no need to copy the arrays */
			tlat = (float*)cnp->sfp->y_arr->data;
			tlon = (float*)cnp->sfp->x_arr->data;
			tdat = (float*)cnp->sfp->d_arr->data;

		}
		else {
			coords_alloced = 1;
                        rdat = (float*)cnp->sfp->d_arr->data;

			tlat = NhlMalloc( tsize * sizeof(float));
			tlon = NhlMalloc( tsize * sizeof(float));
			tdat = NhlMalloc( tsize * sizeof(float));
			if (cnp->sfp->x_arr && cnp->sfp->x_arr->num_dimensions == 2) {
                                rlon = (float*)cnp->sfp->x_arr->data;
                                rlat = (float*)cnp->sfp->y_arr->data;
				float *rlon_off = rlon + blk_j_off;
				float *rlat_off = rlat + blk_j_off;
		
				/* 
				 * since the triangular mesh algorithm does not handle
				 * coordinates not defined at the grid points we 
				 * must interpolate bounding coordinates to the 
				 * cell centers as best we can.
				 */
				if (cnp->sfp->xc_is_bounds && cnp->sfp->yc_is_bounds) {
					int jd, jbd, jbdp;
					int ix;
					int xcoord_len = idim + 1;
				    
					for (j = 0; j < blk_jdim; j++) {
						jd = j * idim;
						jbd = j * xcoord_len;
						jbdp = (j+1) * xcoord_len;
						for (i = 0; i < blk_idim; i++) {
							ix = i + blk_i_off;
							*(tlon+jd+i) = 
								(*(rlon_off + jbd+ix) + *(rlon_off+jbd+ix+1) +
								 *(rlon_off+jbdp + ix) + *(rlon_off+jbdp+ix+1)) 
								/ 4.0;
							*(tlat+jd+i) =
								(*(rlat_off+jbd+ix) + *(rlat_off+jbd+ix+1) +
								 *(rlat_off+jbdp+ix) + *(rlat_off+jbdp+ix+1)) 
								/ 4.0;
						}
					}
					if (tbp->ixmn == 0) {
						memcpy(tdat,rdat + tbp->iymn * cnp->sfp->fast_len,tsize);
					}
					else {
						int tysize = tbp->iymx - tbp->iymn + 1;
						int txsize = tbp->ixmx -tbp->ixmn + 1;
						int rstart = tbp->iymn * cnp->sfp->fast_len;
						for (i = 0; i < tysize; i++) {
							memcpy(tdat + i * txsize, rdat + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
						}
					}
				}
				else if (cnp->sfp->xc_is_bounds) {
					int jd, jbd;
					int ix;
					int xcoord_len = idim + 1;
					for (j = 0; j < blk_jdim; j++) {
						jd = j * idim;
						jbd = j * xcoord_len;
						for (i = 0; i < blk_idim; i++) {
							ix = i + blk_i_off;
							*(tlon+jd+i) = 
								(*(rlon_off+jbd+ix) + *(rlon_off+jbd+ix+1)) / 2.0;
							*(tlat+jd+i) = 
								(*(rlat_off+jbd+ix) + *(rlat_off+jbd+ix+1)) / 2.0;
						}
					}
					if (tbp->ixmn == 0) {
						memcpy(tdat,rdat + tbp->iymn * cnp->sfp->fast_len,tsize);
					}
					else {
						int tysize = tbp->iymx - tbp->iymn + 1;
						int txsize = tbp->ixmx -tbp->ixmn + 1;
						int rstart = tbp->iymn * cnp->sfp->fast_len;
						for (i = 0; i < tysize; i++) {
							memcpy(tdat + i * txsize, rdat + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
						}
					}
				}
				else if (cnp->sfp->yc_is_bounds) {
					int jd, jdp;
					int ix;
					for (j = 0; j < blk_jdim; j++) {
						jd = j * idim;
						jdp = (j + 1) * idim;
						for (i = 0; i < blk_idim; i++) {
							ix = i + blk_i_off;
							*(tlon+jd+i) = 
								(*(rlon_off+jd+ix) + *(rlon_off+jdp+ix)) / 2.0;
							*(tlat+jd+i) = 
								(*(rlat_off+jd+ix) + *(rlat_off+jdp+ix)) / 2.0;
						}
					}
					if (tbp->ixmn == 0) {
						memcpy(tdat,rdat + tbp->iymn * cnp->sfp->fast_len,tsize);
					}
					else {
						int tysize = tbp->iymx - tbp->iymn + 1;
						int txsize = tbp->ixmx -tbp->ixmn + 1;
						int rstart = tbp->iymn * cnp->sfp->fast_len;
						for (i = 0; i < tysize; i++) {
							memcpy(tdat + i * txsize, rdat + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
						}
					}
				}
				else {
					if (tbp->ixmn == 0) {
						memcpy(tlat,rlat + tbp->iymn * cnp->sfp->fast_len,tsize);
						memcpy(tlon,rlon + tbp->iymn * cnp->sfp->fast_len,tsize);
						memcpy(tdat,rdat + tbp->iymn * cnp->sfp->fast_len,tsize);
					}
					else {
						int tysize = tbp->iymx - tbp->iymn + 1;
						int txsize = tbp->ixmx -tbp->ixmn + 1;
						int rstart = tbp->iymn * cnp->sfp->fast_len;
						for (i = 0; i < tysize; i++) {
							memcpy(tlat + i * txsize, rlat + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
							memcpy(tlon + i * txsize, rlon + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
							memcpy(tdat + i * txsize, rdat + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
						}
					}
				}
			}
			else {
				if (cnp->sfp->y_arr) {
					float y;
					int jix;
                                        rlat = (float*)cnp->sfp->y_arr->data;
					if (! cnp->sfp->yc_is_bounds) {
						for (j = 0; j < blk_jdim; j++) {
							jix = tbp->iymn + j;
							y = rlat[jix];
							for (i = 0; i < blk_idim; i++) {
								*(tlat + j*blk_idim+i) = y;
							}
						}
					}
					else {
						for (j = 0; j < blk_jdim; j++) {
							jix = tbp->iymn + j;
							y = (rlat[jix] + rlat[jix + 1]) * 0.5;
							for (i = 0; i < blk_idim; i++) {
								*(tlat + j*blk_idim+i) = y;
							}
						}
					}
				}
				else {
					/* need to do something here */
					float y;
					float step = (cnp->sfp->y_end - cnp->sfp->y_start) /    
						(blk_jdim - 1);
					for (j = 0; j < blk_jdim; j++) {
						y = cnp->sfp->y_start + (j + tbp->iymn) * step;
						for (i = 0; i < blk_idim; i++) {
							*(tlat + j*blk_idim+i) = y;
						}
					}
				}
				if (cnp->sfp->x_arr) {
                                        rlon = (float*)cnp->sfp->x_arr->data;
					if (! cnp->sfp->xc_is_bounds) {
						float *x = rlon + tbp->ixmn; 
						for (j = 0; j < blk_jdim; j++) {
							memcpy(tlon + j*blk_idim,x,blk_idim * sizeof(float));
						}
					}
					else {
						int ix;
						float x;
						for (i = 0; i < blk_idim; i++) {
							ix = i + tbp->ixmn;
							x = (rlon[ix] + rlon[ix + 1]) * 0.5;
							for (j = 0; j < blk_jdim; j++) {
								tlon[j * blk_idim + i] = x;
							}
						}
					}
				}
				else {
					/* need to do something here */
					float x;
					float step = (cnp->sfp->x_end - cnp->sfp->x_start) /
						(blk_idim - 1);
					for (i = 0; i < blk_idim; i++) {
						x = cnp->sfp->x_start + (i + tbp->iymn) * step;
						for (j = 0; j < blk_jdim; j++) {
							*(tlon + j*blk_idim + i) = x;
						}
					}
				}
				if (tbp->ixmn == 0) {
					memcpy(tdat,rdat + tbp->iymn * cnp->sfp->fast_len,tsize);
				}
				else {
					int tysize = tbp->iymx - tbp->iymn + 1;
					int txsize = tbp->ixmx -tbp->ixmn + 1;
					int rstart = tbp->iymn * cnp->sfp->fast_len;
					for (i = 0; i < tysize; i++) {
						memcpy(tdat + i * txsize, rdat + rstart + i * cnp->sfp->fast_len + tbp->ixmn, txsize * sizeof(float));
					}
				}

			}
		}

		if (tmp->ezmap) {
			SetCyclicFlag(cnl,blk_jdim,blk_idim,tlat,tlon);
			c_cttmrg(blk_idim,blk_jdim,tlat,tlon,tdat,
				 iscr,missing_val,
				 _NHLCALLF(rtmi,RTMI),
				 rpnt,mpnt,&npnt,Lopn,
				 iedg,medg,&nedg,Loen,
				 itri,mtri,&ntri,Lotn);
		}
		else {
			_NHLCALLF(trmrgr,TRMRGR)
				(&blk_idim,&blk_jdim,tlon,tlat,tdat,
				 iscr,&missing_val,
				 rpnt,&mpnt,&npnt,&Lopn,
				 iedg,&medg,&nedg,&Loen,
				 itri,&mtri,&ntri,&Lotn);
		}

		NhlFree(iscr);
		if (coords_alloced) {
			NhlFree(tdat);
			NhlFree(tlon);
			NhlFree(tlat);
		}
	
		tbp->npnt = npnt;
		tbp->nedg = nedg;
		tbp->ntri = ntri;
		tbp->rpnt = rpnt;
		tbp->iedg = iedg;
		tbp->itri = itri;
	}

	tmp->update_mode = TRIMESH_NOUPDATE;
	return NhlNOERROR;

}

#define DEGTORAD 0.017453292519943

static NhlErrorTypes BuildNativeMesh 
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	int mnot  = cnp->sfp->element_nodes->len_dimensions[0];
	int mnop = cnp->sfp->fast_len;
	int mnoe = 3 * mnot;
	int mpnt = mnop * Lopn;
	int medg = mnoe * Loen;
	int mtri = mnot * Lotn;
	float *rpnt;
	int *el;
	int *iedg, *itri;
	int *ippp,*ippe;
	int npnt,nedg;
	float *rlat,*rlon,*rdat;
	float tbuf[5021][12];
	int kbuf = 173;
	int mbuf = 5021;
	int nppp = 0;
	int nppe = 0;
	int nbuf = 0;
	int ntri = 0;
	int i;
	int ix_offset = cnp->sfp->ix_start;
	int err_num;
	char *e_msg;
	char *e_text;
	TriBlock *tbp;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	FreeTriBlockContents(tmp->tri_block,&(tmp->nblocks));

	rpnt = NhlMalloc(mpnt * sizeof(float));
	iedg = NhlMalloc(medg * sizeof(int));
	itri = NhlMalloc(mtri * sizeof(int));
	ippp = NhlMalloc(2 * mnop * sizeof(int));
	ippe = NhlMalloc(2 * mnoe * sizeof(int));
	
	if (! (rpnt && iedg && itri && ippp && ippe )) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	    
	rlat = (float*)cnp->sfp->y_arr->data;
	rlon = (float*)cnp->sfp->x_arr->data;
	rdat = (float*)cnp->sfp->d_arr->data;
	el = (int*) cnp->sfp->element_nodes->data;

	for (i = 0; i < mnot; i++) {
		int *ep;
		int e0,e1,e2;
		if (nbuf >= mbuf) 
			_NHLCALLF(hlucttmtl,HLUCTTMTL)
				(&kbuf,(float*)tbuf,&mbuf,&nbuf,
				 ippp,&mnop,&nppp,
				 ippe,&mnoe,&nppe,
				 rpnt,&mpnt,&npnt,&Lopn,
				 iedg,&medg,&nedg,&Loen,
				 itri,&mtri,&ntri,&Lotn);
		if (c_nerro(&err_num) != 0) {
			e_msg = c_semess(0);
			e_text = "%s: %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,e_msg);
			return NhlFATAL;
		}

		ep = el + i * 3;
		e0 = *ep - ix_offset;
		e1 = *(ep+1) - ix_offset;
		e2 = *(ep+2) - ix_offset;
		if (tmp->ezmap) {
			tbuf[nbuf][0] = cos(DEGTORAD * (double)rlat[e0]) *
				cos(DEGTORAD * (double)rlon[e0]);
			tbuf[nbuf][1] = cos(DEGTORAD * (double)rlat[e0]) *
				sin(DEGTORAD * (double)rlon[e0]);
			tbuf[nbuf][2] = sin(DEGTORAD * (double)rlat[e0]);
			tbuf[nbuf][3] = rdat[e0];
			tbuf[nbuf][4] = cos(DEGTORAD * (double)rlat[e1]) *
				cos(DEGTORAD * (double)rlon[e1]);
			tbuf[nbuf][5] = cos(DEGTORAD * (double)rlat[e1]) *
				sin(DEGTORAD * (double)rlon[e1]);
			tbuf[nbuf][6] = sin(DEGTORAD * (double)rlat[e1]);
			tbuf[nbuf][7] = rdat[e1];
			tbuf[nbuf][8] = cos(DEGTORAD * (double)rlat[e2]) *
				cos(DEGTORAD * (double)rlon[e2]);
			tbuf[nbuf][9] = cos(DEGTORAD * (double)rlat[e2]) *
				sin(DEGTORAD * (double)rlon[e2]);
			tbuf[nbuf][10] = sin(DEGTORAD * (double)rlat[e2]);
			tbuf[nbuf][11] = rdat[e2];
		}
		else {
			tbuf[nbuf][0] = rlon[e0];
			tbuf[nbuf][1] = rlat[e0];
			tbuf[nbuf][2] = 0.0;
			tbuf[nbuf][3] = rdat[e0];

			tbuf[nbuf][4] = rlon[e1];
			tbuf[nbuf][5] = rlat[e1];
			tbuf[nbuf][6] = 0.0;
			tbuf[nbuf][7] = rdat[e1];

			tbuf[nbuf][8] = rlon[e2];
			tbuf[nbuf][9] = rlat[e2];
			tbuf[nbuf][10] = 0.0;
			tbuf[nbuf][11] = rdat[e2];
		}			
		if (! cnp->sfp->missing_value_set)
			nbuf++;
		else if (tbuf[nbuf][3] != cnp->sfp->missing_value && 
			 tbuf[nbuf][7] != cnp->sfp->missing_value &&
			 tbuf[nbuf][11] != cnp->sfp->missing_value) {
			nbuf++;
		}
	}
	if (nbuf > 0) {
		_NHLCALLF(hlucttmtl,HLUCTTMTL)
			(&nbuf,(float*)tbuf,&mbuf,&nbuf,
			 ippp,&mnop,&nppp,
			 ippe,&mnoe,&nppe,
			 rpnt,&mpnt,&npnt,&Lopn,
			 iedg,&medg,&nedg,&Loen,
			 itri,&mtri,&ntri,&Lotn);

		if (c_nerro(&err_num) != 0) {
			e_msg = c_semess(0);
			e_text = "%s: %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,e_msg);
			return NhlFATAL;
		}
	}

	tbp = &(tmp->tri_block[0]);
	tbp->npnt = npnt;
	tbp->nedg = nedg;
	tbp->ntri = ntri;
	tbp->rpnt = rpnt;
	tbp->iedg = iedg;
	tbp->itri = itri;
	tbp->xs = tbp->xsr = wlx;
	tbp->xe = tbp->xer = wrx;
	tbp->ys = tbp->ysr = wby;
	tbp->ye = tbp->yer = wuy;
	tbp->ixmn = tbp->iymn = 0;
	if (cnp->sfp->slow_len == 0) {
		tbp->ixmx = tbp->iymx = cnp->sfp->fast_len - 1;
	}
	else {
		tbp->iymx = cnp->sfp->slow_len - 1;
		tbp->ixmx = cnp->sfp->fast_len - 1;
	}
	tmp->nblocks = 1;
	tmp->update_mode = TRIMESH_NOUPDATE;
	NhlFree(ippp);
	NhlFree(ippe);
	/*printf("total number of edges %d\n",nedg);*/

	return NhlNOERROR;

}

typedef struct _vertex {
	float x;
	float y;
	int node;
	int vert;
} Vertex;

static int vcomp(const void *vv1, const void *vv2)
{
	Vertex *v1 = (Vertex *) vv1;
	Vertex *v2 = (Vertex *) vv2;

	if (v1->x < v2->x)
		return -1;
	if (v1->x > v2->x)
		return 1;
	if (v1->y < v2->y)
		return -1;
	if (v1->y > v2->y)
		return 1;
	if (v1->node < v2->node)
		return -1;
	if (v1->node > v2->node)
		return 1;
	return 0;
}

static void AddElement
#if	NhlNeedProto
(
	int *el,
	Vertex *v,
	int vcount,
	NhlBoolean ezmap,
	float *x,
	float *y
	)
#else 
(el,v,vcount,ezmap,x,y)
	int *el;
	Vertex *v;
	int vcount;
	NhlBoolean ezmap;
	float *x;
	float *y;
#endif       
{
	int n0,n1,n2;
	float detleft, detright,det;
	float xt[3],yt[3];
	int i;

	n0 = v->node;
	n1 = (v+1)->node;
	n2 = (v+2)->node;
	if (vcount > 3) { /* extra node */
		if (n1 == n0) {
			n1 = (v+3)->node;
		}
		else if (n2 == n1 || n2 == n0) {
			n2 = (v+3)->node;
		}
	}
	xt[0] = x[n0];
	xt[1] = x[n1];
	xt[2] = x[n2];
	yt[0] = y[n0];
	yt[1] = y[n1];
	yt[2] = y[n2];

	if (ezmap) {
		float min, max;
		min = max = xt[0];
		for (i = 1; i < 3; i++) {
			if (xt[i] < min)
				min = xt[i];
			if (xt[i] > max) 
				max = xt[i];
		}
		if (max - min > 180) {
			for (i = 0; i < 3; i++) {
				if (xt[i] - min > 180) {
					xt[i] -= 360;
				}
			}
		}
	}
			
	detleft = (xt[0] - xt[2]) * (yt[1] - yt[2]);
       	detright = (yt[0] - yt[2]) * (xt[1] - xt[2]);
	det = detleft - detright;
	if (det > 0) {
	    el[0] = n0;
	    el[1] = n1;
	    el[2] = n2;
	}
	else {
		el[0] = n0;
		el[1] = n2;
		el[2] = n1;
	}
	return;
}


static int *GetTriangleNodes
#if	NhlNeedProto
(
	NhlGenArray xnodes,
	NhlGenArray ynodes,
	NhlGenArray xbounds,
	NhlGenArray ybounds,
	NhlBoolean ezmap,
	int *ntri
)
#else
(xnodes,ynodes,xbounds,ybounds,ezmap,ntri)
	NhlGenArray xnodes;
	NhlGenArray ynodes;
	NhlGenArray xbounds;
	NhlGenArray ybounds;
	NhlBoolean ezmap;
	int *ntri;
#endif
{
	int ncount = xnodes->len_dimensions[0];
	int nvert = xbounds->len_dimensions[1];
	int tcount = xbounds->len_dimensions[0] * xbounds->len_dimensions[1];
	Vertex *verts;
	float *x = (float *) xnodes->data;
	float *y = (float *) ynodes->data;
	float *xb = (float *) xbounds->data;
	float *yb = (float *) ybounds->data;
	int i,j;
	int *el;
	int nmatched;
	int tfound;

	verts = NhlMalloc(tcount * sizeof(Vertex));

	for (i = 0; i < ncount; i++) {
		for (j = 0; j < nvert; j++) {
			int ix = i * nvert +j;
			Vertex *v = verts + ix;
			v->node = i;
			v->vert = j;
			v->x = *(xb + ix);
			v->y = *(yb + ix);
		}
	}
	qsort(verts,ncount * nvert,sizeof(Vertex),vcomp);
	
	/*
	 * There should now be sets of three vertices in a row that 
	 * are co-located. However, because of possible boundaries there 
	 * may be singles or doubles interspersed. Each set of three
	 * represents a triangle in the mesh, with the vertex as its 
	 * center. Put the nodes into the triangle element list. 
	 * There should be at most ncount * nvert / 3 triangles with
	 * three elements each.
	 */
	
	el = (int *) NhlMalloc(sizeof(int) * ncount * nvert);

	tfound = 0;
	for (i = 0; i < ncount * nvert; ) {
		nmatched = 0;
		for (j = i + 1;;j++) {
			if (j == ncount * nvert)
				break;
			if (! (verts[j].x == verts[i].x &&
			       verts[j].y == verts[i].y))
				break;
			nmatched++;
		}
		if (nmatched > 1) {
			AddElement(el + 3 * tfound,
				   &verts[i],nmatched+1,ezmap,x,y);
			tfound++;
		}
#if 0
		/* debugging code */
		else {
			
			printf("unmatched node %d %d %f %f\n",verts[i].node,
			       verts[i].vert, verts[i].x, verts[i].y);
			if (nmatched == 1) {
				printf("unmatched node %d %d %f %f\n",
				       verts[i+1].node,verts[i+1].vert,
				       verts[i+1].x, verts[i+1].y);
			}
		}
#endif 

		i = j;
	}
	NhlFree(verts);

	*ntri = tfound;
	return el;
}
static NhlErrorTypes BuildNativeMeshFromBounds 
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	int mnot;
	int mtri;
	int mnop = cnp->sfp->fast_len;
	int mnoe;
	int mpnt = mnop * Lopn;
	int medg;
	float *rpnt;
	int *el;
	int *iedg, *itri;
	int *ippp,*ippe;
	int npnt,nedg;
	float *rlat,*rlon,*rdat;
	float tbuf[5021][12];
	int kbuf = 173;
	int mbuf = 5021;
	int nppp = 0;
	int nppe = 0;
	int nbuf = 0;
	int ntri = 0;
	int i;
	int ix_offset = 0;
	int err_num;
	char *e_msg;
	char *e_text;
	TriBlock *tbp;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	FreeTriBlockContents(tmp->tri_block,&(tmp->nblocks));

	el = GetTriangleNodes(cnp->sfp->x_arr,cnp->sfp->y_arr,
			      cnp->sfp->x_cell_bounds,cnp->sfp->y_cell_bounds,
			      tmp->ezmap,&mnot);
	mtri = mnot * Lotn;
	mnoe = 3 * mnot;
	medg = mnoe * Loen;

	rpnt = NhlMalloc(mpnt * sizeof(float));
	iedg = NhlMalloc(medg * sizeof(int));
	itri = NhlMalloc(mtri * sizeof(int));
	ippp = NhlMalloc(2 * mnop * sizeof(int));
	ippe = NhlMalloc(2 * mnoe * sizeof(int));
	
	if (! (rpnt && iedg && itri && ippp && ippe )) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	    
	rlat = (float*)cnp->sfp->y_arr->data;
	rlon = (float*)cnp->sfp->x_arr->data;
	rdat = (float*)cnp->sfp->d_arr->data;


	for (i = 0; i < mnot; i++) {
		int *ep;
		int e0,e1,e2;
		if (nbuf >= mbuf) 
			_NHLCALLF(hlucttmtl,HLUCTTMTL)
				(&kbuf,(float*)tbuf,&mbuf,&nbuf,
				 ippp,&mnop,&nppp,
				 ippe,&mnoe,&nppe,
				 rpnt,&mpnt,&npnt,&Lopn,
				 iedg,&medg,&nedg,&Loen,
				 itri,&mtri,&ntri,&Lotn);
		if (c_nerro(&err_num) != 0) {
			e_msg = c_semess(0);
			e_text = "%s: %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,e_msg);
			return NhlFATAL;
		}

		ep = el + i * 3;
		e0 = *ep - ix_offset;
		e1 = *(ep+1) - ix_offset;
		e2 = *(ep+2) - ix_offset;
		if (tmp->ezmap) {
			tbuf[nbuf][0] = cos(DEGTORAD * rlat[e0]) *
				cos(DEGTORAD * rlon[e0]);
			tbuf[nbuf][1] = cos(DEGTORAD * rlat[e0]) *
				sin(DEGTORAD * rlon[e0]);
			tbuf[nbuf][2] = sin(DEGTORAD * rlat[e0]);
			tbuf[nbuf][3] = rdat[e0];
			tbuf[nbuf][4] = cos(DEGTORAD * rlat[e1]) *
				cos(DEGTORAD * rlon[e1]);
			tbuf[nbuf][5] = cos(DEGTORAD * rlat[e1]) *
				sin(DEGTORAD * rlon[e1]);
			tbuf[nbuf][6] = sin(DEGTORAD * rlat[e1]);
			tbuf[nbuf][7] = rdat[e1];
			tbuf[nbuf][8] = cos(DEGTORAD * rlat[e2]) *
				cos(DEGTORAD * rlon[e2]);
			tbuf[nbuf][9] = cos(DEGTORAD * rlat[e2]) *
				sin(DEGTORAD * rlon[e2]);
			tbuf[nbuf][10] = sin(DEGTORAD * rlat[e2]);
			tbuf[nbuf][11] = rdat[e2];
		}
		else {
			tbuf[nbuf][0] = rlon[e0];
			tbuf[nbuf][1] = rlat[e0];
			tbuf[nbuf][2] = 0.0;
			tbuf[nbuf][3] = rdat[e0];

			tbuf[nbuf][4] = rlon[e1];
			tbuf[nbuf][5] = rlat[e1];
			tbuf[nbuf][6] = 0.0;
			tbuf[nbuf][7] = rdat[e1];

			tbuf[nbuf][8] = rlon[e2];
			tbuf[nbuf][9] = rlat[e2];
			tbuf[nbuf][10] = 0.0;
			tbuf[nbuf][11] = rdat[e2];
		}			
		if (! cnp->sfp->missing_value_set)
			nbuf++;
		else if (tbuf[nbuf][3] != cnp->sfp->missing_value && 
			 tbuf[nbuf][7] != cnp->sfp->missing_value &&
			 tbuf[nbuf][11] != cnp->sfp->missing_value) {
			nbuf++;
		}
	}
	if (nbuf > 0) {
		_NHLCALLF(hlucttmtl,HLUCTTMTL)
			(&nbuf,(float*)tbuf,&mbuf,&nbuf,
			 ippp,&mnop,&nppp,
			 ippe,&mnoe,&nppe,
			 rpnt,&mpnt,&npnt,&Lopn,
			 iedg,&medg,&nedg,&Loen,
			 itri,&mtri,&ntri,&Lotn);

		if (c_nerro(&err_num) != 0) {
			e_msg = c_semess(0);
			e_text = "%s: %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,e_msg);
			return NhlFATAL;
		}
	}

	tbp = &(tmp->tri_block[0]);
	tbp->npnt = npnt;
	tbp->nedg = nedg;
	tbp->ntri = ntri;
	tbp->rpnt = rpnt;
	tbp->iedg = iedg;
	tbp->itri = itri;
	tbp->xs = tbp->xsr = wlx;
	tbp->xe = tbp->xer = wrx;
	tbp->ys = tbp->ysr = wby;
	tbp->ye = tbp->yer = wuy;
	tbp->ixmn = tbp->iymn = 0;
	if (cnp->sfp->slow_len == 0) {
		tbp->ixmx = tbp->iymx = cnp->sfp->fast_len - 1;
	}
	else {
		tbp->iymx = cnp->sfp->slow_len - 1;
		tbp->ixmx = cnp->sfp->fast_len - 1;
	}
	tmp->nblocks = 1;
	tmp->update_mode = TRIMESH_NOUPDATE;
	NhlFree(ippp);
	NhlFree(ippe);
	NhlFree(el);
	/*printf("total number of edges %d\n",nedg); */

	return NhlNOERROR;

}

/* conpackt structures */

typedef struct _cpoint {   /* a point node */
  float x;
  float y;
  float z;
  float dat;
} Cpoint;

typedef struct _cedge {
  int pix_1;   /* base index of edge point 1  - Lopn (4)  * (cpoint index + 1) (for Fortran indexing) */
  int pix_2;
  int trix_l;   /* base index of triangle to the left (Lotn (4)  * ctri index + 1) */
  int trix_r;   /* base index of triangle to the right (Lotn (4) * ctri index + 1) */
  int flag;
} Cedge;

typedef struct _ctri {
  int edge[3]; /* base index of edges of the triangle (Loen (5)  * cedge index + edge number) */
  int flag;
} Ctri;

static void SortEdges (
	TriBlock *tbp
       )
{
  Cpoint *cpoints = (Cpoint *)tbp->rpnt;
  Cedge  *cedges = (Cedge *) tbp->iedg;
  int i,tmp;
  
  for (i = 0; i < tbp->nedg / Loen; i++) {
    if (cpoints[cedges[i].pix_1 / Lopn].dat > cpoints[cedges[i].pix_2 / Lopn].dat) {
      tmp = cedges[i].pix_1;
      cedges[i].pix_1 = cedges[i].pix_2;
      cedges[i].pix_2 = tmp;
      tmp = cedges[i].trix_l;
      cedges[i].trix_l = cedges[i].trix_r;
      cedges[i].trix_r = tmp;
    }
  }
}

#ifdef BuildTRIANGLE

/* these are the shewchuk structures */

typedef struct _stri {
  int nodes[3];  /* vertex node ids */
} Stri;

typedef struct _snode {  /* a vertex node */
  double x;
  double y;
} Snode;

typedef struct _sedge {
  int nodes[2];   /* vertex nodes */
} Sedge;


#if 0   /* experimental boundary generation code (depends on Triangle) */
typedef struct _PointAndIndex {
  double x,y;
  int index;
} PointAndIndex;

static int hlu_sort(const void *p1, const void *p2)
{
  PointAndIndex *pi1 = (PointAndIndex *)p1;
  PointAndIndex *pi2 = (PointAndIndex *)p2;

  if (pi1->x < pi2->x)
    return -1;
  if (pi1->x > pi2->x)
    return 1;
  if (pi1->y < pi2->y)
    return -1;
  if (pi1->y > pi2->y)
    return 1;
  printf("points %d and %d compare as equal\n", pi1->index, pi2->index);
  return 0;
}

static double ccw(PointAndIndex* p1, PointAndIndex* p2, PointAndIndex* p3)
{
  return (p2->x - p1->x)*(p3->y - p1->y) - (p2->y - p1->y)*(p3->x - p1->x);
}

static void
convex_hull(PointAndIndex* points, ng_size_t npoints, PointAndIndex*** out_hull, ng_size_t* out_hullsize)
{
  PointAndIndex** hull;
  ng_size_t i, t, k = 0;
 
  hull = *out_hull;
 
  /* lower hull */
  for (i = 0; i < npoints; ++i) {
    while (k >= 2 && ccw(hull[k-2], hull[k-1], &(points[i])) < 0) --k;
    hull[k++] = &points[i];
  }
 
  /* upper hull */
  for (i = npoints-2, t = k+1; i >= 0; --i) {
    while (k >= t && ccw(hull[k-2], hull[k-1], &(points[i])) < 0) --k;
    hull[k++] = &points[i];
  }
 
  *out_hull = hull;
  *out_hullsize = k;
}


static int* MarkBoundaryPoints(int npnts, int *plist, float *rlon, float *rlat)
{
  double ax, ay, bx, by, cx, cy, dx, dy;
  double x1,x2,y1,y2,tx,ty;
  int i;
  int pcount, pix;
  PointAndIndex *pandi;
  PointAndIndex **hull;
  ng_size_t hull_point_count;
  
  ax = bx = cx = dx = rlon[plist[0]];
  ay = by = cy = dy = rlat[plist[0]];
  x1 = x2 = ax;
  y1 = y2 = ay;

  pcount = npnts;
  for (i = 1; i < npnts; i++) {
    tx = rlon[plist[i]];
    ty = rlat[plist[i]];
    if (tx > x1 && tx < x2 && ty > y1 && ty < y2) {
      plist[i] = -1;
      pcount--;
      continue;
    }
    if (tx - ty > ax - ay) {
      ax = tx;
      ay = ty;
    }
    if (tx + ty > bx + by) {
      bx = tx;
      by = ty;
    }
    if (tx - ty < cx - cy) {
      cx = tx;
      cy = ty;
    }
    if (tx + ty < dx + dy) {
      dx = tx;
      dy = ty;
    }
    x1 = MAX(cx,dx);
    x2 = MIN(ax,bx);
    y1 = MAX(ay,dy);
    y2 = MIN(cy,by);
  }
  for (i = 0; i < npnts; i++) {
    if (plist[i] < 0) 
      continue; /* already removed */
    tx = rlon[plist[i]];
    ty = rlat[plist[i]];
    if (tx > x1 && tx < x2 && ty > y1 && ty < y2) {
      plist[i] = -1;
      pcount--;
      continue;
    }
    /*printf("%f,%f\n",tx,ty);*/
  }
  printf("points remaining %d\n",pcount);
  
  pandi = (PointAndIndex *)  NhlMalloc(pcount * sizeof(PointAndIndex));

  pix = 0;
  for (i = 0; i < npnts; i++) {
    if (plist[i] < 0)
      continue;
    pandi[pix].x = rlon[plist[i]];
    pandi[pix].y = rlat[plist[i]];
    pandi[pix].index = i;  /* now indexing to the current list of points, not the overall list */
    pix++;
  }
  printf("hull candidate point count: %d\n", pix);

  qsort(pandi,pix,sizeof(PointAndIndex),hlu_sort);

  hull = (PointAndIndex **) NhlMalloc(pix * sizeof(PointAndIndex *));
	
  convex_hull(pandi,pix,&hull,&hull_point_count);

/*  printf("hull point count: %ld\n", hull_point_count);*/

  /* use plist as the boundary point indicator */

  memset(plist,0,npnts * sizeof(int));
  for (i = 0; i < hull_point_count; i++) {
    printf("%f %f\n",hull[i]->x,hull[i]->y);
    plist[hull[i]->index] = 1;
  }

  return plist;

}
    

static NhlErrorTypes AddBoundarySegments
  (
   TriBlock *tbp,
   int *npnt,
   int *npnt_alloc,
   double **points,
   float **dat,
   int *npnt_added,
   float missing_value
   )
{
  int nbounds = sqrt(*npnt);
  int xcount,ycount;
  double xsize = tbp->xe - tbp->xs;
  double ysize = tbp->ye - tbp->ys;
  double xinc, yinc;
  double xin, yin, xt, yt;
  int pcount = *npnt;
  int i;
  
  xcount = 2 * nbounds * xsize / (xsize + ysize);
  ycount = 2 * nbounds * ysize / (xsize + ysize);
  xinc = xsize / xcount;
  yinc = ysize / ycount;

  if (*npnt + 4 * nbounds > *npnt_alloc) {
    *npnt_alloc= *npnt+ 5 * nbounds;
    *points = (double *)NhlRealloc(*points,2 * *npnt_alloc * sizeof(double));
    *dat = (float *) NhlRealloc(*dat,*npnt_alloc * sizeof(float));
  }
    
  yin = tbp->ys;
  if (yin != -90 && yin != 90) {
	  for (i = 0; i < xcount; i++) {
		  xin = tbp->xs + i * xinc;
		  c_mdptra(yin,xin,&xt,&yt);
		  if (xt > 1e10 || yt > 1e10)
			  continue;
		  (*points)[2 * pcount] = xt;
		  (*points)[2 * pcount + 1] = yt;
		  pcount++;
	  }
  }
  xin = tbp->xe;
  for (i = 0; i < ycount; i++) {
    yin = tbp->ys + i * yinc;
    c_mdptra(yin,xin,&xt,&yt);
    if (xt > 1e10 || yt > 1e10)
      continue;
    (*points)[2 * pcount] = xt;
    (*points)[2 * pcount + 1] = yt;
    pcount++;
  }
  yin = tbp->ye;
  if (yin != -90 && yin != 90) {
	  for (i = xcount - 1; i >= 0; i--) {
		  xin = tbp->xs + i * xinc;
		  c_mdptra(yin,xin,&xt,&yt);
		  if (xt > 1e10 || yt > 1e10)
			  continue;
		  (*points)[2 * pcount] = xt;
		  (*points)[2 * pcount + 1] = yt;
		  pcount++;
	  }
  }
  xin = tbp->xs;
  for (i = ycount - 1; i >= 0; i--) {
	  yin = tbp->ys + i * yinc;
	  c_mdptra(yin,xin,&xt,&yt);
	  if (xt > 1e10 || yt > 1e10)
		  continue;
	  (*points)[2 * pcount] = xt;
	  (*points)[2 * pcount + 1] = yt;
	  pcount++;
  }
  *npnt_added = pcount - *npnt;
  for (i = *npnt; i < pcount; i++) {
	  (*dat)[i] = missing_value;
  }
  return NhlNOERROR;
}
#endif   /* #if 0 for boundary generation code that is not used now but may be in the future */


static NhlErrorTypes BuildDelaunayMesh
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
	int npnt,nedg;
	int ntri = 0;

	int i,j;
	struct triangulateio in,out,vout;
	char *flags;
	Stri *stris = NULL;
        Snode *snodes;
	Sedge *sedges, *vedges;
        Cpoint *cpoints;
        Cedge *cedges;
        Ctri *ctris;
	TriBlock *tbp;
	int block_ix;
	int tid;

#pragma omp parallel shared(cnp, tmp) \
	private(tbp,block_ix,npnt,ntri,nedg,i,j,in,out,vout, \
		snodes,stris,sedges,vedges,cpoints,cedges,ctris,tid)
	{
#ifdef _OPENMP
	    tid = omp_get_thread_num();
#else
	    tid = 0;
#endif
	    if (tid == 0) {
		    CreateTilePartitions(tmp,cnl,entry_name);
	    }

#pragma omp barrier


#pragma omp for schedule(static,1)
	    for (block_ix = 0; block_ix < tmp->nblocks; block_ix++) {
		    npnt = 0;
		    ntri = 0;
		    nedg = 0;
		    tbp = &(tmp->tri_block[block_ix]);

		    if (tbp->npnt == 0) {
			    tbp->npnt = 0;
			    tbp->nedg = 0;
			    tbp->ntri = 0;
			    tbp->rpnt = NULL;
			    tbp->iedg = NULL;
			    tbp->itri = NULL;
			    NhlFree(tbp->points);
			    NhlFree(tbp->dat);
			    continue;
		    }
		    memset(&in,0,sizeof(struct triangulateio));
		    memset(&out,0,sizeof(struct triangulateio));
		    memset(&vout,0,sizeof(struct triangulateio));
		    in.numberofpointattributes = 0;
		    in.numberoftriangles = 0;

		    if (cnp->verbose_triangle_info) {
			    flags = "IBzveVV";
		    }
		    else {
			    flags = "IBzveQ";
		    }

		    in.pointlist = tbp->points;
		    out.pointlist = in.pointlist;
		    in.numberofpoints = tbp->npnt;

		    triangulate(flags,&in,&out,&vout);
/*
  printf("triangulation completed\n");
*/
                    /* The call to triangulate() above has allocated 4 blocks of memory, 
                     * but we only need vout.edgelist.  Go ahead and free up the others.
                     * We'll free up vout.edgelist later when we're done with it.
                     * Jira ncl-2516.
                     */
                    free(vout.pointlist);
                    free(vout.pointattributelist);
                    free(vout.normlist);

		    stris = (Stri *) out.trianglelist;
		    sedges = (Sedge *) out.edgelist;
		    vedges = (Sedge *) vout.edgelist;

		    npnt = out.numberofpoints;
		    ntri = out.numberoftriangles;
		    nedg = out.numberofedges;
		    snodes = (Snode *)tbp->points;
		    cpoints = NhlMalloc(npnt * sizeof(Cpoint));
		    cedges = NhlMalloc(nedg * sizeof(Cedge));
		    ctris = NhlMalloc(ntri * sizeof(Ctri));
		    memset(ctris,(char) 0,ntri *sizeof(Ctri));


		    for (i = 0; i < npnt; i++) {
			    cpoints[i].x = (float) snodes[i].x;
			    cpoints[i].y = (float) snodes[i].y;
			    cpoints[i].z = 0.0;
			    cpoints[i].dat = tbp->dat[i];
		    }
		    NhlFree(tbp->points);
		    tbp->points = NULL;
		    NhlFree(tbp->dat);
		    tbp->dat = NULL;

		    for (i = 0; i < nedg; i++) {
			    cedges[i].pix_1 = sedges[i].nodes[0] * Lopn;
			    cedges[i].pix_2 = sedges[i].nodes[1] * Lopn;
			    /* the Voronoi edges have the same indexes as the Delaunay triangles (they are duals) */
			    cedges[i].trix_l =  vedges[i].nodes[0] > -1 ? vedges[i].nodes[0] * Lotn : -1; /* plus edge number within triangle */
			    cedges[i].trix_r =  vedges[i].nodes[1] > -1 ? vedges[i].nodes[1] * Lotn : -1; /* plus edge number within triangle */
			    cedges[i].flag = 0;
			    for (j = 0; j < 3; j++) {
				    if (vedges[i].nodes[0] > -1 && stris[vedges[i].nodes[0]].nodes[j] == sedges[i].nodes[0]) {
					    ctris[vedges[i].nodes[0]].edge[j] = i * Loen;
					    cedges[i].trix_l += (j + 1);
				    }
				    if (vedges[i].nodes[1] > -1 && stris[vedges[i].nodes[1]].nodes[j] == sedges[i].nodes[1]) {
					    ctris[vedges[i].nodes[1]].edge[j] = i * Loen;
					    cedges[i].trix_r += (j+1);
				    }
			    }
		    }
		    if (cnp->sfp->missing_value_set) {
			    /* since there are only 3 values, stored in 6 possible locations, looking at 5 of them should be sufficient */
			    for (i = 0; i < ntri; i++) {
				    if (cpoints[cedges[ctris[i].edge[0]/Loen].pix_1/Lopn].dat == cnp->sfp->missing_value ||
					cpoints[cedges[ctris[i].edge[1]/Loen].pix_1/Lopn].dat == cnp->sfp->missing_value ||
					cpoints[cedges[ctris[i].edge[2]/Loen].pix_1/Lopn].dat == cnp->sfp->missing_value ||
					cpoints[cedges[ctris[i].edge[0]/Loen].pix_2/Lopn].dat == cnp->sfp->missing_value ||
					cpoints[cedges[ctris[i].edge[1]/Loen].pix_2/Lopn].dat == cnp->sfp->missing_value) { 
					    ctris[i].flag = 1;
				    }
			    }
		    }
		    else {
			    for (i = 0; i < ntri; i++) {
				    if (cpoints[cedges[ctris[i].edge[0]/Loen].pix_1/Lopn].dat >= 1e32 || 
					cpoints[cedges[ctris[i].edge[1]/Loen].pix_1/Lopn].dat >= 1e32 || 
					cpoints[cedges[ctris[i].edge[2]/Loen].pix_1/Lopn].dat >= 1e32 ||
					cpoints[cedges[ctris[i].edge[0]/Loen].pix_2/Lopn].dat >= 1e32 ||
					cpoints[cedges[ctris[i].edge[1]/Loen].pix_2/Lopn].dat >= 1e32) {
					    ctris[i].flag = 1;
				    }
			    }
		    }

		    free(stris);
		    free(sedges);
		    free(vedges);   /* AKA, vout.edgelist */

		    tbp->npnt = npnt * Lopn;
		    tbp->nedg = nedg * Loen;
		    tbp->ntri = ntri * Lotn;
		    tbp->rpnt = (float *) cpoints;
		    tbp->iedg = (int *) cedges;
		    tbp->itri = (int *) ctris;
		    SortEdges(tbp);
	    }

	}

        tmp->update_mode = TRIMESH_NOUPDATE;

	return NhlNOERROR;
}

#endif   /* ifdef BuildTRIANGLE */

static void FreeTriBlockContents (
	TriBlock *tri_block,
	int *count
)
{
	TriBlock *tb;
	int i;
	if (*count == 0) 
		return;

	for (i = 0; i < *count; i++) {
		tb = &(tri_block[i]);
		NhlFree(tb->rpnt);
		NhlFree(tb->iedg);
		NhlFree(tb->itri);
	}
	*count = 0;

	return;
}

/*
 * Function:	CnTriMeshRendererInitialize
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
CnTriMeshRendererInitialize
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
	NhlCnTriMeshRendererLayer tml = (NhlCnTriMeshRendererLayer) new;
	NhlCnTriMeshRendererLayerPart *tmp =  &tml->cntrimeshrenderer;
	static int initial_block_count = 16;

	load_hluct_routines(False);
	
	/* allowing for multiple blocks -- initialize the block pointers to allow an initial supply */

	tmp->tri_block = (TriBlock *) NhlCalloc(initial_block_count, sizeof (TriBlock));
	tmp->nblocks = 0;
	tmp->nblocks_alloced = initial_block_count;
        tmp->trans_change_count = 0;
        tmp->ezmap = False;

        return ret;
}

/*
 * Function:	CnTriMeshRendererDestroy
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
static NhlErrorTypes CnTriMeshRendererDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlCnTriMeshRendererLayer tml = (NhlCnTriMeshRendererLayer) inst;
	NhlCnTriMeshRendererLayerPart *tmp =  &tml->cntrimeshrenderer;

	FreeTriBlockContents(tmp->tri_block,&(tmp->nblocks));
	NhlFree(tmp->tri_block);

	return ret;
}

/*
 * Function:	SetCtParams
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

static NhlErrorTypes SetCtParams
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
			for (j = 0; j < NhlNumber(Ct_Params); j ++) {
				if (! strcmp(Ct_Params[j].name,param)) {
					matched = True;
					type = Ct_Params[j].type;
					break;
				}
			}
			if (matched && type == cnInt) {
				c_ctseti(param,(int) value);
			}
			else if (matched && type == cnFloat) {
				c_ctsetr(param,value);
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
	
	c_ctseti("PAI",cpix);
	if (! reg_attrs->perim_on)
		c_ctseti("CLU",0);
	else if (cpix == -1 && cl->contourplot.missing_val_perim_grid_bound_on)
		c_ctseti("CLU",2);
	else
		c_ctseti("CLU",1);

	/* Only set the grid bound identifier (99) if the GridBoundFill resources are set to allow the grid bound area to be visible;
	   this is because the grid boundary needs to be calculated with more precision, potentially impacting performance */
	if (reg_attrs == &cl->contourplot.grid_bound) {
		if (cpix == -1 && reg_attrs->fill_color > NhlTRANSPARENT && reg_attrs->fill_pat > NhlHOLLOWFILL) {
			c_ctseti("AIA",99);     
			/*c_ctsetr("PIT",0.001); */ /* forced to the minimum recommended value, regardless of max_point_distance */
		}
	}
	else if (cpix == -1)
                c_ctseti("AIA",-1);
	else if (cpix == -2)
		c_ctseti("AIA",97);
	else
		c_ctseti("AIA",-1);

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
                cnp->high_lbls.gks_plcolor = NhlTRANSPARENT;
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
        if (cnp->low_lbls.back_color == NhlTRANSPARENT)
                cnp->low_lbls.gks_bcolor =  NhlTRANSPARENT;
        else
                cnp->low_lbls.gks_bcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->low_lbls.back_color);
        if (cnp->low_lbls.perim_lcolor == NhlTRANSPARENT)
                cnp->low_lbls.gks_plcolor = NhlTRANSPARENT;
        else
                cnp->low_lbls.gks_plcolor =
                        _NhlGetGksCi(cl->base.wkptr,
                                     cnp->low_lbls.perim_lcolor);
#if 0
	if (cnp->missing_val.fill_color > NhlTRANSPARENT && cnp->missing_val.fill_pat > NhlHOLLOWFILL) {
		SetRegionAttrs(cl,&cnp->grid_bound,-1);
		SetRegionAttrs(cl,&cnp->missing_val,-1); 
	}
	else {
		SetRegionAttrs(cl,&cnp->missing_val,-1); 
		SetRegionAttrs(cl,&cnp->grid_bound,-1);
	}
#endif
	SetRegionAttrs(cl,&cnp->grid_bound,-1);
	SetRegionAttrs(cl,&cnp->missing_val,-1); 
	SetRegionAttrs(cl,&cnp->out_of_range,-2);

	*do_lines = True;
	*do_labels = False;

	gset_line_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_text_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_linewidth(1.0);

	c_ctseti("CLS",0);		/* Conpack not to select levels */
	c_ctseti("NCL",cnp->level_count); 
	clvp = (float *) cnp->levels->data;
	clup = (int *) cnp->level_flags->data;
	c_ctseti("DPU",-1); /* dash pattern use flag */

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
		c_ctseti("PAI",pai);
		c_ctsetr("CLV",(float)clvp[i]);
		c_ctseti("AIB",aib);
		c_ctseti("AIA",aia);

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
		c_ctseti("CLU",flag);
		c_ctsetc("LLT",cp);
	}
	if (cnp->level_selection_mode != NhlEXPLICITLEVELS)
		c_ctsetr("CIU",(float)cnp->level_spacing);
 
/* Set up for labels */

/* Conpack not to render the Informational label */
	c_ctsetc("ILT"," ");

/* Line labels */
	if (! cnp->line_lbls.on) {
		c_ctseti("LLP",0); 
	}
	else if (cnp->llabel_placement == NhlCONSTANT) {
		*do_labels = True;
		c_ctseti("LLP",1);
#if 0
		c_ctsetr("DPS",
			 (float)(cnp->line_lbls.real_height / cl->view.width));
		c_ctsetr("DPV",(float).015);
#endif
#if 0
		c_ctsetr("RC3",(float)0.0);
		c_ctseti("LLP",2);
		if (cnp->line_lbls.angle < 0.0) 
			c_ctseti("LLO",1); /* angle to contour direction */
		else {
			c_ctseti("LLO",0); /* fixed angle  */
			c_ctsetr("LLA",(float)cnp->line_lbls.angle);
		}
#endif

	}
	else if (cnp->llabel_placement == NhlRANDOMIZED) {
		*do_labels = True;
		c_ctseti("LLP",-2);
		if (cnp->line_lbls.angle < 0.0) 
			c_ctseti("LLO",1); /* angle to contour direction */
		else {
			c_ctseti("LLO",0); /* fixed angle  */
			c_ctsetr("LLA",(float)cnp->line_lbls.angle);
		}
		if (cnp->llabel_density > 0.0) {
			float rc1 = 0.10 / cnp->llabel_density;
			float rc2 = 0.25 / cnp->llabel_density;
			float rc3 = 0.05 / cnp->llabel_density;	
			c_ctsetr("RC1",rc1);
			c_ctsetr("RC2",rc2);
			c_ctsetr("RC3",rc3);
		}
	}
	else {
		*do_labels = True;
		c_ctseti("LLP",-3);
		if (cnp->line_lbls.angle < 0.0) 
			c_ctseti("LLO",1); /* angle to contour direction */
		else {
			c_ctseti("LLO",0); /* fixed angle  */
			c_ctsetr("LLA",(float)cnp->line_lbls.angle);
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
			pc3 = 360.0;
			pc1 *= cnp->llabel_density;
			pc5 *= cnp->llabel_density;

			c_ctsetr("PC1",pc1);
			c_ctsetr("PC2",pc2);
			c_ctsetr("PC3",pc3);
			c_ctsetr("PC4",pc4);
			c_ctsetr("PC5",pc5);
			c_ctsetr("PC6",pc6);
			c_ctsetr("PW1",pw1);
			c_ctsetr("PW2",pw2);
			c_ctsetr("PW3",pw3);
			c_ctsetr("PW4",pw4);
			/*printf("pc 1-6: %f %f %f %f %f %f pw1-4 %f %f %f %f\n",pc1,pc2,pc3,pc4,pc5,pc6,pw1,pw2,pw3,pw4);*/
		}
	}

	if (*do_labels) {
		height = cnp->line_lbls.real_height / cl->view.width;
		c_ctsetr("LLS",(float)height);
		c_ctsetr("LLW", 
			 (float) (height * cnp->line_lbls.perim_space));
		if (cnp->line_lbls.back_color == NhlTRANSPARENT) {
			if (cnp->line_lbls.perim_lcolor == NhlTRANSPARENT ||
			    ! cnp->line_lbls.perim_on) 
				c_ctseti("LLB",0); 
			else
				c_ctseti("LLB",1);
		}
		else {
			c_ctseti("LBC",cnp->line_lbls.back_color);
			if (cnp->line_lbls.perim_lcolor == NhlTRANSPARENT ||
			    ! cnp->line_lbls.perim_on) 
				c_ctseti("LLB",2);
			else
				c_ctseti("LLB",3);
		}
	}

/*
 * In order to allow user control of the high and low attributes 
 * individually set the appropriate part of the flag on if either 
 * the high or the low is on. Further distinguishing between high and low
 * occurs in the low level routine cpchhl_
 */
	if (! cnp->high_lbls.on)
		c_ctsetc("HIT"," ");
	else 
		c_ctsetc("HIT",(NhlString)cnp->high_lbls.text);

	if (! cnp->low_lbls.on)
		c_ctsetc("LOT"," ");
	else
		c_ctsetc("LOT",(NhlString)cnp->low_lbls.text);

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
 * Update 2013/01/14: using the new transparency features, individual control
 * of cnLowLabelPerimOn is now possible. And cnLowLabelBackgroundColor and
 * cnLowLabelPerimColor can be transparent independent of the cnHighLabel values.
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
		c_ctsetr("HLS",(float)height);
		c_ctsetr("HLW",(float)(cnp->high_lbls.perim_space  * height));
		c_ctsetr("HLA",(float)cnp->high_lbls.angle);
		c_ctseti("HLO", (int) cnp->high_low_overlap);

		if ((!cnp->high_lbls.on || cnp->high_lbls.back_color == NhlTRANSPARENT) &&
		    (!cnp->low_lbls.on || cnp->low_lbls.back_color == NhlTRANSPARENT)) {
			if ((!cnp->high_lbls.perim_on || cnp->high_lbls.perim_lcolor == NhlTRANSPARENT) &&
			    (! cnp->low_lbls.perim_on || cnp->low_lbls.perim_lcolor == NhlTRANSPARENT))
				c_ctseti("HLB",0); 
			else {
				c_ctseti("HLB",1);
				cnp->hlb_val = 1;		
			}
		}
		else {
			if ((!cnp->high_lbls.perim_on || cnp->high_lbls.perim_lcolor == NhlTRANSPARENT) &&
			    (! cnp->low_lbls.perim_on || cnp->low_lbls.perim_lcolor == NhlTRANSPARENT)) {
				c_ctseti("HLB",2);
				cnp->hlb_val = 2;		
			}
			else {
				c_ctseti("HLB",3);
				cnp->hlb_val = 3;		
			}
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
	NhlBoolean	*do_fill,
	NhlBoolean      *almost_const
)
#else
(cl,do_fill)
        NhlContourPlotLayer	cl;
	NhlBoolean	*do_fill;
	NhlBoolean      *almost_const;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlContourPlotLayerPart	*cnp = &(cl->contourplot);
	float *levels = (float *) cnp->levels->data;
	int i;

	*almost_const = False;
        _NhlSetFillOpacity(cl, cnp->fill_opacity);
/*
 * Since the missing value fill resources are not supposed to be affected
 * by the mono flags, you cannot optimize the fill away if mono fill color is
 * true and fill color is transparent or mono fill pattern is true and the
 * fill pattern is hollow. So just keep it simple.
 * 
 */

	if (! cnp->fill_on) {
		*do_fill = False;
		return ret;
	}

	*do_fill = True;

	for (i = 0; i< cnp->level_count -1 ; i++) { 
		if (cnp->zmin >= levels[i] &&
		    cnp->zmax <= levels[i + 1]) {
			*almost_const = True; 
			return ret;
		}
	}
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
	NhlContourPlotLayerPart	*cnp = 
		(NhlContourPlotLayerPart *) &cl->contourplot;
	NhlBoolean		ezmap = False;
	int			status;
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	int			xrev,yrev;
	float			xa[5],ya[5];
	float		        xeps,yeps;
	char		cval[4];
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


	if (! ezmap) {
		float twlx,twrx,twby,twuy;
		float gwlx,gwrx,gwby,gwuy;
		float txmin,txmax,tymin,tymax;
		float gxmin,gxmax,gymin,gymax;
		NhlBoolean lbox, rbox, bbox, tbox;

#if 0
		if (cnp->smoothing_on)
			c_arseti("RC",1);
		else
			c_arseti("RC",0);
#endif
		c_arseti("RC",1);
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

		if (twlx < twrx) {
			xrev = cl->trans.x_reverse;
		}
		else {
			xrev = ! cl->trans.x_reverse;
		}
		if (twby < twuy) {
			yrev = cl->trans.y_reverse;
		}
		else {
			yrev = ! cl->trans.y_reverse;
		}

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
#if 0
		float wb,wt,wl,wr;


		/* apparently none of this stuff is necessary as long as you set the vertical strips correctly*/
		if (! cnp->fix_fill_bleed)
			return NhlNOERROR; 
		ret = NhlVAGetValues(cnp->trans_obj->base.id,
				     NhlNmpBottomWindowF,&wb,
				     NhlNmpTopWindowF,&wt,
				     NhlNmpLeftWindowF,&wl,
				     NhlNmpRightWindowF,&wr,
				     NULL);
		/* draw thin rectangles */
		xeps = 1e-5 * fabs(wt - wb);
		yeps = 1e-5 * fabs(wr - wl);
		xa[0] = xa[3] = xa[4] = wl;
		xa[1] = xa[2] = wl + xeps;
		ya[0] = ya[1] = ya[4] = wb;
		ya[2] = ya[3] = wt;
		_NhlAredam(cnp->aws,xa,ya,1,3,0,-1,entry_name);
		xa[0] = xa[3] = xa[4] = wr;
		xa[1] = xa[2] = wr - xeps;
		ya[0] = ya[1] = ya[4] = wb;
		ya[2] = ya[3] = wt;
		_NhlAredam(cnp->aws,xa,ya,1,3,0,-1,entry_name);
		xa[0] = xa[3] = xa[4] = wl + xeps;
		xa[1] = xa[2] = wr - xeps;
		ya[0] = ya[1] = ya[4] = wb;
		ya[2] = ya[3] = wb + yeps;
		_NhlAredam(cnp->aws,xa,ya,1,3,0,-1,entry_name);
		xa[0] = xa[3] = xa[4] = wl + xeps;
		xa[1] = xa[2] = wr - xeps;
		ya[0] = ya[1] = ya[4] = wt - yeps;
		ya[2] = ya[3] = wt;
		_NhlAredam(cnp->aws,xa,ya,1,3,0,-1,entry_name);

#endif
		c_arseti("RC",1);
		c_mpgetc("OU",cval,3);
		c_mpsetc("OU","NO");
		c_mpseti("G2",3);
		c_mpseti("VS",1);
		_NhlMapbla(cnp->aws,entry_name);
		c_mpsetc("OU",cval);

	}

	return NhlNOERROR;
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

	c_ctseti("CAF", -1);
	xlinear = True;
        ylinear = True;
	if (cnp->sfp->d_arr->num_dimensions == 1) {
		/* x and y cells are meaningless, but we need something */
		mcount = ncount = sqrt(cnp->sfp->fast_len);
	} else {
		mcount = cnp->sfp->fast_len;
		ncount = cnp->sfp->slow_len;
	}
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
	
	if (cnp->cws_id > 1) {
		_NhlFreeWorkspace(cnp->cws_id);
	}
	cnp->cws_id = 
		_NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
					 (*msize * *nsize) * sizeof(int));
	if (cnp->cws_id < 1) 
		return MIN(ret,(NhlErrorTypes)cnp->cws_id);
	if ((cnp->cws = _NhlUseWorkspace(cnp->cws_id)) == NULL) {
		e_text = 
			"%s: error reserving cell array workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	{
		NhlWorkspaceRec *cwsrp = (NhlWorkspaceRec *) cnp->cws;
		int *cell = cwsrp->ws_ptr;
		int grid_fill_ix, i, j;
		if (Cnp->missing_val.gks_fcolor > NhlTRANSPARENT &&
		    Cnp->missing_val.fill_pat > NhlHOLLOWFILL) {
			grid_fill_ix =	Cnp->missing_val.gks_fcolor;
		}
		else {
			grid_fill_ix =	Cnp->grid_bound.gks_fcolor;
		}
		grid_fill_ix = grid_fill_ix < 0 ? NhlTRANSPARENT_CI : grid_fill_ix;
		/*grid_fill_ix = -9999;*/
		for (j = 0; j < *nsize; j++) {
			for (i = 0; i < *msize; i++) {
				*(cell + j * *msize + i) = grid_fill_ix;
			}

		}
	}

	return ret;
}

/*
 * Function:	cnInitDataArray
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

static NhlErrorTypes cnInitDataArray
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

	c_ctseti("CAF", -1);
	xlinear = True;
        ylinear = True;
	if (cnp->sfp->d_arr->num_dimensions == 1) {
		/* x and y cells are meaningless, but we need something */
		mcount = ncount = sqrt(cnp->sfp->fast_len);
	} else {
		mcount = cnp->sfp->fast_len;
		ncount = cnp->sfp->slow_len;
	}
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
	return ret;
}

static NhlErrorTypes UpdateMeshData
#if	NhlNeedProto
(
	NhlCnTriMeshRendererLayerPart *tmp,
	NhlContourPlotLayer     cnl,
	NhlString entry_name
)
#else
(tmp,cnl,entry_name)
        NhlCnTriMeshRendererLayerPart *tmp;
	NhlContourPlotLayer     cnl;
	NhlString entry_name;
#endif
{
	NhlContourPlotLayerPart	*cnp = &cnl->contourplot;
 	float *rlat,*rlon;
	float *rdat;
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	double xtmp,ytmp,xt,yt;
	int block_ix;
	int pcount[256];
	TriBlock *tbp;
	Cpoint *cpp;
	float xs,xe,ys,ye;

	rlat = (float*)cnp->sfp->y_arr->data;
	rlon = (float*)cnp->sfp->x_arr->data;
	rdat = (float*)cnp->sfp->d_arr->data;
	memset(pcount,0,sizeof(int) * MIN(tmp->nblocks,256));

	for (i = 0; i < cnp->sfp->fast_len; i++) { 
		xtmp = (double) rlon[i];
		ytmp = (double) rlat[i];
		if (tmp->ezmap) {
			NGCALLF(mdptra,MDPTRA)(&ytmp,&xtmp,&xt,&yt);
			if (xt > 1e10 || yt > 1e10)
				continue;
		}
		else {
			xt = xtmp;
			yt = ytmp;
		}
		for (block_ix = 0; block_ix < tmp->nblocks; block_ix++) {
			tbp = &(tmp->tri_block[block_ix]);
			cpp = (Cpoint *) tbp->rpnt;
			xs = tbp->xs;
			xe = tbp->xe;
			ys = tbp->ys;
			ye = tbp->ye;
			if (xt < xs || xt > xe || yt < ys || yt > ye) {
				continue;
			}
			if (tmp->ezmap != 2) {
				xt = xtmp;
				yt = ytmp;
			}
			if (_NhlCmpFAny2((float)xt,cpp[pcount[block_ix]].x,6,_NhlMIN_NONZERO) == 0 &&
			    _NhlCmpFAny2((float)yt,cpp[pcount[block_ix]].y,6,_NhlMIN_NONZERO)) {
				cpp[pcount[block_ix]].dat = rdat[i];
				pcount[block_ix]++;
			}
			if (pcount[block_ix] > tbp->npnt / Lopn) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: internal logic error",entry_name);
				return NhlFATAL;
			}
		}
	}
	for (block_ix = 0; block_ix < tmp->nblocks; block_ix++) {
		tbp = &(tmp->tri_block[block_ix]);
		SortEdges(tbp);
	}
	return ret;
}

static NhlErrorTypes InitMesh
#if	NhlNeedProto
(
        NhlContourPlotLayer     cnl,
	NhlCnTriMeshRendererLayerPart	  *tmp,
	int                     do_ctmesh,
	NhlString		entry_name
        )
#else
(cnl,tmp)
        NhlContourPlotLayer     cnl;
	NhlCnTriMeshRendererLayerPart	  *tmp;
	int                     do_ctmesh;
	NhlString		entry_name;
#endif
{
	NhlContourPlotLayerPart 	  *cnp = &cnl->contourplot;
        NhlErrorTypes ret = NhlNOERROR;
	TriBlock *tbp;


	if (tmp->update_mode == TRIMESH_DATAUPDATE && tmp->nblocks > 0 && ! do_ctmesh) {
		ret = UpdateMeshData(tmp,cnl,entry_name);
		return ret;
	}
	else if (tmp->update_mode > TRIMESH_NOUPDATE || tmp->nblocks == 0) {
		if (cnp->sfp->grid_type == NhlMESHGRID) {
			if (cnp->sfp->element_nodes) {
				ret = BuildNativeMesh(tmp,cnl,entry_name);
			}
			else if (cnp->sfp->x_cell_bounds && 
				 cnp->sfp->y_cell_bounds) {
				ret = BuildNativeMeshFromBounds
					(tmp,cnl,entry_name);
			}				
			else {
#ifdef BuildTRIANGLE
				/* this routine sorts the edges for left/right so the ctmesh routine does not have to */
				ret = BuildDelaunayMesh(tmp,cnl,entry_name);
				/* we need to call ctmesh at least once */
				tbp = &(tmp->tri_block[0]);
				_NhlHLUCtmesh(tbp->rpnt,tbp->npnt,Lopn,
					      tbp->iedg,tbp->nedg,Loen,
					      tbp->itri,tbp->ntri,Lotn,
					      cnp->fws,cnp->iws,entry_name);
				return ret;
#else
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "Cannot create triangular mesh: supply additional resources or build with Triangle package");
				return NhlFATAL;
#endif
			}
		}

#ifdef _OPENMP
#ifdef BuildTRIANGLE
		else if (tmp->ezmap == 2) {
				ret = BuildDelaunayMesh(tmp,cnl,entry_name);
				/* we need to call ctmesh at least once */
				tbp = &(tmp->tri_block[0]);
				_NhlHLUCtmesh(tbp->rpnt,tbp->npnt,Lopn,
					      tbp->iedg,tbp->nedg,Loen,
					      tbp->itri,tbp->ntri,Lotn,
					      cnp->fws,cnp->iws,entry_name);
				return ret;
			
		}
#endif
#endif
		else {
			ret = BuildTriangularMesh(tmp,cnl,entry_name);
		}
	}

	tbp = &(tmp->tri_block[0]);
	_NhlCtmesh(tbp->rpnt,tbp->npnt,Lopn,
		   tbp->iedg,tbp->nedg,Loen,
		   tbp->itri,tbp->ntri,Lotn,
		   cnp->fws,cnp->iws,entry_name);

	return ret;
}

static NhlErrorTypes ContourLineRender (
        NhlContourPlotLayer 	          cnl,
	NhlCnTriMeshRendererLayerPart	  *tmp,
	int *                   mesh_inited,
	NhlString		entry_name
	)
{
	NhlErrorTypes  ret, subret;
	NhlContourPlotLayerPart   *cnp = &cnl->contourplot;
	TriBlock *tbp;
	int i;
	int do_ctmesh = 0;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;
	float pflx,pfrx,pfby,pfuy;

	if (! *mesh_inited) {
		ret = InitMesh(cnl,tmp,1,entry_name);
		if (ret < NhlWARNING) {
			ContourAbortDraw(cnl);
			return ret;
		}
		*mesh_inited = 1;

	}

/*
 * line rendering of the plot in segments now works allowing lines to be drawn when using OpenMP with raster fill.
 * Line rendering is still a sequential operation because the LLU code is not thread safe.
 */ 
#if 0
#pragma omp parallel shared(cnp, tmp,entry_name,Lopn,Loen,Lotn,flx,frx,fby,fuy,wlx,wrx,wby,wuy, ll) \
	private(tbp,Tbp,pflx,prrx,pfby,pfuy,ret,subret)							 
        {
#pragma omp for schedule(static,1)
#endif
	ret = subret = NhlNOERROR;
	do_ctmesh = 1;
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
	for (i = 0; i < tmp->nblocks; i++) {
		tbp = &(tmp->tri_block[i]);
		Tbp = tbp;

		pflx = c_cufx(tbp->xsr);
		pfrx = c_cufx(tbp->xer);
		pfby = c_cufy(tbp->ysr);
		pfuy = c_cufy(tbp->yer);
		c_set(pflx,pfrx,pfby,pfuy,tbp->xsr,tbp->xer,tbp->ysr,tbp->yer,ll);
		if (do_ctmesh)  {
			_NhlHLUCtmesh(tbp->rpnt,tbp->npnt,Lopn,
				      tbp->iedg,tbp->nedg,Loen,
				      tbp->itri,tbp->ntri,Lotn,
				      cnp->fws,cnp->iws,entry_name);
		}
#if 0
                {
			size_t j;
			float max = -1e30;
			float min = 1e30;
			for (j = 0; j < tbp->npnt; j++) {
				if (tbp->rpnt[j] > max) 
					max = tbp->rpnt[j];
				if (tbp->rpnt[j] < min)
					min = tbp->rpnt[j];
			}
			printf ("rpnt min/max %f %f\n",min,max);
		}
#endif
                
		subret = _NhlCtcldr(tbp->rpnt,tbp->iedg,tbp->itri,
				    cnp->fws,cnp->iws,entry_name);

		c_set(flx,frx,fby,fuy,wlx,wrx,wby,wuy,ll);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
		  ContourAbortDraw(cnl);
		  break;
		}
	}

#if 0
	}
#endif

	return ret;
}	

static NhlErrorTypes ContourLabelRender (
        NhlContourPlotLayer 	          cnl,
	NhlCnTriMeshRendererLayerPart	  *tmp,
	int *                   mesh_inited,
	NhlString		entry_name
	)
{
	NhlErrorTypes  ret, subret;
	NhlContourPlotLayerPart   *cnp = &cnl->contourplot;
	TriBlock *tbp;
	int i;
	int do_ctmesh = 0;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int ll;
	float pflx,pfrx,pfby,pfuy;
	float fheight, hlr, hlr_adj;

	if (! *mesh_inited) {
		ret = InitMesh(cnl,tmp,1,entry_name);
		if (ret < NhlWARNING) {
			ContourAbortDraw(cnl);
			return ret;
		}
		*mesh_inited = 1;

	}

	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
	tbp = &(tmp->tri_block[0]);
	cnp->line_lbls.count = 0;
	cnp->high_lbls.count = 0;
	cnp->low_lbls.count = 0;
	gset_fill_int_style(GSTYLE_SOLID);

	fheight = cnp->line_lbls.real_height / cnl->view.width; 
	fheight *= (wrx-wlx) / (tbp->xer - tbp->xsr);
	c_ctsetr("LLS",fheight);
	c_ctsetr("LLW", (float) (fheight * cnp->line_lbls.perim_space));
	c_pcsetr("PH",(float)cnp->line_lbls.pheight);
	c_pcsetr("PW",(float)cnp->line_lbls.pwidth);
	c_pcsetr("CS",(float)cnp->line_lbls.cspacing);
	c_pcseti("FN",cnp->line_lbls.font);
	c_pcseti("QU",cnp->line_lbls.quality);
	c_pcsetc("FC",cnp->line_lbls.fcode);
	fheight = cnp->high_lbls.real_height / cnl->view.width;
	fheight *= (wrx-wlx) / (tbp->xer - tbp->xsr);
	c_ctsetr("HLS",(float)fheight);
	c_ctsetr("HLW",(float)(cnp->high_lbls.perim_space  * fheight));
	c_ctgetr("HLR",&hlr);
	hlr_adj = hlr * (wrx - wlx) / (tbp->xer - tbp->xsr);
	c_ctsetr("HLR",hlr_adj);


#if 0
#pragma omp parallel shared(cnp, tmp,entry_name,Lopn,Loen,Lotn,flx,frx,fby,fuy,wlx,wrx,wby,wuy, ll) \
	private(tbp,Tbp,pflx,prrx,pfby,pfuy,ret,subret)							 
        {
#pragma omp for schedule(static,1)
#endif
	ret = subret = NhlNOERROR;
	do_ctmesh = 1;

	for (i = 0; i < tmp->nblocks; i++) {
		tbp = &(tmp->tri_block[i]);
		Tbp = tbp;

		pflx = c_cufx(tbp->xsr);
		pfrx = c_cufx(tbp->xer);
		pfby = c_cufy(tbp->ysr);
		pfuy = c_cufy(tbp->yer);
		c_set(pflx,pfrx,pfby,pfuy,tbp->xsr,tbp->xer,tbp->ysr,tbp->yer,ll);
		if (do_ctmesh)  {
			_NhlHLUCtmesh(tbp->rpnt,tbp->npnt,Lopn,
				      tbp->iedg,tbp->nedg,Loen,
				      tbp->itri,tbp->ntri,Lotn,
				      cnp->fws,cnp->iws,entry_name);
		}
		_NhlCtlbdr(tbp->rpnt,tbp->iedg,tbp->itri,
			 cnp->fws,cnp->iws,entry_name);

	}
	c_set(flx,frx,fby,fuy,wlx,wrx,wby,wuy,ll);
	c_ctsetr("HLR",hlr);

#if 0
	}
#endif

	return ret;
}	

static NhlErrorTypes RasterFillRender (
        NhlContourPlotLayer 	          cnl,
	NhlCnTriMeshRendererLayerPart	  *tmp,
	int *                   mesh_inited,
	NhlString		entry_name
	)
{
	int msize,nsize;
	float min_cell_size;
	NhlBoundingBox bbox;
	NhlErrorTypes  ret, subret;
	NhlContourPlotLayerPart   *cnp = &cnl->contourplot;
	TriBlock *tbp;
	int i;
	int nthreads;
	int fill_op = 0;

	ret = cnInitCellArray(cnl,&msize,&nsize,&bbox,
				 &min_cell_size,entry_name);
	if (ret < NhlWARNING) {
		return ret;
	}

	if (! *mesh_inited) {
		ret = InitMesh(cnl,tmp,0,entry_name);
		if (ret < NhlWARNING) {
			ContourAbortDraw(cnl);
			return ret;
		}
		*mesh_inited = 1;
	}



#pragma omp parallel shared(cnp, tmp,entry_name,Lopn,Loen,Lotn,nthreads,bbox,msize,nsize,min_cell_size,fill_op) \
  private(tbp,i)							 
	{
#ifdef _OPENMP
		nthreads = omp_get_num_threads();
#else
    nthreads = 1;
#endif
		if (nthreads > 1) fill_op = 2;

#pragma omp for schedule(static,1)

/*		for (i = tmp->nblocks -1 ; i >= 0; i--) { */
	        for (i = 0; i < tmp->nblocks ; i++) { 
			tbp = &(tmp->tri_block[i]);
			Tbp = tbp;
			if (tbp->npnt == 0) 
				continue;

			subret = _NhlCtcica(tbp->rpnt,tbp->iedg,tbp->itri,
					    cnp->fws,cnp->iws,cnp->cws,
					    msize,msize,nsize,
					    bbox.l,bbox.b,bbox.r,bbox.t,
					    min_cell_size,
					    cnp->raster_smoothing_on,
					    fill_op,
					    (void *) tbp,
					    entry_name);
		}
#pragma omp barrier
	}


#if 0   /* for debugging */

	{
		NhlWorkspaceRec *cwsrp = (NhlWorkspaceRec *) cnp->cws;
		int *cell = cwsrp->ws_ptr;
		int j;
		int grid_fill_ix;
		int cell_count;
		grid_fill_ix = MAX(Cnp->missing_val.gks_fcolor, Cnp->grid_bound.gks_fcolor);
		grid_fill_ix = grid_fill_ix < 0 ? NhlTRANSPARENT_CI : grid_fill_ix;

		cell_count = 0;
		for (j = 0; j < nsize; j++) {
			for (i = 0; i < msize; i++) {
				if (*(cell + j * msize + i) == grid_fill_ix) {
					printf("cell i %d j %d not initialized\n", i, j);
					cell_count++;
				}
			}
		}
		printf("%d cells of %d x %d array not initialized\n",cell_count,msize,nsize);
	}
#endif

	if (fill_op == 2) {
		subret = _NhlCtcica(NULL,NULL,NULL,
				    cnp->fws,cnp->iws,cnp->cws,
				    msize,msize,nsize,
				    bbox.l,bbox.b,bbox.r,bbox.t,
				    min_cell_size,
				    cnp->raster_smoothing_on,
				    3,
				    NULL,
				    entry_name);
	}

	if (cnp->cws != NULL) {
		subret = _NhlIdleWorkspace(cnp->cws);
		ret = MIN(subret,ret);
		cnp->cws = NULL;
	}
	return ret;
}


static NhlErrorTypes DoConstFillHack(
	NhlContourPlotLayerPart           *cnp,
	NhlBoolean on
	)
{
	int i,ix;
	float *levels = (float *) cnp->levels->data;
	static int save_fill_color = 0, save_fill_pattern = 0;
	static float save_fill_scale = 0;
	static NhlBoolean save_mono_fill_color = False, save_mono_fill_pattern = False,
		save_mono_fill_scale;
	float test_val;
	static float save_test_val;
	static int save_ix;
#if 0
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
	printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
	       flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
#endif
	if (! on) {
		cnp->mono_fill_color = save_mono_fill_color;
		cnp->mono_fill_pattern = save_mono_fill_pattern;
		cnp->mono_fill_scale = save_mono_fill_scale;
		cnp->fill_color = save_fill_color;
		cnp->fill_pattern = save_fill_pattern;
		cnp->fill_scale = save_fill_scale;
		cnp->data[save_ix] = save_test_val;
		return NhlNOERROR;
	}

	if (! cnp->data) {
		/*printf("no data\n");*/
		return NhlWARNING;
	}
	if (cnp->sfp->d_arr->num_dimensions == 2) {
		save_ix = (cnp->sfp->slow_len / 2) * cnp->sfp->fast_len + cnp->sfp->fast_len / 2;
		save_test_val = test_val = cnp->data[save_ix];
	}
	else {
		save_ix = cnp->sfp->d_arr->num_elements / 2;
		save_test_val = test_val = cnp->data[save_ix];
	}

	ix = -1;
	for (i = 0; i< cnp->level_count; i++) {
                if (test_val >= levels[i])
			continue;
		ix = i;
		break;
		
        }
	if (ix == -1) {
		ix = cnp->level_count;
	}
	if (ix > 1) {
		cnp->data[save_ix] = levels[0];
	}
	else {
		cnp->data[save_ix] = levels[cnp->level_count - 1];
	}
	
	save_mono_fill_color = cnp->mono_fill_color;
	save_mono_fill_pattern = cnp->mono_fill_pattern;
	save_mono_fill_scale = cnp->mono_fill_scale;
	save_fill_color = cnp->fill_color;
	save_fill_pattern = cnp->fill_pattern;
	save_fill_scale = cnp->fill_scale;
	save_test_val = test_val;

	if (! cnp->mono_fill_pattern)
		cnp->fill_pattern = ((int *) cnp->fill_patterns->data)[ix];
	if (! cnp->mono_fill_scale) 
		cnp->fill_scale = ((float *) cnp->fill_scales->data)[ix];
	if (! cnp->mono_fill_color)
		cnp->fill_color = ((int *) cnp->fill_colors->data)[ix];

	cnp->mono_fill_pattern = True;
	cnp->mono_fill_color = True;
	cnp->mono_fill_scale = True;

	return NhlNOERROR;
}

static NhlErrorTypes CnTriMeshRender
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
        NhlCnTriMeshRendererLayer tml = (NhlCnTriMeshRendererLayer) instance;
	NhlCnTriMeshRendererLayerPart	  *tmp = &tml->cntrimeshrenderer;
	NhlContourPlotLayerPart 	  *cnp = &cnl->contourplot;
	NhlTransformLayerPart   *tfp = &(cnl->trans);
	NhlString e_text;
        NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
	int mesh_inited = 0;
        Gint            err_ind;
        Gclip           clip_ind_rect;
	TriBlock *tbp;
	int trans_change_count;
	NhlBoolean     almost_const;
	int do_fill;
	int do_const_fill_hack = 0;
	int nthreads;

	tbp = &(tmp->tri_block[0]);

	Cnl = cnl;
	Cnp = cnp;
	Tmp = tmp;
	c_ctrset();


	subret = NhlVAGetValues(cnl->trans.overlay_trans_obj->base.id,
                                NhlNtrChangeCount,&trans_change_count,
                                NULL);
	if (trans_change_count > tmp->trans_change_count) {
		tmp->update_mode = TRIMESH_NEWMESH;
		tmp->trans_change_count = trans_change_count;
	}


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
		c_ctsetr("ORV",cnp->out_of_range_val);
		/* make sure the contour plot's own trans obj has the 
		   same out of range value */
		NhlVASetValues(tfp->trans_obj->base.id, 
			       NhlNtrOutOfRangeF,cnp->out_of_range_val, 
			       NULL);

#pragma omp parallel shared(nthreads) 
		{
#ifdef _OPENMP
			nthreads = omp_get_num_threads();
#else
			nthreads = 1;
#endif
			if (! (cnp->sfp->element_nodes ||
			       (cnp->sfp->x_cell_bounds && cnp->sfp->y_cell_bounds))) {
				if (cnp->sfp->d_arr->num_dimensions == 1) {
					tmp->ezmap = 2;
				}
#ifdef BuildTRIANGLE
				else if (nthreads > 1) {
					tmp->ezmap = 2;
				}
#endif
				else {
					tmp->ezmap = 1;
				}
			}
			else {
				tmp->ezmap = 1;
			}
		}
		if (tmp->ezmap == 2) 
			c_ctseti("MAP",Nhlcn1DMESHMAPVAL);
		else 
			c_ctseti("MAP",NhlcnMAPVAL);
	}
	else {
		tmp->ezmap = 0;
		c_ctseti("MAP",NhlcnTRIMESHMAPVAL);
	}


	ginq_clip(&err_ind,&clip_ind_rect);
        gset_clip_ind(GIND_CLIP);
	

	SetCtParams(cnl,entry_name);

	c_ctseti("WSO", 3);		/* error recovery on */
	c_ctseti("NVS",0);		/* no vertical strips */
	/*c_ctseti("HLE",1);*/              /* search for equal high/lows */
	c_ctseti("HLE",0);              /* search for equal high/lows */
        c_ctseti("SET",0);
        c_ctseti("RWC",500);
        c_ctseti("RWG",1500);
	c_ctsetc("CFT","");

	c_ctsetr("PIT",MAX(0.0,cnp->max_point_distance));
	
        if (cnp->smoothing_on) {
                c_ctsetr("T2D",cnp->smoothing_tension);
                c_ctsetr("SSL",cnp->smoothing_distance);
        }
        else {
                c_ctsetr("T2D",(float)0.0);
        }
	gset_fill_colr_ind((Gint)_NhlGetGksCi(cnl->base.wkptr,0));

	subret = UpdateLineAndLabelParams(cnl,&cnp->do_lines,&cnp->do_labels);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return ret;
	}

	subret = UpdateFillInfo(cnl, &cnp->do_fill,&almost_const);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return ret;
	}
#if 0
	if (cnp->fill_mode == NhlAREAFILL && (almost_const || (cnp->const_field  && cnp->do_constf_fill))) {
		DoConstFillHack(cnp, True);
		do_const_fill_hack = 1;
	}
#endif


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


	do_fill = cnp->do_fill;
	if (cnp->const_field && ! cnp->do_constf_fill) {
		do_fill = False;
	}
	if (cnp->output_gridded_data) {
		int msize,nsize;
		NhlBoundingBox bbox;
		float min_cell_size;

		if (! mesh_inited) {
			subret = InitMesh(cnl,tmp,1,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			mesh_inited = 1;
		}
		if (tmp->nblocks > 1) {
			subret = MIN(NhlFATAL,subret);
			e_text = "%s: Threading not implemented for gridded data output -- set env var OMP_NUM_THREADS to 1";
			NhlPError(NhlFATAL,NhlEUNKNOWN, e_text,entry_name);
			ContourAbortDraw(cnl);
			gset_clip_ind(clip_ind_rect.clip_ind);
			return ret;
		}
		subret = cnInitDataArray(cnl,&msize,&nsize,&bbox,
					 &min_cell_size,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			gset_clip_ind(clip_ind_rect.clip_ind);
			ContourAbortDraw(cnl);
			return ret;
		}
		subret = CnTriMeshWriteCellData
			(tbp->rpnt,tbp->iedg,tbp->itri,
			 msize,nsize,
			 bbox.l,bbox.b,bbox.r,bbox.t,
			 entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			ContourAbortDraw(cnl);
			gset_clip_ind(clip_ind_rect.clip_ind);
			return ret;
		}
	}
	else if (do_fill && cnp->fill_order == order) {
		NhlcnFillMode fill_mode = cnp->fill_mode;
		int is_constant;

		if (fill_mode == NhlAREAFILL) {
			if (! mesh_inited) {
				subret = InitMesh(cnl,tmp,1,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					ContourAbortDraw(cnl);
					gset_clip_ind(clip_ind_rect.clip_ind);
					return ret;
				}
				mesh_inited = 1;
			}
			if (tmp->nblocks > 1) {
				subret = MIN(NhlFATAL,subret);
				e_text = "%s: Threading not implemented for AreaFill -- set env var OMP_NUM_THREADS to 1";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return ret;
			}
			if (cnp->aws == NULL) {
				subret = cnInitAreamap(cnl,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					gset_clip_ind(clip_ind_rect.clip_ind);
					ContourAbortDraw(cnl);
					return ret;
				}
			}
			if (! cnp->aws) {
				e_text = "%s: Error reserving workspace";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return NhlFATAL;
			}
			subret = AddDataBoundToAreamap(cnl,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}

			subret = _NhlCtclam(tbp->rpnt,tbp->iedg,tbp->itri,
					    cnp->fws,cnp->iws,
					    cnp->aws,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}


			/* flag1 is set to 999 to indicate that the HLU version
			   of ARPRAM should be called. It has special handling
			   to fix a problem with the grid boundary */

			_NhlArpram(cnp->aws,999,0,0,entry_name);
			_NhlArpram(cnp->aws,999,0,0,entry_name);

			if (cnp->dump_area_map)
				_NhlDumpAreaMap(cnp->aws,entry_name);

			subret = _NhlArscam(cnp->aws,
					    (_NHLCALLF(hluctfill,HLUCTFILL)),
					    entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			subret = _NhlIdleWorkspace(cnp->aws);
			ret = MIN(subret,ret);
			cnp->aws = NULL;
#if 0
			if (do_const_fill_hack) {
				DoConstFillHack(cnp, False);
				do_const_fill_hack = 0;
			}
#endif
		}
		else if (fill_mode == NhlCELLFILL) {
			if (cnp->sfp->x_arr->num_dimensions == 1 &&
			    ! (cnp->sfp->x_cell_bounds && cnp->sfp->y_cell_bounds)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "%s: The CellFill method for non-rectangular Mesh data requires vertices to be explicitly defined using the sf[XY]CellBounds resources",entry_name);
				ContourAbortDraw(cnl);
				return NhlFATAL;
			}
			_NhlCellFill((NhlLayer)cnl,entry_name);
		}
		else if (fill_mode == NhlMESHFILL) { /* NhlMESHFILL */
			int msize,nsize;
			float min_cell_size;
			NhlBoundingBox bbox;

			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "%s: the MeshFill method does not yet produce correct results for unstructured grids\n",entry_name);
			ContourAbortDraw(cnl);
			return NhlFATAL;

			subret = cnInitCellArray(cnl,&msize,&nsize,&bbox,
						 &min_cell_size,entry_name);
 			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			subret = _NhlCtcica(NULL,NULL,NULL,NULL,NULL,
					    cnp->cws,
					    msize,msize,nsize,
					    bbox.l,bbox.b,bbox.r,bbox.t,
					    min_cell_size,
					    cnp->raster_smoothing_on,
					    1,
					    NULL,
					    entry_name);
 			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			if (cnp->cws != NULL) {
				subret = _NhlIdleWorkspace(cnp->cws);
				ret = MIN(subret,ret);
				cnp->cws = NULL;
			}
		}
		else { /* NhlRASTERFILL */
			ret = RasterFillRender(cnl,tmp,&mesh_inited,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
		}
	}

	if (! cnp->output_gridded_data &&
	    cnp->line_order == order &&
	    (cnp->do_lines || cnp->missing_val.perim_on ||
	     cnp->grid_bound.perim_on || cnp->out_of_range.perim_on)) {
		if (cnp->do_labels && cnp->label_masking) {
			if (! mesh_inited) {
				subret = InitMesh(cnl,tmp,1,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					gset_clip_ind(clip_ind_rect.clip_ind);
					ContourAbortDraw(cnl);
					return ret;
				}
				mesh_inited = 1;
			}
			if (tmp->nblocks > 1) {
				subret = MIN(NhlFATAL,subret);
				e_text = "%s: Threading not implemented for contour lines -- set env var OMP_NUM_THREADS to 1";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			c_ctseti("GIL",5);
			if (cnp->aws == NULL) {
				subret = cnInitAreamap(cnl,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					gset_clip_ind(clip_ind_rect.clip_ind);
					ContourAbortDraw(cnl);
					return ret;
				}
			}
			if (! cnp->aws) {
				e_text = "%s: Error reserving workspace";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return NhlFATAL;
			}
			c_pcsetr("PH",(float)cnp->line_lbls.pheight);
			c_pcsetr("PW",(float)cnp->line_lbls.pwidth);
			c_pcsetr("CS",(float)cnp->line_lbls.cspacing);
			c_pcseti("FN",cnp->line_lbls.font);
			c_pcseti("QU",cnp->line_lbls.quality);
			c_pcsetc("FC",cnp->line_lbls.fcode);
			subret = _NhlCtlbam(tbp->rpnt,tbp->iedg,tbp->itri,
					    cnp->fws,cnp->iws,
					    cnp->aws,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			subret = _NhlCtcldm(tbp->rpnt,tbp->iedg,tbp->itri,
					    cnp->fws,cnp->iws,cnp->aws,
					    (_NHLCALLF(ctdrpl,CTDRPL)),
					    entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			subret = _NhlIdleWorkspace(cnp->aws);
			ret = MIN(subret,ret);
			cnp->aws = NULL;
		}
		else {
			ret = ContourLineRender(cnl,tmp,&mesh_inited,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
		}
	}
	
	if (! cnp->output_gridded_data &&
	    cnp->llabel_placement != NhlCONSTANT &&
	    cnp->do_labels && cnp->label_order == order) {
		ret = ContourLabelRender(cnl,tmp,&mesh_inited,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			gset_clip_ind(clip_ind_rect.clip_ind);
			ContourAbortDraw(cnl);
			return ret;
		}
#if 0
		if (! mesh_inited) {
			subret = InitMesh(cnl,tmp,1,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				gset_clip_ind(clip_ind_rect.clip_ind);
				ContourAbortDraw(cnl);
				return ret;
			}
			mesh_inited = 1;
		}
		if (tmp->nblocks > 1) {
			subret = MIN(NhlFATAL,subret);
			e_text = "%s: Threading not implemented for contour line labels -- set env var OMP_NUM_THREADS to 1";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			gset_clip_ind(clip_ind_rect.clip_ind);
			ContourAbortDraw(cnl);
			return ret;
		}
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
		_NhlCtlbdr(tbp->rpnt,tbp->iedg,tbp->itri,
			 cnp->fws,cnp->iws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			gset_clip_ind(clip_ind_rect.clip_ind);
			ContourAbortDraw(cnl);
			return ret;
		}
#endif
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
	gset_clip_ind(clip_ind_rect.clip_ind);

	return MIN(subret,ret);
}

static NhlIsoLine *CnTriMeshGetIsoLines
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlContourPlotLayer     cnl,
        int			n_levels,
        float			*levels,
	NhlString		entry_name
        )
#else
(instance,cnl,order,entry_name)
	NhlLayer		instance;
        NhlContourPlotLayer     cnl;
        int			n_levels;
        float			*levels;
	NhlString		entry_name;
#endif
{
        NhlCnTriMeshRendererLayer tml = (NhlCnTriMeshRendererLayer) instance;
	NhlCnTriMeshRendererLayerPart	  *tmp = &tml->cntrimeshrenderer;
	NhlContourPlotLayerPart 	  *cnp = &cnl->contourplot;
        NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
	NhlString e_text;
	int mesh_inited = 0;
        Gint            err_ind;
        Gclip           clip_ind_rect;
	float           *clvp;
	int             count;
	int             i;
	NhlIsoLine      *isolines, *ilp;
	int trans_change_count;

	Cnl = cnl;
	Cnp = cnp;
	Tmp = tmp;


	ginq_clip(&err_ind,&clip_ind_rect);
        gset_clip_ind(GIND_CLIP);
	
	c_ctrset();
	SetCtParams(cnl,entry_name);

	subret = NhlVAGetValues(cnl->trans.overlay_trans_obj->base.id,
                                NhlNtrChangeCount,&trans_change_count,
                                NULL);
	if (trans_change_count > tmp->trans_change_count) {
		tmp->update_mode = TRIMESH_NEWMESH;
		tmp->trans_change_count = trans_change_count;
	}

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
		tmp->ezmap = 1;
		c_ctsetr("ORV",cnp->out_of_range_val);
		if (cnp->sfp->d_arr->num_dimensions == 1 &&
		    ! (cnp->sfp->element_nodes ||
		       (cnp->sfp->x_cell_bounds && cnp->sfp->y_cell_bounds))) {
			c_ctseti("MAP",Nhlcn1DMESHMAPVAL);
		}
		else {
			c_ctseti("MAP",NhlcnMAPVAL);
		}
	}
	else {
		tmp->ezmap = 0;
		c_ctseti("MAP",NhlcnTRIMESHMAPVAL);
	}

	c_ctseti("WSO", 3);		/* error recovery on */
	c_ctseti("NVS",0);		/* no vertical strips */
	c_ctseti("HLE",1);              /* search for equal high/lows */
        c_ctseti("SET",0);
        c_ctseti("RWC",500);
        c_ctseti("RWG",1500);

	c_ctsetr("PIT",MAX(0.0,cnp->max_point_distance));
	
        if (cnp->smoothing_on) {
                c_ctsetr("T2D",cnp->smoothing_tension);
                c_ctsetr("SSL",cnp->smoothing_distance);
        }
        else {
                c_ctsetr("T2D",(float)0.0);
        }
	gset_fill_colr_ind((Gint)_NhlGetGksCi(cnl->base.wkptr,0));

	subret = UpdateLineAndLabelParams(cnl,&cnp->do_lines,&cnp->do_labels);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return NULL;
	}


/* Retrieve workspace pointers */

	if ((cnp->fws = _NhlUseWorkspace(cnp->fws_id)) == NULL) {
		e_text = "%s: error reserving float workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return(NULL);
	}
	if ((cnp->iws = _NhlUseWorkspace(cnp->iws_id)) == NULL) {
		e_text = "%s: error reserving integer workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return(NULL);
	}

		 
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		ContourAbortDraw(cnl);
		gset_clip_ind(clip_ind_rect.clip_ind);
		return NULL;
	}


	if (n_levels <= 0) {
		clvp = (float *) cnp->levels->data;
		count = cnp->level_count;
	}
	else {
		count = n_levels;
		clvp = levels;
	}

	isolines = (NhlIsoLine *) NhlMalloc(sizeof(NhlIsoLine) * count);
	if (! mesh_inited) {
		subret = InitMesh(cnl,tmp,1,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			gset_clip_ind(clip_ind_rect.clip_ind);
			ContourAbortDraw(cnl);
			return NULL;
		}
		mesh_inited = 1;
	}
	for (i = 0, ilp = isolines; i < count; i++, ilp++) {
		int flag,npoints;
		NhlBoolean done = False;
		float *xloc = NULL, *yloc = NULL;
		int current_seg_alloc = 10;
		int current_point_count = 0;
		int current_seg = -1;
		int j;
		float save_xloc, save_yloc;
		int same_segment;
		int npoints_in_cur_segment;
		TriBlock *tbp;

		flag = 0;
/*
		printf("Points for level %f:\n", clvp[i]);
*/
		ilp->level = clvp[i];
		ilp->x = ilp->y = NULL;
		ilp->start_point = ilp->n_points = NULL;
		tbp = &(tmp->tri_block[0]);
		while (! done) {
			subret = _NhlCtcltr(tbp->rpnt,tbp->iedg,tbp->itri,cnp->fws,cnp->iws,clvp[i],
					    &flag,&xloc,&yloc,&npoints,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				ContourAbortDraw(cnl);
				gset_clip_ind(clip_ind_rect.clip_ind);
				return NULL;
			}

			if (flag == 0)
				break;
			if (current_seg == -1) {
				ilp->x = NhlMalloc(sizeof(float) * npoints);
				ilp->y = NhlMalloc(sizeof(float) * npoints);
				save_xloc = xloc[npoints-1];
				save_yloc = yloc[npoints-1];
				same_segment = 0;
			}
			else {
				ilp->x = NhlRealloc(ilp->x, sizeof(float) * (current_point_count + npoints));
				ilp->y = NhlRealloc(ilp->y, sizeof(float) * (current_point_count + npoints));
				if (xloc[0] == save_xloc && yloc[0] == save_yloc) {
					same_segment = 1;
					npoints_in_cur_segment += npoints;
				}
				else {
					same_segment = 0;
					npoints_in_cur_segment = npoints;
				}
				save_xloc = xloc[npoints-1];
				save_yloc = yloc[npoints-1];
			}
			memcpy((char*)(ilp->x + current_point_count),xloc, npoints * sizeof(float)); 
			memcpy((char*)(ilp->y + current_point_count),yloc, npoints * sizeof(float)); 
			if (tmp->ezmap) { /* points need to be transformed back into map coordinates */
				double xlon, ylat,last_xlon;
				int mod_360 = 0;
				int k = current_point_count;
				int first = 1;
				for (j = current_point_count; j < current_point_count + npoints; j++) {
					c_mdptri((double)ilp->x[j],(double)ilp->y[j],&ylat,&xlon);
					if (xlon > 1e10) 
						continue;
					if (first) {
						last_xlon = xlon;
						first = 0;
					}
					switch (mod_360) {
					case 0:
					default:
						if (last_xlon - xlon < -180) {
							mod_360 = -1;
						}
						else if (last_xlon - xlon > 180) {
							mod_360 = 1;
						}
						break;
					case 1:
						if (xlon - last_xlon > 180) {
							mod_360 = 0;
						}
						break;
					case -1:
						if (xlon - last_xlon < -180) {
							mod_360 = 0;
						}
						break;
					}
					ilp->x[k] = (float)xlon + mod_360 * 360;
					ilp->y[k] = (float)ylat;
					last_xlon = xlon;
					k++;
				}
				npoints = k - current_point_count;
			}
			if (npoints == 0) 
				continue;

			if (same_segment) {
				ilp->n_points[current_seg] += npoints;
			}
			else {
				current_seg++;
				if (current_seg == 0) {
					ilp->n_points = NhlMalloc(sizeof(int) * current_seg_alloc);
					ilp->start_point = NhlMalloc(sizeof(int) * current_seg_alloc);
				}
				else if (current_seg == current_seg_alloc) {
					ilp->n_points = NhlRealloc(ilp->n_points,sizeof(int) * current_seg_alloc * 2);
					ilp->start_point = NhlRealloc(ilp->start_point,sizeof(int) * current_seg_alloc * 2);
					current_seg_alloc *= 2;
				}
				ilp->n_points[current_seg] = npoints; 	
				ilp->start_point[current_seg] = current_point_count;
			}
			current_point_count += npoints;
		}
		ilp->point_count = current_point_count;
		ilp->n_segments = current_seg + 1;
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
	if (ret < NhlWARNING)
		return NULL;

	return isolines;
}

typedef struct {
	float vx,vy,c;
} PlaneSet;

/*
 * Function:  _NhlUnstructuredMeshFill
 *
 * Description: performs a mesh raster fill - 
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

NhlErrorTypes _NhlUnstructuredMeshFill
#if	NhlNeedProto
(
	int             *cell,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	float           min_cell_size,
	NhlBoolean      smooth,
	char		*entry_name
)
#else
(cell,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,min_cell_size,smooth,entry_name)
	int		*cell;
	int		ica1;
	int		icam;
	int		ican;
	float		xcpf;
	float		ycpf;
	float		xcqf;
	float		ycqf;
	float           min_cell_size;
	NhlBoolean      smooth;
	char		*entry_name;
#endif
{
	NhlContourPlotLayerPart *cnp = Cnp;
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	float *xv, *yv,*zdat;
	float *levels;
	int nv;
	float cxstep;
	int i,j;
	int             grid_fill_ix;
	int cell_count;
	float avg_cells_per_grid_box;
	float mflx,mfby,mfrx,mfuy;
	float min_minx, max_maxx, min_miny, max_maxy;
	float max_coverage = 0;
	int icaf, map;
	float orv,spv;

        if (cnp == NULL) {
		e_text = "%s: invalid call to _NhlRasterFill";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
        }
        levels = (float*) cnp->levels->data;

	c_ctgetr("ORV",&orv);
	c_ctgeti("CAF",&icaf);
	c_ctgeti("MAP",&map);
	c_cpgetr("SPV",&spv);

	nv = cnp->sfp->x_cell_bounds->len_dimensions[1];
	cell_count = cnp->sfp->x_cell_bounds->len_dimensions[0];
	xv = (float*)cnp->sfp->x_cell_bounds->data;
	yv = (float*)cnp->sfp->y_cell_bounds->data;
	zdat = (float*)cnp->sfp->d_arr->data;
	
	cxstep = (xcqf-xcpf)/(float)icam;
	
/*
 *      initialize cell array with the missing value.
 */      
	grid_fill_ix = Cnp->grid_bound.gks_fcolor;
	grid_fill_ix = grid_fill_ix < 0 ? NhlBACKGROUND : grid_fill_ix;
	for (j = 0; j < ican; j++) {
		for (i = 0; i < icam; i++) {
			*(cell + j * ica1 + i) = grid_fill_ix;
		}
	
	}
	avg_cells_per_grid_box = (icam * ican) / ((float)cell_count);
/*
	printf("in unstructured mesh fill\n");
	printf("avg_cells_per_grid_box = %f\n",avg_cells_per_grid_box);
	printf("icam %d ican %d\n",icam,ican);
*/

	mflx = xcpf - (xcqf - xcpf) * .1;
	mfrx = xcqf + (xcqf - xcpf) * .1;
	mfby = ycpf - (ycqf - ycpf) * .1;
	mfuy = ycqf + (ycqf - ycpf) * .1;
	min_minx = min_miny = 1e30;
	max_maxx = max_maxy = 0;
	for (i = 0; i < cell_count; i++) {
		float xi[10], yi[10],xo[10],yo[10],xp[10],yp[10];
		float minx,miny,maxx,maxy;
		int flip_edge;
		int status;
		PlaneSet *pps, ps[10];
		int p,p1,p2,p0;
		int jcv,icv;
		int iplus,jplus;
		int iaid,k;
		float fvali;

		memcpy(xi,xv + i * nv,nv * sizeof(float));
		memcpy(yi,yv + i * nv,nv * sizeof(float));
		minx = miny = 1e30;
		maxx = maxy = 0;
		_NhlDataToWin(Cnp->trans_obj,xi,yi,
			      nv,xo,yo,&status,
			      NULL,NULL);
			
		if (status) {
			continue;
		}

		for (p = 0; p < nv; p++) {
			float tx,ty;

			tx = c_cufx(xo[p]);
			ty = c_cufy(yo[p]);
#if 1
			if (tx < mflx || tx > mfrx || ty < mfby || ty > mfuy) {
				status = 1;
				break;
			}
#endif
			xp[p] = (tx - xcpf) / cxstep; 
			yp[p] = (ty - ycpf) / cxstep; 
			if (xp[p] < minx)
				minx = xp[p];
			if (xp[p] > maxx)
				maxx = xp[p];
			if (yp[p] < miny)
				miny = yp[p];
			if (yp[p] > maxy)
				maxy = yp[p];
		}
#if 1
		if (maxx - minx > icam / 2.0) {
			float new_maxx = -1e30,new_minx = 1e30;
			for (p = 0; p < nv; p++) {
				if (xp[p] < icam / 2) {
					xp[p] += (float) icam;
				}
				if (xp[p] < new_minx)
					new_minx = xp[p];
				if (xp[p] > new_maxx)
					new_maxx = xp[p];
			}
			maxx = new_maxx;
			minx = new_minx;
		}
#endif
		pps = &ps[0];
		flip_edge = (xp[0] - xp[1]) * (yp[1] - yp[2]) >
			(yp[0] - yp[1]) * (xp[1] - xp[2]);
		for (p1 = 0, p2 = 1; p2 < nv; p1 = p2, p2++,pps++) {
			pps->vx = yp[p1] - yp[p2];
			pps->vy = xp[p2] - xp[p1];
			pps->c = pps->vx * xp[p1] + pps->vy * yp[p1];

			/* check sense and reverse plane edge if need be */
			if ( flip_edge ) {
				pps->vx = -pps->vx ;
				pps->vy = -pps->vy ;
				pps->c  = -pps->c ;
			}
		}
		/*printf("coverage : %f\n", ((maxy + 1) - miny) * ((maxx +1) - minx));*/
		if (((maxy + 1) - miny) * ((maxx +1) - minx) > max_coverage)
			max_coverage = ((maxy + 1) - miny) * ((maxx +1) - minx);
		if (((maxy + 1) - miny) * ((maxx +1) - minx) > 400 * avg_cells_per_grid_box)
			continue;
		if (minx < min_minx) min_minx = minx; 
		if (miny < min_miny) min_miny = miny; 
		if (maxx > max_maxx) max_maxx = maxx; 
		if (maxy > max_maxy) max_maxy = maxy; 
		for (jcv = MAX(0,(int) miny); jcv < MIN(ican,(int) (maxy + 1)); jcv++) {
			float ty = jcv + .5;
			for (icv = MAX(0,(int) minx); icv < MAX(icam,(int) (maxx + 1)); icv++) {
				float tx = icv +.5;
				for (p0 = nv-1, pps = ps; --p0; pps++) {
					if (pps->vx * tx + pps->vy * ty > pps->c) {
						break;
					}
				}
				if (p0 > 0)
					continue;
				/*iplus = icv % icam;*/
				iplus = MIN(icv, icam);
				jplus = jcv;
				fvali = zdat[i]; 
				iaid = -1;
				if (spv != 0.0 &&
				    fvali == spv)
					iaid = 98;
				else {
					for (k=0; k < Cnp->level_count; k++) {
						if (fvali < levels[k]) {
							iaid = NhlcnAREAID_OFFSET+k;
							break;
						}
					}
				}
				if (iaid == -1)
					iaid = NhlcnAREAID_OFFSET +
						Cnp->level_count;     

				(_NHLCALLF(hluctscae,HLUCTSCAE))
					(cell,&ica1,&icam,&ican,
					 &xcpf,&ycpf,&xcqf,&ycqf,
					 &iplus,&jplus,&icaf,&iaid);
			}
		}
	}

	return ret;

}
#define HERO(A,B,C) \
	sqrt(MAX(0.,((A)+(B)+(C))*((B)+(C)-(A))*((A)+(C)-(B))*((A)+(B)-(C))))
/*
 * Function:  _NhlTriMeshRasterFill
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

NhlErrorTypes _NhlTriMeshRasterFill
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	int		*cell,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	void            *info,
	char		*entry_name
)
#else
(rpnt,iedg,itri,cell,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,entry_name)
	float		*rpnt,
	int             *iedg,
	int             *itri,
	int		*cell;
	int		ica1;
	int		icam;
	int		ican;
	float		xcpf;
	float		ycpf;
	float		xcqf;
	float		ycqf;
	void            *info;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;

	int		i,j,k,n,icaf,map,imap,iaid;
	float		xccf,xccd,xcci,yccf,yccd,ycci;
	float		zval,orv;
        float		*levels;
	float		cxstep,cystep;
	float           xsoff,xeoff,ysoff,yeoff;
	float           tol1,tol2;
	int             ipp1,ipp2,ipp3;
	float           xcu1,xcu2,xcu3,ycu1,ycu2,ycu3;
	float           xcf1,xcf2,xcf3,ycf1,ycf2,ycf3;
	float           xd12,xd23,xd31,yd12,yd23,yd31;
	float           fva1,fva2,fva3;
	float           dn12,dn23,dn31;
	int             bound1,bound2;
	int             ibeg,iend,jbeg,jend;
	int             grid_fill_ix;
	TriBlock *tbp;
	int tid;
	int mni,mxi,mnj,mxj;
	float wid, hgt;
	float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;

	if (! info) {
	  tbp = Tbp;
	}
	else {
	  tbp = (TriBlock *) info;
	}

	tid = 0;
#ifdef _OPENMP
	{
	tid = omp_get_thread_num();
	/*printf("%d x s&e %f %f y s&e %f %f\n", tid, tbp->xs,tbp->xe,tbp->ys,tbp->ye);*/
	}
#endif

        if (Cnp == NULL) {
		e_text = "%s: invalid call to _NhlRasterFill";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
        }
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
	/*
	  printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
	  flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
	*/
	wid = wrx - wlx;
	hgt = wuy - wby;
	mni = tbp->xsr == wlx ? 0 : icam * (tbp->xsr - wlx) / wid;
	mxi = tbp->xer == wrx ? icam : icam * (tbp->xer - wlx) / wid -1;
	mnj = tbp->ysr == wby ? 0 : ican * (tbp->ysr - wby) / hgt;
	mxj = tbp->yer == wuy ? ican : ican * (tbp->yer - wby) / hgt -1;

        levels = (float*) Cnp->levels->data;
        
/* 
 * replacement for CTCICA
 */
	c_ctgetr("ORV",&orv);
	c_ctgeti("CAF",&icaf);
	c_ctgeti("MAP",&map);


	cxstep = (xcqf-xcpf)/(float)icam;
	cystep = (ycqf-ycpf)/(float)ican;

	xsoff = Xsoff + .5 * (1.0 - Xsoff);
	xeoff = Xeoff + .5 * (1.0 - Xeoff);
	ysoff = Ysoff + .5 * (1.0 - Ysoff);
	yeoff = Yeoff + .5 * (1.0 - Yeoff);

	tol1 = 0.00001 * MIN(Cnl->view.width,Cnl->view.height);
	tol2 = 0.5 * MIN(Cnl->view.width,Cnl->view.height);
	
/*
 * Now overwrite out-of-range areas with the out-of-range color
 */
	grid_fill_ix = Cnp->out_of_range.gks_fcolor < 0 ? NhlTRANSPARENT_CI : Cnp->out_of_range.gks_fcolor;
	if (tid ==0) {
	    if (Tmp->ezmap) {
	      imap = -map;
	      zval = 0;
	      for (j = 0; j < ican; j++) {
		if (j == 0)
		  yccf = ycpf + ysoff * cystep;
		else if (j == ican - 1)
		  yccf = ycpf + (ican - yeoff) * cystep;
		else
		  yccf = ycpf + (j + ysoff) * cystep;
		yccd = c_cfuy(yccf);
		for (i = 0; i < icam; i++) {
		  if (i == 0)
		    xccf = xcpf + xsoff * cxstep;
		  else if (i == icam - 1)
		    xccf = xcpf + (icam - xeoff) * cxstep; 
		  else
		    xccf = xcpf + (i+xsoff) * cxstep;
		  xccd = c_cfux(xccf);
		  (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		    (&imap,&xccd,&yccd,&zval,&xcci,&ycci);
		  if (xcci == orv) {
		    *(cell + j * ica1 + i) = grid_fill_ix;
		  }
		}
	      }
	    }
	}

/*
 * examine each triangle in turn
 */

	for (n = 0; n <= tbp->ntri - Lotn; n += Lotn) {
	     if (itri[n+3] != 0)
		     continue;

/*
 * project point 1; if invisible skip it.
 */
	     if (iedg[itri[n]] == iedg[itri[n+1]] ||
		 iedg[itri[n]] == iedg[itri[n+1]+1]) {
		     ipp1 = iedg[itri[n]];
	     }
	     else {
		     ipp1 = iedg[itri[n]+1];
	     }
	     (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		     (&map,&rpnt[ipp1],&rpnt[ipp1+1],&rpnt[ipp1+2],
		      &xcu1,&ycu1);
	     if (orv != 0.0 && (xcu1 == orv || ycu1 == orv))
		     continue;

/*
 * project point 2; if invisible skip the triangle
 */
	     
	     if (iedg[itri[n+1]] == iedg[itri[n+2]] ||
		 iedg[itri[n+1]] == iedg[itri[n+2]+1]) {
		     ipp2 = iedg[itri[n+1]];
	     }
	     else {
		     ipp2 = iedg[itri[n+1]+1];
	     }
	     
	     (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		     (&map,&rpnt[ipp2],&rpnt[ipp2+1],&rpnt[ipp2+2],
		      &xcu2,&ycu2);
	     if (orv != 0.0  && (xcu2 == orv || ycu2 == orv))
		     continue;	     

/*
 * project point 3; if invisible skip the triangle
 */
	     
	     if (iedg[itri[n+2]] == iedg[itri[n]] ||
		 iedg[itri[n+2]] == iedg[itri[n]+1]) {
		     ipp3 = iedg[itri[n+2]];
	     }
	     else {
		     ipp3 = iedg[itri[n+2]+1];
	     }
	     
	     (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		     (&map,&rpnt[ipp3],&rpnt[ipp3+1],&rpnt[ipp3+2],
		      &xcu3,&ycu3);
	     if (orv != 0.0 && (xcu3 == orv || ycu3 == orv))
		     continue;	     
	     

	     xcf1 = c_cufx(xcu1);
	     ycf1 = c_cufy(ycu1);
	     xcf2 = c_cufx(xcu2);
	     ycf2 = c_cufy(ycu2);
	     xcf3 = c_cufx(xcu3);
	     ycf3 = c_cufy(ycu3);

	     
	     xd12 = xcf2 - xcf1;
	     yd12 = ycf2 - ycf1;
	     xd23 = xcf3 - xcf2;
	     yd23 = ycf3 - ycf2;
	     xd31 = xcf1 - xcf3;
	     yd31 = ycf1 - ycf3;


/*
 * skip triangle if too small or too large
 */
	     if ((fabs(xd12) < tol1 && fabs(yd12) < tol1) ||
		 (fabs(xd23) < tol1 && fabs(yd23) < tol1) ||
		 (fabs(xd31) < tol1 && fabs(yd31) < tol1))
		     continue;

	     if ((fabs(xd12) > tol2 || fabs(yd12) > tol2) ||
		 (fabs(xd23) > tol2 || fabs(yd23) > tol2) ||
		 (fabs(xd31) > tol2 || fabs(yd31) > tol2))
		     continue;

/*
 * get the field values at the 3 points of the triangle
 */
	     fva1 = rpnt[ipp1+3];
	     fva2 = rpnt[ipp2+3];
	     fva3 = rpnt[ipp3+3];

/*
 * compute triangle lengths and the area
 */
	     dn12 = sqrt(xd12*xd12 + yd12 * yd12);
	     dn23 = sqrt(xd23*xd23 + yd23 * yd23);
	     dn31 = sqrt(xd31*xd31 + yd31 * yd31);

/*
 * Now set loop limits to examine center points of all cells that overlap
 * the bounding box of the triangle
 */
	     
	     bound1 = MAX(0,
			  MIN(icam-1,(int)
			      ((MIN(xcf1,MIN(xcf2,xcf3)) - xcpf) /
			       (xcqf-xcpf) * (float) icam)));
	     bound2 = MAX(0,
			  MIN(icam-1,(int)
			      ((MAX(xcf1,MAX(xcf2,xcf3)) - xcpf) /
			       (xcqf-xcpf) * (float) icam)));

	     ibeg = MIN(bound1,bound2);
	     iend = MAX(bound1,bound2);

	     bound1 = MAX(0,
			  MIN(ican-1,(int)
			      ((MIN(ycf1,MIN(ycf2,ycf3)) - ycpf) /
			       (ycqf-ycpf) * (float) ican)));
	     bound2 = MAX(0,
			  MIN(ican-1,(int)
			      ((MAX(ycf1,MAX(ycf2,ycf3)) - ycpf) /
			       (ycqf-ycpf) * (float) ican)));

	     jbeg = MIN(bound1,bound2);
	     jend = MAX(bound1,bound2);

/*
 * find each cell whose center point lies within the triangle and
 * set its color index appropriately
 */
	     for (j = jbeg; j <= jend; j++) {
		     float ts12,ts23,ts31;
		     float dnc1,dnc2,dnc3;
		     float yfp,xfp;
		     int jplus = j+1;
		     float a1,a2,a3;

		     yfp = ycpf + ((float)j+.5)/ican * (ycqf - ycpf);

		     for (i = ibeg; i <= iend; i++) {
			     float atot;
			     int iplus = i+1;
			     xfp = xcpf + ((float)i+.5)/icam * (xcqf - xcpf);
			     ts12 = (yd12*xfp-xd12*yfp-yd12*xcf1+xd12*ycf1)/
				     dn12;
			     ts23 = (yd23*xfp-xd23*yfp-yd23*xcf2+xd23*ycf2)/
				     dn23;
			     ts31 = (yd31*xfp-xd31*yfp-yd31*xcf3+xd31*ycf3)/
				     dn31;
			     if ((ts12 < 0.00001 && ts23 < 0.00001 &&
				  ts31 < 0.00001) ||
				 (ts12 > -0.00001 && ts23 > -0.00001 &&
				  ts31 > -0.00001)) {
				     float xd1,xd2,xd3,yd1,yd2,yd3;
				     float fvali;
				     xd1 = xfp - xcf1;
				     xd2 = xfp - xcf2;
				     xd3 = xfp - xcf3;
				     yd1 = yfp - ycf1;
				     yd2 = yfp - ycf2;
				     yd3 = yfp - ycf3;
				     dnc1 = sqrt(xd1*xd1 + yd1*yd1);
				     dnc2 = sqrt(xd2*xd2 + yd2*yd2);
				     dnc3 = sqrt(xd3*xd3 + yd3*yd3);
				     a1 = HERO(dn23,dnc2,dnc3);
				     a2 = HERO(dn31,dnc3,dnc1);
				     a3 = HERO(dn12,dnc1,dnc2);
				     atot = a1 + a2 + a3;
				     if (atot == 0.0) 
					     continue;


				     if (Cnp->raster_smoothing_on) {
					     fvali = (fva1 * a1 + 
						      fva2 * a2 + fva3 * a3) / atot;
				     }
				     else if (a1 > a2 && a1 > a3) {
					     fvali = fva1;
				     }
				     else if (a2 > a1 && a2 > a3) {
					     fvali = fva2;
				     }
				     else {
					     fvali = fva3;
				     }

				     iaid = -1;
				     for (k=0; k < Cnp->level_count; k++) {
					     if (fvali < levels[k]) {
						  iaid = NhlcnAREAID_OFFSET+k;
						  break;
					     }
				     }
				     if (iaid == -1)
					     iaid = NhlcnAREAID_OFFSET +
						     Cnp->level_count;     
				     if (Tmp->nblocks == 1 || (i >= mni && i <= mxi && j >= mnj && j <= mxj)) {
					     (_NHLCALLF(hluctscae,HLUCTSCAE))
						     (cell,&ica1,&icam,&ican,
						      &xcpf,&ycpf,&xcqf,&ycqf,
						      &iplus,&jplus,&icaf,&iaid);
				     }
			     }
		     }
	     }
	}
	/*printf("tid %d mnx %d mxx %d mny %d mxy %d \n", tid, tbp->mnx, tbp->mxx, tbp->mny, tbp->mxy);*/

#if 0
	if (tid == 0) { /* check corners and fill them if necessary */
		if (cell[0] == grid_fill_ix) {
			printf("filling lower left\n");
		}
	}
#endif

	return ret;
}

/*
 * Function:  CnTriMeshWriteCellData
 *
 * Description: Writes out the interpolated data associated with each
 * cell. This is a way of interpolating from one grid to another.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

NhlErrorTypes CnTriMeshWriteCellData
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	char		*entry_name
)
#else
(rpnt,iedg,itri,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,entry_name)
	float		*rpnt,
	int             *iedg,
	int             *itri,
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

	int		i,j,n,icaf,map;
	float		orv;
	double           tol1,tol2;
	int             ipp1,ipp2,ipp3;
	float           xcu1,xcu2,xcu3,ycu1,ycu2,ycu3;
	double          xcf1,xcf2,xcf3,ycf1,ycf2,ycf3;
	double          xd12,xd23,xd31,yd12,yd23,yd31;
	double          fva1,fva2,fva3;
	double          dn12,dn23,dn31;
	int             bound1,bound2;
	int             ibeg,iend,jbeg,jend;
	float           *data;
	float           init_val;
	FILE 		*fp;
	float           out_of_range;
	int             count;
	float wlx,wrx,wby,wuy,wxstep,wystep;
	int licam,lican;
/*
	printf("in CnWriteCellData\n");
*/
        if (Cnp == NULL) {
		e_text = "%s: invalid call to _NhlRasterFill";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
        }
	NhlVAGetValues(Cnp->trans_obj->base.id,
		       NhlNtrOutOfRangeF,&out_of_range,NULL);

	if (Cnp->sfp->missing_value_set) {
		init_val = Cnp->sfp->missing_value;
	}
	else {
		init_val = 1E32;
	}

	wlx = c_cfux(xcpf);
	wrx = c_cfux(xcqf);
	wby = c_cfuy(ycpf);
	wuy = c_cfuy(ycqf);
	wxstep = (wrx - wlx) / (icam); 
	wystep = (wuy - wby) / (ican); 

        
/* 
 * replacement for CTCICA
 */
	c_ctgetr("ORV",&orv);
	c_ctgeti("CAF",&icaf);
	c_ctgeti("MAP",&map);



	tol1 = 0.00001 * MIN(Cnl->view.width,Cnl->view.height);
	tol2 = 0.5 * MIN(Cnl->view.width,Cnl->view.height);
	
/*
 *      initialize data array.
 *      make the data array larger by 2 because the outer edges
 *      never get written using this algorithm.
 */      
	
	licam = icam + 2;
	lican = ican + 2;
	data = NhlMalloc(licam * lican * sizeof(float));
	if (!data) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	for (j = 0; j < lican; j++) {
		for (i = 0; i < licam; i++) {
			*(data + j * licam + i) = init_val;
		}
	}
/*
 * examine each triangle in turn
 */

	for (n = 0; n < Tbp->ntri - Lotn; n += Lotn) {
	     if (itri[n+3] != 0)
		     continue;

/*
 * project point 1; if invisible skip it.
 */
	     if (iedg[itri[n]] == iedg[itri[n+1]] ||
		 iedg[itri[n]] == iedg[itri[n+1]+1]) {
		     ipp1 = iedg[itri[n]];
	     }
	     else {
		     ipp1 = iedg[itri[n]+1];
	     }
	     (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		     (&map,&rpnt[ipp1],&rpnt[ipp1+1],&rpnt[ipp1+2],
		      &xcu1,&ycu1);
	     if (orv != 0.0 && (xcu1 == orv || ycu1 == orv))
		     continue;

/*
 * project point 2; if invisible skip the triangle
 */
	     
	     if (iedg[itri[n+1]] == iedg[itri[n+2]] ||
		 iedg[itri[n+1]] == iedg[itri[n+2]+1]) {
		     ipp2 = iedg[itri[n+1]];
	     }
	     else {
		     ipp2 = iedg[itri[n+1]+1];
	     }
	     
	     (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		     (&map,&rpnt[ipp2],&rpnt[ipp2+1],&rpnt[ipp2+2],
		      &xcu2,&ycu2);
	     if (orv != 0.0  && (xcu2 == orv || ycu2 == orv))
		     continue;	     

/*
 * project point 3; if invisible skip the triangle
 */
	     
	     if (iedg[itri[n+2]] == iedg[itri[n]] ||
		 iedg[itri[n+2]] == iedg[itri[n]+1]) {
		     ipp3 = iedg[itri[n+2]];
	     }
	     else {
		     ipp3 = iedg[itri[n+2]+1];
	     }
	     
	     (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
		     (&map,&rpnt[ipp3],&rpnt[ipp3+1],&rpnt[ipp3+2],
		      &xcu3,&ycu3);
	     if (orv != 0.0 && (xcu3 == orv || ycu2 == orv))
		     continue;	     

	     xcf1 = (double)c_cufx(xcu1);
	     ycf1 = (double)c_cufy(ycu1);
	     xcf2 = (double)c_cufx(xcu2);
	     ycf2 = (double)c_cufy(ycu2);
	     xcf3 = (double)c_cufx(xcu3);
	     ycf3 = (double)c_cufy(ycu3);

	     
	     xd12 = xcf2 - xcf1;
	     yd12 = ycf2 - ycf1;
	     xd23 = xcf3 - xcf2;
	     yd23 = ycf3 - ycf2;
	     xd31 = xcf1 - xcf3;
	     yd31 = ycf1 - ycf3;

/*
 * skip triangle if too small or too large
 */
	     if ((fabs(xd12) < tol1 && fabs(yd12) < tol1) ||
		 (fabs(xd23) < tol1 && fabs(yd23) < tol1) ||
		 (fabs(xd31) < tol1 && fabs(yd31) < tol1))
		     continue;

	     if ((fabs(xd12) > tol2 || fabs(yd12) > tol2) ||
		 (fabs(xd23) > tol2 || fabs(yd23) > tol2) ||
		 (fabs(xd31) > tol2 || fabs(yd31) > tol2))
		     continue;

/*
 * get the field values at the 3 points of the triangle
 */
	     fva1 = rpnt[ipp1+3];
	     fva2 = rpnt[ipp2+3];
	     fva3 = rpnt[ipp3+3];

/*
 * compute triangle lengths and the area
 */
	     dn12 = sqrt(xd12*xd12 + yd12 * yd12);
	     dn23 = sqrt(xd23*xd23 + yd23 * yd23);
	     dn31 = sqrt(xd31*xd31 + yd31 * yd31);

/*
 * Now set loop limits to examine center points of all cells that overlap
 * the bounding box of the triangle
 */
	     
	     bound1 = MAX(0,
			  MIN(licam-1,(int)
			      ((MIN(xcf1,MIN(xcf2,xcf3)) - xcpf) /
			       (xcqf-xcpf) * (float) licam)));
	     bound2 = MAX(0,
			  MIN(licam-1,(int)
			      ((MAX(xcf1,MAX(xcf2,xcf3)) - xcpf) /
			       (xcqf-xcpf) * (float) licam)));

	     ibeg = MIN(bound1,bound2);
	     iend = MAX(bound1,bound2);

	     bound1 = MAX(0,
			  MIN(lican-1,(int)
			      ((MIN(ycf1,MIN(ycf2,ycf3)) - ycpf) /
			       (ycqf-ycpf) * (float) lican)));
	     bound2 = MAX(0,
			  MIN(lican-1,(int)
			      ((MAX(ycf1,MAX(ycf2,ycf3)) - ycpf) /
			       (ycqf-ycpf) * (float) lican)));

	     jbeg = MIN(bound1,bound2);
	     jend = MAX(bound1,bound2);

/*
 * find each cell whose center point lies within the triangle and
 * set its color index appropriately
 */
	     for (j = jbeg; j <= jend; j++) {
		     double ts12,ts23,ts31;
		     double dnc1,dnc2,dnc3;
		     double yfp,xfp;
		     double a1,a2,a3;

		     yfp = ycpf + ((double)j+.5)/lican * (ycqf - ycpf);

		     for (i = ibeg; i <= iend; i++) {
			     double atot;
			     xfp = xcpf + ((float)i+.5)/licam * (xcqf - xcpf);
			     ts12 = (yd12*xfp-xd12*yfp-yd12*xcf1+xd12*ycf1)/
				     dn12;
			     ts23 = (yd23*xfp-xd23*yfp-yd23*xcf2+xd23*ycf2)/
				     dn23;
			     ts31 = (yd31*xfp-xd31*yfp-yd31*xcf3+xd31*ycf3)/
				     dn31;
			     if ((ts12 < 0.00001 && ts23 < 0.00001 &&
				  ts31 < 0.00001) ||
				 (ts12 > -0.00001 && ts23 > -0.00001 &&
				  ts31 > -0.00001)) {
				     float xd1,xd2,xd3,yd1,yd2,yd3;
				     float fvali;
				     xd1 = xfp - xcf1;
				     xd2 = xfp - xcf2;
				     xd3 = xfp - xcf3;
				     yd1 = yfp - ycf1;
				     yd2 = yfp - ycf2;
				     yd3 = yfp - ycf3;
				     dnc1 = sqrt(xd1*xd1 + yd1*yd1);
				     dnc2 = sqrt(xd2*xd2 + yd2*yd2);
				     dnc3 = sqrt(xd3*xd3 + yd3*yd3);
				     a1 = HERO(dn23,dnc2,dnc3);
				     a2 = HERO(dn31,dnc3,dnc1);
				     a3 = HERO(dn12,dnc1,dnc2);
				     atot = a1 + a2 + a3;
#if 0
				     if (a1 > a2 && a1 > a3)
					     fvali = fva1;
				     else if (a2 > a1 && a2 > a3)
					     fvali = fva2;
				     else
					     fvali = fva3;
#endif
				     if (atot == 0.0)
					     continue;

				     fvali = (fva1 * a1 +
					      fva2 * a2 + fva3 * a3) / atot;

				     *(data + j * licam + i) = (float)fvali;
			     }
		     }
	     }
	}
        fp = fopen("tmp.bin","w");
	for (j = 1; j <= ican; j++) {
		float           *d = data + j * licam + 1;
		count = fwrite(d,sizeof(float),icam,fp);
		if (count < icam) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "Error writing output file\n");
			return NhlFATAL;
		}
	}
	fclose(fp);

	fp = fopen("tmp-lon.bin","w");
	for (i = 0; i < icam; i++) {
		float lon = wlx + i * wxstep;
		fwrite(&lon,sizeof(float),1,fp);
	}
	fclose(fp);
	fp = fopen("tmp-lat.bin","w");
	for (j = 0; j < ican; j++) {
		float lat = wby + j * wystep;
		fwrite(&lat,sizeof(float),1,fp);
	}
	fclose(fp);

	NhlFree(data);

	return ret;
}

/*
 * Function:  hluctfill
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
int (_NHLCALLF(hluctfill,HLUCTFILL))
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
		printf("hluctfill i %d iai %d iag %d\n",i,iai[i],iag[i]);
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
#if 0
		                printf("hluctfill region i %d iai %d iag %d\n",i,iai[i],iag[i]);
#endif

				switch (iai[i]) {
				case 99:
				case 98:
#if 0
					if (Cnp->missing_val.gks_fcolor > NhlTRANSPARENT &&
					    Cnp->missing_val.fill_pat > NhlHOLLOWFILL) {
						col_ix = Cnp->missing_val.fill_color;
						pat_ix = Cnp->missing_val.fill_pat;
						fscale = Cnp->missing_val.fill_scale;
					}
					else {
						col_ix = Cnp->grid_bound.fill_color;
						pat_ix = Cnp->grid_bound.fill_pat;
						fscale = Cnp->grid_bound.fill_scale;
					}
#endif
					col_ix = Cnp->missing_val.fill_color;
					pat_ix = Cnp->missing_val.fill_pat;
					fscale = Cnp->missing_val.fill_scale;
					break;
				case 97:
					reg_attrs = &Cnp->out_of_range;
					col_ix = reg_attrs->fill_color;
					pat_ix = reg_attrs->fill_pat;
					fscale = reg_attrs->fill_scale;
					break;
				default:
					return 0;
				}
			}
                        
                        float fill_opacity;
                        NhlVAGetValues(Cnl->base.wkptr->base.id,
				       _NhlNwkFillOpacityF, &fill_opacity, 
				       NULL);
                                                
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
                        
       			NhlVASetValues(Cnl->base.wkptr->base.id,
                                _NhlNwkFillOpacityF, fill_opacity, 
                                NULL);                        
		}
	}
	return 0;
}


/*
 * Function:  hluctscae
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
void  (_NHLCALLF(hluctscae,HLUCTSCAE))
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
		_NHLCALLF(ctscae,CTSCAE)
			(icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf,
			 ind1,ind2,icaf,iaid);
		return;
	}

	/* no support in cell arrays for transparent, so it's necessary
	 *  to reset transparent color indexes to background
	 * 5-29-2013 - this is no longer true. Replace NhlTRANSPARENT with a transparent color index.
	 */
	   

	if (*iaid > 99 && *iaid < 100 + Cnp->fill_count) {
		col_ix = Cnp->gks_fill_colors[*iaid - 100];
		if (col_ix < 0) col_ix = NhlTRANSPARENT_CI;
	}
	else if (*iaid == 99 || *iaid == 98) {
#if 0
		printf("hluctscae iaid = %d\n",*iaid);
#endif
		col_ix = Cnp->missing_val.gks_fcolor;
		if (col_ix <= 0 && Cnp->grid_bound.gks_fcolor > 0) 
			col_ix = Cnp->grid_bound.gks_fcolor;
		if (col_ix < 0)
			col_ix = NhlTRANSPARENT_CI;
	}
	else if (*iaid == 97) {
		col_ix = Cnp->out_of_range.gks_fcolor;
		if (col_ix < 0)
			col_ix = NhlTRANSPARENT_CI;
#if 0
		printf("hluctscae iaid = %d\n",*iaid);
#endif
	}
	else  {
#if 0
		printf("hluctscae iaid = %d\n",*iaid);
#endif
		col_ix = NhlTRANSPARENT_CI;
	}
	*(icra + ((*ind2 - 1) * *ica1 + (*ind1 - 1))) = col_ix;

	return;
}

/*
 * Function:  hluctchcl
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
void   (_NHLCALLF(hluctchcl,HLUCTCHCL))
#if	NhlNeedProto
(
	int	*iflg
)
#else
(iflg)
	int	*iflg;
#endif

{

	char func[] = "HLUCTCHCL";
	int i, pai, dpix;
	char buffer[NhlDASHBUFSIZE];
	int lcol;
	float thickness, tf;
	float *thp;
	int   *dpp;

	if (Cnp == NULL) {
		_NHLCALLF(ctchcl,CTCHCL)(iflg);
		return;
	}

	dpp = (int *) Cnp->line_dash_patterns->data;
	thp = (float *) Cnp->line_thicknesses->data;

	if (*iflg != 1) return;

	c_ctgeti("PAI", &pai);
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
#if 0
		printf("hluctchcl pai: %d\n", pai);
#endif
		switch (pai) {
		case -1:
			reg_attrs = &Cnp->missing_val;
			break;
		case -2:
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
                        _NhlSetFillOpacity(Cnl, 1.0);  /* NCL-1509 */
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
 * Function:  hluctchhl
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
void   (_NHLCALLF(hluctchhl,HLUCTCHHL))
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
	float dva;
	NhlFormatRec *frec;
	int *fwidth, *sig_digits, *left_sig_digit, *point_pos, *exp_switch_len, *exp_field_width;

	if (Cnp == NULL) {
		_NHLCALLF(ctchhl,CTCHHL)(iflg);
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
			c_ctsetc("CTM"," ");
			return;
		}
		if ( Cnp->high_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->high_lbls.gks_color);
			c_pcseti("OC", Cnp->high_lbls.gks_color);
                        _NhlSetFillOpacity(Cnl, 1.0);  /* NCL-1509 */
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
		c_ctgetr("dva",&dva);
		dva /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->high_lbls.format,dva,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->high_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_ctsetc("CTM",buf);
		break;
	case 2:
		if (! Cnp->high_lbls.on) return;
		if (Cnp->hlb_val > 1) {
			if (Cnp->high_lbls.gks_bcolor == NhlTRANSPARENT) 
				_NhlSetFillOpacity(Cnl, 0.0); 
			else 
				gset_fill_colr_ind(Cnp->high_lbls.gks_bcolor);
		}
		break;
	case -2:
		if (! Cnp->high_lbls.on) return;
		if (Cnp->hlb_val > 1 && Cnp->high_lbls.gks_bcolor == NhlTRANSPARENT)
			_NhlSetFillOpacity(Cnl, 1.0); 
		break;
	case 3:
		if (! Cnp->high_lbls.on) {
			c_ctsetc("CTM"," ");
			return;
		}
		if ( Cnp->high_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->high_lbls.gks_color);
			c_pcseti("OC", Cnp->high_lbls.gks_color);
                        _NhlSetFillOpacity(Cnl, 1.0);  /* NCL-1509 */
		}
		else {
			c_ctsetc("CTM"," ");
			return;
		}
		c_pcsetr("PH",(float)Cnp->high_lbls.pheight);
		c_pcsetr("PW",(float)Cnp->high_lbls.pwidth);
		c_pcsetr("CS",(float)Cnp->high_lbls.cspacing);
		c_pcseti("FN",Cnp->high_lbls.font);
		c_pcseti("QU",Cnp->high_lbls.quality);
		c_pcsetc("FC",Cnp->high_lbls.fcode);
		gset_linewidth((float)Cnp->high_lbls.thickness);

		strcpy(buf,(char *)Cnp->high_lbls.text);
		if ((sub = strstr(buf,"$ZDV$")) == NULL) {
			return;
		}
		c_ctgetr("dva",&dva);
		dva /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->high_lbls.format,dva,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->high_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_ctsetc("CTM",buf);
		Cnp->high_lbls.count++;
		break;
	case 4:
		if (( Cnp->hlb_val % 2 == 1) &&
		    (Cnp->high_lbls.perim_on == False || Cnp->high_lbls.perim_lcolor == NhlTRANSPARENT)) 
			_NhlSetLineOpacity(Cnl, 0.0); 
		else {
			gset_line_colr_ind(Cnp->high_lbls.gks_plcolor);
			gset_linewidth(Cnp->high_lbls.perim_lthick);
		}
		break;
	case -4:
		if (( Cnp->hlb_val % 2 == 1) && 
		    (Cnp->high_lbls.perim_on == False || Cnp->high_lbls.perim_lcolor == NhlTRANSPARENT))
			_NhlSetLineOpacity(Cnl, 1.0); 
		break;
	case 5:
		if (! Cnp->low_lbls.on) {
			c_ctsetc("CTM"," ");
			return;
		}
		if (Cnp->low_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->low_lbls.gks_color);
			c_pcseti("OC", Cnp->low_lbls.gks_color);
                        _NhlSetFillOpacity(Cnl, 1.0);  /* NCL-1509 */
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
		c_ctgetr("dva",&dva);
		dva /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->low_lbls.format,dva,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->low_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_ctsetc("CTM",buf);
		break;
	case 6:
		if (! Cnp->low_lbls.on) return;
		if (Cnp->hlb_val > 1) {
			if (Cnp->low_lbls.gks_bcolor == NhlTRANSPARENT) 
				_NhlSetFillOpacity(Cnl, 0.0); 
			else 
				gset_fill_colr_ind(Cnp->low_lbls.gks_bcolor);
		}
		break;
	case -6:
		if (! Cnp->low_lbls.on) return;
		if (Cnp->hlb_val > 1 && Cnp->low_lbls.gks_bcolor == NhlTRANSPARENT)
			_NhlSetFillOpacity(Cnl, 1.0); 
		break;
	case 7:
		if (! Cnp->low_lbls.on) {
			c_ctsetc("CTM"," ");
			return;
		}
		if (Cnp->low_lbls.gks_color > NhlTRANSPARENT) {
			c_pcseti("CC", Cnp->low_lbls.gks_color);
			c_pcseti("OC", Cnp->low_lbls.gks_color);
                        _NhlSetFillOpacity(Cnl, 1.0);  /* NCL-1509 */
		}
		else {
			c_ctsetc("CTM"," ");
			return;
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
		c_ctgetr("dva",&dva);
		dva /= Cnp->label_scale_factor;
		fstr = _NhlFormatFloat(&Cnp->low_lbls.format,dva,
				       fwidth, sig_digits,
				       left_sig_digit, exp_field_width,
				       exp_switch_len, point_pos,
				       Cnp->low_lbls.fcode[0],
				       "ContourPlotDraw");
		Substitute(sub,5,fstr);
		c_ctsetc("CTM",buf);
		Cnp->low_lbls.count++;
		break;
	case 8:
		if (( Cnp->hlb_val % 2 == 1) &&
		    (Cnp->low_lbls.perim_on == False || Cnp->low_lbls.perim_lcolor == NhlTRANSPARENT))
			_NhlSetLineOpacity(Cnl, 0.0); 
		else {
			gset_line_colr_ind(Cnp->low_lbls.gks_plcolor);
			gset_linewidth(Cnp->low_lbls.perim_lthick);
		}
		break;
	case -8:
		if (( Cnp->hlb_val % 2 == 1) &&
		    (Cnp->low_lbls.perim_on == False || Cnp->low_lbls.perim_lcolor == NhlTRANSPARENT))
			_NhlSetLineOpacity(Cnl, 1.0); 
		break;
	default:
		break;
	}

	return;
}

/*
 * Function:  hluctchll
 *
 * Description: 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values
 *
 * Side Effects: 
 */

/*ARGSUSED*/
void   (_NHLCALLF(hluctchll,HLUCTCHLL))
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
		_NHLCALLF(ctchll,CTCHLL)(iflg);
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
		c_ctgeti("PAI", &pai);
		if (pai > 0) {
			pai -= 1;

			llcol = Cnp->line_lbls.mono_color ?
				 Cnp->line_lbls.gks_color : 
					Cnp->line_lbls.colors[pai];
			if (llcol > NhlTRANSPARENT) {
				c_pcseti("CC",llcol);
				c_pcseti("OC",llcol);
                                _NhlSetFillOpacity(Cnl, 1.0);  /* NCL-1509 */
			}
			else {
				c_ctsetc("CTM"," ");
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
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToWin(tfp->overlay_trans_obj,
			     xout,yout,1,xout,yout,&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
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

        if (! tfp->overlay_trans_obj ||
            tfp->overlay_trans_obj == tfp->trans_obj) {
		_NhlWinToCompc(tfp->trans_obj,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlWinToData(tfp->overlay_trans_obj,
			      xin,yin,1,xout,yout,
			      &status,NULL,NULL);

		if (status) return;
#if 0
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToCompc(tfp->trans_obj,xout,yout,1,xout,yout,
				&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

	return;
}

/*
 * Function:  hluctmxyz
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
void   (_NHLCALLF(hluctmxyz,HLUCTMXYZ))
#if	NhlNeedProto
(
	int	*imap,
	float	*xinp,
	float	*yinp,
	float	*zinp,
	float	*xotp,
	float	*yotp
)
#else
(imap,xinp,yinp,zinp,xotp,yotp)
	int	*imap;
	float	*xinp;
	float	*yinp;
	float	*zinp,
	float	*xotp;
	float	*yotp;
#endif

{
	int status;
	float xtmp,ytmp;
	double rtod = 57.2957795130823;

	if (Cnp == NULL) {
		_NHLCALLF(ctmxyz,CTMXYZ)(imap,xinp,yinp,zinp,xotp,yotp);
		return;
	}

        if (*imap == - Nhlcn1DMESHMAPVAL && Tmp->ezmap) {
		OverlayInvMapXY(&Cnl->trans,xinp,yinp,xotp,yotp);
	}
        else if (abs(*imap) != NhlcnMAPVAL) {
                *xotp = *xinp;
                *yotp = *yinp;
        }
	else if (Cnl->trans.overlay_status == _tfCurrentOverlayMember &&
		 ! Cnl->trans.do_ndc_overlay) { 
		if (*imap > 0) {
			double dtmp = (double)*zinp/
				sqrt((double)*xinp * (double)*xinp + (double)*yinp * (double)*yinp +                                                                                        
				     (double)*zinp * (double)*zinp);
			if (dtmp >= 1.0) 
				ytmp = 90.0;
			else if (dtmp <= -1.0) 
				ytmp = -90.0;
			else {
				ytmp = rtod*asin(dtmp);
				if (isnan(ytmp)) {
					*xotp = Cnp->out_of_range_val;
					*yotp = Cnp->out_of_range_val;
					return;
				}
			}
			if (*xinp == 0 && *yinp == 0) {
				xtmp = 0.0;
			}
			else {
				xtmp = rtod * atan2(*yinp,*xinp);
			}
			if (xtmp < Cnp->xlb)
				xtmp += 360.0;
			if (xtmp > Cnp->xub) 
				xtmp -= 360.0;
			OverlayMapXY(&Cnl->trans,&xtmp,&ytmp,xotp,yotp);
		}
		else
			OverlayInvMapXY(&Cnl->trans,xinp,yinp,xotp,yotp);
	}
	else {
		/* I don't know if this branch is ever taken any more */
		if (*imap > 0) {
			ytmp = rtod*asin(*zinp/
					 sqrt(*xinp * *xinp + *yinp * *yinp +
					      *zinp * *zinp));
			if (*xinp == 0 && *yinp == 0) {
				xtmp = 0.0;
			}
			else {
				xtmp = rtod * atan2(*yinp,*xinp);
			}
			if (xtmp < Cnp->xlb)
				xtmp += 360.0;
			if (xtmp > Cnp->xub) 
				xtmp -= 360.0;
			_NhlCompcToWin((NhlLayer)Cnp->trans_obj,
				       &xtmp,&ytmp,1,xotp,yotp,
				       &status,NULL,NULL);
		}
		else { 
			_NhlWinToCompc((NhlLayer)Cnp->trans_obj,
				       xinp,yinp,1,xotp,yotp,
				       &status,NULL,NULL);

		}
		
	}
	return;
}


/*
 * Function:  load_hluct_routines
 *
 * Description: Forces the hluct... routines to load from the HLU library
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
static void   load_hluct_routines
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
		_NHLCALLF(hluctmxyz,HLUCTMXYZ)
			(&idum,&fdum,&fdum,&fdum,&fdum,&fdum);
		_NHLCALLF(hluctchll,HLUCTCHLL)(&idum);
		_NHLCALLF(hluctchhl,HLUCTCHHL)(&idum);
		_NHLCALLF(hluctchcl,HLUCTCHCL)(&idum);
		_NHLCALLF(hluctscae,HLUCTSCAE)(&idum,&idum,&idum,&idum,
					       &fdum,&fdum,&fdum,&fdum,
					       &idum,&idum,&idum,&idum);
	}
	return;
}

void _NhlSetCnl
#if	NhlNeedProto
(
	NhlContourPlotLayer cnl
)
#else
(cnl)
	NhlContourPlotLayer cnl;

#endif
{

	if (! cnl) {
		Cnl = NULL;
		Cnp = NULL;
	}
	else {
		Cnl = cnl;
		Cnp = &cnl->contourplot;
	}
	return;
}
