/*
 *      $Id: WorkspaceI.h,v 1.16 2004-03-11 02:00:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		WorkspaceI.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Mar  9 11:10:12 MST 1994
 *
 *	Description:	
 */
#ifndef	_NWORKSPACEI_H
#define	_NWORKSPACEI_H

#include <ncarg/hlu/Workspace.h>

typedef enum _NhlwsType {
	NhlwsAREAMAP, NhlwsCNFLOAT, NhlwsCNINT, NhlwsSEGDATA, 
	NhlwsCTFLOAT, NhlwsCTINT, NhlwsOTHER
} NhlwsType;

typedef enum _NhlPersistence {
	NhlwsNONE,
	NhlwsCORE,
	NhlwsDISK
} NhlPersistence;

typedef struct _NhlWorkspace {
	int			ws_id;     	/* Workspace identifier */
	NrmQuark		type;   	/* Workspace type */
	NhlPersistence		persistence;	/* Need for preservation */
        int			req_size;
} NhlWorkspace;

extern int _NhlNewWorkspace(
#if	NhlNeedProto
	NhlwsType	type,
	NhlPersistence	persistence,			    
	int		req_size
#endif
);

extern void _NhlFreeWorkspace(
#if	NhlNeedProto
	int		ws_id
#endif
);

extern NhlWorkspace *_NhlUseWorkspace(
#if	NhlNeedProto
	int		ws_id
#endif
);

extern NhlErrorTypes _NhlIdleWorkspace(
#if	NhlNeedProto
	NhlWorkspace *ws
#endif
);

extern NhlBoolean _NhlWorkspaceDataIntact(
#if	NhlNeedProto
	int		ws_id
#endif
);

/* Dyanamic Areas interface functions */

extern NhlErrorTypes _NhlArinam(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlArpram(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		flag1,
	int		flag2,
	int		flag3,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlAredam(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	float		*x,
	float		*y,
	int		npoints,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlArscam(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		(*apr)(float *xcs, 
			       float *ycs, 
			       int *ncs, 
			       int *iai, 
			       int *iag, 
			       int *nai),
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlArdbpa(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		igi,
	char		*label,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlDumpAreaMap(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

/* Dynamic Conpack interfaces */


extern NhlErrorTypes _NhlCpback(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpclam(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpcldm(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	int		(*rtpl)(float *xcs, 
			       float *ycs,
			       int *ncs,
			       int *iai,
			       int *iag,
			       int *nai),
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpcldr(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpcltr(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	float           clevel,
	int             *flag,
	float           **xloc,
	float           **yloc,
	int             *npoints,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCplbam(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCplbdr(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpcica(
#if	NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	NhlWorkspace	*cell_ws,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	float		min_cell_size,
	NhlBoolean	smooth,
	NhlBoolean      use_mesh_fill,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCprect(
#if	NhlNeedProto
	float		*zdat,
	int		kzdt,
	int		mzdt,
	int		nzdt,				
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

/* Dynamic Conpackt interface functions */

NhlErrorTypes _NhlCtmesh(
#if	NhlNeedProto
	float		*rpnt,
	int		npnt,
	int		lopn,
	int             *iedg,
	int             nedg,
	int             loen,
	int             *itri,
	int             ntri,
	int             lotn,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

/* modified version used to improve threading performance */
NhlErrorTypes _NhlHLUCtmesh(
#if	NhlNeedProto
	float		*rpnt,
	int		npnt,
	int		lopn,
	int             *iedg,
	int             nedg,
	int             loen,
	int             *itri,
	int             ntri,
	int             lotn,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

NhlErrorTypes _NhlCtcldr(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCtcltr(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	float           clevel,
	int             *flag,
	float           **xloc,
	float           **yloc,
	int             *npoints,
	char		*entry_name
#endif
);

NhlErrorTypes _NhlCtlbdr(
#if	NhlNeedProto

	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

/* 
  fill_op: 0 : RasterFill,  1 : MeshFill,  2 : RasterFill - no GCA, 3 GCA only
*/

NhlErrorTypes _NhlCtcica(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	NhlWorkspace	*cell_ws,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	float		min_cell_size,
	NhlBoolean	smooth,
	int             fill_op,
	void            *info,
	char		*entry_name
#endif
);

NhlErrorTypes _NhlCtclam(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

NhlErrorTypes _NhlCtcldm(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	int		(*rtpl)(float *xcs, 
				float *ycs,
				int *ncs,
				int *iai,
				int *iag,
				int *nai),
	char		*entry_name
#endif
);

NhlErrorTypes _NhlCtlbam(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);



/* Dynamic Ezmap interface functions */

extern NhlErrorTypes _NhlMapbla(
#if	NhlNeedProto
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);


extern NhlErrorTypes _NhlMapita(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	float		x,
	float		y,
	int		up_or_down,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlMapiqa(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlMapblm(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		(*ulpr)(float *xcra, 
			       float *ycra, 
			       int *mcra, 
			       int *iaai, 
			       int *iagi, 
			       int *nogi),
	char		*entry_name
#endif
);


extern NhlErrorTypes _NhlMapgrm(
#if	NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		(*ulpr)(float *xcra, 
			       float *ycra, 
			       int *mcra, 
			       int *iaai, 
			       int *iagi, 
			       int *nogi),
	char		*entry_name
#endif
);


/* Dynamic Ezmapb interface functions */


extern NhlErrorTypes _NhlMplnam(
#if	NhlNeedProto
 	NhlWorkspace	*amap_ws,
        NhlString	map_data_filename,
        int		level,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlMplndm(
#if	NhlNeedProto
 	NhlWorkspace	*amap_ws,
        NhlString	map_data_filename,
        int		level,
	int		(*ulpr)(float *xcra, 
			       float *ycra, 
			       int *mcra, 
			       int *iaai, 
			       int *iagi, 
			       int *nogi),
	char		*entry_name
#endif
);


/* Dynamic Ezmap interface functions for high-res database */

extern NhlErrorTypes _NhlMdrgol(
#if	NhlNeedProto
	int		irgl,
	NhlWorkspace	*flt_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlMdrgsf(
#if	NhlNeedProto
	int		irgl,
	NhlWorkspace	*flt_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);




/* Dynamic Vectors interface functions */

extern NhlErrorTypes _NhlVvinit(
#if	NhlNeedProto
	float		*u,
	int		lu,
	float		*v,
	int		lv,
	float		*p,
	int		lp,
	int		m,
	int		n,				
	NhlWorkspace	*flt_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlVvectr(
#if	NhlNeedProto
	float		*u,
	float		*v,
	float		*p,
        NhlWorkspace	*amap_ws,
	int 		(*vvudmv_)(float *xcs, 
				   float *ycs,
				   int *ncs,
				   int *iai,
				   int *iag,
				   int *nai),
	NhlWorkspace	*flt_ws,
	char		*entry_name
#endif
);

/* Dynamic Streamlines interface functions */

extern NhlErrorTypes _NhlStinit(
#if	NhlNeedProto
	float		*u,
	int		lu,
	float		*v,
	int		lv,
	float		*p,
	int		lp,
	int		m,
	int		n,				
	NhlWorkspace	*flt_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlStream(
#if	NhlNeedProto
	float		*u,
	float		*v,
	float		*p,
        NhlWorkspace	*amap_ws,
	int 		(*stumsl_)(float *xcs, 
				   float *ycs,
				   int *ncs,
				   int *iai,
				   int *iag,
				   int *nai),
	NhlWorkspace	*flt_ws,
	char		*entry_name
#endif
);


#endif	/* _NWORKSPACEI_H */
