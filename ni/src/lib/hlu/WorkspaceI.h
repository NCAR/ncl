/*
 *      $Id: WorkspaceI.h,v 1.12 1998-05-22 01:59:14 dbrown Exp $
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
	NhlwsAREAMAP, NhlwsCNFLOAT, NhlwsCNINT, NhlwsSEGDATA, NhlwsOTHER
} NhlwsType;

typedef enum _NhlPersistence {
	NhlwsNONE,
	NhlwsCORE,
	NhlwsDISK
} NhlPersistence;

typedef struct _NhlAreaSetRec {
	struct	_NhlAreaSetRec	*next;
	NrmQuark		aset_name;
	int			group_id;
	int			aset_size;
	int			*aset_ids;
} NhlAreaSetRec;

typedef struct _NhlWorkspace {
	int			ws_id;     	/* Workspace identifier */
	NrmQuark		type;   	/* Workspace type */
	NhlPersistence		persistence;	/* Need for preservation */
	NhlPointer		ws_data;	/* Workspace information */
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
	NhlBoolean	smooth,
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
