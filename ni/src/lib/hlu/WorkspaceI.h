/*
 *      $Id: WorkspaceI.h,v 1.5 1994-07-13 17:27:43 dbrown Exp $
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
	NhlwsAREAMAP, NhlwsCNFLOAT, NhlwsCNINT, NhlwsSEGDATA
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
} NhlWorkspace;

extern int _NhlNewWorkspace(
#ifdef NhlNeedProto
	NhlwsType	type,
	NhlPersistence	persistence,			    
	int		req_size
#endif
);

extern void _NhlFreeWorkspace(
#ifdef NhlNeedProto
	int		ws_id
#endif
);

extern NhlWorkspace *_NhlUseWorkspace(
#ifdef NhlNeedProto
	int		ws_id
#endif
);

extern NhlErrorTypes _NhlIdleWorkspace(
#ifdef NhlNeedProto
	NhlWorkspace *ws
#endif
);

/* Dyanamic Areas interface functions */

extern NhlErrorTypes _NhlArinam(
#ifdef NhlNeedProto
	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlArpram(
#ifdef NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		flag1,
	int		flag2,
	int		flag3,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlAredam(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		igi,
	char		*label,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlDumpAreaMap(
#ifdef NhlNeedProto
	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

/* Dynamic Conpack interfaces */


extern NhlErrorTypes _NhlCpback(
#ifdef NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpclam(
#ifdef NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCpcldm(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCplbam(
#ifdef NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCplbdr(
#ifdef NhlNeedProto
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlCprect(
#ifdef NhlNeedProto
	float		*zdat,
	int		kzdt,
	int		mzdt,
	int		nzdt,				
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
#endif
);

extern NhlErrorTypes _NhlMapbla(
#ifdef NhlNeedProto
 	NhlWorkspace	*amap_ws,
	char		*entry_name
#endif
);


extern NhlErrorTypes _NhlMapita(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
	NhlWorkspace	*amap_ws,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
#endif
);

#endif	/* _NWORKSPACEI_H */



