/*
 *      $Id: WorkspaceP.h,v 1.4 1994-09-12 21:01:15 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		WorkspaceP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	
 */

#ifndef _NWORKSPACEP_h
#define _NWORKSPACEP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/WorkspaceI.h>

#define NhlwsDEF_THRESHOLD	1048576
#define NhlwsDEF_MAXIMUM	16777216
#define NhlwsMIN_THRESHOLD	8096
#define NhlwsMIN_MAXIMUM	262144
#define NhlwsIDLE_REC_ALLOC	64
#define NhlwsMAX_GKS_POINTS	20000
#define NhlwsMAX_AREA_GROUPS	64

typedef struct _NhlWorkspaceRec {
	int			ws_id;     	/* Workspace identifier */
	NrmQuark		type;   	/* Workspace type */
	NhlPersistence		persistence;	/* Need for preservation */
	NhlPointer		ws_data;	/* Workspace information */
	NhlPointer		ws_ptr;		/* Workspace address */
	int			cur_size;  	/* Current size in bytes */
	NhlBoolean		in_use;		/* Workspace in use? */
	NhlBoolean		data_intact;	/* Data preserved flag */
	FILE			*tmp_fp;
	struct _NhlWorkspaceRec *next;
} NhlWorkspaceRec;

typedef struct _NhlwsIdleRec {
	struct _NhlwsIdleRec	*next;
	struct _NhlwsIdleRec	*prev;
	NhlWorkspaceRec		*wsrp;
} NhlwsIdleRec;

typedef struct _NhlWorkspaceLayerPart {
	/* public resources */
	long			maximum_size;
	long			threshold_size;
	long			current_size;

	/* private fields */

	long			total_size;
	int		        last_ws_id;
	NhlWorkspaceRec		*ws_list;
	
} NhlWorkspaceLayerPart;

typedef struct _NhlWorkspaceLayerRec {
	NhlBaseLayerPart	base;
	NhlWorkspaceLayerPart	workspace;
} NhlWorkspaceLayerRec;

typedef struct NhlWorkspaceLayerClassPart{
	int			num_ws_instances;
} NhlWorkspaceLayerClassPart;

typedef struct _NhlWorkspaceLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlWorkspaceLayerClassPart	workspace_class;
} NhlWorkspaceLayerClassRec;

typedef struct _NhlWorkspaceLayerClassRec *NhlWorkspaceLayerClass;
typedef struct _NhlWorkspaceLayerRec	*NhlWorkspaceLayer;

extern NhlWorkspaceLayerClassRec		NhlworkspaceLayerClassRec;

/* Private external functions */

extern void _NhlInitWorkspace(
#if	NhlNeedProto
	void
#endif
);

extern void _NhlCloseWorkspace(
#if	NhlNeedProto
	void
#endif
);

#endif  /* _NWORKSPACEP_h */
