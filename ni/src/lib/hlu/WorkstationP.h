/*
 *      $Id: WorkstationP.h,v 1.1 1993-04-30 17:26:08 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		WorkstationP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 9 10:44:17 MDT 1992
 *
 *	Description:	Private header file for workstation class.
 */


#ifndef _NWorkstationP_h
#define	_NWorkstationP_h

#include	<ncarg/hlu/BaseP.h>
#include 	<ncarg/hlu/Workstation.h>

#define MAX_COLOR_MAP	256
/*
* Background is a special index in the color map and should either be
* first or last and should only be set ONCE per workstation at create
* time
*/
#define BACKGROUND	0
/*
* All ci fields in the private_color_map field get set to this originally
*/

#define REMOVE		-4
#define UNSET		-3
#define SETALMOST	-2


typedef struct _NhlPrivateColor {
	int ci;
	float red;
	float green;
	float blue;
} NhlPrivateColor;

typedef NhlErrorTypes (*NhlWorkstationProc)(
#if	NhlNeedProto
	Layer	l	/* layer to operate on	*/
#endif
);

/*
 * This is used as the Inheritance constant
 */
#define NhlInheritUpdate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritClear ((NhlWorkstationProc)_NhlInherit)

typedef struct _WorkstationLayerPart {
	/* User setable resource fields */

	NhlColor	*color_map;
	int		color_map_len;
	NhlColor	*bkgnd_color;

	/* Private internal fields */

	LayerList	children;
	int		num_children;
	NhlPrivateColor	private_color_map[MAX_COLOR_MAP];
	int		num_private_colors;

	/* Export Values */

	int		gkswksid;

	/* Import Values */
	int		gkswkstype;
	int		gkswksconid;
} WorkstationLayerPart;

typedef struct _WorkstationLayerRec {
	BaseLayerPart		base;
	WorkstationLayerPart	work;
} WorkstationLayerRec;

typedef struct _WorkstationLayerClassPart {
	NhlWorkstationProc	open_work;
	NhlWorkstationProc	close_work;
	NhlWorkstationProc	activate_work;
	NhlWorkstationProc	deactivate_work;
	NhlWorkstationProc	update_work;
	NhlWorkstationProc	clear_work;
} WorkstationLayerClassPart;

typedef struct _WorkstationLayerClassRec {
	BaseLayerClassPart		base_class;
	WorkstationLayerClassPart	work_class;
} WorkstationLayerClassRec;
	

extern WorkstationLayerClassRec workstationLayerClassRec;	

extern  NhlErrorTypes _NhlAddWorkChildLayer(
#ifdef NhlNeedProto
        Layer   /* parent */,
        Layer   /* child */
#endif
);

extern  NhlErrorTypes _NhlDeleteWorkChildLayer(
#ifdef NhlNeedProto
        Layer   /* parent */,
        Layer   /* child */
#endif
);



#endif	/* _NWorkstationP_h */

