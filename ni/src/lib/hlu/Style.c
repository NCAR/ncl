/*
 *      $Id: Style.c,v 1.4 1997-07-25 21:12:36 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Style.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	The Style class controls the attributes of 
 *                      drawing primitives
 */

#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/StyleP.h>

#define Oset(field)     NhlOffset(NhlStyleLayerRec,style.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(foo),NhlTImmediate,
		 _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL}

/* End-documented-resources */

};


NhlStyleClassRec NhlstyleClassRec = {
	{
/* class_name			*/	"styleClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlStyleLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlbaseClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,
/* layer_draw			*/      NULL,
/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL
	},
	{
					NULL
	}
};

NhlClass NhlstyleClass = 
			(NhlClass)&NhlstyleClassRec;

/*
 * Function:	nhlfstyleclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlfstyleclass,NHLFSTYLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlstyleClass;
}

