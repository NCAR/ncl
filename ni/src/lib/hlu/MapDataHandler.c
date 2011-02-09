/*
 *      $Id: MapDataHandler.c,v 1.2 1998-11-12 21:40:02 dbrown Exp $
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

#include <ncarg/hlu/MapDataHandlerP.h>

#define Oset(field)	NhlOffset(NhlMapDataHandlerLayerRec,mapdh.field)
static NhlResource resources[] = {
	{NhlNmpAreaNames,NhlCmpAreaNames,
		 NhlTStringGenArray,sizeof(NhlPointer),
		 Oset(area_names),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpAreaTypes,NhlCmpAreaTypes,
		 NhlTIntegerGenArray,sizeof(NhlPointer),
		 Oset(area_types),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpDynamicAreaGroups,NhlCmpDynamicAreaGroups,
		 NhlTIntegerGenArray,sizeof(NhlPointer),
		 Oset(dynamic_groups),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFixedAreaGroups,NhlCmpFixedAreaGroups,
		 NhlTIntegerGenArray,sizeof(NhlPointer),
		 Oset(fixed_groups),NhlTImmediate,
         	_NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpAreaGroupCount,NhlCmpAreaGroupCount,NhlTInteger,
		 sizeof(int),Oset(area_group_count),NhlTImmediate,
		 _NhlUSET((NhlPointer) Nhl_mpMIN_AREA_GROUPS),0,NULL},
};
#undef Oset

static NhlErrorTypes MapDHInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes    MapDHDestroy(
#if	NhlNeedProto
	NhlLayer        l
#endif
);

NhlMapDataHandlerClassRec NhlmapDataHandlerClassRec = {
	{
/* class_name 		*/      "mapDataHandlerClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlMapDataHandlerLayerRec),
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
/* layer_initialize 	*/  	MapDHInitialize,
/* layer_set_values 	*/  	NULL,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	NULL,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	MapDHDestroy,
	},
	{
/* update_draw_list */		NULL,
/* draw_map_list	*/      NULL                        
	}
};

NhlClass NhlmapDataHandlerClass = (NhlClass)&NhlmapDataHandlerClassRec;

/*
 * Function:	MapDHInitialize
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
MapDHInitialize
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


/*
 * Function:    MapV41DHDestroy
 *
 * Description: Retrieves the current setting of MapV41DataHandler resources.
 *      Actually the resources belong to the superclass MapDataHandler --
 *      but they get their contents from the subclass.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpDynamicAreaGroups
 *		NhlNmpSpecifiedFillColors
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapDHDestroy
#if	NhlNeedProto
(
        NhlLayer l
        )
#else
(l)
        NhlLayer        l;
#endif
{

        return NhlNOERROR;
}

NhlErrorTypes _NhlUpdateDrawList
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlBoolean  		init,
        NhlMapPlotLayer 	newmp,
        NhlMapPlotLayer 	oldmp,
        _NhlArgList		args,
        int             	num_args
        )
#else
(instance,newmp,oldmp,args,num_args)
	NhlLayer	instance;
        NhlBoolean  	init;
        NhlMapPlotLayer newmp;
        NhlMapPlotLayer oldmp;
        _NhlArgList	args;
        int             num_args;
        
#endif
{
      NhlMapDataHandlerClassPart *mdhcp =
              &((NhlMapDataHandlerClass)
                instance->base.layer_class)->mapdh_class;
      
      return (*mdhcp->update_draw_list)
              (instance,init,newmp,oldmp,args,num_args);
}


NhlErrorTypes _NhlDrawMapList
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlMapPlotLayer 	mp,
        mpDrawOp		draw_op,
	NhlBoolean		init_draw
        )
#else
(instance,mpl,draw_op,init_draw)
	NhlLayer		instance;
        NhlMapPlotLayer 	mpl;
        mpDrawOp		draw_op;
	NhlBoolean		init_draw;
#endif
{
      NhlMapDataHandlerClassPart *mdhcp =
              &((NhlMapDataHandlerClass)
                instance->base.layer_class)->mapdh_class;
      
      return (*mdhcp->draw_map_list)(instance,mp,draw_op,init_draw);
}
