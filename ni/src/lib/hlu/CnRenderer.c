/*
 *      $Id: CnRenderer.c,v 1.1 2004-03-11 02:00:16 dbrown Exp $
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
/* render */		        NULL
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "CnRendererInitialize";
	char			*e_text;

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
