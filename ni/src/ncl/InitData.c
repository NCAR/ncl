
/*
 *      $Id: InitData.c,v 1.1 1994-07-14 20:45:55 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jan 13 14:52:24 MST 1994
 *
 *	Description:	
 */


#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"



static NhlErrorTypes CallInitializePart
#if  __STDC__
(NclObjClass oc,NclObjClass asuper_class)
#else 
(oc,asuper_class)
NclObjClass oc;
NclObjClass asuper_class;
#endif
{
	NhlErrorTypes ret1 = NhlNOERROR;
	NhlErrorTypes ret2 = NhlNOERROR;

	if(asuper_class->obj_class.super_class != NULL) {
		ret1 = CallInitializePart(oc,
			asuper_class->obj_class.super_class);
	
		if(ret1 < NhlWARNING)
			return(ret1);
	}

	if(asuper_class->obj_class.initialize_part != NULL) {
		ret2 = (*(asuper_class->obj_class.initialize_part))(oc);
	}

	return(MIN(ret1,ret2));
}


static NhlErrorTypes CallInitializeClass
#if  __STDC__
(NclObjClass	oc)
#else
(oc)
NclObjClass oc;
#endif
{
	NhlErrorTypes ret1 = NhlNOERROR,ret2 = NhlNOERROR, ret3 = NhlNOERROR;

	if(oc->obj_class.inited) return(NhlNOERROR);

	if((oc->obj_class.super_class != NULL)&&
		!((oc->obj_class.super_class->obj_class.inited))) {
		
		ret1 = CallInitializeClass(oc->obj_class.super_class);

		if(ret1 < NhlWARNING) 
			return(ret1);
	} else {
		ret1 = NhlNOERROR;
	}

	if(oc->obj_class.initialize_class != NULL) {
		ret2 = (*(oc->obj_class.initialize_class))();
		if(ret2 < NhlWARNING)
			return(ret2);
	}

	ret3 = CallInitializePart(oc,oc);	
	if(ret3 < NhlWARNING)
		return(ret3);

	oc->obj_class.inited = 0x1;

	return(MIN((MIN(ret1,ret2)),ret3));
}


NhlErrorTypes _NclInitClass
#if  __STDC__
(NclObjClass  oc)
#else
(oc)
NclObjClass oc;
#endif
{
	return(CallInitializeClass(oc));
}
