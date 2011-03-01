/*
 *      $Id: VarArg.c,v 1.8 1997-01-17 18:57:46 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VarArg.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 3 17:13:07 MDT 1992
 *
 *	Description:	This file contains all the var arg support functions
 *			that are used in the hlu library.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>

/*
 * Function:	_NhlVarToSetArgList
 *
 * Description:	This function is used to take a vararg list with resource
 *		names and values and convert it into an arglist that will
 *		be used to set the named resources.
 *
 * In Args:     va_list ap              vararg list 
 *              int     num_vargs       number of argument pairs in ap 
 * 
 * Out Args:    _NhlArgList     *args   pointer to return arglist in 
 * 
 * Scope:       Global Private
 * Returns:     int	number of args filled in. 
 * Side Effect: 
 */ 
int 
_NhlVarToSetArgList 
#if	NhlNeedProto
( 
	va_list		list,		/* vararg list  */ 
	_NhlArgList	args		/* pointer to return arglist in */ 
) 
#else 
(list,args)
	va_list		list;		/* vararg list  */ 
	_NhlArgList	args;		/* pointer to return arglist in */ 
#endif 
{ 
	register int	i=0;
	NhlString	name=NULL;

	for(name = va_arg(list,NhlString); name != NULL;
						name = va_arg(list,NhlString)){

		if(i >= _NhlMAXARGLIST){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"_NhlVarToSetArgList:Only %d args can be passed in-Ignoring the rest",
								_NhlMAXARGLIST);
			break;
		}

		if(_NhlIsFloatRes(name))
			args[i].value.dblval = va_arg(list,double);
		else
			args[i].value.lngval = (long)va_arg(list,long);

		args[i].quark = NrmStringToQuark(name);
		args[i].type = NrmNULLQUARK;
		i++;
	}

	qsort(args,i,sizeof(_NhlArg),_NhlCompareArg);

	return i;
}
