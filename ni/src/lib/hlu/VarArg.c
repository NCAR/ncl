/*
 *      $Id: VarArg.c,v 1.1 1993-04-30 17:25:36 boote Exp $
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
 * Function:	_NhlCountSetVarList
 *
 * Description:	This function is used to count the number of resources that
 *		are being referenced in the given var arg list.
 *
 * In Args:	va_list	list	The variable argument list
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	int - the number of resources to set
 * Side Effect:	
 */
int
_NhlCountSetVarList
#if	__STDC__
(
	va_list	list	/* The variable argument list	*/
)
#else
(list)
	va_list	list;	/* The variable argument list	*/
#endif
{
	NhlString	name = NULL;
	int		count = 0;

	/* SUPPRESS 112 */
	for(name = (NhlString)(va_arg(list, NhlString)); name != NULL;
				name = (NhlString)(va_arg(list, NhlString))){
		if(_NhlIsFloatRes(name))
			(void)va_arg(list, double);
		else
			/* SUPPRESS 112 */
			(void)va_arg(list, _NhlArgVal);
		count++;
	}

	return(count);
}

/*
 * Function:	_NhlCountGetVarList
 *
 * Description:	This function is used to count the number of resources that
 *		are being referenced in the given var arg list.
 *
 * In Args:	va_list	list	The variable argument list
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	int - the number of resources to set
 * Side Effect:	
 */
int
_NhlCountGetVarList
#if	__STDC__
(
	va_list	list	/* The variable argument list	*/
)
#else
(list)
	va_list	list;	/* The variable argument list	*/
#endif
{
	NhlString	name = NULL;
	int		count = 0;

	/* SUPPRESS 112 */
	for(name = va_arg(list, NhlString); name != NULL;
						name = va_arg(list, NhlString)){
		/* SUPPRESS 112 */
		(void)va_arg(list, _NhlArgVal);
		count++;
	}

	return(count);
}

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
 * Returns:     NULL 
 * Side Effect: 
 */ 
void 
_NhlVarToSetArgList 
#if     __STDC__ 
( 
	va_list         ap,             /* vararg list  */ 
	_NhlArgList     *args,          /* pointer to return arglist in */ 
	int             num_vargs       /* number of arg pairs in ap    */ 
) 
#else 
(ap,args,num_vargs) 
	va_list         ap;             /* vararg list  */ 
	_NhlArgList     *args;          /* pointer to return arglist in */ 
	int             num_vargs;      /* number of arg pairs in ap    */ 
#endif 
{ 
	register int	i;
	double		tmp;
	NhlString	name=NULL;

	if(num_vargs == 0){
		*args = NULL;
		return;
	}

	*args = (_NhlArgList)NhlMalloc((unsigned)(num_vargs * sizeof(_NhlArg)));

	for(i=0; i < num_vargs; i++){
		/* SUPPRESS 112 */
		name = (NhlString)va_arg(ap,NhlString); 
		(*args)[i].quark = NrmStringToQuark(name);

		if(_NhlIsFloatRes(name)){
			tmp = va_arg(ap,double);
			*(float *)&((*args)[i].value) =(float)tmp;
		}
		else
			/* SUPPRESS 112 */
			(*args)[i].value = va_arg(ap,_NhlArgVal);
	}

	return;
}

/*
 * Function:	_NhlVarToGetArgList
 *
 * Description:	This function is used to take a vararg list with resource
 *		names and addresses and convert it into an arglist that will
 *		be used to retrieve the named resources into the given addresses
 *
 * In Args:     va_list ap              vararg list 
 *              int     num_vargs       number of argument pairs in ap 
 * 
 * Out Args:    _NhlArgList     *args   pointer to return arglist in 
 * 
 * Scope:       Global Private
 * Returns:     NULL 
 * Side Effect: 
 */ 
void 
_NhlVarToGetArgList 
#if     __STDC__ 
( 
	va_list         ap,             /* vararg list  */ 
	_NhlArgList     *args,          /* pointer to return arglist in */ 
	int             num_vargs       /* number of arg pairs in ap    */ 
) 
#else 
(ap,args,num_vargs) 
	va_list         ap;             /* vararg list  */ 
	_NhlArgList     *args;          /* pointer to return arglist in */ 
	int             num_vargs;      /* number of arg pairs in ap    */ 
#endif 
{ 
	register int	i;
	NhlString	name=NULL;

	*args = (_NhlArgList)NhlMalloc((unsigned)(num_vargs * sizeof(_NhlArg)));

	for(i=0; i < num_vargs; i++){
		/* SUPPRESS 112 */
		name = va_arg(ap,NhlString); 
		(*args)[i].quark = NrmStringToQuark(name);
		/* SUPPRESS 112 */
		(*args)[i].value = va_arg(ap,_NhlArgVal);
	}

	return;
}
