/*
 *      $Id: Open.c,v 1.15 1998-06-04 16:22:42 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Open.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 09:50:11 MDT 1992
 *
 *	Description:	This file contains the functions neccessary to
 *			initialize the hlu library.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/AppI.h>

static NhlBoolean LIB_INITIALIZED = False;

/*
 * Function:	_NhlOpen
 *
 * Description:	internal init function - called for "C" and  "Fortran"
 *		interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static void _NhlOpen
#if	NhlNeedProto
(
	_NhlC_OR_F	init_type
)
#else
(init_type)
	_NhlC_OR_F	init_type;
#endif
{
	int	tint;

	if(LIB_INITIALIZED){
		NhlPError(NhlINFO,NhlEUNKNOWN,
			"The HLU library can only be initialized once!");
		return;
	}
	LIB_INITIALIZED = True;

	_NhlSetLang(init_type);

	(void)NhlVACreate(&tint,"hlu",NhlappClass,0,
			_NhlNnoAppDB,		True,
			NhlNappDefaultParent,	True,
			NULL);

	return;
}

/*
 * Function:	NhlOpen
 *
 * Description:	Init function for "C" interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void NhlOpen
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlOpen(_NhlCLIB);

	return;
}

/*
 * Function:	nhlfopen
 *
 * Description:	init hlu library for use from the "Fortran" bindings.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfopen,NHLPFOPEN)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlOpen(_NhlFLIB);

	return;
}

/*
 * Function:	_NhlInitialize
 *
 * Description:	internal init function - called for "C" and  "Fortran"
 *		interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static void _NhlInitialize
#if	NhlNeedProto
(
	_NhlC_OR_F	init_type
)
#else
(init_type)
	_NhlC_OR_F	init_type;
#endif
{
	if(LIB_INITIALIZED){
		NhlPError(NhlINFO,NhlEUNKNOWN,
			"The HLU library can only be initialized once!");
		return;
	}
	LIB_INITIALIZED = True;

	_NhlSetLang(init_type);

	return;
}

/*
 * Function:	NhlInitialize
 *
 * Description:	Init function for "C" interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void NhlInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlInitialize(_NhlCLIB);

	return;
}

/*
 * Function:	nhlfinitialize
 *
 * Description:	init hlu library for use from the "Fortran" bindings.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfinitialize,NHLPFINITIALIZE)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlInitialize(_NhlFLIB);

	return;
}
