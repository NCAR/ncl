
/*
 *      $Id: wksisopn.c,v 1.2 1994-12-16 20:05:19 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		wksisopn.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 3 15:45:06 MDT 1992
 *
 *	Description:	This file holds a utility function that queries
 *			the workstation list and returns either true(1) or
 *			false(0) if the workstation is currently open.
 */
#include <ncarg/hlu/hluutil.h>

/*
 * Function:	
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int	wksisopn
#if	NhlNeedProto
(int	n)
#else
(n)
	int n;
#endif
{
	int i;
	int errind,numopen,wkid,tmp = 1;

/* FORTRAN */ _NHLCALLF(gqopwk,GQOPWK)(&tmp,&errind,&numopen,&wkid);
	if(wkid == n)
		return(1);

	for(i  = 2; i <= numopen; i++ ) {
/* FORTRAN */ _NHLCALLF(gqopwk,GQOPWK)(&i,&errind,&tmp,&wkid);
		if(wkid == n) 
			return(1);
	}
	return(0);
}
