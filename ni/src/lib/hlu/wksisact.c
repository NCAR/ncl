
/*
 *      $Id: wksisact.c,v 1.3 1998-03-11 18:36:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		wksisact.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 3 15:57:39 MDT 1992
 *
 *	Description:	This file contains a utility function for determining
 *			whether a workstation is active or not.
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>

/*
 * Function:	wksisact
 *
 * Description: determins whether a workstation is active.
 *
 * In Args: n is the workstation id to be checked
 *
 * Out Args: NONE 
 *
 * Return Values: True if active false if not
 *
 * Side Effects: NONE
 */
int	wksisact
#if	NhlNeedProto
(int	n)
#else
(n)
	int	n;
#endif
{
	int i;
	int errind,numact,wkid,tmp = 1;
	int state;
	i = n;

/* FORTRAN */ _NHLCALLF(gqwks,GQWKS)(&i,&errind,&state);
	if (state == 1)
		 return (1);
	return 0;
#if 0
/* FORTRAN */ _NHLCALLF(gqacwk,GQACWK)(&tmp,&errind,&numact,&wkid);
	if(wkid == n)
		return(1);

	for(i= 2; i<=numact; i++ ) {
/* FORTRAN */ _NHLCALLF(gqacwk,GQACWK)(&i,&errind,&tmp,&wkid);
		if(wkid == n)
			return(1);
	}
	return(0);
#endif
}

