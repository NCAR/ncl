
/*
 *      $Id: wksisopn.c,v 1.3 1998-03-11 18:36:23 dbrown Exp $
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
#include <ncarg/hlu/WorkstationP.h>

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
NhlBoolean wksisopn
#if	NhlNeedProto
(int	n)
#else
(n)
	int n;
#endif
{
	int i;
	int errind,wkid,tmp = 1;
        wkGksWksRec *gksrec = NhlworkstationClassRec.work_class.gks_wks_recs;
        int numopen = *NhlworkstationClassRec.work_class.current_wks_count;

        for (i = 0; i < numopen; i++) {
                if (n == gksrec[i].gks_id)
                        return True;
                else if (n < gksrec[i].gks_id)
                        return False;
        }
        return False;
#if 0        
                
/* FORTRAN */ _NHLCALLF(gqopwk,GQOPWK)(&tmp,&errind,&numopen,&wkid);
	if(wkid == n)
		return(1);

	for(i  = 2; i <= numopen; i++ ) {
/* FORTRAN */ _NHLCALLF(gqopwk,GQOPWK)(&i,&errind,&tmp,&wkid);
		if(wkid == n) 
			return(1);
	}
	return(0);
#endif        
}
