/*
 *      $Id: gksP.c,v 1.1 1996-03-16 21:43:47 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		gksP.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat Mar 9 14:16:43 MST 1996
 *
 *	Description:	
 */
#include <ncarg/gksP.h>
#include <ncarg/c.h>
#include "gks.h"
#include "gksc.h"
#include "gks_device.h"

/*
 * Prototypes for Fortran calls
 */
void NGCALLF(gzxid,GZXID)(
#ifdef	NeedFuncProto
	int	*wkid,
	int	*loc_id,
	int	*ierr
#endif
);

/*
 * Function:	_NGCescape
 *
 * Description:	Send a native "C" type to one of the cdrivers using gescape
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
int _NGCescape
#ifdef	NeedFuncProto
(
	int	func_id,
	_NGCesc	*cesc
)
#else
(func_id,cesc)
	int	func_id;
	_NGCesc	*cesc;
#endif
{
	_NGCAny	*escape = &cesc->any;
	int	lid,ierr;
	GKSC	*gksc;

	/*
	 * Get local id for workstation.
	 */
	NGCALLF(gzxid,GZXID)(&escape->work_id,&lid,&ierr);
	if(ierr != 0) return ierr;

	/*
	 * Get gksc ptr from local id.
	 */
	gksc = IndexToGKSC(lid);
	if(!gksc) return 182;

	/*
	 * Call the escape function - put the func_id in the ilist,
	 * and the native data there, then call it.
	 */
	gksc->opcode = ESCAPE;
	*(int*)gksc->i.list = func_id;
	gksc->i.num = 1;
	gksc->native = cesc;
	ierr = ExecGKSC(gksc);
	ClearGKSC(gksc);
	gksc->native = NULL;

	return ierr;
}
