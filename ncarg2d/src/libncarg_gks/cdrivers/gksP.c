/*
 *      $Id: gksP.c,v 1.3 2000-07-12 16:51:57 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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

#define MaxEsc	10

static _NGCesc init_escape[MaxEsc];
static int	num_init=0;

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
	if(escape->work_id == -1){
		/*
		 * escape applies to next workstation created.
		 */
		if(num_init >= (MaxEsc - 1))
			return 300;
		init_escape[num_init++] = *cesc;

		return 0;
	}

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

_NGCesc	*
_NGGetCEscInit
(
	void
)
{
	if(!num_init)
		return NULL;
	return &init_escape[--num_init];
}
