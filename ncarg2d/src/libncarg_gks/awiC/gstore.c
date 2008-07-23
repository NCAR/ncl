/*
 *      $Id: gstore.c,v 1.4 2008-07-23 17:24:19 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		gstore.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 19 09:22:41 MDT 1994
 *
 *	Description:	
 */
#include <stdlib.h>
#include <string.h>
#include "gstore.h"

/*
 * Function:	init_store
 *
 * Description:	Allocate and initialize a gstore recored.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	a valid Gstore, or NULL on error.
 * Side Effect:	
 */
static Gstore
init_store
#ifdef	NeedFuncProto
(
	void
)
#else
()
#endif
{
	Gstore			gstor_ret;

	gstor_ret = (Gstore)malloc(sizeof(struct _GstoreRec_));

	if(gstor_ret == NULL)
		return NULL;

	/* init struct */
	memset(gstor_ret,0,sizeof(struct _GstoreRec_));

	return gstor_ret;
}

/*
 * Function:	gcreate_store
 *
 * Description:	Allocate a Gstore to be passed into gks(C) functions that
 *		need dynamic memory allocation.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	void
 * Side Effect:	*store will point to a valid Gstore if err_ind is 0.
 */
void
gcreate_store
#ifdef	NeedFuncProto
(
	Gint	*err_ind,
	Gstore	*store
)
#else
(err_ind,store)
	Gint	*err_ind;
	Gstore	*store;
#endif
{
	*store = init_store();

	if(*store)
		*err_ind = 0;		/* no error			*/
	else
		*err_ind = 2204;	/* cannot alloc mem error	*/

	return;
}

/*
 * Function:	_gstore_alloc
 *
 * Description:	Memory allocation function to be used by functions that need
 *		dynamic memory.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	pointer to memory of size requested, or NULL
 * Side Effect:	
 */
void *
_gstore_alloc
#ifdef	NeedFuncProto
(
	Gstore		stor,
	unsigned int	size
)
#else
(stor,size)
	Gstore		stor;
	unsigned int	size;
#endif
{
	void	*ptr;
	Gstore	loc_stor = stor;
	
	if(loc_stor == NULL)
		return NULL;

	while(loc_stor->num >= GSTALLOCSIZE){
		if(loc_stor->next == NULL){
			loc_stor->next = init_store();
			if(loc_stor->next == NULL)
				return NULL;
		}
		loc_stor = loc_stor->next;
	}

	ptr = (void*)malloc(size);

	if(ptr != NULL)
		loc_stor->ptrs[loc_stor->num++] = ptr;

	return ptr;
}

/*
 * Function:	_gstore_clear
 *
 * Description:	This function is called by gks functions that need dynamic
 *		memory to clear out the current Gstore.  According to
 *		the gks(C) standard, if a Gstore is used for a second inq
 *		function, then the memory should be reused.  So, this function
 *		free's up any memory that is currently allocated in the
 *		Gstore.  Then the inq function can call _gstore_alloc to
 *		allocate memory from the Gstore.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	0 on success, non-0 on error
 * Side Effect:	
 */
int
_gstore_clear
#ifdef	NeedFuncProto
(
	Gstore	stor
)
#else
(stor)
	Gstore	stor;
#endif
{
	int	i;

	if(!stor)
		return 1;

	gdel_store(stor->next);

	stor->next = NULL;

	for(i=0;i < stor->num;i++){
		free(stor->ptrs[i]);
		stor->ptrs[i] = NULL;
	}

	return 0;
}

/*
 * Function:	gdel_store
 *
 * Description:	This function should be called by the user to free the dynamic
 *		memory allocated on behalf of the user by an inq function.
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	void
 * Side Effect:	
 */
void
gdel_store
#ifdef	NeedFuncProto
(
	Gstore	stor
)
#else
(stor)
	Gstore	stor;
#endif
{
	int	i;

	if(stor == NULL)
		return;

	gdel_store(stor->next);

	for(i=0;i < stor->num;i++)
		free(stor->ptrs[i]);
	free(stor);
}
