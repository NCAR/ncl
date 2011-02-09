
/*
 *      $Id: Memory.c,v 1.16 2008-12-10 20:12:16 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Thu Jul 1 10:51:22 MDT 1993
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include <errno.h>
#include "defs.h"
#include "Symbol.h"
#include "NclData.h"
#include "NclMultiDValData.h"

unsigned long total = 0;

void
*NclMalloc
#if	NhlNeedProto
(
        ng_usize_t    size    /* size of memory requested     */
)
#else
(size)
        ng_usize_t    size;   /* size of memory requested     */
#endif
{
        void *ptr;

        if(size == 0)
                return NULL;

        ptr = (void *)malloc(size);

        if(ptr == NULL)
                NhlPError(NhlFATAL,errno,"NclMalloc Failed");

        return(ptr);
}

NhlErrorTypes
NclFree
#if	NhlNeedProto
(
        void            *ptr    /* pointer to memory to free    */
)
#else
(ptr)
        void            *ptr;   /* pointer to memory to free    */
#endif
{
/*
        register int ret;
*/

        if(ptr == NULL)
                return(NhlNOERROR);

        else{
#ifdef  __sgi 
                free(ptr);
                return NhlNOERROR;
#else
                free(ptr);
                return NhlNOERROR;
#endif
        }
}


/*
 * Function:	NclCalloc
 *
 * Description:	This function is our interface to the regular calloc
 *		system call.  We are using it so we can do error handleing
 *		for memory allocation in one place and so we can impliment
 *		our own memory management code if we need to.
 *
 * In Args:	ng_usize_t	num	number of elements
 *		ng_usize_t	size	size of each element
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NclCalloc
#if	NhlNeedProto
(
	ng_usize_t	num,	/* number of elements		*/
	ng_usize_t	size	/* size of each element		*/
)
#else
(num,size)
	ng_usize_t	num;	/* number of elements		*/
	ng_usize_t	size;	/* size of each element		*/
#endif
{
	void *ptr;

	if((num * size) == 0)
		return NULL;

	ptr = (void *)calloc(num, size);

	if(ptr == NULL)
		NhlPError(NhlFATAL,errno,"NhlCalloc Failed");

	return(ptr);
}

/*
 * Function:	NclRealloc
 *
 * Description:	This function is our interface to the regular realloc
 *		system call.  We are using it so we can do error handleing
 *		for memory allocation in one place and so we can impliment
 *		our own memory management code if we need to.
 *
 * In Args:	void		*ptr	pointer to old memory
 *		ng_usize_t	size	size of memory requested
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NclRealloc
#if	NhlNeedProto
(
	void		*ptr,	/* pointer to old memory	*/
	ng_usize_t	size	/* size of memory requested	*/
)
#else
(ptr,size)
	void		*ptr;	/* pointer to old memory	*/
	ng_usize_t	size;	/* size of memory requested	*/
#endif
{
	void *tptr;

	if(ptr == NULL)
		return NclMalloc(size);
	else{
		tptr = (void *)realloc(ptr,size);

		if(tptr == NULL)
			NhlPError(NhlFATAL,errno,"NhlRealloc Failed");

		return(tptr);
	}
}


void _NclFreeSubRec 
#if	NhlNeedProto
(struct _NclSubRec * sub_rec)
#else 
(sub_rec)
struct _NclSubRec * sub_rec;
#endif
{
	switch(sub_rec->sub_type) {
	case COORD_VECT:
	case INT_VECT:
		if((sub_rec->u.vec.vec != NULL)&&(sub_rec->u.vec.vec->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)sub_rec->u.vec.vec);
		}
		break;
	case COORD_SINGLE:
	case COORD_RANGE:
	case INT_RANGE:
	case INT_SINGLE:
/*
* This might happen when single indices are used
*/
		if(sub_rec->u.range.start == sub_rec->u.range.finish) {
			sub_rec->u.range.finish = NULL;
		}
		if((sub_rec->u.range.start != NULL)&&(sub_rec->u.range.start->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)sub_rec->u.range.start);
		} 
		if((sub_rec->u.range.finish != NULL)&&(sub_rec->u.range.finish->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)sub_rec->u.range.finish);
		}
		if((sub_rec->u.range.stride != NULL)&&(sub_rec->u.range.stride->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)sub_rec->u.range.stride);
		}
		break;
	default:
		break;
	}
}


#ifdef __cplusplus
}
#endif
