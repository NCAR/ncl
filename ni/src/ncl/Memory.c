
/*
 *      $Id: Memory.c,v 1.1 1993-09-24 23:40:40 ethan Exp $
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

#include <ncarg/hlu/hlu.h>
#include <errno.h>


void
*NclMalloc
#if     __STDC__
(
        unsigned int    size    /* size of memory requested     */
)
#else
(size)
        unsigned int    size;   /* size of memory requested     */
#endif
{
        void *ptr;

        if(size == 0)
                return NULL;

        ptr = (void *)malloc(size);

        if(ptr == NULL)
                NhlPError(FATAL,errno,"NclMalloc Failed");

        return(ptr);
}

NhlErrorTypes
NclFree
#if     __STDC__
(
        void            *ptr    /* pointer to memory to free    */
)
#else
(ptr)
        void            *ptr;   /* pointer to memory to free    */
#endif
{
        register int ret;

        if(ptr == NULL)
                return(NOERROR);

        else{
#ifdef  __sgi
                free(ptr);
                return NOERROR;
#else
                ret = free(ptr);

                if(ret == 0){

                        NhlPError(WARNING,errno,"Error in NclFree");
                        return(WARNING);
                }
                else{
                        return(NOERROR);
                }
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
 * In Args:	unsigned int	num	number of elements
 *		unsigned int	size	size of each element
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NclCalloc
#if	__STDC__
(
	unsigned int	num,	/* number of elements		*/
	unsigned int	size	/* size of each element		*/
)
#else
(num,size)
	 unsigned int	num;	/* number of elements		*/
	unsigned int	size;	/* size of each element		*/
#endif
{
	void *ptr;

	if((num * size) == 0)
		return NULL;

	ptr = (void *)calloc(num, size);

	if(ptr == NULL)
		NhlPError(FATAL,errno,"NhlCalloc Failed");

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
 *		unsigned int	size	size of memory requested
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NclRealloc
#if	__STDC__
(
	void		*ptr,	/* pointer to old memory	*/
	unsigned int	size	/* size of memory requested	*/
)
#else
(ptr,size)
	void		*ptr;	/* pointer to old memory	*/
	unsigned int	size;	/* size of memory requested	*/
#endif
{
	void *tptr;

	if(ptr == NULL)
		return NhlMalloc(size);
	else{
		tptr = (void *)realloc(ptr,size);

		if(tptr == NULL)
			NhlPError(FATAL,errno,"NhlRealloc Failed");

		return(tptr);
	}
}

