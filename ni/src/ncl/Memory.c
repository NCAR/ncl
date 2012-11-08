
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
 *	Description:	
 *
 *	Modifier:	Wei Huang
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *	Date:		Thu Nov 8, 2012
 *	Description:	Track memory when need to.
 *
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

#ifdef NCLDEBUG
void _initializeNclMemoryRecord()
{
    ncl_memory_record.totalMemoryAllocated = 0;
    ncl_memory_record.totalMemoryFreed = 0;

    ncl_memory_record.num_allocated = 0;
    ncl_memory_record.max_allocated = NCL_MAX_MEMORY_RECORD;

    ncl_memory_record.num_freed = 0;

    ncl_memory_record.record = (NclMemoryStruct *)calloc(NCL_MAX_MEMORY_RECORD, sizeof(NclMemoryStruct));
}

void _finalizeNclMemoryRecord()
{
    fprintf(stderr, "\nNCL allocated <%ld> pieces of memory in total of <%ld> bytes.\n",
                       ncl_memory_record.num_allocated, ncl_memory_record.totalMemoryAllocated);
    fprintf(stderr, "NCL freed <%ld> pieces of memory in total of <%ld> bytes.\n",
                     ncl_memory_record.num_freed, ncl_memory_record.totalMemoryFreed);

    if(ncl_memory_record.num_freed == ncl_memory_record.num_allocated)
    {
        fprintf(stderr, "\nNCL has freed all memory it allocated.\n");
    }
    else
    {
        fprintf(stderr, "\nNCL has left <%ld> pieces in total of <%ld> bytes unfreed memory.\n",
                         ncl_memory_record.num_allocated - ncl_memory_record.num_freed,
                         ncl_memory_record.totalMemoryAllocated - ncl_memory_record.totalMemoryFreed);
    }

    free(ncl_memory_record.record);
}

void _addNclMemoryRecord(int linenumb, const char *filename, const char *funcname,
                         ng_usize_t size, size_t memloc)
{
    if(ncl_memory_record.num_allocated >= ncl_memory_record.max_allocated)
    {
        ncl_memory_record.max_allocated *= 2;
        ncl_memory_record.record = (NclMemoryStruct *)realloc(ncl_memory_record.record,
                                    (ncl_memory_record.max_allocated * sizeof(NclMemoryStruct)));
    }

    fprintf(stderr, "\tMem loc %ld: NCL allocate <%ld> bytes of memory in function <%s> at line <%d> of file <%s>\n",
                     ncl_memory_record.num_allocated, size, funcname, linenumb, filename);
                     
    ncl_memory_record.totalMemoryAllocated += size;
    ncl_memory_record.record[ncl_memory_record.num_allocated].status = 1;
    ncl_memory_record.record[ncl_memory_record.num_allocated].size = size;
    ncl_memory_record.record[ncl_memory_record.num_allocated].memloc = memloc;
    ncl_memory_record.record[ncl_memory_record.num_allocated].linenumb = linenumb;
    if(NCL_MAX_NAME_LENGTH <= strlen(filename))
        strncpy(ncl_memory_record.record[ncl_memory_record.num_allocated].filename, filename, NCL_MAX_NAME_LENGTH);
    else
        strcpy(ncl_memory_record.record[ncl_memory_record.num_allocated].filename, filename);

    if(NCL_MAX_NAME_LENGTH <= strlen(funcname))
        strncpy(ncl_memory_record.record[ncl_memory_record.num_allocated].funcname, funcname, NCL_MAX_NAME_LENGTH);
    else
        strcpy(ncl_memory_record.record[ncl_memory_record.num_allocated].funcname, funcname);

    ++ncl_memory_record.num_allocated;
}

void _removeNclMemoryRecord(int linenumb, const char *filename, const char *funcname, size_t memloc)
{
    size_t n;

    for(n = 0; n < ncl_memory_record.num_allocated; ++n)
    {
        if(memloc == ncl_memory_record.record[n].memloc)
        {
            fprintf(stderr, "\tMem loc %ld: NCL freed <%ld> bytes of memory in function <%s> at line <%d> of file <%s>\n",
                               n,  ncl_memory_record.record[n].size, funcname, linenumb, filename);

            ncl_memory_record.totalMemoryFreed += ncl_memory_record.record[n].size;
            ncl_memory_record.record[n].status = 0;

            fprintf(stderr, "\t\tThis memory is allocated in function <%s> at line <%d> of file <%s>\n",
                             funcname, linenumb, filename);
            break;
        }
    }
}

void *_underNclMalloc(int linenumb, const char *filename, const char *funcname,
                      ng_usize_t size)
{
        void *ptr;

        if(0 == size)
                return NULL;

        ptr = (void *)malloc(size);

        if(NULL == ptr)
	{
		fprintf(stderr, "\nFailed to malloc <%ld> bytes of memory in function <%s> at line <%d> of file <%s>\n",
				size, funcname, linenumb, filename);
                NhlPError(NhlFATAL,errno,"NclMalloc Failed");
	}

	if(NCLdebug_on)
		_addNclMemoryRecord(linenumb, filename, funcname, size, (size_t) ptr);

        return(ptr);
}

void _underNclFree(int linenumb, const char *filename, const char *funcname,
                   void *ptr)
{
        if(NULL != ptr)
	{
		if(NCLdebug_on)
			_removeNclMemoryRecord(linenumb, filename, funcname, (size_t) ptr);
                free(ptr);
                ptr = NULL;
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
void *_underNclCalloc(int linenumb, const char *filename, const char *funcname,
	              ng_usize_t num,	/* number of elements		*/
	              ng_usize_t size	/* size of each element		*/
                     )
{
	void *ptr;

	if(0 == (num * size))
		return NULL;

	ptr = (void *)calloc(num, size);

	if(NULL == ptr)
	{
		fprintf(stderr, "\nFailed to calloc <%ld> bytes of memory in function <%s> at line <%d> of file <%s>\n",
				num * size, funcname, linenumb, filename);
		NhlPError(NhlFATAL,errno,"NhlCalloc Failed");
	}

	if(NCLdebug_on)
		_addNclMemoryRecord(linenumb, filename, funcname, num * size, (size_t) ptr);

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
void *_underNclRealloc(int linenumb, const char *filename, const char *funcname,
	               void		*ptr,	/* pointer to old memory	*/
	               ng_usize_t	size	/* size of memory requested	*/
                      )
{
	if(NULL == ptr)
		ptr = _underNclMalloc(linenumb, filename, funcname, size);
	else
	{
		if(NCLdebug_on)
			_removeNclMemoryRecord(linenumb, filename, funcname, (size_t) ptr);

		ptr = (void *)realloc(ptr,size);

		if(NULL == ptr)
		{
			fprintf(stderr, "\nFailed to realloc <%ld> bytes of memory in function <%s> at line <%d> of file <%s>\n",
					size, funcname, linenumb, filename);
			NhlPError(NhlFATAL,errno,"NhlRealloc Failed");
		}

	}

	if(NCLdebug_on)
		_addNclMemoryRecord(linenumb, filename, funcname, size, (size_t) ptr);

	return(ptr);
}
#else
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
#endif

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

