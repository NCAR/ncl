/*
 *      $Id: sort.c,v 1.1 1997-06-04 18:08:32 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		sort.c
 *
 *	Authors:	David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 11:15:34 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/sort.h>
#include <stdlib.h>

static int quarkcomp
(
 	const void *p1,
	const void *p2
)
{
	const NrmQuark qvar1 = *(NrmQuark *) p1;
	const NrmQuark qvar2 = *(NrmQuark *) p2;
	int ret;

	ret =  strcmp(NrmQuarkToString(qvar1),NrmQuarkToString(qvar2));
	return ret;
}

static int asciicomp
(
 	const void *p1,
	const void *p2
)
{
	const NgOrderData vo1 = *(NgOrderData *) p1;
	const NgOrderData vo2 = *(NgOrderData *) p2;
	int ret;

	ret =  strcmp(vo1.name,vo2.name);
	return ret;
}

static int asciicomp_rev
(
 	const void *p1,
	const void *p2
)
{
	const NgOrderData vo1 = *(NgOrderData *) p1;
	const NgOrderData vo2 = *(NgOrderData *) p2;
	int ret;

	ret =  - strcmp(vo1.name,vo2.name);
	return ret;
}

static int dimcomp
(
 	const void *p1,
	const void *p2
)
{
	const NgOrderData vo1 = *(NgOrderData *) p1;
	const NgOrderData vo2 = *(NgOrderData *) p2;
	int ret;

        if (vo1.n_dims < vo2.n_dims)
                return 1;
        if (vo1.n_dims > vo2.n_dims)
                return -1;
	ret = strcmp(vo1.name,vo2.name);
	return ret;
}

static int dimcomp_rev
(
 	const void *p1,
	const void *p2
)
{
	const NgOrderData vo1 = *(NgOrderData *) p1;
	const NgOrderData vo2 = *(NgOrderData *) p2;
	int ret;

        if (vo1.n_dims > vo2.n_dims)
                return 1;
        if (vo1.n_dims < vo2.n_dims)
                return -1;
	ret = -strcmp(vo1.name,vo2.name);
	return ret;
}

static int sizecomp
(
 	const void *p1,
	const void *p2
)
{
	const NgOrderData vo1 = *(NgOrderData *) p1;
	const NgOrderData vo2 = *(NgOrderData *) p2;
	int ret;

        if (vo1.size < vo2.size)
                return 1;
        if (vo1.size > vo2.size)
                return -1;
	ret = strcmp(vo1.name,vo2.name);
	return ret;
}

static int sizecomp_rev
(
 	const void *p1,
	const void *p2
)
{
	const NgOrderData vo1 = *(NgOrderData *) p1;
	const NgOrderData vo2 = *(NgOrderData *) p2;
	int ret;

        if (vo1.size > vo2.size)
                return 1;
        if (vo1.size < vo2.size)
                return -1;
	ret = - strcmp(vo1.name,vo2.name);
	return ret;
}

extern void NgSortQuarkList
(
        NrmQuark	*qlist,
        int		count,
        NhlBoolean	reverse
        )
{
        qsort(qlist,count,sizeof(NrmQuark),quarkcomp);
	return;
}
static void Reverse
(
        NgOrderData	*odlist,
        int		count
        )
{
        NgOrderData tmp_odata;
        int i;
        
        for (i = 0 ; i < count/2; i++) {
                memcpy(&tmp_odata,odlist+i,sizeof(NgOrderData));
                memcpy(odlist+i,odlist+count-i-1,sizeof(NgOrderData));
                memcpy(odlist+count-i-1,&tmp_odata,sizeof(NgOrderData));
        }
        return;
}

extern void NgSortOrderDataList
(
        NgOrderData	*odlist,
        int		count,
        NgSortMode	mode,
        NhlBoolean	reverse
        )
{
        int (*func)(const void *, const void *);

        switch (mode) {
            case NgASCII_SORT:
                    func = reverse ? asciicomp_rev : asciicomp;
                    break;
            case NgDIM_SORT:
                    func = reverse ? dimcomp_rev : dimcomp;
                    break;
            case NgSIZE_SORT:
                    func = reverse ? sizecomp_rev : sizecomp;
                    break;
            case NgNO_SORT:
                    if (reverse)
                            Reverse(odlist,count);
                    return;
        }
        qsort(odlist,count,sizeof(NgOrderData),func);
        
        return;
}
