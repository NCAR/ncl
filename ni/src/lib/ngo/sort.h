/*
 *      $Id: sort.h,v 1.1 1997-06-04 18:08:33 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		sort.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 11:15:34 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_SORT_H
#define	_NG_SORT_H

#include <ncarg/ngo/go.h>
#include <ncarg/hlu/NresDB.h>
/*
 * Public api
 */

typedef enum _NgSortMode 
{
        NgASCII_SORT,NgDIM_SORT,NgSIZE_SORT,NgNO_SORT
} NgSortMode;

/*
 * standard order is greater to lesser dims, greater to lesser size,
 * and lesser to greater ascii values; reverse changes each of these.
 */

typedef struct _NgOrderData 
{
        char	*name;
        int	n_dims;
        int	size;
} NgOrderData;

extern void NgSortQuarkList
(
        NrmQuark	*qlist,
        int		count,
        NhlBoolean	reverse
        );

extern void NgSortOrderDataList
(
        NgOrderData	*odlist,
        int		count,
        NgSortMode	mode,
        NhlBoolean	reverse
        );

#endif	/* _NG_SORT_H */
