/*
 *	$Id: s_gca.c,v 1.7 2008-07-23 17:24:19 haley Exp $
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

/*
 *  Cell array  
 */
#include <stdlib.h>

#include <ncarg/gks.h>

extern void NGCALLF(gca,GCA)(const Gfloat*,const Gfloat*,const Gfloat*,
                             const Gfloat*,const Gint*,const Gint*,int*,
                             int*,int*,int*,int*);

void gcell_array
#ifdef NeedFuncProto
(
    const Grect    *rect,        /* cell rectangle */
    const Gpat_rep *colr_array   /* colour array   */
)
#else
( rect, colr_array )
    Grect    *rect;
    Gpat_rep *colr_array;
#endif
{
    int i, j, idx, idy;

    if( colr_array->colr_array != NULL ) {
        idx = colr_array->dims.size_x;
        idy = colr_array->dims.size_y;
        i = j = 1;
        NGCALLF(gca,GCA)(&rect->p.x,&rect->p.y,
                         &rect->q.x,&rect->q.y,
                         &colr_array->dims.size_x,
                         &colr_array->dims.size_y,&i,&j,
                         &idx,&idy,colr_array->colr_array);
    }
    else {
        fprintf( stderr, "gcell_array:  colr_array is a NULL structure\n" );
        return;
    }
}
