/*
 *	$Id: s_gca.c,v 1.1 1997-03-05 19:12:45 haley Exp $
 */
/*
 *  Cell array  
 */

#include <ncarg/gks.h>

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
    int i, j, k, l, idx, idy, *colia_trans;

    if( colr_array->colr_array != NULL ) {
        idx = colr_array->dims.size_x;
        idy = colr_array->dims.size_y;
        colia_trans = (int *)malloc(idx*idy*sizeof(int));
        if( colia_trans ) {
            l = 0;
            for( j = 0; j < idy; j++ ) {
                for( i = 0; i < idx; i++ ) {
                    k = i * idy + j;
                    colia_trans[l++] = colr_array->colr_array[k];
                }
            }
            i = j = 1;
            NGCALLF(gca,GCA)(&rect->p.x,&rect->p.y,
                             &rect->q.x,&rect->q.y,
                             &colr_array->dims.size_x,
                             &colr_array->dims.size_y,&i,&j,
                             &idx,&idy,colia_trans);
        }
        else {
            fprintf( stderr, "gcell_array: not enough memory to create transposed array\n" );
            return;
        }
    }
    else {
        fprintf( stderr, "gcell_array:  colr_array is a NULL structure\n" );
        return;
    }
}
