/*
 *	$Id: s_gssgt.c,v 1.1 1997-03-05 19:13:28 haley Exp $
 */
/*
 *  Set Segment Transformation
 */

#include <ncarg/gks.h>

void gset_seg_tran
#ifdef NeedFuncProto
(
    Gint seg_name,                 /* segment name          */
    const Gtran_matrix tran_matrix /* transformation matrix */
)
#else
( seg_name, tran_matrix )
    Gint seg_name;
    Gtran_matrix tran_matrix;
#endif
{
    Gfloat tm2[3][2];
    int i;
/*
 * Transpose 2x3 array
 */
    for( i = 0; i < 3; i++ ) {
        tm2[i][0] = tran_matrix[0][i];
        tm2[i][1] = tran_matrix[1][i];
	}
    NGCALLF(gssgt,GSSGT)(&seg_name,tm2);
}
