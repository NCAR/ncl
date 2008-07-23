/*
 *	$Id: s_gssgt.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set Segment Transformation
 */

#include <ncarg/gks.h>

extern void NGCALLF(gssgt,GSSGT)(Gint*_name,Gfloat tm2[3][2]);

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
