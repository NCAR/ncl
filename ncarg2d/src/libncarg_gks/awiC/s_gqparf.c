/*
 *  $Id: s_gqparf.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire pattern reference point  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqparf,GQPARF)(Gint*,Gfloat*,Gfloat*);

void ginq_pat_ref_point
#ifdef NeedFuncProto
(
    Gint   *err_ind,      /* OUT error indicator                 */
    Gpoint *pat_ref_point /* OUT current pattern reference point */
)
#else
( err_ind, pat_ref_point )
    Gint   *err_ind;
    Gpoint *pat_ref_point;
#endif
{
    NGCALLF(gqparf,GQPARF)(err_ind,&pat_ref_point->x,&pat_ref_point->y);
}

