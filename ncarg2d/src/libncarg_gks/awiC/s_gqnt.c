/*
 *	$Id: s_gqnt.c,v 1.6 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire normalization transformation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqnt,GQNT)(Gint*,Gint*,Gfloat*,Gfloat*);

void ginq_norm_tran
#ifdef NeedFuncProto
(
    Gint  num,       /* normalization transformation number */   
    Gint  *err_ind,  /* OUT error indicator                 */
    Gtran *norm_tran /* OUT normalization tranformation     */
)
#else
( num, err_ind, norm_tran )
    Gint  num;
    Gint  *err_ind;
    Gtran *norm_tran;
#endif
{
    Gfloat win[4], vp[4];
    NGCALLF(gqnt,GQNT)(&num,err_ind, win, vp);
    norm_tran->win.x_min = (Gfloat)win[0];
    norm_tran->win.x_max = (Gfloat)win[1];
    norm_tran->win.y_min = (Gfloat)win[2];
    norm_tran->win.y_max = (Gfloat)win[3];
    norm_tran->vp.x_min = (Gfloat)vp[0];
    norm_tran->vp.x_max = (Gfloat)vp[1];
    norm_tran->vp.y_min = (Gfloat)vp[2];
    norm_tran->vp.y_max = (Gfloat)vp[3];
    return;
}
