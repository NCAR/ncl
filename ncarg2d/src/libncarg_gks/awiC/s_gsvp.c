/*
 *	$Id: s_gsvp.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Set viewport
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsvp,GSVP)(Gint*,const Gfloat*,const Gfloat*,
                                     const Gfloat*,const Gfloat*);

void gset_vp
#ifdef NeedFuncProto
(
    Gint            tran_num,     /* transformation number */
    const Glimit    *vp_limits    /* viewport limits       */
)
#else
( tran_num, vp_limits )
    Gint      tran_num;
    Glimit    *vp_limits;
#endif
{
    NGCALLF(gsvp,GSVP)(&tran_num,&vp_limits->x_min,&vp_limits->x_max,
                                 &vp_limits->y_min,&vp_limits->y_max);
}
