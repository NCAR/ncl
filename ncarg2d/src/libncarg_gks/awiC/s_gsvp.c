/*
 *	$Id: s_gsvp.c,v 1.1 1997-03-05 19:13:32 haley Exp $
 */
/*
 *  Set viewport
 */

#include <ncarg/gks.h>

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
