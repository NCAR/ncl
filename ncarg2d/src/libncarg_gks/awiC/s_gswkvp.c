/*
 *  $Id: s_gswkvp.c,v 1.1 1997-03-05 19:13:32 haley Exp $
 */
/*
 *  Set workstation viewport  
 */

#include <ncarg/gks.h>

void gset_ws_vp
#ifdef NeedFuncProto
(
    Gint   ws_id,          /* workstation identifier      */
    Glimit *ws_vp_limits   /* workstation viewport limits */
)
#else
( ws_id, ws_vp_limits )
    Gint   ws_id;
    Glimit *ws_vp_limits;
#endif
{
    NGCALLF(gswkvp,GSWKVP)(&ws_id,&ws_vp_limits->x_min,&ws_vp_limits->x_max,
                                  &ws_vp_limits->y_min,&ws_vp_limits->y_max);
}
