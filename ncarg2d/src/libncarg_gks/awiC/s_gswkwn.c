/*
 *  $Id: s_gswkwn.c,v 1.1 1997-03-05 19:13:32 haley Exp $
 */
/*
 *  Set workstation window  
 */

#include <ncarg/gks.h>

void gset_ws_win
#ifdef NeedFuncProto
(
    Gint   ws_id,           /* workstation identifier    */
    Glimit *ws_win_limits   /* workstation window limits */
)
#else
( ws_id, ws_win_limits )
    Gint   ws_id;
    Glimit *ws_win_limits;
#endif
{
    NGCALLF(gswkwn,GSWKWN)(&ws_id,&ws_win_limits->x_min,&ws_win_limits->x_max,
                                  &ws_win_limits->y_min,&ws_win_limits->y_max);
}
