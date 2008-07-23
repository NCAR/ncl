/*
 *  $Id: s_gswkwn.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Set workstation window  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gswkwn,GSWKWN)(Gint*,const Gfloat*,const Gfloat*,
                                         const Gfloat*,const Gfloat*);

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
