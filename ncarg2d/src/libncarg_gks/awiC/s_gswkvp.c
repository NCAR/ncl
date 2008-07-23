/*
 *  $Id: s_gswkvp.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Set workstation viewport  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gswkvp,GSWKVP)(Gint*,const Gfloat*,const Gfloat*,
                                         const Gfloat*,const Gfloat*);

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
