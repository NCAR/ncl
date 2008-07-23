/*
 *	$Id: s_gqclip.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire clipping indicator  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqclip,GQCLIP)(Gint*,Gclip_ind*,Gfloat clrect[4]);

void ginq_clip
#ifdef NeedFuncProto
(
    Gint  *err_ind,        /* OUT error indicator                         */
    Gclip *clip_ind_rect   /* OUT current clippig indicator and rectangle */
)
#else
( err_ind, clip_ind_rect )
    Gint  *err_ind;
    Gclip *clip_ind_rect;
#endif
{
    Gfloat clrect[4];

    NGCALLF(gqclip,GQCLIP)(err_ind,&clip_ind_rect->clip_ind,clrect);

    clip_ind_rect->clip_rect.x_min = (Gfloat) clrect[0];
    clip_ind_rect->clip_rect.x_max = (Gfloat) clrect[1];
    clip_ind_rect->clip_rect.y_min = (Gfloat) clrect[2];
    clip_ind_rect->clip_rect.y_max = (Gfloat) clrect[3];
}
