/*
 *	$Id: s_gqclip.c,v 1.1 1997-03-05 19:12:56 haley Exp $
 */
/*
 *  Inquire clipping indicator  
 */

#include <ncarg/gks.h>

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
