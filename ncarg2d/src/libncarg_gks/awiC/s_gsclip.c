/*
 *	$Id: s_gsclip.c,v 1.1 1997-03-05 19:13:21 haley Exp $
 */
/*
 *  Set clipping indicator 
 */

#include <ncarg/gks.h>

void gset_clip_ind
#ifdef NeedFuncProto
(
    Gclip_ind clip_ind  /* clipping indicator */
)
#else
( clip_ind )
    Gclip_ind clip_ind;
#endif
{
    NGCALLF(gsclip,GSCLIP)(&clip_ind);
}
