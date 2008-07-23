/*
 *	$Id: s_gsclip.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set clipping indicator 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsclip,GSCLIP)(Gclip_ind*);

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
