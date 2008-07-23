/*
 *	$Id: s_gstxal.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set text alignment  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gstxal,GSTXAL)(const Ghor_text_align*,
                                   const Gvert_text_align*);

void gset_text_align
#ifdef NeedFuncProto
(
    const Gtext_align *text_align  /* text alignment */
)
#else
( text_align )
    Gtext_align *text_align;
#endif
{
    NGCALLF(gstxal,GSTXAL)(&text_align->hor,&text_align->vert);
}
