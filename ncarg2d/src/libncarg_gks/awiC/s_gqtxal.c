/*
 *	$Id: s_gqtxal.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire text alignment  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxal,GQTXAL)(Gint*,Ghor_text_align*,Gvert_text_align*);

void ginq_text_align
#ifdef NeedFuncProto
(
    Gint        *err_ind,     /* OUT error indicator          */
    Gtext_align *text_align   /* OUT current text alignment   */
)
#else
( err_ind, text_align )
    Gint        *err_ind;
    Gtext_align *text_align;
#endif
{
    NGCALLF(gqtxal,GQTXAL)(err_ind,&text_align->hor,&text_align->vert);
}
