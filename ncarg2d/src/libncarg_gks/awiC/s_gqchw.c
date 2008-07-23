/*
 *	$Id: s_gqchw.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire character width
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqchw,GQCHW)(Gint*, Gfloat*);

void ginq_char_width
#ifdef NeedFuncProto
(
    Gint    *err_ind,    /* OUT error indicator          */
    Gdouble *char_width  /* OUT current character width  */
)
#else
( err_ind, char_width )
    Gint    *err_ind;
    Gdouble *char_width;
#endif
{
    Gfloat chw;
    NGCALLF(gqchw,GQCHW)(err_ind, &chw);
    *char_width = (Gdouble) chw;
}
