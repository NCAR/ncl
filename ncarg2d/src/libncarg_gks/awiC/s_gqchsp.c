/*
 *	$Id: s_gqchsp.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire character spacing  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqchsp,GQCHSP)(Gint*,Gfloat*);

void ginq_char_space
#ifdef NeedFuncProto
(
    Gint    *err_ind,     /* OUT error indicator           */
    Gdouble *char_space   /* OUT current character spacing */
)
#else
(err_ind,char_space)
    Gint    *err_ind;
    Gdouble *char_space;
#endif
{
    Gfloat space;
    NGCALLF(gqchsp,GQCHSP)(err_ind,&space);
    *char_space = (Gdouble) space;
}
