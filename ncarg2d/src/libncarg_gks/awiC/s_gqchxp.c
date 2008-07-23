/*
 *	$Id: s_gqchxp.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 * Inquire character expansion factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqchxp,GQCHXP)(Gint*,Gfloat*);

void ginq_char_expan
#ifdef NeedFuncProto
(
    Gint    *err_ind,    /* OUT error indicator                    */
    Gdouble *char_expan  /* OUT current character expansion factor */
)
#else
( err_ind, char_expan )
    Gint    *err_ind;
    Gdouble *char_expan;
#endif
{
    Gfloat expan;
    NGCALLF(gqchxp,GQCHXP)(err_ind,&expan);
    *char_expan = (Gdouble) expan;
}
