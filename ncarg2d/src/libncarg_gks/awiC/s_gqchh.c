/*
 *	$Id: s_gqchh.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *Inquire character height  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqchh,GQCHH)(Gint*,Gfloat*);

void ginq_char_ht
#ifdef NeedFuncProto
(
    Gint    *err_ind,  /* OUT error indicator          */
    Gdouble *char_ht   /* OUT current character height */
)
#else
( err_ind, char_ht )
    Gint    *err_ind;
    Gdouble *char_ht;
#endif
{
    Gfloat ht;
    NGCALLF(gqchh,GQCHH)(err_ind,&ht);
    *char_ht = (Gdouble) ht;
}
