/*
 *	$Id: s_gqchup.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire character up vector  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqchup,GQCHUP)(Gint*,Gfloat*,Gfloat*);

void ginq_char_up_vec
#ifdef NeedFuncProto
(
    Gint *err_ind,      /* OUT error indicator             */
    Gvec *char_up_vec   /* OUT current character up vector */
)
#else
( err_ind, char_up_vec )
    Gint *err_ind;
    Gvec *char_up_vec;
#endif
{
    NGCALLF(gqchup,GQCHUP)(err_ind,
                           &char_up_vec->delta_x,
                           &char_up_vec->delta_y);
}
