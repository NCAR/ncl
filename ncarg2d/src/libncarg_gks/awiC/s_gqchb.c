/*
 *	$Id: s_gqchb.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire character base vector
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqchb,GQCHB)(Gint*,Gfloat*,Gfloat*);

void ginq_char_base_vec
#ifdef NeedFuncProto
(
    Gint *err_ind,       /* OUT error indicator                */
    Gvec *char_base_vec  /* OUT current character base vector  */
)
#else
( err_ind, char_base_vec )
    Gint *err_ind;
    Gvec *char_base_vec;
#endif
{
    NGCALLF(gqchb,GQCHB)(err_ind,
                         &char_base_vec->delta_x,
                         &char_base_vec->delta_y );
}
