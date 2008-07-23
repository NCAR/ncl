/*
 *	$Id: s_gschup.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Set character up vector  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gschup,GSCHUP)(const Gfloat*,const Gfloat*);

void gset_char_up_vec
#ifdef NeedFuncProto
(
    const Gvec *char_up_vec  /* character up vector */
)
#else
( char_up_vec )
    Gvec *char_up_vec;
#endif
{
    NGCALLF(gschup,GSCHUP)(&char_up_vec->delta_x,&char_up_vec->delta_y);
}
