/*
 *	$Id: s_gschup.c,v 1.1 1997-03-05 19:13:20 haley Exp $
 */
/*
 *  Set character up vector  
 */

#include <ncarg/gks.h>

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
