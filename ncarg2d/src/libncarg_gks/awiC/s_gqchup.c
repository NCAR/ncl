/*
 *	$Id: s_gqchup.c,v 1.1 1997-03-05 19:12:55 haley Exp $
 */
/*
 *  Inquire character up vector  
 */

#include <ncarg/gks.h>

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
