/*
 *	$Id: s_gqchh.c,v 1.1 1997-03-05 19:12:54 haley Exp $
 */
/*  
 *Inquire character height  
 */

#include <ncarg/gks.h>

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
