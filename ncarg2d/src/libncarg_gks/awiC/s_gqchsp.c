/*
 *	$Id: s_gqchsp.c,v 1.1 1997-03-05 19:12:54 haley Exp $
 */
/*
 *  Inquire character spacing  
 */

#include <ncarg/gks.h>

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
