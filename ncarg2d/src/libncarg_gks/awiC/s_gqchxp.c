/*
 *	$Id: s_gqchxp.c,v 1.1 1997-03-05 19:12:55 haley Exp $
 */
/* 
 * Inquire character expansion factor  
 */

#include <ncarg/gks.h>

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
