/*
 *	$Id: s_gschsp.c,v 1.1 1997-03-05 19:13:20 haley Exp $
 */
/*
 *  Set character spacing  
 */

#include <ncarg/gks.h>

void gset_char_space
#ifdef NeedFuncProto
(
    Gdouble char_space  /* character spacing */
)
#else
(char_space)
    Gdouble char_space;
#endif
{
    Gfloat space;
    space = (Gfloat)char_space;
    NGCALLF(gschsp,GSCHSP)(&space);
}
