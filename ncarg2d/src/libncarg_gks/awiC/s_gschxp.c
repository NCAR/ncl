/*
 *	$Id: s_gschxp.c,v 1.1 1997-03-05 19:13:21 haley Exp $
 */
/*
 *  Set character expansion factor  
 */

#include <ncarg/gks.h>

void gset_char_expan
#ifdef NeedFuncProto
(
    Gdouble char_expan  /* character expansion factor */
)
#else
( char_expan )
    Gdouble char_expan;
#endif
{
    Gfloat expan;
    expan = (Gfloat)char_expan;
    NGCALLF(gschxp,GSCHXP)(&expan);
}
