/*
 *	$Id: s_gschh.c,v 1.1 1997-03-05 19:13:19 haley Exp $
 */
/*
 *  Set character height  
 */

#include <ncarg/gks.h>

void gset_char_ht
#ifdef NeedFuncProto
(
    Gdouble char_ht  /* character height */
)
#else
( char_ht )
    Gdouble char_ht;
#endif
{
    Gfloat ht;
    ht = (Gfloat) char_ht;
    NGCALLF(gschh,GSCHH)(&ht);
}
