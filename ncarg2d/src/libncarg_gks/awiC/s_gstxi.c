/*
 *	$Id: s_gstxi.c,v 1.1 1997-03-05 19:13:31 haley Exp $
 */
/*
 *  Set text index  
 */

#include <ncarg/gks.h>

void gset_text_ind
#ifdef NeedFuncProto
(
    Gint text_ind  /*  text index  */
)
#else
( text_ind )
    Gint text_ind;
#endif
{
    NGCALLF(gstxi,GSTXI)(&text_ind);
}
