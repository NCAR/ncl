/*
 *	$Id: s_gsfai.c,v 1.1 1997-03-05 19:13:23 haley Exp $
 */
/*
 *  Set fill area index  
 */

#include <ncarg/gks.h>

void gset_fill_ind
#ifdef NeedFuncProto
(
    Gint fill_ind  /*  fill area index  */
)
#else
( fill_ind )
    Gint fill_ind;
#endif
{
    NGCALLF(gsfai,GSFAI)(&fill_ind);
}
