/*
 *	$Id: s_gsfasi.c,v 1.1 1997-03-05 19:13:24 haley Exp $
 */
/*
 *  Set fill area style index  
 */

#include <ncarg/gks.h>

void gset_fill_style_ind
#ifdef NeedFuncProto
(
    Gint fill_style_ind  /* fill area style index */
)
#else
( fill_style_ind )
    Gint fill_style_ind;
#endif
{
    NGCALLF(gsfasi,GSFASI)(&fill_style_ind);
}
