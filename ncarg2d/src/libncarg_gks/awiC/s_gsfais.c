/*
 *	$Id: s_gsfais.c,v 1.1 1997-03-05 19:13:23 haley Exp $
 */
/*
 *  Set fill area interior style  
 */

#include <ncarg/gks.h>

void gset_fill_int_style
#ifdef NeedFuncProto
(
    Gfill_int_style fill_int_style  /* fill area style index */
)
#else
( fill_int_style )
    Gfill_int_style fill_int_style;
#endif
{
    NGCALLF(gsfais,GSFAIS)(&fill_int_style);
}
