/*
 *	$Id: s_gsfaci.c,v 1.1 1997-03-05 19:13:22 haley Exp $
 */
/*
 *  Set fill area colour index  
 */

#include <ncarg/gks.h>

void gset_fill_colr_ind
#ifdef NeedFuncProto
(
    Gint fill_colr_ind  /* fill area colour index  */
)
#else
( fill_colr_ind )
    Gint fill_colr_ind;
#endif
{
    NGCALLF(gsfaci,GSFACI)(&fill_colr_ind);
}
