/*
 *	$Id: s_gsplci.c,v 1.1 1997-03-05 19:13:27 haley Exp $
 */
/*
 *  Set polyline colour index  
 */

#include <ncarg/gks.h>

void gset_line_colr_ind
#ifdef NeedFuncProto
(
    Gint line_colr_ind  /* polyline colour index */
)
#else
( line_colr_ind )
    Gint line_colr_ind;
#endif
{
    NGCALLF(gsplci,GSPLCI)(&line_colr_ind);
}
