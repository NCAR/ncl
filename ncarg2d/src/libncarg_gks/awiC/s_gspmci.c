/*
 *	$Id: s_gspmci.c,v 1.1 1997-03-05 19:13:28 haley Exp $
 */
/*
 *  Set polymarker colour index  
 */

#include <ncarg/gks.h>

void gset_marker_colr_ind
#ifdef NeedFuncProto
(
    Gint marker_colr_ind  /* polymarker colour index */
)
#else
( marker_colr_ind )
    Gint marker_colr_ind;
#endif
{
    NGCALLF(gspmci,GSPMCI)(&marker_colr_ind);
}
