/*
 *	$Id: s_gspmi.c,v 1.1 1997-03-05 19:13:28 haley Exp $
 */
/*
 *  Set polymarker index  
 */

#include <ncarg/gks.h>

void gset_marker_ind
#ifdef NeedFuncProto
(
    Gint marker_ind  /*  polymarker index  */
)
#else
( marker_ind )
    Gint marker_ind;
#endif
{
    NGCALLF(gspmi,GSPMI)(&marker_ind);
}
