/*
 *	$Id: s_gsmk.c,v 1.1 1997-03-05 19:13:25 haley Exp $
 */
/*
 *  Set marker type  
 */

#include <ncarg/gks.h>

void gset_marker_type
#ifdef NeedFuncProto
(
    Gint marker_type  /* marker type  */
)
#else
( marker_type )
    Gint marker_type;
#endif
{
    NGCALLF(gsmk,GSMK)(&marker_type);
}
