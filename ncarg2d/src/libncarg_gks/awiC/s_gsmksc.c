/*
 *	$Id: s_gsmksc.c,v 1.1 1997-03-05 19:13:25 haley Exp $
 */
/*
 *  Set marker size scale factor  
 */

#include <ncarg/gks.h>

void gset_marker_size
#ifdef NeedFuncProto
(
    Gdouble marker_size  /* marker size scale factor */
)
#else
( marker_size )
    Gdouble marker_size;
#endif
{
    Gfloat size;
    size = (Gfloat)marker_size;
    NGCALLF(gsmksc,GSMKSC)(&size);
}
