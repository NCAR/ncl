/*
 *	$Id: s_gspli.c,v 1.1 1997-03-05 19:13:27 haley Exp $
 */
/*
 *  Set polyline index  
 */

#include <ncarg/gks.h>

void gset_line_ind
#ifdef NeedFuncProto
(
    Gint line_ind  /*  polyline index  */
)
#else
( line_ind )
    Gint line_ind;
#endif
{
    NGCALLF(gspli,GSPLI)(&line_ind);
}
