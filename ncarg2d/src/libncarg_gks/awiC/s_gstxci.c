/*
 *	$Id: s_gstxci.c,v 1.1 1997-03-05 19:13:30 haley Exp $
 */
/*
 *  Set text colour index  
 */

#include <ncarg/gks.h>

void gset_text_colr_ind
#ifdef NeedFuncProto
(
    Gint text_colr_ind  /* text colour index */
)
#else
( text_colr_ind )
    Gint text_colr_ind;
#endif
{
    NGCALLF(gstxci,GSTXCI)(&text_colr_ind);
}
