/*
 *	$Id: s_gstxp.c,v 1.1 1997-03-05 19:13:31 haley Exp $
 */
/*
 *  Set text path  
 */

#include <ncarg/gks.h>

void gset_text_path
#ifdef NeedFuncProto
(
    Gtext_path text_path  /* text path */
)
#else
( text_path )
    Gtext_path text_path;
#endif
{
    NGCALLF(gstxp,GSTXP)(&text_path);
}
