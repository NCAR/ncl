/*
 *	$Id: s_gsln.c,v 1.1 1997-03-05 19:13:24 haley Exp $
 */
/*
 *  Set linetype  
 */

#include <ncarg/gks.h>

void gset_linetype
#ifdef NeedFuncProto
(
    Gint linetype  /* linetype */
)
#else
( linetype )
    Gint linetype;
#endif
{
    NGCALLF(gsln,GSLN)(&linetype);
}
