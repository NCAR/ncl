/*
 *	$Id: s_gsasf.c,v 1.1 1997-03-05 19:13:19 haley Exp $
 */
/*
 * Set aspect source flags 
 */

#include <ncarg/gks.h>

void gset_asfs
#ifdef NeedFuncProto
(
    const Gasfs *list_asf  /* list of aspect source flags */
)
#else
( list_asf )
    Gasfs *list_asf;
#endif
{
    NGCALLF(gsasf,GSASF)(list_asf);
}
