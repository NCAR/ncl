/*
 *	$Id: s_gqlvks.c,v 1.1 1997-03-05 19:13:01 haley Exp $
 */
/*
 *  Inquire level of gks 
 */

#include <ncarg/gks.h>

void ginq_level_gks
#ifdef NeedFuncProto
(
    Gint *err_ind,  /* OUT error indicator */
    Glevel *level   /* OUT level of GKS    */
)
#else
( err_ind, level )
    Gint *err_ind;
    Glevel *level;
#endif
{
    NGCALLF(gqlvks,GQLVKS)(err_ind,level);
}
