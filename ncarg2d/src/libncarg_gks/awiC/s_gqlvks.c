/*
 *	$Id: s_gqlvks.c,v 1.5 2008-07-23 17:24:21 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *  Inquire level of gks 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqlvks,GQLVKS)(Gint*,Glevel*);

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
