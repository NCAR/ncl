/*
 *	$Id: s_gstxp.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Set text path  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gstxp,GSTXP)(Gtext_path*);

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
