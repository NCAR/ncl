/*
 *	$Id: s_gqtxp.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 * Inquire text path  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxp,GQTXP)(Gint*,Gtext_path*);

void ginq_text_path
#ifdef NeedFuncProto
(
    Gint       *err_ind,    /* OUT error indicator   */
    Gtext_path *text_path   /* OUT current text path */
)
#else
( err_ind, text_path )
    Gint       *err_ind;
    Gtext_path *text_path;
#endif
{
    NGCALLF(gqtxp,GQTXP)(err_ind,text_path);
}
