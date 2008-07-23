/*
 *	$Id: s_gqtxci.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire text colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxci,GQTXCI)(Gint*,Gint*);

void ginq_text_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator     */
    Gint *text_colr_ind   /* OUT text colour index   */
)
#else
( err_ind, text_colr_ind )
    Gint *err_ind;
    Gint *text_colr_ind;
#endif
{
    NGCALLF(gqtxci,GQTXCI)(err_ind,text_colr_ind);
}
