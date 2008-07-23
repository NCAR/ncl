/*
 *	$Id: s_gqtxi.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire text index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxi,GQTXI)(Gint*,Gint*);

void ginq_text_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,  /*  OUT error indicator     */
    Gint *text_ind  /*  OUT current text index  */
)
#else
( err_ind, text_ind )
    Gint *err_ind;
    Gint *text_ind;
#endif
{
    NGCALLF(gqtxi,GQTXI)(err_ind,text_ind);
}
