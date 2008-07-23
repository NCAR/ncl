/*
 *	$Id: s_gqln.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 * Inquire linetype  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqln,GQLN)(Gint*,Gint*);

void ginq_linetype
#ifdef NeedFuncProto
(
    Gint *err_ind,   /* OUT error indicator  */
    Gint *linetype   /* OUT current linetype */
)
#else
( err_ind, linetype )
    Gint *err_ind;
    Gint *linetype;
#endif
{
    NGCALLF(gqln,GQLN)(err_ind,linetype);
}
