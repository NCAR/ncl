/*
 *	$Id: s_gqasf.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 * Inquire aspect source flags 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqasf,GQASF)(Gint*,Gasfs*);

void ginq_asfs
#ifdef NeedFuncProto
(
    Gint  *err_ind,   /* OUT error indicator             */
    Gasfs *list_asf   /* OUT current aspect source flags */
)
#else
( err_ind, list_asf )
    Gint  *err_ind;
    Gasfs *list_asf;
#endif
{
    NGCALLF(gqasf,GQASF)(err_ind,list_asf);
}
