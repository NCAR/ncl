/*
 *	$Id: s_gqopsg.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire name of open segment
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqopsg,GQOPSG)(Gint*,Gint*);

void ginq_name_open_seg
#ifdef NeedFuncProto
(
    Gint *err_ind,      /* OUT error indicator      */
    Gint *name_open_seg /* OUT name of open segment */
)
#else
( err_ind, name_open_seg )
    Gint *err_ind;
    Gint *name_open_seg;
#endif
{
    NGCALLF(gqopsg,GQOPSG)(err_ind,name_open_seg);
}
