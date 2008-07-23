/*
 *  $Id: s_gqwks.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire workstation state  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwks,GQWKS)(Gint*,Gint*,Gws_st*);

void ginq_ws_st
#ifdef NeedFuncProto
(
    Gint   ws_id,    /* workstation identifier */
    Gint   *err_ind, /* OUT error indicator    */
    Gws_st *ws_st    /* OUT workstation state  */
)
#else
( ws_id, err_ind, ws_st )
    Gint   ws_id;
    Gint   *err_ind;
    Gws_st *ws_st;
#endif
{
    NGCALLF(gqwks,GQWKS)(&ws_id,err_ind,ws_st);
}
