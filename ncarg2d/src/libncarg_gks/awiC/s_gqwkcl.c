/*
 *  $Id: s_gqwkcl.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire workstation classification  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkcl,GQWKCL)(Gint*,Gint*,Gws_class*);

void ginq_ws_class
#ifdef NeedFuncProto
(
    Gint      ws_type,  /* workstation type      */
    Gint      *err_ind, /* OUT error indicator   */
    Gws_class *wsclass  /* OUT workstation class */
)
#else
( ws_type, err_ind, wsclass )
    Gint      ws_type;
    Gint      *err_ind;
    Gws_class *wsclass;
#endif
{
    NGCALLF(gqwkcl,GQWKCL)(&ws_type,err_ind,wsclass);
}
