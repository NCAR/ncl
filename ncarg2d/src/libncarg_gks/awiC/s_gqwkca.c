/*
 *  $Id: s_gqwkca.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire workstation category  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkca,GQWKCA)(Gint*,Gint*,Gws_cat*);

void ginq_ws_cat
#ifdef NeedFuncProto
(
    Gint    ws_type,  /* workstation type         */
    Gint    *err_ind, /* OUT error indicator      */
    Gws_cat *cat      /* OUT workstation category */
)
#else
( ws_type, err_ind, cat )
    Gint    ws_type;
    Gint    *err_ind;
    Gws_cat *cat;
#endif
{
    NGCALLF(gqwkca,GQWKCA)(&ws_type,err_ind,cat);
}
