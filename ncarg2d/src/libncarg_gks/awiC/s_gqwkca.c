/*
 *  $Id: s_gqwkca.c,v 1.1 1997-03-05 19:13:16 haley Exp $
 */
/*
 *  Inquire workstation category  
 */

#include <ncarg/gks.h>

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
