/*
 *  $Id: s_gqwkcl.c,v 1.1 1997-03-05 19:13:16 haley Exp $
 */
/*
 *  Inquire workstation classification  
 */

#include <ncarg/gks.h>

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
