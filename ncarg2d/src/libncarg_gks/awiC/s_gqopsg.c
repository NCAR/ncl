/*
 *	$Id: s_gqopsg.c,v 1.1 1997-03-05 19:13:04 haley Exp $
 */
/*
 *  Inquire name of open segment
 */

#include <ncarg/gks.h>

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
