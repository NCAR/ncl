/*
 *  $Id: s_gqpx.c,v 1.1 1997-03-05 19:13:11 haley Exp $
 */
/*
 *  Inquire pixel  
 */

#include <ncarg/gks.h>

void ginq_pixel
#ifdef NeedFuncProto
(
    Gint   ws_id,             /* workstation identifier */
    const  Gpoint *pixel_loc, /* pixel location         */
    Gint   *err_ind,          /* OUT error indicator    */
    Gint   *colr_ind          /* OUT colour index       */
)
#else
( ws_id, pixel_loc, err_ind, colr_ind )
    Gint   ws_id;
    Gpoint *pixel_loc;
    Gint   *err_ind;
    Gint   *colr_ind;
#endif
{
    NGCALLF(gqpx,GQPX)(&ws_id,&pixel_loc->x,&pixel_loc->y,err_ind,colr_ind);
}

