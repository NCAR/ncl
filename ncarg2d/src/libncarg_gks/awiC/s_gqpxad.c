/*
 *  $Id: s_gqpxad.c,v 1.1 1997-03-05 19:13:11 haley Exp $
 */
/*
 *  Inquire pixel array dimensions  
 */

#include <ncarg/gks.h>

void ginq_pixel_array_dims
#ifdef NeedFuncProto
(
    Gint      ws_id,      /* workstation identifier     */
    Grect     *rect,      /* rectangle                  */
    Gint      *err_ind,   /* OUT error indicator        */
    Gint_size *dims       /* OUT pixel array dimensions */
)
#else
(ws_id,rect,err_ind,dims)
    Gint      ws_id;
    Grect     *rect;
    Gint      *err_ind;
    Gint_size *dims;
#endif
{
    NGCALLF(gqpxad,GQPXAD)(&ws_id,&rect->p.x,&rect->p.y,&rect->q.x,&rect->q.y,
                           err_ind,&dims->size_x,&dims->size_y);
}
