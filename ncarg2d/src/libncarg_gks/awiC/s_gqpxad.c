/*
 *  $Id: s_gqpxad.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire pixel array dimensions  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpxad,GQPXAD)(Gint*,Gfloat*,Gfloat*,Gfloat*,Gfloat*,
                                   Gint*,Gint*,Gint*);

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
