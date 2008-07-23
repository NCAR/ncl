/*
 *  $Id: s_gqpx.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire pixel  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpx,GQPX)(Gint*,const Gfloat*,const Gfloat*,Gint*,Gint*);

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

