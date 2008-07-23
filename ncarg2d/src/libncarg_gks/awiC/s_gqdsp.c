/*
 *  $Id: s_gqdsp.c,v 1.5 2008-07-23 17:24:20 haley Exp $
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
 *  Inquire display space size  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqdsp,GQDSP)(Gint*,Gint*,Gdc_units*,Gfloat*,Gfloat*,
                                 Gint*,Gint*);

void ginq_disp_space_size
#ifdef NeedFuncProto
(
    Gint             ws_type,    /* workstation type         */
    Gint             *err_ind,   /* OUT error indicator      */
    Gdisp_space_size *disp_size  /* OUT display [space] size */
)
#else
( ws_type, err_ind, disp_size )
    Gint             ws_type;
    Gint             *err_ind;
    Gdisp_space_size *disp_size;
#endif
{
    NGCALLF(gqdsp,GQDSP)(&ws_type,err_ind,&disp_size->dc_units,
                         &disp_size->size_dc.size_x,
                         &disp_size->size_dc.size_y,
                         &disp_size->size_raster.size_x,
                         &disp_size->size_raster.size_y);
}

