/*
 *  $Id: s_gqwkdu.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire workstation deferral and update states  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkdu,GQWKDU)(Gint*,Gint*,Gdefer_mode*,Girg_mode*,
                                   Gdisp_space_empty*,Gnew_frame_nec_upd*);

void ginq_ws_defer_upd_sts
#ifdef NeedFuncProto
(
    Gint               ws_id,       /* workstation identifier         */
    Gint               *err_ind,    /* OUT error indicator            */
    Gdefer_mode        *defer_mode, /* OUT deferral mode              */
    Girg_mode          *irg_mode,   /* OUT implicit regeneration mode */
    Gdisp_space_empty  *disp_empty, /* OUT display space empty        */
    Gnew_frame_nec_upd *new_frame   /* OUT new frame action necessary 
                                       at update                      */
)
#else
(ws_id,err_ind,defer_mode,irg_mode,disp_empty,new_frame)
    Gint               ws_id;       /* workstation identifier         */
    Gint               *err_ind;    /* OUT error indicator            */
    Gdefer_mode        *defer_mode; /* OUT deferral mode              */
    Girg_mode          *irg_mode;   /* OUT implicit regeneration mode */
    Gdisp_space_empty  *disp_empty; /* OUT display space empty        */
    Gnew_frame_nec_upd *new_frame;  /* OUT new frame action necessary 
                                       at update                      */
#endif
{
    NGCALLF(gqwkdu,GQWKDU)(&ws_id,err_ind,defer_mode,irg_mode,
                           disp_empty,new_frame);
}





