/*
 *  $Id: s_gqwkdu.c,v 1.1 1997-03-05 19:13:17 haley Exp $
 */
/*
 *  Inquire workstation deferral and update states  
 */

#include <ncarg/gks.h>

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





