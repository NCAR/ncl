/*
 *	$Id: s_gqpfar.c,v 1.1 1997-03-05 19:13:07 haley Exp $
 */
/*
 *  Inquire predefined fill area representation  
 */

#include <ncarg/gks.h>

void ginq_pred_fill_rep
#ifdef NeedFuncProto
(
    Gint         ws_type,   /* workstation type              */
    Gint         ind,       /* predefined index              */
    Gint         *err_ind,  /* OUT error indicator           */
    Gfill_bundle *fill_rep  /* OUT predefined fill area rep. */
)
#else
( ws_type, ind, err_ind, fill_rep )
    Gint ws_type;
    Gint ind;
    Gint *err_ind;
    Gfill_bundle *fill_rep;
#endif
{
    NGCALLF(gqpfar,GQPFAR)(&ws_type,&ind,err_ind,&fill_rep->int_style,
                           &fill_rep->style_ind,&fill_rep->colr_ind);
}
