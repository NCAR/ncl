/*
 *  $Id: s_gqppmr.c,v 1.1 1997-03-05 19:13:10 haley Exp $
 */
/*
 *  Inquire predefined polymarker representation  
 */

#include <ncarg/gks.h>

void ginq_pred_marker_rep
#ifdef NeedFuncProto
(
    Gint           ws_type,     /* workstation type               */
    Gint           ind,         /* predefined index               */
    Gint           *err_ind,    /* OUT error indicator            */
    Gmarker_bundle *marker_rep  /* OUT predefined polymarker rep. */
)
#else
( ws_type, ind, err_ind, marker_rep )
    Gint           ws_type;
    Gint           ind;
    Gint           *err_ind;
    Gmarker_bundle *marker_rep;
#endif
{
    Gfloat mksscf;
    NGCALLF(gqppmr,GQPPMR)(&ws_type,&ind,err_ind,&marker_rep->type,&mksscf,
                           &marker_rep->colr_ind);
    marker_rep->size = (Gdouble) mksscf;
}
