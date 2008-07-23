/*
 *  $Id: s_gqppmr.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire predefined polymarker representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqppmr,GQPPMR)(Gint*,Gint*,Gint*,Gint*,Gfloat*,
                                   Gint*);

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
