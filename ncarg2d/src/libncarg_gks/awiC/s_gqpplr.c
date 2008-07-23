/*
 *  $Id: s_gqpplr.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire predefined polyline representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpplr,GQPPLR)(Gint*,Gint*,Gint*,Gint*,Gfloat*,Gint*);

void ginq_pred_line_rep
#ifdef NeedFuncProto
(
    Gint         ws_type,           /* workstation type             */
    Gint         ind,               /* predefined index             */
    Gint         *err_ind,          /* OUT error indicator          */
    Gline_bundle *line_rep  /* OUT predefined polyline rep. */
)
#else
( ws_type, ind, err_ind, line_rep )
    Gint         ws_type;
    Gint         ind;
    Gint         *err_ind;
    Gline_bundle *line_rep;
#endif
{
    Gfloat width;

    NGCALLF(gqpplr,GQPPLR)(&ws_type,&ind,err_ind,&line_rep->type,
                           &width,&line_rep->colr_ind);
    line_rep->width = (Gdouble) width;
}
