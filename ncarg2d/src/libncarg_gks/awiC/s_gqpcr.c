/*
 *  $Id: s_gqpcr.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire predefined colour representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpcr,GQPCR)(Gint*,Gint*,Gint*,Gfloat*,Gfloat*,Gfloat*);

void ginq_pred_colr_rep
#ifdef NeedFuncProto
(
    Gint      ws_type,  /* workstation identifier     */
    Gint      ind,      /* predefined index           */
    Gint      *err_ind, /* OUT error indicator        */
    Gcolr_rep *colr_rep /* OUT predefined colour rep. */
)
#else
( ws_type, ind, err_ind, colr_rep )
    Gint      ws_type;
    Gint      ind;
    Gint      *err_ind;
    Gcolr_rep *colr_rep;
#endif
{
    NGCALLF(gqpcr,GQPCR)(&ws_type,&ind,err_ind,
                         &colr_rep->rgb.red,
                         &colr_rep->rgb.green,
                         &colr_rep->rgb.blue);
}
