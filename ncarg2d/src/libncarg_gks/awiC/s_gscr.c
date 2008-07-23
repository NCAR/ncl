/*
 *	$Id: s_gscr.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set colour representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gscr,GSCR)(Gint*,Gint*,const Gfloat*,const Gfloat*,
                               const Gfloat*);

void gset_colr_rep
#ifdef NeedFuncProto
(
    Gint            ws_id,     /* workstation identifier */
    Gint            colr_ind,  /* colour index           */
    const Gcolr_rep *colr_rep  /* colour representation  */
)
#else
( ws_id, colr_ind, colr_rep )
    Gint          ws_id;
    Gint       colr_ind;
    Gcolr_rep *colr_rep;
#endif
{
    NGCALLF(gscr,GSCR)(&ws_id,&colr_ind,&colr_rep->rgb.red,
                                        &colr_rep->rgb.green,
                                        &colr_rep->rgb.blue);
}
