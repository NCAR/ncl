/*
 *	$Id: s_gscr.c,v 1.1 1997-03-05 19:13:21 haley Exp $
 */
/*
 *  Set colour representation  
 */

#include <ncarg/gks.h>

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
