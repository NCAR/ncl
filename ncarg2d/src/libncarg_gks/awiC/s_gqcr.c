/*
 *	$Id: s_gqcr.c,v 1.1 1997-03-05 19:12:56 haley Exp $
 */
/* 
 * Inquire colour representation  
 */

#include <ncarg/gks.h>

void ginq_colr_rep
#ifdef NeedFuncProto
(
    Gint       ws_id,     /* workstation identifier     */
    Gint       colr_ind,  /* colour index               */
    Ginq_type  type,      /* type of returned values    */
    Gint       *err_ind,  /* OUT error indicator        */
    Gcolr_rep  *colr_rep  /* OUT colour representation  */
)
#else
( ws_id, colr_ind, type, err_ind, colr_rep )
    Gint       ws_id;
    Gint       colr_ind;
    Ginq_type  type;
    Gint       *err_ind;
    Gcolr_rep  *colr_rep;
#endif
{
    NGCALLF(gqcr,GQCR)(&ws_id,&colr_ind,&type,err_ind,
                       &colr_rep->rgb.red,
                       &colr_rep->rgb.green,
                       &colr_rep->rgb.blue);
}
