/*
 *  $Id: s_gqppar.c,v 1.1 1997-03-05 19:13:09 haley Exp $
 */
/*
 *  Inquire predefined pattern representation  
 */

#include <ncarg/gks.h>

void ginq_pred_pat_rep
#ifdef NeedFuncProto
(
    Gint     ws_type,    /* workstation type            */
    Gint     ind,        /* predefined index            */
    Gstore   *store,     /* size of buffer              */
    Gint     *err_ind,   /* OUT error indicator         */
    Gpat_rep **pat_rep   /* OUT predefined pattern rep. */
)
#else
( ws_type, ind, store, err_ind, pat_rep )
    Gint     ws_type;
    Gint     ind;
    Gstore   *store;
    Gint     *err_ind;
    Gpat_rep **pat_rep;
#endif
{
/*
 * This routine doesn't do anything but return an error condition
 */
    int nmx, mmx, n, m;
    int *parray;
    NGCALLF(gqppar,GQPPAR)(&ws_type,&ind,&nmx,&mmx,err_ind,&n,&m,parray);
}
