/*
 *  $Id: s_gqppar.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire predefined pattern representation  
 */

#include <ncarg/gks.h>
#include <stdlib.h>

extern void NGCALLF(gqppar,GQPPAR)(Gint*,Gint*,int*,int*,Gint*,int*,
                                   int*,int*);

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

    parray = (Gint *)malloc(nmx*mmx*sizeof(Gint));

    (void)NGCALLF(gqppar,GQPPAR)(&ws_type,&ind,&nmx,&mmx,err_ind,&n,&m,parray);

    free(parray);
}
