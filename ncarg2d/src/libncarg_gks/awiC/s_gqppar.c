/*
 *  $Id: s_gqppar.c,v 1.4 2000-08-22 15:09:06 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *  Inquire predefined pattern representation  
 */

#include <ncarg/gks.h>

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
    NGCALLF(gqppar,GQPPAR)(&ws_type,&ind,&nmx,&mmx,err_ind,&n,&m,parray);
}
