/*
 *  $Id: s_gqwkt.c,v 1.3 2000-08-01 14:35:56 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *  Inquire workstation transformation 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkt,GQWKT)(Gint*,Gint*,Gupd_st*,Glimit*,Glimit*,
                                 Glimit*,Glimit*);

void ginq_ws_tran
#ifdef NeedFuncProto
(
    Gint    ws_id,           /* workstation identifier                      */
    Gint    *err_ind,        /* OUT error indicator                         */
    Gupd_st *ws_tran_upd_st, /* OUT workstation transformation update state */
    Glimit  *req_ws_win,     /* OUT requested workstation window            */
    Glimit  *cur_ws_win,     /* OUT current workstation window              */
    Glimit  *req_ws_vp,      /* OUT requested workstation viewport          */
    Glimit  *cur_ws_vp       /* OUT current workstation viewport            */
)
#else
(ws_id,err_ind,ws_tran_upd_st,req_ws_win,cur_ws_win,
                  req_ws_vp,cur_ws_vp)
    Gint    ws_id;
    Gint    *err_ind;
    Gupd_st *ws_tran_upd_st;
    Glimit  *req_ws_win;
    Glimit  *cur_ws_win;
    Glimit  *req_ws_vp;
    Glimit  *cur_ws_vp;
#endif
{
    NGCALLF(gqwkt,GQWKT)(&ws_id,err_ind,ws_tran_upd_st,req_ws_win,cur_ws_win,
                                                       req_ws_vp, cur_ws_vp);
}
