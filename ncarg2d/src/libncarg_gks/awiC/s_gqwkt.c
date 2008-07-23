/*
 *  $Id: s_gqwkt.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
