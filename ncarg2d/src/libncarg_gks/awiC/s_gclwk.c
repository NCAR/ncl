/*
 *	$Id: s_gclwk.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Close workstation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkc,GQWKC)(Gint*,int*,int*,int*);

extern void NGCALLF(gclwk,GCLWK)(Gint*);

extern void remove_conn_id(int);

void gclose_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    int err_ind, conn_id, ws_type;
/*
 * Get workstation connection and type associated with the
 * workstation identifier
 */
    NGCALLF(gqwkc,GQWKC)(&ws_id,&err_ind,&conn_id,&ws_type);
/*
 * Remove the connection identifier from list of
 * connection identifiers in use
 */
    remove_conn_id(conn_id);
/*
 * Close workstation
 */
    NGCALLF(gclwk,GCLWK)(&ws_id);
}
