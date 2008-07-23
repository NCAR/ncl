/*
 *	$Id: s_gacwk.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Activate workstation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gacwk,GACWK)(Gint*);

void gactivate_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    NGCALLF(gacwk,GACWK)(&ws_id);
}
