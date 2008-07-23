/*
 *	$Id: s_gdawk.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 * Deactivate workstation 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gdawk,GDAWK)(Gint*);

void gdeactivate_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    NGCALLF(gdawk,GDAWK)(&ws_id);
}
