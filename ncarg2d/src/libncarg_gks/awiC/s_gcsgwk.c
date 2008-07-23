/*
 *	$Id: s_gcsgwk.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Copy Segment to Workstation
 */

#include <ncarg/gks.h>

extern void NGCALLF(gcsgwk,GCSGWK)(Gint*,Gint*);

void gcopy_seg_ws
#ifdef NeedFuncProto
(
    Gint ws_id,    /* workstation identifier */
    Gint seg_name  /* segment name           */
)
#else
( ws_id, seg_name )
    Gint ws_id;     /* workstation identifier */
    Gint seg_name;  /* segment name           */
#endif
{
    NGCALLF(gcsgwk,GCSGWK)(&ws_id,&seg_name);
}
