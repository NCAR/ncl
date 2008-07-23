/*
 *  $Id: s_gsparf.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set pattern reference point  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsparf,GSPARF)(const Gfloat*,const Gfloat*);

void gset_pat_ref_point
#ifdef NeedFuncProto
(
    const Gpoint *pat_ref_point  /* pattern reference point */
)
#else
( pat_ref_point )
    Gpoint *pat_ref_point;
#endif
{
    NGCALLF(gsparf,GSPARF)(&pat_ref_point->x, &pat_ref_point->y);
}
