/*
 *  $Id: s_gsparf.c,v 1.1 1997-03-05 19:13:26 haley Exp $
 */
/*
 *  Set pattern reference point  
 */

#include <ncarg/gks.h>

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
