/*
 *	$Id: s_gsmksc.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set marker size scale factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsmksc,GSMKSC)(Gfloat*);

void gset_marker_size
#ifdef NeedFuncProto
(
    Gdouble marker_size  /* marker size scale factor */
)
#else
( marker_size )
    Gdouble marker_size;
#endif
{
    Gfloat size;
    size = (Gfloat)marker_size;
    NGCALLF(gsmksc,GSMKSC)(&size);
}
