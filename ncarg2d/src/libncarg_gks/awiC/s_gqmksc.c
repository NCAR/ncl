/*
 *	$Id: s_gqmksc.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire marker size scale factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqmksc,GQMKSC)(Gint*,Gfloat*);

void ginq_marker_size
#ifdef NeedFuncProto
(
    Gint    *err_ind,      /* OUT error indicator                  */
    Gdouble *marker_size   /* OUT current marker size scale factor */
)
#else
( err_ind, marker_size )
    Gint    *err_ind;
    Gdouble *marker_size;
#endif
{
    Gfloat size;
    NGCALLF(gqmksc,GQMKSC)(err_ind,&size);
    *marker_size = (Gdouble)size;
}
