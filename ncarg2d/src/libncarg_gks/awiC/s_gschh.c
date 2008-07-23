/*
 *	$Id: s_gschh.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Set character height  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gschh,GSCHH)(Gfloat*);

void gset_char_ht
#ifdef NeedFuncProto
(
    Gdouble char_ht  /* character height */
)
#else
( char_ht )
    Gdouble char_ht;
#endif
{
    Gfloat ht;
    ht = (Gfloat) char_ht;
    NGCALLF(gschh,GSCHH)(&ht);
}
