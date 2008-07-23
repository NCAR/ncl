/*
 *	$Id: s_gschsp.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 *  Set character spacing  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gschsp,GSCHSP)(Gfloat*);

void gset_char_space
#ifdef NeedFuncProto
(
    Gdouble char_space  /* character spacing */
)
#else
(char_space)
    Gdouble char_space;
#endif
{
    Gfloat space;
    space = (Gfloat)char_space;
    NGCALLF(gschsp,GSCHSP)(&space);
}
