/*
 *	$Id: s_gsln.c,v 1.5 2008-07-23 17:24:24 haley Exp $
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
 *  Set linetype  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsln,GSLN)(Gint*);

void gset_linetype
#ifdef NeedFuncProto
(
    Gint linetype  /* linetype */
)
#else
( linetype )
    Gint linetype;
#endif
{
    NGCALLF(gsln,GSLN)(&linetype);
}
