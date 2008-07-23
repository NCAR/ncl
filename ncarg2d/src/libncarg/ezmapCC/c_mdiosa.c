/*
 *      $Id: c_mdiosa.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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


#include <ncarg/ncargC.h>

extern int NGCALLF(mdiosa,MDIOSA)(int*,int*);

int c_mdiosa
#ifdef NeedFuncProto
(
    int iaid,
    int ilvl
)
#else
(iaid,ilvl)
    int iaid;
    int ilvl;
#endif
{
    return(NGCALLF(mdiosa,MDIOSA)(&iaid,&ilvl));
}
