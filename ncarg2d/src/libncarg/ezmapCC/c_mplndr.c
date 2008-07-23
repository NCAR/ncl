/*
 *      $Id: c_mplndr.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern void NGCALLF(mplndr,MPLNDR)(NGstring,int*,int);

void c_mplndr
#ifdef NeedFuncProto
(
    char *flnm,
    int   ilvl
)
#else
(flnm,ilvl)
    char *flnm;
    int   ilvl;
#endif
{
    int len;
    NGstring flnm_f;
    len=NGSTRLEN(flnm);
    flnm_f=NGCstrToFstr(flnm,len);
    NGCALLF(mplndr,MPLNDR)(flnm_f,&ilvl,len);
    return;
}
