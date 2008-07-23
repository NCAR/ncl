/*
 *      $Id: c_mdlndr.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdlndr,MDLNDR)(NGstring,int*,int);

void c_mdlndr
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
    NGCALLF(mdlndr,MDLNDR)(flnm_f,&ilvl,len);
    return;
}
