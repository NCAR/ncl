/*
 *      $Id: c_mdlnam.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern void NGCALLF(mdlnam,MDLNAM)(NGstring,int*,int*,int);

void c_mdlnam
#ifdef NeedFuncProto
(
    char *flnm,
    int   ilvl,
    int  *iama
)
#else
(flnm,ilvl,iama)
    char *flnm;
    int   ilvl;
    int  *iama;
#endif
{
    int len;
    NGstring flnm_f;
    len=NGSTRLEN(flnm);
    flnm_f=NGCstrToFstr(flnm,len);
    NGCALLF(mdlnam,MDLNAM)(flnm_f,&ilvl,iama,len);
    return;
}
