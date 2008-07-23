/*
 *      $Id: c_mdlnri.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdlnri,MDLNRI)(NGstring,int);

void c_mdlnri
#ifdef NeedFuncProto
(
    char *flnm
)
#else
(flnm)
    char *flnm;
#endif
{
    int len;
    NGstring flnm_f;
    len=NGSTRLEN(flnm);
    flnm_f=NGCstrToFstr(flnm,len);
    NGCALLF(mdlnri,MDLNRI)(flnm_f,len);
    return;
}
