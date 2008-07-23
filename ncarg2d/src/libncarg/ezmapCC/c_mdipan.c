/*
 *      $Id: c_mdipan.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern int NGCALLF(mdipan,MDIPAN)(int*,NGstring,int);

int c_mdipan
#ifdef NeedFuncProto
(
    int  iain,
    char *anme
)
#else
(iain,anme)
    int  iain;
    char *anme;
#endif
{
    int len;
    NGstring anme_f;
    len=NGSTRLEN(anme);
    anme_f=NGCstrToFstr(anme,len);
    return(NGCALLF(mdipan,MDIPAN)(&iain,anme_f,len));
}
