/*
 *      $Id: c_mpipan.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern int NGCALLF(mpipan,MPIPAN)(int*,NGstring,int);

int c_mpipan
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
    return(NGCALLF(mpipan,MPIPAN)(&iain,anme_f,len));
}
