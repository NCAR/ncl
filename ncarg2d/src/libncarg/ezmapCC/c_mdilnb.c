/*
 *      $Id: c_mdilnb.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern int NGCALLF(mdilnb,MDILNB)(NGstring,int);

int c_mdilnb
#ifdef NeedFuncProto
(
    char *chrs
)
#else
(chrs)
    char *chrs;
#endif
{
    int len;
    NGstring chrs_f;
    len=NGSTRLEN(chrs);
    chrs_f=NGCstrToFstr(chrs,len);
    return(NGCALLF(mdilnb,MDILNB)(chrs_f,len));
}
