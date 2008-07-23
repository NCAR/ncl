/*
 *      $Id: c_mdifnb.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern int NGCALLF(mdifnb,MDIFNB)(NGstring,int);

int c_mdifnb
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
    return(NGCALLF(mdifnb,MDIFNB)(chrs_f,len));
}
