/*
 *	$Id: c_wmgtln.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

int c_wmgtln
#ifdef NeedFuncProto
(
    char *lab,
    int lablen,    
    int ilr
)
#else
(lab,lablen,ilr)
    char *lab,
    int lablen,    
    int ilr
#endif
{
	extern int NGCALLF(wmgtln,WMGTLN)(NGstring,int*,int*,int);
    NGstring lab2;
    int len;
    len = NGSTRLEN(lab);
    lab2 = NGCstrToFstr(lab,len);
    return(NGCALLF(wmgtln,WMGTLN)(lab2,&lablen,&ilr,len));
}
