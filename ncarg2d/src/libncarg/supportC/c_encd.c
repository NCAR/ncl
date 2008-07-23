/*
 *	$Id: c_encd.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(encd,ENCD)(float*,float*,NGstring,int*,int*,int);


void c_encd
#ifdef NeedFuncProto
(
    float valu,
    float ash,
    char *iout,
    int *nc,
    int ioffd
)
#else
(valu,ash,iout,nc,ioffd)
    float valu;
    float ash;
    char *iout;
    int *nc;
    int ioffd;
#endif
{
    NGstring iout2;
    int len;
    len = NGSTRLEN(iout);
    iout2 = NGCstrToFstr(iout,len);
    NGCALLF(encd,ENCD)(&valu,&ash,iout2,nc,&ioffd,len);
}
