/*
 *	$Id: c_q8qst4.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(q8qst4,Q8QST4)(NGstring,NGstring,NGstring,NGstring,int,
                                   int,int,int);

void c_q8qst4
#ifdef NeedFuncProto
(
    char *name,
    char *lbrary,
    char *entry,
    char *vrsion
)
#else
(name,lbrary,entry,vrsion)
    char *name;
    char *lbrary;
    char *entry;
    char *vrsion;
#endif
{
    NGstring name2;
    NGstring lbrary2;
    NGstring entry2;
    NGstring vrsion2;
    int nlen, llen, elen, vlen;
    nlen = NGSTRLEN(name);
    llen = NGSTRLEN(lbrary);
    elen = NGSTRLEN(entry);
    vlen = NGSTRLEN(vrsion);
    name2 = NGCstrToFstr(name,nlen);
    lbrary2 = NGCstrToFstr(lbrary,llen);
    entry2 = NGCstrToFstr(entry,elen);
    vrsion2 = NGCstrToFstr(vrsion,vlen);
    NGCALLF(q8qst4,Q8QST4)(name2,lbrary2,entry2,vrsion2,nlen,llen,elen,vlen);
}
