/*
 *	$Id: c_ngreop.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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
#include <ncarg/gks.h>

extern void NGCALLF(ngreop,NGREOP)(int*,int*,int*,NGstring,int*,int*,float*,
                                   int*,int*,Gcolr_rep*,int);

void c_ngreop
#ifdef NeedFuncProto
(
    int wkid,
    int conid,
    int itype,
    char *fname,
    int iopt,
    int *iat,
    float *rat,
    int ncolrs,
    int nstart,
    Gcolr_rep *ctab
)
#else
(wkid,conid,itype,fname,iopt,iat,rat,ncolrs,nstart,ctab)   
    int wkid;
    int conid;
    int itype;
    char *fname;
    int iopt; 
    int *iat; 
    float *rat; 
    int ncolrs; 
    int nstart;
    Gcolr_rep *ctab;
#endif
{
    NGstring fname2;
    int len;

    len = NGSTRLEN(fname);
    fname2 = NGCstrToFstr(fname,len);
    NGCALLF(ngreop,NGREOP)(&wkid,&conid,&itype,fname2,&iopt,iat,rat,
                           &ncolrs,&nstart,ctab,len);
}


