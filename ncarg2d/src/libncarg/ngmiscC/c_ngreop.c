/*
 *	$Id: c_ngreop.c,v 1.1 1997-04-11 17:43:41 haley Exp $
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

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
    NGCALLF(ngreop,NGREOP)(&wkid,&conid,&itype,fname2,&iopt,iat,rat,&ncolrs,&nstart,ctab,len);
}


