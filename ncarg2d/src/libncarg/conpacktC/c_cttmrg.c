/*
 *      $Id: c_cttmrg.c,v 1.3 2008-07-23 16:16:45 haley Exp $
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

extern void NGCALLF(cttmrg,CTTMRG)(int*,int*,float*,float*,float*,int*,
                                   float*,int (*rtmi_)(),
                                   float*,int*,int*,int*,
                                     int*,int*,int*,int*,
                                     int*,int*,int*,int*);


void c_cttmrg
#ifdef NeedFuncProto
(
    int idim,
    int jdim,
    float *rlat,
    float *rlon,
    float *rdat,
    int *iscr,
    float sval,
    int (*rtmi_) (
        int *idim,
        int *jdim,
        int *iini,
        int *jini,
        int *iino,
        int *jino),
    float *rpnt,
    int mpnt,
    int *npnt,
    int lopn,
    int *iedg,
    int medg,
    int *nedg,
    int loen,
    int *itri,
    int mtri,
    int *ntri,
    int lotn
)
#else
(idim,jdim,rlat,rlon,rdat,iscr,sval,rtmi_,rpnt,mpnt,npnt,lopn,
 iedg,medg,nedg,loen,itri,mtri,ntri,lotn)
    int idim;
    int jdim;
    float *rlat;
    float *rlon;
    float *rdat;
    int *iscr;
    float sval;
    int (*rtmi_) ();
    float *rpnt;
    int mpnt;
    int *npnt;
    int lopn;
    int *iedg;
    int medg;
    int *nedg;
    int loen;
    int *itri;
    int mtri;
    int *ntri;
    int lotn;

#endif
{
    NGCALLF(cttmrg,CTTMRG)(&idim,&jdim,rlat,rlon,rdat,iscr,&sval,rtmi_,
                           rpnt,&mpnt,npnt,&lopn,
                           iedg,&medg,nedg,&loen,
                           itri,&mtri,ntri,&lotn);
}
