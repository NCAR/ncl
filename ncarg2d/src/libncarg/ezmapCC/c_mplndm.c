/*
 *      $Id: c_mplndm.c,v 1.2 2008-07-23 16:16:53 haley Exp $
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

extern void NGCALLF(mplndm,MPLNDM)(NGstring,int*,int*,float*,float*,int*,
                                   int*,int*,int*,int (*ulpr_)(),int);

void c_mplndm
#ifdef NeedFuncProto
(
    char  *flnm,
    int    ilvl,
    int   *iama,
    float *xcra,
    float *ycra,
    int    mcra,
    int   *iaai,
    int   *iagi,
    int    mnog,
    int (*ulpr_)(
        float *xcra,
        float *ycra,
	int   *ncra,
	int   *iaai,
	int   *iagi,
	int   *ngpsi
                )
)
#else
(flnm,ilvl,iama,iama,xcra,ycra,mcra,iaai,iagi,mnog,ulpr)
    char  *flnm;
    int    ilvl;
    int   *iama;
    float *xcra;
    float *ycra;
    int    mcra;
    int   *iaai;
    int   *iagi;
    int    mnog;
    int (*ulpr_)();
#endif
{
    int len;
    NGstring flnm_f;
    len=NGSTRLEN(flnm);
    flnm_f=NGCstrToFstr(flnm,len);
    NGCALLF(mplndm,MPLNDM)(flnm_f,&ilvl,iama,xcra,ycra,&mcra,
                           iaai,iagi,&mnog,ulpr_,len);
    return;
}
