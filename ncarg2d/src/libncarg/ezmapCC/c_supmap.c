/*
 *	$Id: c_supmap.c,v 1.2 2008-07-23 16:16:54 haley Exp $
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

extern void NGCALLF(supmap,SUPMAP)(int*,float*,float*,float*,float*,float*,
                                   float*,float*,int*,int*,int*,int*,int*);

void c_supmap
#ifdef NeedFuncProto
(
    int jprj,
    float plat,
    float plon,
    float rota,
    float *plm1,
    float *plm2,
    float *plm3,
    float *plm4,
    int jlts,
    int jgrd,
    int iout,
    int idot,
    int *ierr
)
#else
 (jprj,plat,plon,rota,plm1,plm2,plm3,plm4,jlts,jgrd,iout,idot,ierr)
    int jprj;
    float plat;
    float plon;
    float rota;
    float *plm1;
    float *plm2;
    float *plm3;
    float *plm4;
    int jlts;
    int jgrd;
    int iout;
    int idot;
    int *ierr;
#endif
{
    NGCALLF(supmap,SUPMAP)(&jprj,&plat,&plon,&rota,plm1,plm2,plm3,plm4,
                           &jlts,&jgrd,&iout,&idot,ierr);
}
