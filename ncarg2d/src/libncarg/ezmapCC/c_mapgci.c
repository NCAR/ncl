/*
 *	$Id: c_mapgci.c,v 1.2 2008-07-23 16:16:46 haley Exp $
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

extern void NGCALLF(mapgci,MAPGCI)(float*,float*,float*,float*,int*,
                                   float*,float*);

void c_mapgci 
#ifdef NeedFuncProto
(
    float alat,
    float alon,
    float blat,
    float blon,
    int nopi,
    float *rlti,
    float *rlni
)
#else
 (alat,alon,blat,blon,nopi,rlti,rlni)
    float alat;
    float alon;
    float blat;
    float blon;
    int nopi;
    float *rlti;
    float *rlni;
#endif
{
    NGCALLF(mapgci,MAPGCI)(&alat,&alon,&blat,&blon,&nopi,rlti,rlni);
}
