/*
 *      $Id: c_mdpgci.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdpgci,MDPGCI)(double*,double*,double*,double*,int*,
				   double*,double*);

void c_mdpgci
#ifdef NeedFuncProto
(
    double alat,
    double alon,
    double blat,
    double blon,
    int nopi,
    double *rlti,
    double *rlni
)
#else
 (alat,alon,blat,blon,nopi,rlti,rlni)
    double alat;
    double alon;
    double blat;
    double blon;
    int nopi;
    double *rlti;
    double *rlni;
#endif
{
    NGCALLF(mdpgci,MDPGCI)(&alat,&alon,&blat,&blon,&nopi,rlti,rlni);
}
