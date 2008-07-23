/*
 *      $Id: c_cttdbm.c,v 1.3 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(cttdbm,CTTDBM)(int*,int*,int*,int*,int*,int*,int*,int*);

void c_cttdbm
#ifdef NeedFuncProto
(
    int ihbx,
    int iebx,
    int iwbx,
    int iubx,
    int ihba,
    int ieba,
    int iwba,
    int iuba
)
#else
(ihbx,iebx,iwbx,iubx,ihba,ieba,iwba,iuba)
    int ihbx;
    int iebx;
    int iwbx;
    int iubx;
    int ihba;
    int ieba;
    int iwba;
    int iuba;
#endif
{
    NGCALLF(cttdbm,CTTDBM)(&ihbx,&iebx,&iwbx,&iubx,&ihba,&ieba,&iwba,&iuba);
}
