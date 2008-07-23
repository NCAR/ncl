/*
 *      $Id: c_mdlach.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern void NGCALLF(mdlach,MDLACH)(double*,NGstring,int*,int);

void c_mdlach
#ifdef NeedFuncProto
(
    double rlat,
    char *chrs,
    int clen,
    int *nchr
)
#else
(rlat,chrs,clen,nchr)
    double rlat;
    char *chrs;
    int clen;
    int *nchr;
#endif
{
    int i;
    char error_msg[256];
    NGstring chrs2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( chrs, clen, error_msg ) ) {
        fprintf( stderr, "c_mdlach:  %s\n", error_msg );
        return;
    }
    chrs2 = NGCstrToFstr(chrs,clen);
    NGCALLF(mdlach,MDLACH)(&rlat,chrs2,nchr,clen-1);
    chrs = NGFstrToCstr(chrs2);
    chrs[*nchr] = '\0';
}
