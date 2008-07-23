/*
 *      $Id: c_mdloch.c,v 1.2 2008-07-23 16:16:50 haley Exp $
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

extern void NGCALLF(mdloch,MDLOCH)(double*,NGstring,int*,int);

void c_mdloch
#ifdef NeedFuncProto
(
    double rlon,
    char *chrs,
    int clen,
    int *nchr
)
#else
(rlon,chrs,clen,nchr)
    double rlon;
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
        fprintf( stderr, "c_mdloch:  %s\n", error_msg );
        return;
    }
    chrs2 = NGCstrToFstr(chrs,clen);
    NGCALLF(mdloch,MDLOCH)(&rlon,chrs2,nchr,clen-1);
    chrs = NGFstrToCstr(chrs2);
    chrs[*nchr] = '\0';
}
