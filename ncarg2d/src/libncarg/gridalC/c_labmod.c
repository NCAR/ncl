/*
 *	$Id: c_labmod.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(labmod,LABMOD)(NGstring,NGstring,int*,int*,int*,int*,
                                   int*,int*,int*,int,int);

void c_labmod
#ifdef NeedFuncProto
(
    char *fmtx,
    char *fmty,
    int numx,
    int numy,
    int iszx,
    int iszy,
    int ixdc,
    int iydc,
    int ixor
)
#else
(fmtx,fmty,numx,numy,iszx,iszy,ixdc,iydc,ixor)
    char *fmtx;
    char *fmty;
    int numx;
    int numy;
    int iszx;
    int iszy;
    int ixdc;
    int iydc;
    int ixor;
#endif
{
    NGstring fmtx2;
    NGstring fmty2;
    int len1, len2;
/*
 * Make sure label formats are not NULL
 */
    if( !fmtx || !fmty ) {
        fprintf( stderr, "c_labmod:  illegal format string (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(fmtx);
    len2 = NGSTRLEN(fmty);
    fmtx2 = NGCstrToFstr(fmtx,len1);
    fmty2 = NGCstrToFstr(fmty,len2);
    NGCALLF(labmod,LABMOD)(fmtx2,fmty2,&numx,&numy,&iszx,&iszy,&ixdc,&iydc,
                           &ixor,len1,len2);
}

