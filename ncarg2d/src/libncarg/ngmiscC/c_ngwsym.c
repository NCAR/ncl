/*
 *	$Id: c_ngwsym.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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

extern void NGCALLF(ngwsym,NGWSYM)(NGstring,int*,float*,float*,float*,
                                   int*,int*,int);

void c_ngwsym
#ifdef NeedFuncProto
(
    char *ftype,
    int num,
    float x,
    float y,
    float size,
    int icolor,
    int ialt
)
#else
(ftype,num,x,y,size,icolor,ialt)
    char *ftype;
    int num;
    float x;
    float y;
    float size;
    int icolor;
    int ialt;
#endif
{
    NGstring ftype2;
    int len;
/*
 * Make sure font parameter is not NULL
 */
    if( !ftype ) {
        fprintf( stderr, "c_ngwsym:  illegal font parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(ftype);
    ftype2 = NGCstrToFstr(ftype,len);
    NGCALLF(ngwsym,NGWSYM)(ftype2,&num,&x,&y,&size,&icolor,&ialt,len);
}
