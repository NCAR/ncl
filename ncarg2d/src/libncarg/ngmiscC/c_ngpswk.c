/*
 *	$Id: c_ngpswk.c,v 1.1 1997-04-11 17:43:40 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_ngpswk
#ifdef NeedFuncProto
(
    char *pstype,
    char *orient,
    char *color
)
#else
( pstype, orient, color )
    char *pstype;
    char *orient;
    char *color;
#endif
{
    NGstring pstype2;
    NGstring orient2;
    NGstring color2;
    extern int NGCALLF(ngpswk,NGPSWK)();
    int plen, clen, olen;

    if( !orient ) {
        orient = (char *)malloc(2*sizeof(char));
        strcpy( orient, "" );
        olen = 0;
    }
    else {
        olen = strlen(orient);
    }
    if( !color ) {
        color = (char *)malloc(2*sizeof(char));
        strcpy( color, "" );
        clen = 0;
    }
    else {
        clen = strlen(color);
    }
    plen = NGSTRLEN(pstype);
    pstype2 = NGCstrToFstr(pstype,plen);
    orient2 = NGCstrToFstr(orient,olen);
    color2 = NGCstrToFstr(color,clen);
    return(NGCALLF(ngpswk,NGPSWK)(pstype2,orient2,color2,plen,olen,clen));
}


