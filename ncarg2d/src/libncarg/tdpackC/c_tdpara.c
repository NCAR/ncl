/*
 *      $Id: c_tdpara.c,v 1.2 1997-07-02 22:26:58 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdpara
#ifdef NeedFuncProto
(
    float ua00,
    float va00,
    float wa00,
    float uv10,
    float vv10,
    float wv10,
    float uv01,
    float vv01,
    float wv01
)
#else
(ua00,va00,wa00,uv10,vv10,wv10,uv01,vv01,wv01)
    float ua00;
    float va00;
    float wa00;
    float uv10;
    float vv10;
    float wv10;
    float uv01;
    float vv01;
    float wv01;
#endif
{
    float ua002,va002,wa002,uv102,vv102,wv102,uv012,vv012,wv012;
    ua002=ua00;
    va002=va00;
    wa002=wa00;
    uv102=uv10;
    vv102=vv10;
    wv102=wv10;
    uv012=uv01;
    vv012=vv01;
    wv012=wv01;
    NGCALLF(tdpara,TDPARA)(&ua002,&va002,&wa002,&uv102,&vv102,&wv102,
                                                &uv012,&vv012,&wv012);
}
