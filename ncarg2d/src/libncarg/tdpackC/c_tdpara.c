/*
 *      $Id: c_tdpara.c,v 1.1 1997-06-30 21:47:41 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdpara
#ifdef NeedFuncProto
(
    float arg1,
    float arg2,
    float arg3,
    float arg4,
    float arg5,
    float arg6,
    float arg7,
    float arg8,
    float arg9
)
#else
(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
    float arg1;
    float arg2;
    float arg3;
    float arg4;
    float arg5;
    float arg6;
    float arg7;
    float arg8;
    float arg9;
#endif
{
    float arg12,arg22,arg32,arg42,arg52,arg62,arg72,arg82,arg92;
    arg12=arg1;
    arg22=arg2;
    arg32=arg3;
    arg42=arg4;
    arg52=arg5;
    arg62=arg6;
    arg72=arg7;
    arg82=arg8;
    arg92=arg9;
    NGCALLF(tdpara,TDPARA)(&arg12,&arg22,&arg32,&arg42,&arg52,&arg62,
                                                &arg72,&arg82,&arg92);
}
