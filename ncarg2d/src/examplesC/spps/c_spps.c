/*
 * $Id: c_spps.c,v 1.1 1994-10-31 04:05:05 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int ival, ix1, iy1, ix2, iy2, k, k1, k2, jx, jy;
    float px, py, c, x, x1, x2;
    extern float NGCALLF(cufx,CUFX)(
#ifdef NeedFuncProto
    float *ix
#endif
    );
    extern float NGCALLF(cufy,CUFY)(
#ifdef NeedFuncProto
    float *iy
#endif
    );
    extern float NGCALLF(cfux,CFUX)(
#ifdef NeedFuncProto
    float *ix
#endif
    );
    extern float NGCALLF(cfuy,CFUY)(
#ifdef NeedFuncProto
    float *iy
#endif
    );
    extern float NGCALLF(cmfx,CMFX)(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float NGCALLF(cmfy,CMFY)(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern float NGCALLF(cpfx,CPFX)(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float NGCALLF(cpfy,CPFY)(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern float NGCALLF(cmux,CMUX)(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float NGCALLF(cmuy,CMUY)(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern float NGCALLF(cpux,CPUX)(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float NGCALLF(cpuy,CPUY)(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern int NGCALLF(kfmx,KFMX)(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int NGCALLF(kfmy,KFMY)(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int NGCALLF(kumx,KUMX)(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int NGCALLF(kumy,KUMY)(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int NGCALLF(kfpx,KFPX)(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int NGCALLF(kfpy,KFPY)(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int NGCALLF(kupx,KUPX)(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int NGCALLF(kupy,KUPY)(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int NGCALLF(kmpx,KMPX)(
#ifdef  NeedFuncProto
    int *ix
#endif
    );
    extern int NGCALLF(kmpy,KMPY)(
#ifdef  NeedFuncProto
    int *iy
#endif
    );
    extern int NGCALLF(kpmx,KPMX)(
#ifdef  NeedFuncProto
    int *ix
#endif
    );
    extern int NGCALLF(kpmy,KPMY)(
#ifdef  NeedFuncProto
    int *iy
#endif
    );

	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_setusv("LS",4);
    c_getusv("LS",&ival);
    if( ival != 4 ) {
        printf( "c_setusv and c_getusv tests UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_setusv and c_getusv tests SUCCESSFUL\n" );
    }

    k = 2;
    x = .5;
    x1 = c_cfux(.5);
    x2 = (float)NGCALLF(cfux,CFUX)(&x);
    if( x1 != x2 ) {
        printf( "c_cfux test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cfux test SUCCESSFUL\n" );
    }
    x1 = c_cfuy(.5);
    x2 = (float)NGCALLF(cfuy,CFUY)(&x);
    if( x1 != x2 ) {
        printf( "c_cfuy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cfuy test SUCCESSFUL\n" );
    }
    x1 = c_cufx(.5);
    x2 = (float)NGCALLF(cufx,CUFX)(&x);
    if( x1 != x2 ) {
        printf( "c_cufx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cufx test SUCCESSFUL\n" );
    }
    x1 = c_cufy(.5);
    x2 = (float)NGCALLF(cufy,CUFY)(&x);
    if( x1 != x2 ) {
        printf( "c_cufy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cufy test SUCCESSFUL\n" );
    }
    x1 = c_cmfx(2);
    x2 = (float)NGCALLF(cmfx,CMFX)(&k);
    if( x1 != x2 ) {
        printf( "c_cmfx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmfx test SUCCESSFUL\n" );
    }
    x1 = c_cmfy(2);
    x2 = NGCALLF(cmfy,CMFY)(&k);
    if( x1 != x2 ) {
        printf( "c_cmfy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmfy test SUCCESSFUL\n" );
    }
    x1 = c_cmux(2);
    x2 = NGCALLF(cmux,CMUX)(&k);
    if( x1 != x2 ) {
        printf( "c_cmux test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmux test SUCCESSFUL\n" );
    }
    x1 = c_cmuy(2);
    x2 = NGCALLF(cmuy,CMUY)(&k);
    if( x1 != x2 ) {
        printf( "c_cmuy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmuy test SUCCESSFUL\n" );
    }
    x1 = c_cpfx(2);
    x2 = NGCALLF(cpfx,CPFX)(&k);
    if( x1 != x2 ) {
        printf( "c_cpfx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpfx test SUCCESSFUL\n" );
    }
    x1 = c_cpfy(2);
    x2 = NGCALLF(cpfy,CPFY)(&k);
    if( x1 != x2 ) {
        printf( "c_cpfy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpfy test SUCCESSFUL\n" );
    }
    x1 = c_cpux(2);
    x2 = NGCALLF(cpux,CPUX)(&k);
    if( x1 != x2 ) {
        printf( "c_cpux test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpux test SUCCESSFUL\n" );
    }
    x1 = c_cpuy(2);
    x2 = NGCALLF(cpuy,CPUY)(&k);
    if( x1 != x2 ) {
        printf( "c_cpuy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpuy test SUCCESSFUL\n" );
    }
    k1 = c_kfmx(.5);
    k2 = NGCALLF(kfmx,KFMX)(&x);
    if( k1 != k2 ) {
        printf( "c_kfmx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfmx test SUCCESSFUL\n" );
    }
    k1 = c_kfmy(.5);
    k2 = NGCALLF(kfmy,KFMY)(&x);
    if( k1 != k2 ) {
        printf( "c_kfmy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfmy test SUCCESSFUL\n" );
    }
    k1 = c_kumx(.5);
    k2 = NGCALLF(kumx,KUMX)(&x);
    if( k1 != k2 ) {
        printf( "c_kumx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kumx test SUCCESSFUL\n" );
    }
    k1 = c_kumy(.5);
    k2 = NGCALLF(kumy,KUMY)(&x);
    if( k1 != k2 ) {
        printf( "c_kumy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kumy test SUCCESSFUL\n" );
    }
    k1 = c_kfpx(.5);
    k2 = NGCALLF(kfpx,KFPX)(&x);
    if( k1 != k2 ) {
        printf( "c_kfpx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfpx test SUCCESSFUL\n" );
    }
    k1 = c_kfpy(.5);
    k2 = NGCALLF(kfpy,KFPY)(&x);
    if( k1 != k2 ) {
        printf( "c_kfpy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfpy test SUCCESSFUL\n" );
    }
    k1 = c_kupx(.5);
    k2 = NGCALLF(kupx,KUPX)(&x);
    if( k1 != k2 ) {
        printf( "c_kupx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kupx test SUCCESSFUL\n" );
    }
    k1 = c_kupy(.5);
    k2 = NGCALLF(kupy,KUPY)(&x);
    if( k1 != k2 ) {
        printf( "c_kupy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kupy test SUCCESSFUL\n" );
    }
    k1 = c_kmpx(2);
    k2 = NGCALLF(kmpx,KMPX)(&k);
    if( k1 != k2 ) {
        printf( "c_kmpx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kmpx test SUCCESSFUL\n" );
    }
    k1 = c_kmpy(2);
    k2 = NGCALLF(kmpy,KMPY)(&k);
    if( k1 != k2 ) {
        printf( "c_kmpy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kmpy test SUCCESSFUL\n" );
    }
    k1 = c_kpmx(2);
    k2 = NGCALLF(kpmx,KPMX)(&k);
    if( k1 != k2 ) {
        printf( "c_kpmx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kpmx test SUCCESSFUL\n" );
    }
    k1 = c_kpmy(2);
    k2 = NGCALLF(kpmy,KPMY)(&k);
    if( k1 != k2 ) {
        printf( "c_kpmy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kpmy test SUCCESSFUL\n" );
    }
    c_frstpt(.5,.5);
    c_fl2int(.5,.5,&ix1,&iy1);
    ix2 = c_kmpx(ix1);
    iy2 = c_kmpy(iy1);
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}
