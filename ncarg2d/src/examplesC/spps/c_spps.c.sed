/*
 * $Id: c_spps.c.sed,v 1.1 1994-05-13 14:28:57 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    int ival, ix1, iy1, ix2, iy2, k, k1, k2, jx, jy;
    float px, py, c, x, x1, x2;
    extern float cufx_(
#ifdef NeedFuncProto
    float *ix
#endif
    );
    extern float cufy_(
#ifdef NeedFuncProto
    float *iy
#endif
    );
    extern float cfux_(
#ifdef NeedFuncProto
    float *ix
#endif
    );
    extern float cfuy_(
#ifdef NeedFuncProto
    float *iy
#endif
    );
    extern float cmfx_(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float cmfy_(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern float cpfx_(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float cpfy_(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern float cmux_(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float cmuy_(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern float cpux_(
#ifdef NeedFuncProto
    int *ix
#endif
    );
    extern float cpuy_(
#ifdef NeedFuncProto
    int *iy
#endif
    );
    extern int kfmx_(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int kfmy_(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int kumx_(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int kumy_(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int kfpx_(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int kfpy_(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int kupx_(
#ifdef  NeedFuncProto
    float *ix
#endif
    );
    extern int kupy_(
#ifdef  NeedFuncProto
    float *iy
#endif
    );
    extern int kmpx_(
#ifdef  NeedFuncProto
    int *ix
#endif
    );
    extern int kmpy_(
#ifdef  NeedFuncProto
    int *iy
#endif
    );
    extern int kpmx_(
#ifdef  NeedFuncProto
    int *ix
#endif
    );
    extern int kpmy_(
#ifdef  NeedFuncProto
    int *iy
#endif
    );

    c_opngks();
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
    x2 = (float)cfux_(&x);
    if( x1 != x2 ) {
        printf( "c_cfux test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cfux test SUCCESSFUL\n" );
    }
    x1 = c_cfuy(.5);
    x2 = (float)cfuy_(&x);
    if( x1 != x2 ) {
        printf( "c_cfuy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cfuy test SUCCESSFUL\n" );
    }
    x1 = c_cufx(.5);
    x2 = (float)cufx_(&x);
    if( x1 != x2 ) {
        printf( "c_cufx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cufx test SUCCESSFUL\n" );
    }
    x1 = c_cufy(.5);
    x2 = (float)cufy_(&x);
    if( x1 != x2 ) {
        printf( "c_cufy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cufy test SUCCESSFUL\n" );
    }
    x1 = c_cmfx(2);
    x2 = (float)cmfx_(&k);
    if( x1 != x2 ) {
        printf( "c_cmfx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmfx test SUCCESSFUL\n" );
    }
    x1 = c_cmfy(2);
    x2 = cmfy_(&k);
    if( x1 != x2 ) {
        printf( "c_cmfy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmfy test SUCCESSFUL\n" );
    }
    x1 = c_cmux(2);
    x2 = cmux_(&k);
    if( x1 != x2 ) {
        printf( "c_cmux test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmux test SUCCESSFUL\n" );
    }
    x1 = c_cmuy(2);
    x2 = cmuy_(&k);
    if( x1 != x2 ) {
        printf( "c_cmuy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cmuy test SUCCESSFUL\n" );
    }
    x1 = c_cpfx(2);
    x2 = cpfx_(&k);
    if( x1 != x2 ) {
        printf( "c_cpfx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpfx test SUCCESSFUL\n" );
    }
    x1 = c_cpfy(2);
    x2 = cpfy_(&k);
    if( x1 != x2 ) {
        printf( "c_cpfy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpfy test SUCCESSFUL\n" );
    }
    x1 = c_cpux(2);
    x2 = cpux_(&k);
    if( x1 != x2 ) {
        printf( "c_cpux test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpux test SUCCESSFUL\n" );
    }
    x1 = c_cpuy(2);
    x2 = cpuy_(&k);
    if( x1 != x2 ) {
        printf( "c_cpuy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_cpuy test SUCCESSFUL\n" );
    }
    k1 = c_kfmx(.5);
    k2 = kfmx_(&x);
    if( k1 != k2 ) {
        printf( "c_kfmx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfmx test SUCCESSFUL\n" );
    }
    k1 = c_kfmy(.5);
    k2 = kfmy_(&x);
    if( k1 != k2 ) {
        printf( "c_kfmy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfmy test SUCCESSFUL\n" );
    }
    k1 = c_kumx(.5);
    k2 = kumx_(&x);
    if( k1 != k2 ) {
        printf( "c_kumx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kumx test SUCCESSFUL\n" );
    }
    k1 = c_kumy(.5);
    k2 = kumy_(&x);
    if( k1 != k2 ) {
        printf( "c_kumy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kumy test SUCCESSFUL\n" );
    }
    k1 = c_kfpx(.5);
    k2 = kfpx_(&x);
    if( k1 != k2 ) {
        printf( "c_kfpx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfpx test SUCCESSFUL\n" );
    }
    k1 = c_kfpy(.5);
    k2 = kfpy_(&x);
    if( k1 != k2 ) {
        printf( "c_kfpy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kfpy test SUCCESSFUL\n" );
    }
    k1 = c_kupx(.5);
    k2 = kupx_(&x);
    if( k1 != k2 ) {
        printf( "c_kupx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kupx test SUCCESSFUL\n" );
    }
    k1 = c_kupy(.5);
    k2 = kupy_(&x);
    if( k1 != k2 ) {
        printf( "c_kupy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kupy test SUCCESSFUL\n" );
    }
    k1 = c_kmpx(2);
    k2 = kmpx_(&k);
    if( k1 != k2 ) {
        printf( "c_kmpx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kmpx test SUCCESSFUL\n" );
    }
    k1 = c_kmpy(2);
    k2 = kmpy_(&k);
    if( k1 != k2 ) {
        printf( "c_kmpy test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kmpy test SUCCESSFUL\n" );
    }
    k1 = c_kpmx(2);
    k2 = kpmx_(&k);
    if( k1 != k2 ) {
        printf( "c_kpmx test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_kpmx test SUCCESSFUL\n" );
    }
    k1 = c_kpmy(2);
    k2 = kpmy_(&k);
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
    c_clsgks();
}
