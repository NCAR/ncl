#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(rdsstoi,RDSSTOI)(int *,int *,int *,int *,int *,float *);

NhlErrorTypes rdsstoi_W( void )
{
/*
 * Input array variables
 */
    int *nyrstrt, *nyrlast, *mlon, *nlat, *info;
/*
 * Output array variables
 */
    float *sstoi;
    int ndims_sstoi = 2;
    ng_size_t dsizes_sstoi[2] = {180,360};
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
    nyrstrt = (int*)NclGetArgValue(
                               0,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               DONT_CARE);
    nyrlast = (int*)NclGetArgValue(
                               1,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               DONT_CARE);
    mlon= (int*)NclGetArgValue(
                               2,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               DONT_CARE);
    nlat= (int*)NclGetArgValue(
                               3,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               DONT_CARE);
    info = (int*)NclGetArgValue(
                               4,
                               5,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               1);
/*
 * Allocate space for output array.
 */
    sstoi = (float*)NclMalloc(360*180*sizeof(float));
    if( sstoi == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"rdsstoi: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

    NGCALLF(rdsstoi,RDSSTOI)(nyrstrt,nyrlast,mlon,nlat,info,sstoi);

    return(NclReturnValue((void*)sstoi,ndims_sstoi,dsizes_sstoi,NULL,NCL_float,0));
}
