#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

extern void NGCALLF(rdsstoi,RDSSTOI)(int *,int *,int *,int *,int *,float *);

NhlErrorTypes rdsstoi_W( void )
{
/*
 * Input array variables
 */
    int *nyrstrt, *nyrlast, *mlon, *nlat, *info;
    int ndims_info, dsizes_info[NCL_MAX_DIMENSIONS];
/*
 * Output array variables
 */
    float *sstoi;
    int ndims_sstoi = 2, dsizes_sstoi[2] = {180,360};
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
                               2);
    nyrlast = (int*)NclGetArgValue(
                               1,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               2);
    mlon= (int*)NclGetArgValue(
                               2,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               2);
    nlat= (int*)NclGetArgValue(
                               3,
                               5,
                               NULL, 
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               2);
    info = (int*)NclGetArgValue(
                               4,
                               5,
                               &ndims_info, 
                               dsizes_info,
                               NULL,
                               NULL,
                               NULL,
                               1);
/*
 * "info" must be an array of 9 elements. 
 */
    if( ndims_info != 1 && dsizes_info[0] != 9 ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"rdsstoi: The 'info' array must be an integer array of nine elements");
    return(NhlFATAL);
  }
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
