#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <math.h>
#include <ncarg/gks.h>

extern void NGCALLF(ddtrndx,DDTRNDX)(double*,int*,int*,double*,double*,
                                     double*,double*,int*);

NhlErrorTypes dtrend_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x, found_missing;
  NclScalar missing_x, missing_dx, missing_rx;
  logical *return_slope;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  double xmean, xvari, xvaro;
  float *rx;
/*
 * Attribute variables
 */
  int att_id, dsizes_slope[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *slope;
  float *rslope;
/*
 * Declare various variables for random purposes.
 */
  int i, j, l, npts, size_leftmost, size_x, ier, iopt = 1;
  double c[3];
/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

  return_slope = (logical*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check input sizes.
 */
  npts = dsizes_x[ndims_x-1];
  if( npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: The last dimension of x must be greater than 2");
    return(NhlFATAL);
  }
  
/*
 * Compute the total size of the output array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) size_leftmost *= dsizes_x[i];
  size_x = size_leftmost * npts;

/*
 * Check for missing values.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(type_x != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_x,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }

/*
 * Coerce data to double no matter what, since input array also becomes
 * output array. 
 */
  dx = (double*)NclMalloc(sizeof(double)*size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               size_x,
               &missing_dx,
               &missing_x,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               size_x,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }

/*
 * Compute size of slope.
 */
  if(*return_slope) {
    slope = (double *)NclMalloc(size_leftmost*sizeof(double));
    if( slope == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Cannot allocate space for 'slope'");
      return(NhlFATAL);
    }
  }
  
/*
 * Call the Fortran version of this routine.
 */
  l = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Check for missing values.
 */
    found_missing = 0;
    if(has_missing_x) {
      j = 0;
      while( j < npts && !found_missing ) {
        if(dx[l+j] == missing_dx.doubleval) found_missing = 1;
        j++;
      }
    }
    if(found_missing) {
      for(j = 0; j < npts; j++) dx[l+j] = missing_dx.doubleval;
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dtrend: An input array contains missing values. No dtrending performed on this array.");
    }
    else {
      NGCALLF(ddtrndx,DDTRNDX)(&dx[l],&npts,&iopt,&xmean,&xvari,&xvaro,
                               c,&ier);
      if(*return_slope) slope[i] = c[1];
    }
    l += npts;
  }

  if(type_x != NCL_double) {
/*
 * Input is not double, so copy double values to float values.
 *
 * First allocate space for float array.
 */
    rx = (float*)NclMalloc(sizeof(float)*size_x);
    if( rx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
/*
 * Copy double values to float array.
 */
    for( i = 0; i < size_x; i++ ) rx[i] = (float)dx[i];
/*
 * Free double precision values.
 */
    NclFree(dx);
  }

/*
 * Get ready to return all this stuff to NCL.
 */
  if(*return_slope) {
/*
 * The slope will be returned as an attribute.
 */
    if(type_x != NCL_double) {
/*
 * Input is not double, so return float values.
 *
 * First copy double values to float values.
 */
      rslope = (float*)NclMalloc(size_leftmost*sizeof(float));
      if( rslope == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for returning slope value");
        return(NhlFATAL);
      }
/*
 * Copy double precision slope values to float array. 
 */
      for(i = 0; i < size_leftmost; i++ ) rslope[i] = (double)slope[i];
/*
 * Free double precision slope array.
 */
      NclFree(slope);
/* 
 * Set up return float array.
 */
      if(has_missing_x) {
        return_md = _NclCreateVal(
                                  NULL,
                                  NULL,
                                  Ncl_MultiDValData,
                                  0,
                                  (void*)rx,
                                  &missing_rx,
                                  ndims_x,
                                  dsizes_x,
                                  TEMPORARY,
                                  NULL,
                                  (NclObjClass)nclTypefloatClass
                                  );
      }
      else {
        return_md = _NclCreateVal(
                                  NULL,
                                  NULL,
                                  Ncl_MultiDValData,
                                  0,
                                  (void*)rx,
                                  NULL,
                                  ndims_x,
                                  dsizes_x,
                                  TEMPORARY,
                                  NULL,
                                  (NclObjClass)nclTypefloatClass
                                  );
      }
/*
 * Set up attributes to return.
 */
      att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

      dsizes_slope[0] = size_leftmost;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             rslope,
                             NULL,
                             1,
                             dsizes_slope,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "slope",
                 att_md,
                 NULL
                 );
    }
    else {
/* 
 * Input was double, so return double values. The missing value might
 * be NULL.
 */
      return_md = _NclCreateVal(
                                NULL,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                (void*)dx,
                                &missing_dx,
                                ndims_x,
                                dsizes_x,
                                TEMPORARY,
                                NULL,
                                (NclObjClass)nclTypedoubleClass
                                );
/*
 * Set up attributes to return.
 */
      att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

      dsizes_slope[0] = size_leftmost;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             slope,
                             NULL,
                             1,
                             dsizes_slope,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "slope",
                 att_md,
                 NULL
                 );
    
    }
    tmp_var = _NclVarCreate(
                            NULL,
                            NULL,
                            Ncl_Var,
                            0,
                            NULL,
                            return_md,
                            NULL,
                            att_id,
                            NULL,
                            RETURNVAR,
                            NULL,
                            TEMPORARY
                            );
/*
 * Return output grid and attributes to NCL.
 */
    return_data.kind = NclStk_VAR;
    return_data.u.data_var = tmp_var;
    _NclPlaceReturn(return_data);
    return(NhlNOERROR);
  }
  else {
/*
 * No slope is being returned, so we don't need to do all that attribute
 * stuff. The missing value might actually be NULL.
 */
    if(type_x != NCL_double) {
      if(has_missing_x) {
        return(NclReturnValue((void*)rx,ndims_x,dsizes_x,&missing_rx,
                              NCL_float,0));
      }
      else {
        return(NclReturnValue((void*)rx,ndims_x,dsizes_x,NULL,NCL_float,0));
      }
    }
    else {
      if(has_missing_x) {
        return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_dx,
                              NCL_double,0));
      }
      else {
        return(NclReturnValue((void*)dx,ndims_x,dsizes_x,NULL,NCL_double,0));
      }
    }
  }
}

