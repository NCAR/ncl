#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <math.h>

extern void NGCALLF(ddtrndx,DDTRNDX)(double*,int*,int*,double*,double*,
                                     double*,double*,int*);

NhlErrorTypes dtrend_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x, found_missing;
  NclScalar missing_x, missing_dx, missing_rx;
  logical *return_slope;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *dtrend_x, *slope;
  double xmean, xvari, xvaro;
/*
 * Attribute variables
 */
  int att_id, dsizes_slope[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_x, npts, size_leftmost, size_x, ier, iopt = 1;
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
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce data to double no matter what, since input array also becomes
 * output array. 
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Compute size of slope.
 */
  if(*return_slope) {
    if(type_x != NCL_double) {
      slope = (void *)calloc(size_leftmost,sizeof(double));
    }
    else {
      slope = (void *)calloc(size_leftmost,sizeof(float));
    }
    if( slope == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Cannot allocate space for slope");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    dtrend_x = (void*)calloc(size_x,sizeof(float));
  }
  else {
    dtrend_x = (void*)calloc(size_x,sizeof(double));
  }
  if( dtrend_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  index_x = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_x,npts,has_missing_x,
                                     missing_x.doubleval);
    if(found_missing) {
      set_subset_output_missing(dtrend_x,index_x,type_x,npts,
                                missing_dx.doubleval);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dtrend: An input array contains missing values. No dtrending performed on this array.");
    }
    else {
      NGCALLF(ddtrndx,DDTRNDX)(tmp_x,&npts,&iopt,&xmean,&xvari,&xvaro,
                               c,&ier);

      for(j = 0; j < npts; j++) {
        if(type_x != NCL_double) {
          ((float*)dtrend_x)[index_x+j] = (float)(tmp_x[j]);
        }
        else {
          ((double*)dtrend_x)[index_x+j] = tmp_x[j];
        }
      }

      if(*return_slope) {
        if(type_x != NCL_double) {
          ((float*)slope)[i] = (float)c[1];
        }
        else {
          ((double*)slope)[i] = c[1];
        }
      }
    }
    index_x += npts;
  }
/*
 * Free memory.
 */
  NclFree(tmp_x);

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
 */
      if(has_missing_x) {
        return_md = _NclCreateVal(
                                NULL,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                dtrend_x,
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
                                dtrend_x,
                                NULL,
                                ndims_x,
                                dsizes_x,
                                TEMPORARY,
                                NULL,
                                (NclObjClass)nclTypefloatClass
                                );
      }
    }
    else {
/* 
 * Input was double, so return double values.
 */
      if(has_missing_x) {
        return_md = _NclCreateVal(
                                  NULL,
                                  NULL,
                                  Ncl_MultiDValData,
                                  0,
                                  dtrend_x,
                                  &missing_dx,
                                  ndims_x,
                                  dsizes_x,
                                  TEMPORARY,
                                  NULL,
                                  (NclObjClass)nclTypedoubleClass
                                  );
      }
      else {
        return_md = _NclCreateVal(
                                NULL,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                dtrend_x,
                                NULL,
                                ndims_x,
                                dsizes_x,
                                TEMPORARY,
                                NULL,
                                (NclObjClass)nclTypedoubleClass
                                );
      }
    }
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
    dsizes_slope[0] = size_leftmost;
/*
 * Set up float attribute to return.
 */
    if(type_x != NCL_double) {
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
                             (NclObjClass)nclTypefloatClass
                             );
    }
    else {
/*
 * Set up double attribute to return.
 */
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
    }
    _NclAddAtt(
               att_id,
               "slope",
               att_md,
               NULL
               );

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
 * stuff.
 */
    if(type_x != NCL_double) {
      if(has_missing_x) {
        return(NclReturnValue(dtrend_x,ndims_x,dsizes_x,&missing_rx,
                              NCL_float,0));
      }
      else {
        return(NclReturnValue(dtrend_x,ndims_x,dsizes_x,NULL,NCL_float,0));
      }
    }
    else {
      if(has_missing_x) {
        return(NclReturnValue(dtrend_x,ndims_x,dsizes_x,&missing_dx,
                              NCL_double,0));
      }
      else {
        return(NclReturnValue(dtrend_x,ndims_x,dsizes_x,NULL,NCL_double,0));
      }
    }
  }
}

