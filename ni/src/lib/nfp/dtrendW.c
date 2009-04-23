#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(ddtrndx,DDTRNDX)(double*,int*,int*,double*,double*,
                                     double*,double*,int*);

extern void NGCALLF(ddtrndmsg,DDTRNDMSG)(double*,double*,int*,double*,
                                         double*,int*,double*,double*,
                                         double*,int*);

NhlErrorTypes dtrend_W( void )
{
/*
 * Input array variables
 */
  void *y;
  double *tmp_y;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_y, missing_dy, missing_ry;
  logical *return_slope;
  NclBasicDataTypes type_y, type_dtrend_y;
/*
 * Output array variables
 */
  void *dtrend_y, *slope, *yintp;
  double xmean, yvari, yvaro;
/*
 * Attribute variables
 */
  int att_id, dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_y, npts, size_leftmost, size_y, ier, iopt = 1;
  double c[3];
/*
 * Retrieve arguments.
 */
  y = (void*)NclGetArgValue(
          0,
          2,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  return_slope = (logical*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check input sizes.
 */
  npts = dsizes_y[ndims_y-1];
  if( npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: The last dimension of x must be greater than 2");
    return(NhlFATAL);
  }
  
/*
 * Compute the total size of the output array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_y-1; i++ ) size_leftmost *= dsizes_y[i];
  size_y = size_leftmost * npts;

/*
 * Check if the _FillValue attribute is set. If so, print a warning
 * message that this routine doesn't do anything special with missing
 * values, and will actually end up using these values, if they exist,
 * in calculations. 
 *
 * As of Jan 21, 2009, Dennis Shea decided he didn't want the warning
 * message below, so it's been commented out.
 */
/*
  if(has_missing_y) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dtrend: 'y' contains a _FillValue attribute, which means your data may contain missing values.\nThis function doesn't check for missing values, and hence they will get used in the calculation.\nYou may want to consider using 'dtrend_msg' instead.");
  }
*/
/*
 * Coerce the missing value to both float and double so that when
 * we return the variable later, we can set the appropriate missing
 * value depending on the type.
 */
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);

/*
 * Coerce data to double no matter what, since input array also becomes
 * output array. 
 */
  tmp_y = (double*)calloc(npts,sizeof(double));
  if( tmp_y == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for coercing y array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_y != NCL_double) {
    type_dtrend_y = NCL_float;
    dtrend_y = (void*)calloc(size_y,sizeof(float));
  }
  else {
    type_dtrend_y = NCL_double;
    dtrend_y = (void*)calloc(size_y,sizeof(double));
  }
  if( dtrend_y == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Compute size of slope and y intercept.
 */
  if(*return_slope) {
    if(type_dtrend_y != NCL_double) {
      slope = (void *)calloc(size_leftmost,sizeof(float));
      yintp = (void *)calloc(size_leftmost,sizeof(float));
    }
    else {
      slope = (void *)calloc(size_leftmost,sizeof(double));
      yintp = (void *)calloc(size_leftmost,sizeof(double));
    }
    if( slope == NULL || yintp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Cannot allocate space for slope and y-intercept");
      return(NhlFATAL);
    }
  }
/*
 * Call the Fortran version of this routine.
 */
  index_y = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
    coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);

    NGCALLF(ddtrndx,DDTRNDX)(tmp_y,&npts,&iopt,&xmean,&yvari,&yvaro,
                             c,&ier);
/*
 * Copy output back out as float or double.
 */
    coerce_output_float_or_double(dtrend_y,tmp_y,type_dtrend_y,npts,
                                  index_y);
    if(*return_slope) {
      coerce_output_float_or_double(yintp,&c[0],type_dtrend_y,1,i);
      coerce_output_float_or_double(slope,&c[1],type_dtrend_y,1,i);
    }
    index_y += npts;
  }
/*
 * Free memory.
 */
  NclFree(tmp_y);

/*
 * Get ready to return all this stuff to NCL.
 */
  if(*return_slope) {
/*
 * The slope will be returned as an attribute.
 */
    if(type_y != NCL_double) {
/*
 * Input is not double, so return float values.
 */
      if(has_missing_y) {
        return_md = _NclCreateVal(
                                NULL,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                dtrend_y,
                                &missing_ry,
                                ndims_y,
                                dsizes_y,
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
                                dtrend_y,
                                NULL,
                                ndims_y,
                                dsizes_y,
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
      if(has_missing_y) {
        return_md = _NclCreateVal(
                                  NULL,
                                  NULL,
                                  Ncl_MultiDValData,
                                  0,
                                  dtrend_y,
                                  &missing_dy,
                                  ndims_y,
                                  dsizes_y,
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
                                dtrend_y,
                                NULL,
                                ndims_y,
                                dsizes_y,
                                TEMPORARY,
                                NULL,
                                (NclObjClass)nclTypedoubleClass
                                );
      }
    }
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
    dsizes[0] = size_leftmost;
/*
 * Set up float attribute to return.
 */
    if(type_y != NCL_double) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             slope,
                             NULL,
                             1,
                             dsizes,
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

      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             yintp,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "y_intercept",
                 att_md,
                 NULL
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
                             dsizes,
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

      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             yintp,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "y_intercept",
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
 * No slope/y-intercept is being returned, so we don't need to do all
 * that attribute stuff.
 */
    if(has_missing_y) {
      if(type_dtrend_y == NCL_float) {
        return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,&missing_ry,
                              type_dtrend_y,0));
      }
      else {
        return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,&missing_dy,
                              type_dtrend_y,0));
      }
    }
    else {
      return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,NULL,type_dtrend_y,0));
    }
  }
}


NhlErrorTypes dtrend_quadratic_W( void )
{
/*
 * Input array variables
 */
  void *y;
  double *tmp_y;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y, found_missing;
  NclScalar missing_y, missing_dy, missing_ry;
  int *option;
  NclBasicDataTypes type_y, type_dtrend_y;
/*
 * Output array variables
 */
  void *dtrend_y;
  double xmean, yvari, yvaro;
/*
 * Declare various variables for random purposes.
 * Setting iopt to 2 removes the quadratic trend.
 */
  int i, j, index_y, npts, size_leftmost, size_y, ier, iopt = 2;
  double c[3];
/*
 * Retrieve arguments.
 */
  y = (void*)NclGetArgValue(
          0,
          2,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  option = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * "option" is not used yet, so we'll just set it to zero.
 */
  *option = 0;

/*
 * Check input sizes.
 */
  npts = dsizes_y[ndims_y-1];
  if( npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_quadratic: The last dimension of x must be greater than 2");
    return(NhlFATAL);
  }
  
/*
 * Compute the total size of the output array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_y-1; i++ ) size_leftmost *= dsizes_y[i];
  size_y = size_leftmost * npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
/*
 * Coerce data to double no matter what, since input array also becomes
 * output array. 
 */
  tmp_y = (double*)calloc(npts,sizeof(double));
  if( tmp_y == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_quadratic: Unable to allocate memory for coercing y array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_y != NCL_double) {
    type_dtrend_y = NCL_float;
    dtrend_y = (void*)calloc(size_y,sizeof(float));
  }
  else {
    type_dtrend_y = NCL_double;
    dtrend_y = (void*)calloc(size_y,sizeof(double));
  }
  if( dtrend_y == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_quadratic: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  index_y = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
    coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_y,npts,has_missing_y,
                                     missing_y.doubleval);
    if(found_missing) {
      set_subset_output_missing(dtrend_y,index_y,type_y,npts,
                                missing_dy.doubleval);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dtrend_quadratic: An input array contains missing values. No dtrending performed on this array.");
    }
    else {

      NGCALLF(ddtrndx,DDTRNDX)(tmp_y,&npts,&iopt,&xmean,&yvari,&yvaro,
                               c,&ier);
/*
 * Copy output back out as float or double.
 */
      coerce_output_float_or_double(dtrend_y,tmp_y,type_dtrend_y,npts,
                                    index_y);
    }
    index_y += npts;
  }
/*
 * Free memory.
 */
  NclFree(tmp_y);

/*
 * Return to NCL.
 */
  if(has_missing_y) {
    if(type_dtrend_y == NCL_float) {
      return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,&missing_ry,
                            type_dtrend_y,0));
    }
    else {
      return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,&missing_dy,
                            type_dtrend_y,0));
    }
  }
  else {
    return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,NULL,type_dtrend_y,0));
  }
}

NhlErrorTypes dtrend_msg_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_y, missing_dy, missing_ry;
  logical *return_slope, *remove_mean;
  int iremove_mean;
  NclBasicDataTypes type_y, type_x, type_dtrend_y;
/*
 * Output array variables
 */
  void *dtrend_y, *slope, *yintp;
  double *ydt, slpe, yint;
/*
 * Attribute variables
 */
  int att_id, dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_y, npts, size_leftmost, size_y, ier, iopt = 1;
/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          4,
          NULL,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  y = (void*)NclGetArgValue(
          1,
          4,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  remove_mean = (logical*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  if(*remove_mean) {
    iremove_mean = 1;
  }
  else {
    iremove_mean = 0;
  }

  return_slope = (logical*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check input sizes.
 */
  npts = dsizes_y[ndims_y-1];
  if( npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_msg: The last dimension of y must be greater than 2");
    return(NhlFATAL);
  }

  if( dsizes_x[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_msg: The length of x must be the same as the rightmost dimension of y");
    return(NhlFATAL);
  }
  
/*
 * Compute the total size of the output array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_y-1; i++ ) size_leftmost *= dsizes_y[i];
  size_y = size_leftmost * npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
/*
 * Coerce x (tmp_x) to double.
 */
  tmp_x = coerce_input_double(x,type_x,npts,0,NULL,NULL);
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_msg: Unable to coerce x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for temporary y to allocate to double later if
 * necessasry.
 */
  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_msg: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array
 */
  if(type_y != NCL_double) {
    type_dtrend_y = NCL_float;
    dtrend_y = (void*)calloc(size_y,sizeof(float));
  }
  else {
    type_dtrend_y = NCL_double;
    dtrend_y = (void*)calloc(size_y,sizeof(double));
  }
  ydt = (double*)calloc(npts,sizeof(double));
  if( dtrend_y == NULL || ydt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_msg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Compute size of slope and y intercept.
 */
  if(*return_slope) {
    if(type_dtrend_y != NCL_double) {
      slope = (void *)calloc(size_leftmost,sizeof(float));
      yintp = (void *)calloc(size_leftmost,sizeof(float));
    }
    else {
      slope = (void *)calloc(size_leftmost,sizeof(double));
      yintp = (void *)calloc(size_leftmost,sizeof(double));
    }
    if( slope == NULL || yintp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend_msg: Cannot allocate space for slope and y-intercept");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  index_y = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
    if(type_y != NCL_double) {
      coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_y to y.
 */
      tmp_y = &((double*)y)[index_y];
    }

    NGCALLF(ddtrndmsg,DDTRNDMSG)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&iremove_mean,ydt,
                                 &slpe,&yint,&ier);

    coerce_output_float_or_double(dtrend_y,ydt,type_dtrend_y,npts,
                                  index_y);
    if(*return_slope) {
      coerce_output_float_or_double(yintp,&yint,type_dtrend_y,1,i);
      coerce_output_float_or_double(slope,&slpe,type_dtrend_y,1,i);
    }

    index_y += npts;
  }
/*
 * Free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  NclFree(ydt);

/*
 * Get ready to return all this stuff to NCL.
 */
  if(*return_slope) {
/*
 * The slope will be returned as an attribute.
 */
    if(type_dtrend_y == NCL_float) {
/*
 * Input is not double, so return float values.
 */
      return_md = _NclCreateVal(
                                NULL,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                dtrend_y,
                                &missing_ry,
                                ndims_y,
                                dsizes_y,
                                TEMPORARY,
                                NULL,
                                (NclObjClass)nclTypefloatClass
                                );
    }
    else {
/* 
 * Input was double, so return double values.
 */
      return_md = _NclCreateVal(
                                NULL,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                dtrend_y,
                                &missing_dy,
                                ndims_y,
                                dsizes_y,
                                TEMPORARY,
                                NULL,
                                (NclObjClass)nclTypedoubleClass
                                );
    }
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
    dsizes[0] = size_leftmost;
/*
 * Set up float attribute to return.
 */
    if(type_dtrend_y == NCL_float) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             slope,
                             NULL,
                             1,
                             dsizes,
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

      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             yintp,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "y_intercept",
                 att_md,
                 NULL
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
                             dsizes,
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

      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             yintp,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "y_intercept",
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
 * No slope/y-intercept is being returned, so we don't need to do all
 * that attribute stuff.
 */
    if(type_dtrend_y == NCL_float) {
      return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,&missing_ry,
                            type_dtrend_y,0));
    }
    else {
      return(NclReturnValue(dtrend_y,ndims_y,dsizes_y,&missing_dy,
                            type_dtrend_y,0));
    }
  }
}
