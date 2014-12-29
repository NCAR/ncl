#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(buttfilt,BUTTFILT)(double *, double *,double *, 
                                       double *, double *, double *, 
                                       int *, int *, int *, int *);

NhlErrorTypes dim_bfband_n_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *xr;
  double *tmp_xr;
  int       ndims_xr;
  ng_size_t dsizes_xr[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xr;

/*
 * Argument # 1
 */
  int *m;
/*
 * Argument # 2
 */
  void *fca;
  double *tmp_fca;
  NclBasicDataTypes type_fca;

/*
 * Argument # 3
 */
  void *fcb;
  double *tmp_fcb;
  NclBasicDataTypes type_fcb;

/*
 * Argument # 4
 */
  void *dt;
  double *tmp_dt;
  NclBasicDataTypes type_dt;

/*
 * Argument # 5
 */
  int *iflag;
/*
 * Argument # 6
 */
  int *iret;
/*
 * Argument # 7
 */
  int *dims;
  ng_size_t ndims;
/*
 * Return variable
 */
  void *bf;
  int ndims_bf;
  double *tmp_yr, *tmp_er;
  ng_size_t *dsizes_bf;
  NclBasicDataTypes type_bf;

/*
 * Various
 */
  ng_size_t i, nx, total_nl, total_nr, nrnx;
  ng_size_t index_xr, index_nrx, size_xr, size_output;
  int j, inx, ret, ier;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  xr = (void*)NclGetArgValue(
           0,
           8,
           &ndims_xr,
           dsizes_xr,
           NULL,
           NULL,
           &type_xr,
           DONT_CARE);

/*
 * Get argument # 1
 */
  m = (int*)NclGetArgValue(
           1,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 2
 */
  fca = (void*)NclGetArgValue(
           2,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_fca,
           DONT_CARE);
/*
 * Get argument # 3
 */
  fcb = (void*)NclGetArgValue(
           3,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_fcb,
           DONT_CARE);
/*
 * Get argument # 4
 */
  dt = (void*)NclGetArgValue(
           4,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_dt,
           DONT_CARE);
/*
 * Get argument # 5
 */
  iflag = (int*)NclGetArgValue(
           5,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  if(*iflag < 0 || *iflag > 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: iflag must be 0 or 1");
    return(NhlFATAL);
  }

/*
 * Get argument # 6
 *
 * This argument represents what is to be returned by 
 * this function.
 *
 *   iret = 0: return yr only (same dimensionality as x)
 *   iret = 1: return er only (same dimensionality as x)
 *   iret = 2: return yr and er (2 x [dimensionality of x])
 */
  iret = (int*)NclGetArgValue(
           6,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  if(*iret < 0 || *iret > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: iret must be 0, 1, or 2");
    return(NhlFATAL);
  }
/*
 * Get argument # 7
 */
  dims = (int *)NclGetArgValue(7,8,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(ndims > ndims_xr) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: too many dimensions in dimension argument, can't continue");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_xr) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Invalid dimension argument, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Calculate size and dimension sizes of output array.
 * See description for iret above.
 */
  if(*iret == 2) ndims_bf = ndims_xr + 1;
  else           ndims_bf = ndims_xr;

  dsizes_bf = (ng_size_t*)calloc(ndims_bf,sizeof(ng_size_t));  
  if( dsizes_bf == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  if(*iret == 2) dsizes_bf[0] = 2;
  for(i = 0; i < ndims_xr; i++) 
    dsizes_bf[i+(ndims_bf-ndims_xr)] = dsizes_xr[i];


/*
 * Calculate number of leftmost, rightmost, and middle elements.
 */
  nx = total_nl = total_nr = 1;
  for(i = 0; i < ndims ; i++)                 nx = nx*dsizes_xr[dims[i]];
  for(i = 0; i < dims[0]; i++)                total_nl *= dsizes_xr[i];
  for(i = dims[ndims-1]+1; i < ndims_xr; i++) total_nr *= dsizes_xr[i];

/*
 * Calculate xr and output sizes.
 */
  size_xr = total_nr * total_nl * nx;
  if(*iret == 2) size_output = size_xr * 2;
  else           size_output = size_xr;

  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx = (int) nx;

/*
 * Coerce fca, fcb, dt to double, if needed.
 */
  tmp_fca = coerce_input_double(fca,type_fca,1,0,NULL,NULL);
  tmp_fcb = coerce_input_double(fcb,type_fcb,1,0,NULL,NULL);
  tmp_dt  = coerce_input_double(dt,type_dt,  1,0,NULL,NULL);
  if(tmp_fca == NULL || tmp_fcb == NULL || tmp_dt == NULL) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Unable to allocate memory for coercing input scalars to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for input array no matter what, because it 
 * may not be contiguous in memory.
 */
  tmp_xr = (double *)calloc(nx,sizeof(double));
  if(tmp_xr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Return type.
 */
  if(type_xr != NCL_double) type_bf = NCL_float;
  else                      type_bf = NCL_double;

/* 
 * Allocate space for output array.
 */
  if(type_bf != NCL_double) bf = (void *)calloc(size_output, sizeof(float));
  else                      bf = (void *)calloc(size_output, sizeof(double));
  if(bf == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for subset of output array.
 */
  tmp_yr = (double *)calloc(nx, sizeof(double));
  tmp_er = (double *)calloc(nx, sizeof(double));
  if(tmp_yr == NULL || tmp_er == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_bfband_n: Unable to allocate memory for temporary output arrays");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost/rightmost dimensions and call 
 * the Fortran routine for each subsection of the 
 * input arrays.
 */
  nrnx = total_nr * nx;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_xr = index_nrx + j;
/*
 * Coerce subsection of x (tmp_xr) to double if necessary.
 */
      coerce_subset_input_double_step(xr,tmp_xr,index_xr,total_nr,type_xr,
                                      nx,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      printf("xr[1]  = %g\n", tmp_xr[0]);
      printf("xr[nx] = %g\n", tmp_xr[nx-1]);
      NGCALLF(buttfilt,BUTTFILT)(tmp_xr, tmp_yr, tmp_er, tmp_fca, tmp_fcb, 
                                 tmp_dt, m, &inx, iflag, &ier);

      printf("xbf[1]  = %g\n", tmp_yr[0]);
      printf("xbf[nx] = %g\n", tmp_yr[nx-1]);
/*
 * Copy/coerce back to output array
 */
      if(*iret == 0) {
        coerce_output_float_or_double_step(bf,tmp_yr,type_bf,nx,
                                           index_xr,total_nr);
      }
      else if(*iret == 1) {
        coerce_output_float_or_double_step(bf,tmp_er,type_bf,nx,
                                           index_xr,total_nr);
      }
      else if(*iret == 2) {
        coerce_output_float_or_double_step(bf,tmp_yr,type_bf,nx,
                                           index_xr,total_nr);
        coerce_output_float_or_double_step(bf,tmp_er,type_bf,nx,
                                           index_xr+size_xr,total_nr);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_xr);
  NclFree(tmp_er);
  NclFree(tmp_yr);
  if(type_fca != NCL_double) NclFree(tmp_fca);
  if(type_fcb != NCL_double) NclFree(tmp_fcb);
  if(type_dt  != NCL_double) NclFree(tmp_dt);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(bf,ndims_bf,dsizes_bf,NULL,type_bf,0);
  NclFree(dsizes_bf);
  return(ret);
}
