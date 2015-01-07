#include <stdio.h>
#include <string.h>
#include <math.h>
#include "wrapper.h"

extern ng_size_t indx(ng_size_t,ng_size_t);

NhlErrorTypes fftshift_W( void )
{

/*
 * Input variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  int ndims_x;
  ng_size_t *dsizes_x;
  int *kmode;
/*
 * Return variable
 */
  void *xshift;

/*
 * Various
 */
  ng_size_t i, j, k, nleft, nrows, ncols, nrowcol, size_x;
  ng_size_t left_pos, x_pos, xshift_pos;
  int type_size, ret;

/*
 * Retrieve input.
 */
  data = _NclGetArg(0,2,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  default:
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fftshift: invalid first input argument.");
    return(NhlFATAL);
  }
  ndims_x   = tmp_md->multidval.n_dims;
  dsizes_x  = tmp_md->multidval.dim_sizes;
  type_size = tmp_md->multidval.type->type_class.size;

  kmode = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/* 
 * Allocate space for return array.
 */
  size_x = 1;
  for(i = 0; i < ndims_x; i++) size_x *= dsizes_x[i];
  xshift = (void*)NclMalloc(size_x*type_size);
  if( xshift == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fftshift: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  if(ndims_x == 1) {
    nrows = 1;
    ncols = dsizes_x[0];
    nleft = 1;
  }
  else if(ndims_x == 2) {
    nrows = dsizes_x[0];
    ncols = dsizes_x[1];
    nleft = 1;
  }
  else {
    nrows = dsizes_x[ndims_x-2];
    ncols = dsizes_x[ndims_x-1];
    nleft = size_x / (nrows*ncols);
  }
  nrowcol = nrows * ncols;


  if (*kmode == 0) {                           /* xshift(indx(j),indx(i)) = x(j,i) */
    for(k = 0; k < nleft; k++) { 
      left_pos = k*nrowcol;
      for(j = 0; j < nrows; j++) {
        x_pos      = j*ncols;
        xshift_pos = indx(j,nrows)*ncols;
        for(i = 0; i < ncols; i++) {
          memcpy((void*)((char*)xshift + (left_pos+xshift_pos+indx(i,ncols))*type_size),
                 (void*)((char*)tmp_md->multidval.val + (left_pos+x_pos+i)*type_size),
                 type_size);
        }
      }
    }
  }
  else if (*kmode < 0) {                       /* xshift(indx(j),i) = x(j,i) */
    for(k = 0; k < nleft; k++) { 
      left_pos = k*nrowcol;
      for(j = 0; j < nrows; j++) {
        x_pos      = j*ncols;
        xshift_pos = indx(j,nrows)*ncols;
        for(i = 0; i < ncols; i++) {
          memcpy((void*)((char*)xshift + (left_pos+xshift_pos+i)*type_size),
                 (void*)((char*)tmp_md->multidval.val + 
                         (left_pos+x_pos+i)*type_size),type_size);
        }
      }
    }
  }
  else {                       /* xshift(j,indx(i)) = x(j,i) */
    for(k = 0; k < nleft; k++) { 
      left_pos = k*nrowcol;
      for(j = 0; j < nrows; j++) {
        x_pos      = j*ncols;
        xshift_pos = x_pos;
        for(i = 0; i < ncols; i++) {
          memcpy((void*)((char*)xshift + (left_pos+xshift_pos+indx(i,ncols))*type_size),
                 (void*)((char*)tmp_md->multidval.val + (left_pos+x_pos+i)*type_size),
                 type_size);
        }
      }
    }
  }
/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(xshift,ndims_x,dsizes_x,NULL,
                       tmp_md->multidval.data_type,0);
  return(ret);
}


ng_size_t indx(ng_size_t i,ng_size_t nrc)
{
  return((i+nrc/2) % nrc);
}

