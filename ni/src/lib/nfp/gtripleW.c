#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"

extern void NGCALLF(grid2triple,GRID2TRIPLE)(double*,double*,double*,int*,
                                             int*,double*,int*,int*,double*,
                                             int*);

extern void NGCALLF(triple2grid1,TRIPLE2GRID1)(int*,double*,double*,double*,
                                               double*,int*,int*,double*,
                                               double*,double*,double*,
                                               int*,int*,double*,int*,int*,
                                               double*,double*,double*,
                                               double*,double*,double*,int*);

extern void NGCALLF(triple2grid2d,TRIPLE2GRID2D)(double *,double *,double *,
                                                 int *,double *,double *,
                                                 int *,double *,double *,
                                                 double *,int *, int *);

NhlErrorTypes grid2triple_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *z;
  double *tmp_x, *tmp_y, *tmp_z;
  ng_size_t dsizes_x[1], dsizes_y[1], dsizes_z[2];
  NclBasicDataTypes type_x, type_y, type_z;
  int has_missing_z;
  NclScalar missing_z, missing_dz, missing_rz;
/*
 * Output array variables
 */
  void *d;
  double *tmp_d;
  ng_size_t dsizes_d[2];
  NclBasicDataTypes type_d;
/*
 * Various
 */
  ng_size_t mx, ny, ld, ld2, ldmax, ldmax2, ldmax3;
  int ild, ier, imx, iny, ildmax;
/*
 * Retrieve input array. 
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

  y = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           DONT_CARE);

  z = (void*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_z, 
           &missing_z,
           &has_missing_z,
           &type_z,
           DONT_CARE);

  mx  = dsizes_x[0];
  ny  = dsizes_y[0];
  ldmax = mx * ny;
  ldmax2 = 2 * ldmax;
  ldmax3 = 3 * ldmax;

/*
 * Check size of z array.
 */
  if(dsizes_z[0] != ny || dsizes_z[1] != mx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: The last input array must be dimensioned ny x mx, where ny is the length of y, and mx is the length of x");
    return(NhlFATAL);
  }
/*
 * Check input dimension sizes.
 */
  if((mx > INT_MAX) || (ny > INT_MAX) || (ldmax > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: one or input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imx    = (int) mx;
  iny    = (int) ny;
  ildmax = (int) ldmax;

/*
 * Coerce missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);
/*
 * Coerce input to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,mx,0,NULL,NULL);
  tmp_y = coerce_input_double(y,type_y,ny,0,NULL,NULL);
  tmp_z = coerce_input_double(z,type_z,ldmax,0,NULL,NULL);

  if( tmp_x == NULL || tmp_y == NULL || tmp_z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to coerce input arrays to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array. Since the output array can vary  in
 * size depending on how many missing values there are, we allocate
 * space for a temporary array no matter what.  When we return from the
 * function, we'll create a new array to hold the actual number of
 * non-missing values returned.
 */
  tmp_d = (double*)calloc(ldmax3,sizeof(double));
  if(tmp_d == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
/*
 * Get type of return variable. If any of the input is double, then return
 * a double. Return float otherwise.
 */
  if(type_x == NCL_double || type_y == NCL_double || type_z == NCL_double) {
    type_d = NCL_double;
  }
  else {
    type_d = NCL_float;
  }

  NGCALLF(grid2triple,GRID2TRIPLE)(tmp_x,tmp_y,tmp_z,&imx,&iny,tmp_d,&ildmax,
                                   &ild,&missing_dz.doubleval,&ier);
  ld = (ng_size_t)ild;
/*
 * If ld is zero, then this probably means that all of tmp_d is missing,
 * and thus we need to return 3*ldmax missing values.
 */
  if(ld == 0) ld = ldmax;

  dsizes_d[0] = 3;
  dsizes_d[1] = ld;
  if(type_d == NCL_double) {
    d = (void*)calloc(3*ld,sizeof(double));
  }
  else {
    d = (void*)calloc(3*ld,sizeof(float));
  }
  if(d == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }

  if(!ier) {
    ld2 = 2*ld;
/*
 * ld contains the number of non-missing values. It should be <= ldmax.
 *
 * The first ld elements of tmp_d will be the non-missing ones.
 */
    coerce_output_float_or_double(d,tmp_d,type_d,ld,0);
    coerce_output_float_or_double(d,&tmp_d[ldmax],type_d,ld,ld);
    coerce_output_float_or_double(d,&tmp_d[ldmax2],type_d,ld,ld2);
  }
  else {
    if(ier == -10) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"grid2triple: all z input values are missing\n");
      set_subset_output_missing(d,0,type_d,ldmax3,missing_dz.doubleval);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_z != NCL_double) NclFree(tmp_z);
  NclFree(tmp_d);
/*
 * Return.
 */
  if(ier) {
    if(type_d == NCL_double) {
      return(NclReturnValue(d,2,dsizes_d,&missing_dz,type_d,0));
    }
    else {
      return(NclReturnValue(d,2,dsizes_d,&missing_rz,type_d,0));
    }
  }
  else {
    return(NclReturnValue(d,2,dsizes_d,NULL,type_d,0));
  }
}

NhlErrorTypes triple2grid_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *z, *gridx, *gridy;
  double *tmp_x, *tmp_y, *tmp_gridx, *tmp_gridy;
  double *tmp_z = NULL;
  ng_size_t dsizes_x[1], dsizes_y[1], dsizes_gridx[1], dsizes_gridy[1];
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  NclBasicDataTypes type_x, type_y, type_z, type_gridx, type_gridy;
  NclScalar missing_z, missing_rz, missing_dz;
  logical *option;
/*
 * Output array variables
 */
  void *grid;
  double *tmp_grid = NULL;
  int ndims_grid;
  ng_size_t *dsizes_grid, size_grid, size_leftmost;
  NclBasicDataTypes type_grid;
/*
 * Work arrays
 */
  double *dx, *dy, *dz, *gbig, *gxbig, *gybig;
/*
 * Various
 */
  int method, loop;
  ng_size_t i, npts, ngx, ngy, ngx2, ngy2, ngxy2, ngxy;
  ng_size_t index_z, index_grid;
  int ier, ret;
  double *distmx = NULL;
  double *domain = NULL;
  logical has_domain=False, has_distmx=False;
  NclBasicDataTypes type_domain, type_distmx;
  int inpts, ingx, ingy, ingx2, ingy2;

/*
 * Variables for retrieving attributes from "options".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Retrieve input arrays. 
 */
  x = (void*)NclGetArgValue(
           0,
           6,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

  y = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           DONT_CARE);

  z = (void*)NclGetArgValue(
           2,
           6,
           &ndims_z,
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           DONT_CARE);

  gridx = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           dsizes_gridx,
           NULL,
           NULL,
           &type_gridx,
           DONT_CARE);

  gridy = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_gridy,
           NULL,
           NULL,
           &type_gridy,
           DONT_CARE);

  option = (logical*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Check sizes of x, y, and z arrays.
 */
  npts  = dsizes_x[0];
  if(dsizes_y[0] != npts || dsizes_z[ndims_z-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: The length of x and y must be the same as the rightmost dimension of z");
    return(NhlFATAL);
  }

/*
 * Get sizes for output array.
 * Remember, in NCL, the Y dimension is usually associated with dimension
 * '0, and the X dimension with dimension '1'.
 */
  ndims_grid = ndims_z + 1;
  dsizes_grid = (ng_size_t*)calloc(ndims_grid,sizeof(ng_size_t));  
  if( dsizes_grid == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) {
    size_leftmost *= dsizes_z[i];
    dsizes_grid[i] = dsizes_z[i];
  }
  ngx   = dsizes_gridx[0];
  ngy   = dsizes_gridy[0];
  ngx2  = ngx + 2;
  ngy2  = ngy + 2;
  ngxy  = ngx * ngy;
  ngxy2 = ngx2 * ngy2;
  size_grid = size_leftmost * ngxy;

/*
 * Test input dimension sizes.
 */
  if((npts > INT_MAX) || (ngx  > INT_MAX) || (ngy > INT_MAX) || 
     (ngx2 > INT_MAX) || (ngy2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  ingx  = (int) ngx;
  ingy  = (int) ngy;
  ingx2 = (int) ngx2;
  ingy2 = (int) ngy2;

  dsizes_grid[ndims_grid-2] = ngy;
  dsizes_grid[ndims_grid-1] = ngx;
/*
 * Get type of return variable. If any of the input is double, then return
 * a double. Return float otherwise.
 */
  if(type_x == NCL_double || type_y == NCL_double || type_z == NCL_double || 
     type_gridx == NCL_double || type_gridy == NCL_double) {

    type_grid = NCL_double;
    grid      = (void*)calloc(size_grid,sizeof(double));
    if(grid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_grid = NCL_float;
    grid      = (void*)calloc(size_grid,sizeof(float));
    tmp_grid  = (double*)calloc(ngxy,sizeof(double));
    if(grid == NULL || tmp_grid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Coerce missing values.  The z missing value will be used to determine
 * the grid missing value.  If z doesn't have a missing value, then the
 * default missing value will be used.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);
/*
 * Coerce input to double if necessary.
 */
  tmp_x     = coerce_input_double(x,type_x,npts,0,NULL,NULL);
  tmp_y     = coerce_input_double(y,type_y,npts,0,NULL,NULL);
  tmp_gridx = coerce_input_double(gridx,type_gridx,ngx,0,NULL,NULL);
  tmp_gridy = coerce_input_double(gridy,type_gridy,ngy,0,NULL,NULL);

  if( tmp_x == NULL || tmp_y == NULL || tmp_gridx == NULL ||
      tmp_gridy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: Unable to coerce input arrays to double");
    return(NhlFATAL);
  }

/*
 * Create temporary array for z if it is not already double.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(npts,sizeof(double));
    if( tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: Unable to coerce input arrays to double");
      return(NhlFATAL);
    }
  }


/*
 * If "option" is True, then it may contain some attributes that we
 * need to retrieve.
 */
  if(*option) {
/*
 * "method" may be set by the user; its default is 1 if option is True.
 * 
 * Prior to NCL V6.4.0, "loop" was an integer representation of "option".
 * However, the Fortran code that gets invoked when loop=1 appears to be 
 * buggy, so we are now forcing loop=0 no matter what. Also, "domain" 
 * will default to 1.0 if not set by user. It used to be 0.0.
 */ 
    loop   = 0;
    method = 1;
/*
 * Retrieve  "option" again, this time getting all the stuff that
 * might be attached to it (attributes).
 */
    stack_entry = _NclGetArg(5, 6, DONT_CARE);
    switch (stack_entry.kind) {
    case NclStk_VAR:
      if (stack_entry.u.data_var->var.att_id != -1) {
        attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
        if (attr_obj == NULL) {
          break;
        }
      }
      else {
/*
 * att_id == -1, no optional args given, defaults desired
 */
        break;
      }
/*
 * get optional arguments;  if none specified, use defaults 
 */
      if (attr_obj->att.n_atts == 0) {
        break;
      }
      else {
/* 
 * att_n_atts > 0, retrieve optional arguments 
 */
        attr_list = attr_obj->att.att_list;
        while (attr_list != NULL) {
          if ((strcmp(attr_list->attname, "domain")) == 0) {
            type_domain = attr_list->attvalue->multidval.data_type;
            has_domain = True;
            domain = coerce_input_double(attr_list->attvalue->multidval.val,
                                         type_domain,1,0,NULL,NULL);
          }

          if ((strcmp(attr_list->attname, "distmx")) == 0) {
            has_distmx = True;
            type_distmx = attr_list->attvalue->multidval.data_type;
            distmx = coerce_input_double(attr_list->attvalue->multidval.val,
                                         type_distmx,1,0,NULL,NULL);
          }

          if ((strcmp(attr_list->attname, "method")) == 0) {
            method = *(int *) attr_list->attvalue->multidval.val;
          }
          
          if ((strcmp(attr_list->attname, "loop")) == 0) {
            loop = *(int *) attr_list->attvalue->multidval.val;
          }
          
          attr_list = attr_list->next;
        }
      }
      
    default:
      break;
    }
  }
/*
 * Else option is False, so set these two variables to 0.
 */
  else {
    loop   = 0;
    method = 0;
  }

  if(!has_distmx) {
    distmx  = (double *)malloc(sizeof(double));
    *distmx = 0.;
  }
  if(!has_domain) {
    domain  = (double *)malloc(sizeof(double));
    *domain = 1.;   /* As of NCL V6.4.0, this defaults to 1. Was 0 previously. See note above */
  }
/*
 * Allocate space for work arrays.
 */
  gxbig = (double *)calloc(ngx2,sizeof(double));
  gybig = (double *)calloc(ngy2,sizeof(double));
  gbig  = (double *)calloc(ngxy2,sizeof(double));
  dx    = (double *)calloc(npts,sizeof(double));
  dy    = (double *)calloc(npts,sizeof(double));
  dz    = (double *)calloc(npts,sizeof(double));
  if(gxbig == NULL || gybig == NULL || gbig == NULL || dx == NULL ||
     dy == NULL || dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid: Unable to create work arrays");
    return(NhlFATAL);
  }
/*
 * Loop across the leftmost dimensions of z and call the 
 * Fortran subroutine.
 */
  index_z = index_grid = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,npts,has_missing_z,
                                 &missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }

    if(type_grid == NCL_double) tmp_grid = &((double*)grid)[index_grid];

    NGCALLF(triple2grid1,TRIPLE2GRID1)(&inpts,tmp_x,tmp_y,tmp_z,
                                       &missing_dz.doubleval,&ingx,&ingy,
                                       tmp_gridx,tmp_gridy,tmp_grid,domain,
                                       &loop,&method,distmx,&ingx2,&ingy2,
                                       dx,dy,dz,gxbig,gybig,gbig,&ier);
/*
 * Coerce grid back to float if necessary.
 *
 */
    if(type_grid == NCL_float) {
      coerce_output_float_only(grid,tmp_grid,ngxy,index_grid);
    }
    index_grid += ngxy;
    index_z    += npts;
  }
/*
 * Free unneeded memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_y     != NCL_double) NclFree(tmp_y);
  if(type_z     != NCL_double) NclFree(tmp_z);
  if(type_gridx != NCL_double) NclFree(tmp_gridx);
  if(type_gridy != NCL_double) NclFree(tmp_gridy);
  if(type_grid  != NCL_double) NclFree(tmp_grid);
  NclFree(gxbig);
  NclFree(gybig);
  NclFree(gbig);
  NclFree(dx);
  NclFree(dy);
  NclFree(dz);
  if(!has_distmx) NclFree(distmx);
  if(!has_domain) NclFree(domain);

/*
 * Return with missing value set no matter what, b/c even though input
 * may not have missing values, the output grid is bound to not have all
 * of its values filled in.
 */
  if(type_grid == NCL_double) {
    ret = NclReturnValue(grid,ndims_grid,dsizes_grid,&missing_dz,type_grid,0);
  }
  else {
    ret = NclReturnValue(grid,ndims_grid,dsizes_grid,&missing_rz,type_grid,0);
  }
  NclFree(dsizes_grid);
  return(ret);
}


NhlErrorTypes triple2grid2d_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *z, *gridx, *gridy;
  double *tmp_x, *tmp_y, *tmp_z, *tmp_gridx, *tmp_gridy;
  ng_size_t dsizes_x[1], dsizes_y[1], dsizes_z[1], dsizes_gridx[2], dsizes_gridy[2];
  int has_missing_z;
  NclBasicDataTypes type_x, type_y, type_z, type_gridx, type_gridy;
  NclScalar missing_z, missing_rz, missing_dz;
  logical *option;
/*
 * Output array variables
 */
  void *grid;
  double *tmp_grid;
  NclBasicDataTypes type_grid;
/*
 * Various
 */
  ng_size_t npts, nlon, nlat, size_grid;
  int inpts, inlon, inlat, mopt = 0;
  double *distmx = NULL;
  logical has_distmx = False;
  NclBasicDataTypes type_distmx;
/*
 * Variables for retrieving attributes from "options".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Retrieve input arrays. 
 */
  x = (void*)NclGetArgValue(
           0,
           6,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

  y = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           DONT_CARE);

  z = (void*)NclGetArgValue(
           2,
           6,
           NULL,
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           DONT_CARE);

  gridx = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           dsizes_gridx,
           NULL,
           NULL,
           &type_gridx,
           DONT_CARE);

  gridy = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_gridy,
           NULL,
           NULL,
           &type_gridy,
           DONT_CARE);

  option = (logical*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Check sizes of x, y, and z arrays.
 */
  npts = dsizes_x[0];
  if(dsizes_y[0] != npts || dsizes_z[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid2d: The three input arrays must be the same length");
    return(NhlFATAL);
  }

/*
 * Check sizes of gridx and gridy.
 */
  nlat = dsizes_gridx[0];
  nlon = dsizes_gridx[1];

  if(dsizes_gridy[0] != nlat || dsizes_gridy[1] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid2d: The two 2-dimensional arrays that define the output grid must be the same size");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((npts > INT_MAX) || (nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid2d: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inlat = (int) nlat;
  inlon = (int) nlon;

/*
 * Set sizes for output array.
 *
 * Remember, in NCL, the Y dimension is usually associated with dimension
 * '0, and the X dimension with dimension '1'.
 */
  size_grid = nlat * nlon;
/*
 * Get type of return variable. If z is double, then return
 * a double. Return float otherwise.
 */
  if(type_z == NCL_double) {
    type_grid = NCL_double;
    grid      = (double*)calloc(size_grid,sizeof(double));
  }
  else {
    type_grid = NCL_float;
    grid      = (float*)calloc(size_grid,sizeof(float));
  }
  tmp_grid = coerce_output_double(grid,type_grid,size_grid);

  if(grid == NULL || tmp_grid == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid2d: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.  The z missing value will be used to determine
 * the grid missing value.  If z doesn't have a missing value, then the
 * default missing value will be used.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);

/*
 * Coerce input to double if necessary.
 */
  tmp_x     = coerce_input_double(x,type_x,npts,0,NULL,NULL);
  tmp_y     = coerce_input_double(y,type_y,npts,0,NULL,NULL);
  tmp_z     = coerce_input_double(z,type_z,npts,0,NULL,NULL);
  tmp_gridx = coerce_input_double(gridx,type_gridx,size_grid,0,NULL,NULL);
  tmp_gridy = coerce_input_double(gridy,type_gridy,size_grid,0,NULL,NULL);

  if( tmp_x == NULL || tmp_y == NULL || tmp_z == NULL ||
      tmp_gridx == NULL || tmp_gridy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"triple2grid2d: Unable to coerce input arrays to double");
    return(NhlFATAL);
  }

/*
 * If "option" is True, then it may contain some attributes that we
 * need to retrieve.
 */
  if(*option) {
/*
 * Retrieve  "option" again, this time getting all the stuff that
 * might be attached to it (attributes).
 */
    stack_entry = _NclGetArg(5, 6, DONT_CARE);
    switch (stack_entry.kind) {
    case NclStk_VAR:
      if (stack_entry.u.data_var->var.att_id != -1) {
        attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
        if (attr_obj == NULL) {
          break;
        }
      }
      else {
/*
 * att_id == -1, no optional args given, defaults desired
 */
        break;
      }
      
/*
 * Get optional arguments;  if none specified, use defaults
 */
      if (attr_obj->att.n_atts == 0) {
        break;
      }
      else {
/*
 * att_n_atts > 0, retrieve optional arguments
 */
        attr_list = attr_obj->att.att_list;
        while (attr_list != NULL) {
          if ((strcmp(attr_list->attname, "distmx")) == 0) {
            has_distmx = True;
            type_distmx = attr_list->attvalue->multidval.data_type;
            distmx = coerce_input_double(attr_list->attvalue->multidval.val,
                                         type_distmx,1,0,NULL,NULL);
          }
          
          if ((strcmp(attr_list->attname, "mopt")) == 0) {
            mopt = *(int *) attr_list->attvalue->multidval.val;
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

  if(!has_distmx) {
    distmx  = (double *)malloc(sizeof(double));
    *distmx = 1.e20;
  }

/*
 * Call the Fortran subroutine.
 */
  NGCALLF(triple2grid2d,TRIPLE2GRID2D)(tmp_x,tmp_y,tmp_z,&inpts,
                                       &missing_dz.doubleval,distmx,&mopt,
                                       tmp_gridy,tmp_gridx,tmp_grid,
                                       &inlat,&inlon);
/*
 * Coerce grid back to float if necessary.
 *
 */
  if(type_grid == NCL_float) {
    coerce_output_float_only(grid,tmp_grid,size_grid,0);
  }
/*
 * Free unneeded memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_y     != NCL_double) NclFree(tmp_y);
  if(type_z     != NCL_double) NclFree(tmp_z);
  if(type_gridx != NCL_double) NclFree(tmp_gridx);
  if(type_gridy != NCL_double) NclFree(tmp_gridy);
  if(type_grid  != NCL_double) NclFree(tmp_grid);
  if(!has_distmx || (has_distmx && type_distmx != NCL_double) ) NclFree(distmx);

/*
 * Return with missing value set no matter what, b/c even though input
 * may not have missing values, the output grid is bound to not have all
 * of its values filled in.
 */
  if(type_grid == NCL_double) {
    return(NclReturnValue(grid,2,dsizes_gridx,&missing_dz,type_grid,0));
  }
  else {
    return(NclReturnValue(grid,2,dsizes_gridx,&missing_rz,type_grid,0));
  }
}
