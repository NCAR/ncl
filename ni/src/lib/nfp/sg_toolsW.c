#include "wrapper.h"

extern double NGCALLF(gctarea,GCTAREA)(double*,double*,double*,
                                       double*,double*,double*,int*);
extern double NGCALLF(gcqarea,GCQAREA)(double*,double*,double*,
                                       double*,double*,double*,
                                       double*,double*,int*);
extern double NGCALLF(gcaangle,GCAANGLE)(double*,double*,double*,
                                         double*,double*,double*,
                                         double*,double*);
extern double NGCALLF(gcpnt2gc,GCPNT2GC)(double*,double*,double*,
                                         double*,double*,double*);
extern double NGCALLF(gcdangle,GCDANGLE)(double*,double*,double*,
                                         double*,double*,double*);
extern int NGCALLF(gccwise,GCCWISE)(double*,double*,int*);
extern int NGCALLF(gcinout,GCINOUT)(double*,double*,double*,double*,
                                     int*,double*);
extern int NGCALLF(gconarc,GCONARC)(double*,double*,double*,double*,
                                    double*,double*,double*);

NhlErrorTypes gc_aangle_W( void )
{
/*
 * Input variables
 */
  void *lat, *lon;
  double *dlat, *dlon;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *aangle; 
  double *tmp_aangle;
  ng_size_t size_aangle;
  NclBasicDataTypes type_aangle;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat = (void*)NclGetArgValue(
          0,
          2,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check dimension sizes. The arguments must be arrays with
 * rightmost dimension 4.
 */

/*
 * Check rightmost dimension size for lat.
 */
  if(dsizes_lat[ndims_lat-1] != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_aangle: the last (rightmost) dimension of arrays must be 4");
    return(NhlFATAL);
  }

/*
 * Check that the arrays have the same number of dimensions.
 */
  if (!(ndims_lat == ndims_lon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_aangle: the input arrays must have the same number of dimensions.");
    return(NhlFATAL);
  }

/* 
 * Check that the dimension sizes for the arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_aangle: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Determine size for the return array.
 */
  size_aangle = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_aangle *= dsizes_lat[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, 4*size_aangle, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, 4*size_aangle, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_aangle: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_aangle = NCL_float;
  if (type_lat == NCL_double || type_lon == NCL_double) {
    type_aangle = NCL_double;
    aangle = (void *)calloc(size_aangle, sizeof(double));
    if(aangle == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_aangle: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    aangle = (void *)calloc(size_aangle, sizeof(float));
    if(aangle == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_aangle: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  tmp_aangle = (double *)calloc(1,sizeof(double));
  for( i = 0; i < size_aangle; i++ ) {

/*
 *  If the type of the return variable is double, then call the
 *  Fortran function with tmp_aangle set to the correct address in
 *  the output variable.
 */
    if (type_aangle == NCL_double) tmp_aangle = &(((double *)aangle)[i]);
      *tmp_aangle = NGCALLF(gcaangle,GCQAREA)(dlat+4*i, dlon+4*i, 
                             dlat+4*i+1, dlon+4*i+1, dlat+4*i+2, dlon+4*i+2,
                             dlat+4*i+3, dlon+4*i+3);

/*
 *  If the type of the return variable is not double, then return floats
 *  in the output array.
 */
    if(type_aangle != NCL_double) {
      ((float *) aangle)[i] = (float)(*tmp_aangle);
    }
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);
  if(type_aangle != NCL_double) NclFree(tmp_aangle);

/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    return(NclReturnValue(aangle,1,dsizes_lat,NULL,type_aangle,0));
  }
  else {
    return(NclReturnValue(aangle,ndims_lat-1, dsizes_lat,NULL,type_aangle,0));
  }
}

NhlErrorTypes gc_qarea_W( void )
{
/*
 * Input variables
 */
  void *lat, *lon;
  double *dlat, *dlon;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *qarea; 
  double *tmp_qarea;
  ng_size_t size_qarea;
  NclBasicDataTypes type_qarea;
  int ndims_qarea;
  NclScalar missing_qarea;

/*
 * Declare various variables for random purposes.
 */
  int ier;
  ng_size_t i;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat = (void*)NclGetArgValue(
          0,
          2,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check dimension sizes. The arguments must be arrays with
 * rightmost dimension 4.
 */

/*
 * Check rightmost dimension size for lat.
 */
  if(dsizes_lat[ndims_lat-1] != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_qarea: the last (rightmost) dimension of arrays must be 4");
    return(NhlFATAL);
  }

/*
 * Check that the arrays have the same number of dimensions.
 */
  if (!(ndims_lat == ndims_lon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_qarea: the input arrays must have the same number of dimensions.");
    return(NhlFATAL);
  }

/* 
 * Check that the dimension sizes for the arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_qarea: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Determine size for the return array.
 */
  size_qarea = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_qarea *= dsizes_lat[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, 4*size_qarea, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, 4*size_qarea, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_qarea: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_qarea = NCL_float;
  if (type_lat == NCL_double || type_lon == NCL_double) {
    type_qarea = NCL_double;
    qarea = (void *)calloc(size_qarea, sizeof(double));
    if(qarea == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_qarea: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    qarea = (void *)calloc(size_qarea, sizeof(float));
    if(qarea == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_qarea: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  tmp_qarea = (double *)calloc(1,sizeof(double));
  for( i = 0; i < size_qarea; i++ ) {

/*
 *  If the type of the return variable is double, then call the
 *  Fortran function with tmp_qarea set to the correct address in
 *  the output variable.
 */
    if (type_qarea == NCL_double) tmp_qarea = &(((double *)qarea)[i]);
      *tmp_qarea = NGCALLF(gcqarea,GCQAREA)(dlat+4*i, dlon+4*i, dlat+4*i+1,
                                            dlon+4*i+1, dlat+4*i+2, dlon+4*i+2,
                                            dlat+4*i+3, dlon+4*i+3,&ier);

/*
 *  If the type of the return variable is not double, then return floats
 *  in the output array.
 */
    if(type_qarea != NCL_double) {
      ((float *) qarea)[i] = (float)(*tmp_qarea);
    }
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);
  if(type_qarea != NCL_double) NclFree(tmp_qarea);

/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    ndims_qarea = 1;
  }
  else {
    ndims_qarea = ndims_lat-1;
  }

/*
 * Check if we need to return a missing value. 
 */
  if(ier) {
    if(type_qarea == NCL_float) {
      missing_qarea.floatval = 1.e30;  /* This is what the Fortran routine returns. */
                                       /* Don't change unless you change the Fortran too */
    }
    else {
      missing_qarea.doubleval = 1.e30;  /* This is what the Fortran routine returns. */
    }
    return(NclReturnValue(qarea,ndims_qarea,dsizes_lat,&missing_qarea,type_qarea,0));
  }
  else {
    return(NclReturnValue(qarea,ndims_qarea,dsizes_lat,NULL,type_qarea,0));
  }
}

NhlErrorTypes gc_clkwise_W( void )
{
/*
 * Input variables
 */
  void *lat, *lon;
  double *dlat, *dlon, *tlat, *tlon;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  logical *tfval; 
  int itmp, inpts, inptsp1;
  ng_size_t size_tfval,tsize,npts,nptsp1,jpol;
  NclBasicDataTypes type_tfval;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat = (void*)NclGetArgValue(
          0,
          2,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check number of dimensions.
 */
  if (ndims_lon != ndims_lat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_clkwise: the arguments must have the same number of dimensions");
    return(NhlFATAL);
  }

/* 
 * Check that the dimension sizes for the arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_clkwise: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Find the number of points in each polygon and check that it
 * is at least three.
 */
  npts = dsizes_lat[ndims_lat-1];
  nptsp1 = npts+1;
  if (npts < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
         "gc_clkwise: the polygon must have at least three points.");
    return(NhlFATAL);
  }

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX || nptsp1 > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_clkwise: npts and/or nptsp1 is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts   = (int) npts;
  inptsp1 = (int) nptsp1;

/*
 * Determine size for the return array.
 */
  size_tfval = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_tfval *= dsizes_lat[i];
  }

/*
 * Determine total size of input arrays.
 */
  tsize = 1;
  for (i = 0; i < ndims_lat; i++) {
    tsize *= dsizes_lat[i];
  }


/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, tsize, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, tsize, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_clkwise: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_tfval = NCL_logical;
  tfval = (logical *)calloc(size_tfval, sizeof(logical));
  if(tfval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_clkwise: Unable to allocate memory for return.");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  jpol = 0;
  for( i = 0; i < size_tfval; i++ ) {

/*
 * If the polygon is not closed, close it before calling the
 * Fortran.
 */
    
    if (dlat[jpol] != dlat[jpol+npts-1] || dlon[jpol] != dlon[jpol+npts-1]) {
      tlat = (double *) calloc(npts+1,sizeof(double));
      tlon = (double *) calloc(npts+1,sizeof(double));
      memcpy(tlat,dlat+jpol,npts*sizeof(double));
      memcpy(tlon,dlon+jpol,npts*sizeof(double));
      tlat[npts] = tlat[0];
      tlon[npts] = tlon[0];

      itmp = NGCALLF(gccwise,GCCWISE)(tlat,tlon,&inptsp1);

      if (itmp == 0) {
        tfval[i] = True;
      }
      else {
        tfval[i] = False;
      }
      free(tlat);
      free(tlon);
    }
    else {
      itmp = NGCALLF(gccwise,GCCWISE)(dlat+jpol,dlon+jpol,&inpts);

      if (itmp == 0) {
        tfval[i] = True;
      }
      else {
        tfval[i] = False;
      }
    }
    jpol = jpol+npts;
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);


/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    return(NclReturnValue(tfval,1,dsizes_lat,NULL,type_tfval,0));
  }
  else {
    return(NclReturnValue(tfval,ndims_lat-1,dsizes_lat,NULL,type_tfval,0));
  }
}

NhlErrorTypes gc_tarea_W( void )
{
/*
 * Input variables
 */
  void *lat, *lon;
  double *dlat, *dlon;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *tarea; 
  double *tmp_tarea;
  ng_size_t size_tarea;
  NclBasicDataTypes type_tarea;
  int ndims_tarea;
  NclScalar missing_tarea;
/*
 * Declare various variables for random purposes.
 */
  int ier;
  ng_size_t i;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat = (void*)NclGetArgValue(
          0,
          2,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check dimension sizes. The arguments must be arrays with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for lat.
 */
  if(dsizes_lat[ndims_lat-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_tarea: the last (rightmost) dimension of arrays must be 3");
    return(NhlFATAL);
  }

/*
 * Check that the arrays have the same number of dimensions.
 */
  if (!(ndims_lat == ndims_lon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_tarea: the input arrays must have the same number of dimensions.");
  }

/* 
 * Check that the dimension sizes for the arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_tarea: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Determine size for the return array.
 */
  size_tarea = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_tarea *= dsizes_lat[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, 3*size_tarea, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, 3*size_tarea, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_tarea: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_tarea = NCL_float;
  if (type_lat == NCL_double || type_lon == NCL_double) {
    type_tarea = NCL_double;
    tarea = (void *)calloc(size_tarea, sizeof(double));
    if(tarea == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_tarea: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    tarea = (void *)calloc(size_tarea, sizeof(float));
    if(tarea == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_tarea: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  tmp_tarea = (double *)calloc(1,sizeof(double));
  for( i = 0; i < size_tarea; i++ ) {

/*
 *  If the type of the return variable is double, then call the
 *  Fortran function with tmp_tarea set to the correct address in
 *  the output variable.
 */
    if (type_tarea == NCL_double) tmp_tarea = &(((double *)tarea)[i]);
      *tmp_tarea = NGCALLF(gctarea,GCTAREA)(dlat+3*i, dlon+3*i, 
                                            dlat+3*i+1, dlon+3*i+1, 
                                            dlat+3*i+2, dlon+3*i+2,&ier);
/*
 *  If the type of the return variable is not double, then return floats
 *  in the output array.
 */
    if(type_tarea != NCL_double) {
      ((float *) tarea)[i] = (float)(*tmp_tarea);
    }
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);
  if(type_tarea != NCL_double) NclFree(tmp_tarea);

  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    ndims_tarea = 1;
  }
  else {
    ndims_tarea = ndims_lat-1;
  }
/*
 * Check if we need to return a missing value. 
 */
  if(ier) {
    if(type_tarea == NCL_float) {
      missing_tarea.floatval = 1.e30;  /* This is what the Fortran routine returns. */
                                       /* Don't change unless you change the Fortran too */
    }
    else {
      missing_tarea.doubleval = 1.e30;  /* This is what the Fortran routine returns. */
    }
    return(NclReturnValue(tarea,ndims_tarea,dsizes_lat,&missing_tarea,type_tarea,0));
  }
  else {
    return(NclReturnValue(tarea,ndims_tarea,dsizes_lat,NULL,type_tarea,0));
  }
}

NhlErrorTypes gc_inout_W( void )
{
/*
 * Input variables
 */
  void *plat, *plon, *lat, *lon;
  double dplat, dplon, *dlat, *dlon;
  double *dlat_tmp, *dlon_tmp, *dlat_close, *dlon_close;

  ng_size_t dsizes_plat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_plon[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon, ndims_plat, ndims_plon;
  NclBasicDataTypes type_lat, type_lon, type_plat, type_plon;
 
/*
 * output variable 
 */
  logical *output;
  ng_size_t size_output;
  NclBasicDataTypes type_output;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, npts, nptsp1, itmp, index_latlon;
  int inpts, inptsp1;
  double *work;
  logical alloc_dlat, alloc_dlon;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  plat = (void*)NclGetArgValue(
          0,
          4,
          &ndims_plat,
          dsizes_plat,
          NULL,
          NULL,
          &type_plat,
          DONT_CARE);

  plon = (void*)NclGetArgValue(
          1,
          4,
          &ndims_plon,
          dsizes_plon,
          NULL,
          NULL,
          &type_plon,
          DONT_CARE);

  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          3,
          4,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * The plat/plon and lat/lon arrays must be dimensioned as one
 * of the two ways:
 * 
 * - plat/plon are any dimensionality, and lat/lon are 1D arrays.
 *
 * - plat/plon are any dimensionality, and lat/lon are nD arrays
 *   with one fewer dimensions. The dimensions of plat/plon must
 *   be the same as the leftmost-minus-1 dimensions of lon/lat.
 *
 * In either case, lat must be the same dimensionality as lon, and
 * plat must be the same dimensionality as plon.
 */

/*
 * Check number of dimensions for lat and lon.
 */
  if (ndims_lat != ndims_lon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: the lat/lon arguments must have the same number of dimensions");
    return(NhlFATAL);
  }

  if (ndims_plat != ndims_plon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_inout: the first two input arrays must have the same number of dimensions.");
   return(NhlFATAL);
  }

/*
 * Check that the dimension sizes for the lat/lon arrays are the same.
 */ 
  for(i = 0; i < ndims_lat; i++) {
    if (dsizes_lat[i] != dsizes_lon[i])  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_inout: the lat/lon arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Check that the dimension sizes for the plat/plon arrays are the same.
 */ 
  for(i = 0; i < ndims_plat; i++) {
    if (dsizes_plat[i] != dsizes_plon[i])  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_inout: the plat/plon arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Check for multiply-dimensioned lat/lon arrays. 
 */
  if(ndims_lat > 1) {
    if(ndims_plat != (ndims_lat-1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_inout: if the lat/lon arrays are multi-dimensional, then they must have one more dimension than the plat/plon arrays.");
      return(NhlFATAL);
    }
/*
 * If lat/lon are multi-d, then check on the dimension sizes of 
 * plat/plon versus lat/lon.
 */
    for(i = 0; i < ndims_plat; i++) {
      if ((dsizes_plat[i] != dsizes_lon[i]) || 
          (dsizes_plon[i] != dsizes_lon[i]))  {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "gc_inout: the dimensions sizes for the plat/plon arrays must be the same as all but the rightmost dimension of the lat/lon arrays");
        return(NhlFATAL);
      }
    }
  }

/*
 * Find the number of points in each polygon and check that it
 * is at least three.
 */
  npts   = dsizes_lat[ndims_lat-1];
  nptsp1 = npts+1;
  if (npts < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
         "gc_inout: the rightmost dimension of lat/lon must be at least three.");
    return(NhlFATAL);
  }

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX || nptsp1 > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: npts and/or nptsp1 is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts   = (int) npts;
  inptsp1 = (int) nptsp1;

/*
 * Determine size for the return array.
 */
  size_output = 1;
  for (i = 0; i < ndims_plat; i++) {
    size_output *= dsizes_plat[i];
  }

/*
 * If lat/lon are 1D, coerce input variables to double if necessary.
 */
  alloc_dlat = False;
  alloc_dlon = False;
  if(ndims_lat == 1) {
    dlat_tmp = coerce_input_double(lat, type_lat, npts, 0, NULL, NULL);
    dlon_tmp = coerce_input_double(lon, type_lon, npts, 0, NULL, NULL);
    if(dlat_tmp == NULL || dlon_tmp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: unable to allocate memory for coercing input arrays to double precision");
      return(NhlFATAL);
    }
    if(type_lat != NCL_double) alloc_dlat = True;
    if(type_lon != NCL_double) alloc_dlon = True;
/*
 * Close the polygon if necessary.
 */
    if (dlat_tmp[0] != dlat_tmp[npts-1] || dlon_tmp[0] != dlon_tmp[npts-1]) {
      alloc_dlat = True;
      alloc_dlon = True;
      dlat = (double *) calloc(nptsp1,sizeof(double));
      dlon = (double *) calloc(nptsp1,sizeof(double));
      if(dlat == NULL || dlon == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: unable to allocate memory for coercing input arrays to double precision");
        return(NhlFATAL);
      }
      memcpy(dlat,dlat_tmp,npts*sizeof(double));
      memcpy(dlon,dlon_tmp,npts*sizeof(double));
      dlat[npts] = dlat_tmp[0];
      dlon[npts] = dlon_tmp[0];

      if(type_lat != NCL_double) NclFree(dlat_tmp);
      if(type_lon != NCL_double) NclFree(dlon_tmp);

/*
 * We've added a point to the lat/lon polygon, so make sure it
 * gets changed here. 
 */
      npts  = nptsp1;
      inpts = inptsp1;
    }
    else {
      dlat = dlat_tmp;
      dlon = dlon_tmp;
    }
  }
/*
 * If lat/lon are nD, then create temporary arrays, if necessary, to
 * coerce to double precision later.
 */
  else {
    if(type_lat != NCL_double) {
      dlat = (double *)calloc(npts,sizeof(double));
      alloc_dlat = True;
      if(dlat == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: unable to allocate memory for coercing lat to double precision");
        return(NhlFATAL);
      }
    }
    if(type_lon != NCL_double) {
      dlon = (double *)calloc(npts,sizeof(double));
      alloc_dlon = True;
      if(dlon == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: unable to allocate memory for coercing lon to double precision");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Allocate space for output array.
 */
  type_output = NCL_logical;
  output = (logical *)calloc(size_output, sizeof(logical));
  if(output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: Unable to allocate memory for return.");
    return(NhlFATAL);
  }
  work = (double *)calloc(4*nptsp1, sizeof(double));
  if(work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Loop through each plat/plon point and call the Fortran version of 
 * this routine.
 */
  for( i = 0; i < size_output; i++ ) {
/*
 * Coerce subsection of plat/plon to double if necessary.
 */
    if(type_plat != NCL_double) {
      coerce_subset_input_double(plat,&dplat,i,type_plat,1,0,NULL,NULL);
    }
    else {
/*
 * Point dplat to appropriate location in plat
 */
      dplat = ((double*)plat)[i];
    }
    if(type_plon != NCL_double) {
      coerce_subset_input_double(plon,&dplon,i,type_plon,1,0,NULL,NULL);
    }
    else {
/*
 * Point dplon to appropriate location in plon
 */
      dplon = ((double*)plon)[i];
    }

/*
 * If lat/lon are nD, then we have to coerce the subset to double,
 * if necessary, and then close the polygon, if necessary.
 */
    if(ndims_lat > 1) {
      index_latlon = i*npts;
      if(type_lat != NCL_double) {
        coerce_subset_input_double(lat,dlat,index_latlon,type_lat,npts,
                                   0,NULL,NULL);
      }
      else {
        dlat = &((double*)lat)[index_latlon];
      }
      if(type_lon != NCL_double) {
        coerce_subset_input_double(lon,dlon,index_latlon,type_lon,npts,
                                   0,NULL,NULL);
      }
      else {
        dlon = &((double*)lon)[index_latlon];
      }
    }
/*
 * If we have nD lat/lon *and* if the lat/lon polygon is not closed,
 * close it before calling the Fortran routine.
 */
    if(ndims_lat > 1 && (dlat[0] != dlat[npts-1] || 
                         dlon[0] != dlon[npts-1])) {
      dlat_close = (double *) calloc(nptsp1,sizeof(double));
      dlon_close = (double *) calloc(nptsp1,sizeof(double));
      if(dlat_close == NULL || dlon_close == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: unable to allocate memory for closing lat/lon polygon");
        return(NhlFATAL);
      }
      memcpy(dlat_close,dlat,npts*sizeof(double));
      memcpy(dlon_close,dlon,npts*sizeof(double));
      dlat_close[npts] = dlat_close[0];
      dlon_close[npts] = dlon_close[0];
      itmp = NGCALLF(gcinout,GCINOUT)(&dplat,&dplon,
                                      dlat_close,dlon_close,&inptsp1,work);
      NclFree(dlat_close);
      NclFree(dlon_close);
    }
    else {
/*
 * lat/lon don't need to be closed.
 */
      itmp = NGCALLF(gcinout,GCINOUT)(&dplat,&dplon,dlat,dlon,
                                      &inpts,work);
    }
/*
 * Set the return value.
 */
    if (itmp == 0) {
      output[i] = True;
    }
    else {
      output[i] = False;
    }
  }


/*
 * Free memory.
 */
  if(alloc_dlat) NclFree(dlat);
  if(alloc_dlon) NclFree(dlon);
  NclFree(work);

/*
 * Return.
 */
  return(NclReturnValue(output,ndims_plat,dsizes_plat,NULL,type_output,0));
}

/*
 * This function was to test if it was faster to mask a data array
 * based on gc_inout values, as a built-in function, rather than
 * in an NCL script like we've done with some of the shapefiles
 * example. We didn't notice a huge improvement, probably because
 * most of the time is spent in the internal Fortran routine for gc_inout.
 *
 * This function hasn't been updated to keep up with changes to gc_inout,
 * so it may be out-of-date. The main changes to gc_inout were to allow
 * leftmost dimemsnions.
 */
NhlErrorTypes gc_inout_mask_func_W( void )
{
/*
 * Input variables
 */
  void *data, *latdata, *londata, *latmask, *lonmask;
  int *opt;
  double *dlatdata, *dlondata, *dlatmask, *dlonmask;
  double *dlatmask_tmp, *dlonmask_tmp;
  ng_size_t dsizes_data[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_latdata[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_londata[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_latmask[1];
  ng_size_t dsizes_lonmask[1];
  int has_missing_data, ndims_data, size_data, size_data_bytes;
  int ndims_latdata, ndims_londata;
  NclScalar missing_data;
  NclBasicDataTypes type_data, type_latmask, type_lonmask, type_latdata, type_londata;
 
/*
 * Output
 */
  void *data_out;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, k, npts, nlat, nlon, nlatlon, size_leftmost;
  int itmp, inpts;
  double *work;
  logical IS_1D_COORD;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  data = (void*)NclGetArgValue(
          0,
          6,
          &ndims_data,
          dsizes_data,
          &missing_data,
          &has_missing_data,
          &type_data,
          DONT_CARE);

  latdata = (void*)NclGetArgValue(
          1,
          6,
          &ndims_latdata,
          dsizes_latdata,
          NULL,
          NULL,
          &type_latdata,
          DONT_CARE);

  londata = (void*)NclGetArgValue(
          2,
          6,
          &ndims_londata,
          dsizes_londata,
          NULL,
          NULL,
          &type_londata,
          DONT_CARE);
  latmask = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_latmask,
          NULL,
          NULL,
          &type_latmask,
          DONT_CARE);

  lonmask = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lonmask,
          NULL,
          NULL,
          &type_lonmask,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Error checking for input/output arrays.
 */
  if(type_data != NCL_float && type_data != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "gc_inout_mask_func: the data array must be float or double");
   return(NhlFATAL);
  }
  if(!has_missing_data) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
         "gc_inout_mask_func: the data array must have a _FillValue attribute set");
   return(NhlFATAL);
  }

/*
 * latdata and londata can either be 1D coord arrays, or arrays 
 * of the same dimensionality as the rightmost dimension of data.
 *
 * The assumption is if the rightmost two dimensions of data
 * are equal to the rightmost dimension of latdata and londata
 * respectively, then we have 1D coordinate arrays. Otherwise,
 * it's assumed that that latdata/londata have the same dimensions,
 * as each other, and the same rightmost dimensions as data.
 */
  if(ndims_latdata == 1 && ndims_londata == 1 && ndims_data >=2 &&
     dsizes_latdata[0] == dsizes_data[ndims_data-2] && 
     dsizes_londata[0] == dsizes_data[ndims_data-1]) {
    IS_1D_COORD = True;
    nlat = dsizes_latdata[0];
    nlon = dsizes_londata[0];
    nlatlon = nlat * nlon;
  }
  else {
    IS_1D_COORD = False;
    if (ndims_latdata != ndims_londata) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
                "gc_inout_mask_func: the lat/lon data arrays must have the same number of dimensions.");
      return(NhlFATAL);
    }
    nlatlon = 1;
    for(i = 0; i < ndims_latdata; i++) {
      if (dsizes_latdata[i] != dsizes_londata[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                  "gc_inout_mask_func: if lat/lon are not 1D coordinate arrays, then they must have the same dimensionality.");
        return(NhlFATAL);
      }
      nlatlon *= dsizes_latdata[i];
    }

    if (ndims_data == ndims_londata) {
      for(i = 0; i < ndims_latdata; i++) {
        if (dsizes_data[i] != dsizes_londata[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "gc_inout_mask_func: if the data/lat/lon arrays have the same number of dimensions, then the dimensions must be the same");
          return(NhlFATAL);
        }
      }
    }
    else if (ndims_data > ndims_londata) {
      for(i = 0; i < ndims_latdata; i++) {
        if (dsizes_data[ndims_data-ndims_londata+i] != dsizes_londata[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "gc_inout_mask_func: the rightmost dimensions of data must be the same as the dimensions of lat/lon");
          return(NhlFATAL);
        }
      }
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
                "gc_inout_mask_func: either the rightmost dimensions of data must be the same as the dimensions of lat/lon, or all the dimensions must be the same");
      return(NhlFATAL);
    }
  }

  if (dsizes_lonmask[0] != dsizes_latmask[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, 
         "gc_inout_mask_func: the latmask/lonmask arrays must be the same length");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes for mask arrays.
 */
  npts = dsizes_latmask[0];
  if (npts < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
         "gc_inout_mask_func: the lat/lon mask array have at least three points.");
    return(NhlFATAL);
  }
/*
 * Check if larger than INT_MAX.  We will assign the int "inpts" 
 * later once we make sure the mask arrays are cyclic.
 */
  if(npts > INT_MAX || (npts+1) > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_func: npts and/or npts+1 is greater than INT_MAX");
    return(NhlFATAL);
  }

/*
 * Calculate size of data.
 */
  size_leftmost = 1;
  if(IS_1D_COORD) {
    for(i = 0; i < ndims_data-2; i++) {
      size_leftmost *= dsizes_data[i];
    }
    size_data = size_leftmost * nlatlon;
  }
  else {
    for(i = 0; i < ndims_data-ndims_latdata; i++) {
      size_leftmost *= dsizes_data[i];
    }
    size_data = size_leftmost * nlatlon;
  }
/*
 * Make copy of input array
 */
  if(type_data == NCL_float) {
    size_data_bytes = size_data * sizeof(float);
    data_out = (float *) calloc(size_data,sizeof(float));
  }
  else {
    size_data_bytes = size_data * sizeof(double);
    data_out = (double *) calloc(size_data,sizeof(double));
  }
  if(data_out == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_func: unable to make copy of new data array");
    return(NhlFATAL);
  }
  memcpy(data_out,data,size_data_bytes);

/*
 * Coerce input variables to double if necessary.
 */
  dlatmask_tmp = coerce_input_double(latmask, type_latmask, npts,
                                     0, NULL, NULL);
  dlonmask_tmp = coerce_input_double(lonmask, type_lonmask, npts,
                                     0, NULL, NULL);

  if(dlatmask_tmp == NULL || dlonmask_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_func: unable to allocate memory for coercing lat/lon mask arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Close the latmask/lonmask polygon if necessary.
 */
  if (dlatmask_tmp[0] != dlatmask_tmp[npts-1] || 
      dlonmask_tmp[0] != dlonmask_tmp[npts-1]) {
    dlatmask = (double *) calloc(npts+1,sizeof(double));
    dlonmask = (double *) calloc(npts+1,sizeof(double));
    if(dlatmask == NULL || dlonmask == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_func: unable to allocate memory for closing lat/lon mask array");
      return(NhlFATAL);
    }
    memcpy(dlatmask,dlatmask_tmp,npts*sizeof(double));
    memcpy(dlonmask,dlonmask_tmp,npts*sizeof(double));
    dlatmask[npts] = dlatmask[0];
    dlonmask[npts] = dlonmask[0];
    if(type_latmask != NCL_double) NclFree(dlatmask_tmp);
    if(type_lonmask != NCL_double) NclFree(dlonmask_tmp);
    inpts = (int) npts+1;
  }
  else {
    dlatmask = dlatmask_tmp;
    dlonmask = dlonmask_tmp;
    inpts = (int) npts;
  }

  work = (double *)calloc(4*(npts+1), sizeof(double));
  if(work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_func: unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Loop through each lat/lon point of the data and see if it is in or out
 * of the lat/lon mask array.
 */
  if(IS_1D_COORD) {
    dlatdata = coerce_input_double(latdata, type_latdata, nlat, 
                                   0, NULL, NULL);
    dlondata = coerce_input_double(londata, type_londata, nlon, 
                                   0, NULL, NULL);
  }
  else {
    dlatdata = coerce_input_double(latdata, type_latdata, nlatlon, 
                                   0, NULL, NULL);
    dlondata = coerce_input_double(londata, type_londata, nlatlon, 
                                   0, NULL, NULL);
  }
  if(dlatdata == NULL || dlondata == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_func: unable to coerce input lat/lon arrays to double");
    return(NhlFATAL);
  }

  if(IS_1D_COORD) {
/*
 * We are dealing with 1D coordinate arrays, so for each lat value,
 * we have to loop across each lon value and pass the pair to
 * gcinout.
 */
    for( i = 0; i < nlat; i++ ) {
      for( j = 0; j < nlon; j++ ) {
        itmp = NGCALLF(gcinout,GCINOUT)(&dlatdata[i],&dlondata[j],
                                        dlatmask,dlonmask,&inpts,work);
/*
 * itmp==0 implies lat/lon point is inside the mask.
 * 
 * If opt==0, then mask points outside mask. 
 *    opt==1, then mask points inside mask.
 */
        if( (itmp == 0 && *opt == 1) || (itmp != 0 && *opt == 0)) {
          if(type_data == NCL_double) {
            for(k = 0; k < size_leftmost; k++) {
              ((double*)data_out)[(k*nlatlon)+(i*nlon)+j] = missing_data.doubleval;
            }
          }
          else {
            for(k = 0; k < size_leftmost; k++) {
              ((float*)data_out)[(k*nlatlon)+(i*nlon)+j] = missing_data.floatval;
            }
          }
        }
      }
    }
  }
  else {
/*
 * We are NOT dealing with 1D coordinate arrays, so we can loop across
 * the lat/lon arrays together and pass each pair to gcinout.
 */
    for( i = 0; i < nlatlon; i++ ) {
      itmp = NGCALLF(gcinout,GCINOUT)(&dlatdata[i],&dlondata[i],
                                      dlatmask,dlonmask,&inpts,work);
/*
 * itmp==0 implies lat/lon point is inside the mask.
 * 
 * If opt==0, then mask points outside mask. 
 *    opt==1, then mask points inside mask.
 */
      if( (itmp == 0 && *opt == 1) || (itmp != 0 && *opt == 0)) {
        if(type_data == NCL_double) {
          for(j = 0; j < size_leftmost; j++) {
            ((double*)data_out)[i+(j*nlatlon)] = missing_data.doubleval;
          }
        }
        else {
          for(j = 0; j < size_leftmost; j++) {
            ((float*)data_out)[i+(j*nlatlon)] = missing_data.floatval;
          }
        }
      }
    }
  }
/*
 * Free memory.
 */
  if((void*)dlatmask != latmask) NclFree(dlatmask);
  if((void*)dlonmask != lonmask) NclFree(dlonmask);
  if((void*)dlatdata != latdata) NclFree(dlatdata);
  if((void*)dlondata != londata) NclFree(dlondata);
  NclFree(work);

  return(NclReturnValue(data_out,ndims_data,dsizes_data,&missing_data,
                        type_data,0));
}

/*
 * This function was to test if it was faster to mask a data array
 * based on gc_inout values, as a built-in function, rather than
 * in an NCL script like we've done with some of the shapefiles
 * example. We didn't notice a huge improvement, probably because
 * most of the time is spent in the internal Fortran routine for gc_inout.
 *
 * This function hasn't been updated to keep up with changes to gc_inout,
 * so it may be out-of-date. The main changes to gc_inout were to allow
 * leftmost dimemsnions.
 */
NhlErrorTypes gc_inout_mask_proc_W( void )
{
/*
 * Input variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  void *latdata, *londata, *latmask, *lonmask;
  int *opt;
  double *dlatdata, *dlondata, *dlatmask, *dlonmask;
  double *dlatmask_tmp, *dlonmask_tmp;
  ng_size_t *dsizes_data;
  ng_size_t dsizes_latdata[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_londata[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_latmask[1];
  ng_size_t dsizes_lonmask[1];
  int has_missing_data, ndims_data, size_data_type, ndims_latdata, ndims_londata;
  NclBasicDataTypes type_data, type_latmask, type_lonmask, type_latdata, type_londata;
 
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, k, npts, nlat, nlon, nlatlon, size_leftmost;
  int itmp, inpts;
  double *work;
  logical IS_1D_COORD;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  data = _NclGetArg(0,6,WRITE_IT);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  default:
    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
    return(NhlFATAL);
  }
  if(tmp_md == NULL)
    return(NhlFATAL);
/*
 * To make things easier, assign various aspects of multidval
 * to individual variables.
 */
  ndims_data       = tmp_md->multidval.n_dims;
  dsizes_data      = tmp_md->multidval.dim_sizes;
  type_data        = tmp_md->multidval.data_type;
  size_data_type        = tmp_md->multidval.type->type_class.size;
  has_missing_data = tmp_md->multidval.missing_value.has_missing;

  if(!has_missing_data) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
         "gc_inout_mask_proc: the data array must have a _FillValue attribute set");
   return(NhlFATAL);
  }

  latdata = (void*)NclGetArgValue(
          1,
          6,
          &ndims_latdata,
          dsizes_latdata,
          NULL,
          NULL,
          &type_latdata,
          DONT_CARE);

  londata = (void*)NclGetArgValue(
          2,
          6,
          &ndims_londata,
          dsizes_londata,
          NULL,
          NULL,
          &type_londata,
          DONT_CARE);
  latmask = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_latmask,
          NULL,
          NULL,
          &type_latmask,
          DONT_CARE);

  lonmask = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lonmask,
          NULL,
          NULL,
          &type_lonmask,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Error checking for input/output arrays.
 *
 * latdata and londata can either be 1D coord arrays, or arrays 
 * of the same dimensionality as the rightmost dimension of data.
 *
 * The assumption is if the rightmost two dimensions of data
 * are equal to the rightmost dimension of latdata and londata
 * respectively, then we have 1D coordinate arrays. Otherwise,
 * it's assumed that that latdata/londata have the same dimensions,
 * as each other, and the same rightmost dimensions as data.
 */
  if(ndims_latdata == 1 && ndims_londata == 1 && ndims_data >=2 &&
     dsizes_latdata[0] == dsizes_data[ndims_data-2] && 
     dsizes_londata[0] == dsizes_data[ndims_data-1]) {
    IS_1D_COORD = True;
    nlat = dsizes_latdata[0];
    nlon = dsizes_londata[0];
    nlatlon = nlat * nlon;
  }
  else {
    IS_1D_COORD = False;
    if (ndims_latdata != ndims_londata) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
                "gc_inout_mask_proc: the lat/lon data arrays must have the same number of dimensions.");
      return(NhlFATAL);
    }
    nlatlon = 1;
    for(i = 0; i < ndims_latdata; i++) {
      if (dsizes_latdata[i] != dsizes_londata[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                  "gc_inout_mask_proc: the lat/lon data arrays must have the same dimensionality.");
        return(NhlFATAL);
      }
      nlatlon *= dsizes_latdata[i];
    }

    if (ndims_data == ndims_londata) {
      for(i = 0; i < ndims_latdata; i++) {
        if (dsizes_data[i] != dsizes_londata[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "gc_inout_mask_proc: if the data/lat/lon arrays have the same number of dimensions, then the dimensions must be the same");
          return(NhlFATAL);
        }
      }
    }
    else if (ndims_data > ndims_londata) {
      for(i = 0; i < ndims_latdata; i++) {
        if (dsizes_data[ndims_data-ndims_londata+i] != dsizes_londata[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "gc_inout_mask_proc: the rightmost dimensions of data must be the same as the dimensions of lat/lon");
          return(NhlFATAL);
        }
      }
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
                "gc_inout_mask_proc: either the rightmost dimensions of data must be the same as the dimensions of lat/lon, or all the dimensions must be the same");
      return(NhlFATAL);
    }
  }

  if (dsizes_lonmask[0] != dsizes_latmask[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, 
         "gc_inout_mask_proc: the latmask/lonmask arrays must be the same length");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes for mask arrays.
 */
  npts = dsizes_latmask[0];
  if (npts < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
         "gc_inout_mask_proc: the lat/lon mask array have at least three points.");
    return(NhlFATAL);
  }
/*
 * Check if larger than INT_MAX.  We will assign the int "inpts" 
 * later once we make sure the mask arrays are cyclic.
 */
  if(npts > INT_MAX || (npts+1) > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_proc: npts and/or (npts+1) is greater than INT_MAX");
    return(NhlFATAL);
  }

/*
 * Calculate size of data.
 */
  size_leftmost = 1;
  if(IS_1D_COORD) {
    for(i = 0; i < ndims_data-2; i++) {
      size_leftmost *= dsizes_data[i];
    }
  }
  else {
    for(i = 0; i < ndims_data-ndims_latdata; i++) {
      size_leftmost *= dsizes_data[i];
    }
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlatmask_tmp = coerce_input_double(latmask, type_latmask, npts,
                                     0, NULL, NULL);
  dlonmask_tmp = coerce_input_double(lonmask, type_lonmask, npts,
                                     0, NULL, NULL);

  if(dlatmask_tmp == NULL || dlonmask_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_proc: unable to allocate memory for coercing lat/lon mask arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Close the latmask/lonmask polygon if necessary.
 */
  if (dlatmask_tmp[0] != dlatmask_tmp[npts-1] || 
      dlonmask_tmp[0] != dlonmask_tmp[npts-1]) {
    dlatmask = (double *) calloc(npts+1,sizeof(double));
    dlonmask = (double *) calloc(npts+1,sizeof(double));
    if(dlatmask == NULL || dlonmask == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_proc: unable to allocate memory for closing lat/lon mask array");
      return(NhlFATAL);
    }
    memcpy(dlatmask,dlatmask_tmp,npts*sizeof(double));
    memcpy(dlonmask,dlonmask_tmp,npts*sizeof(double));
    dlatmask[npts] = dlatmask[0];
    dlonmask[npts] = dlonmask[0];
    if(type_latmask != NCL_double) NclFree(dlatmask_tmp);
    if(type_lonmask != NCL_double) NclFree(dlonmask_tmp);
    inpts = (int) npts+1;
  }
  else {
    dlatmask = dlatmask_tmp;
    dlonmask = dlonmask_tmp;
    inpts = (int) npts;
  }

  work = (double *)calloc(4*(npts+1), sizeof(double));
  if(work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_proc: unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Loop through each lat/lon point of the data and see if it is in or out
 * of the lat/lon mask array.
 */
  if(IS_1D_COORD) {
    dlatdata = coerce_input_double(latdata, type_latdata, nlat, 
                                   0, NULL, NULL);
    dlondata = coerce_input_double(londata, type_londata, nlon, 
                                   0, NULL, NULL);
  }
  else {
    dlatdata = coerce_input_double(latdata, type_latdata, nlatlon, 
                                   0, NULL, NULL);
    dlondata = coerce_input_double(londata, type_londata, nlatlon, 
                                   0, NULL, NULL);
  }
  if(dlatdata == NULL || dlondata == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout_mask_proc: unable to coerce input lat/lon arrays to double");
    return(NhlFATAL);
  }

  if(IS_1D_COORD) {
/*
 * We are dealing with 1D coordinate arrays, so for each lat value,
 * we have to loop across each lon value and pass the pair to
 * gcinout.
 */
    for( i = 0; i < nlat; i++ ) {
      for( j = 0; j < nlon; j++ ) {
        itmp = NGCALLF(gcinout,GCINOUT)(&dlatdata[i],&dlondata[j],
                                        dlatmask,dlonmask,&inpts,work);
/*
 * itmp==0 implies lat/lon point is inside the mask.
 * 
 * If opt==0, then mask points outside mask. 
 *    opt==1, then mask points inside mask.
 */
        if( (itmp == 0 && *opt == 1) || (itmp != 0 && *opt == 0)) {
          for(k = 0; k < size_leftmost; k++) {
            memcpy(&(((char*)tmp_md->multidval.val)[((k*nlatlon)+(i*nlon)+j)*size_data_type]),
                   &(tmp_md->multidval.missing_value.value),size_data_type);
          }
        }
      }
    }
  }
  else {
/*
 * We are NOT dealing with 1D coordinate arrays, so we can loop across
 * the lat/lon arrays together and pass each pair to gcinout.
 */
    for( i = 0; i < nlatlon; i++ ) {
      itmp = NGCALLF(gcinout,GCINOUT)(&dlatdata[i],&dlondata[i],
                                      dlatmask,dlonmask,&inpts,work);
/*
 * itmp==0 implies lat/lon point is inside the mask.
 * 
 * If opt==0, then mask points outside mask. 
 *    opt==1, then mask points inside mask.
 */
      if( (itmp == 0 && *opt == 1) || (itmp != 0 && *opt == 0)) {
        for(j = 0; j < size_leftmost; j++) {
          memcpy(&(((char*)tmp_md->multidval.val)[(i+(j*nlatlon))*size_data_type]),
                 &(tmp_md->multidval.missing_value.value),size_data_type);
        }
      }
    }
  }

/*
 * Free memory.
 */
  if((void*)dlatmask != latmask) NclFree(dlatmask);
  if((void*)dlonmask != lonmask) NclFree(dlonmask);
  if((void*)dlatdata != latdata) NclFree(dlatdata);
  if((void*)dlondata != londata) NclFree(dlondata);
  NclFree(work);

/*
 * This is a procedure, so just return.
 */
  return(NhlNOERROR);
}

NhlErrorTypes gc_onarc_W( void )
{
/*
 * Input variables
 */
  void *plat, *plon, *lat, *lon;
  double *dplat, *dplon, *dlat, *dlon;

  ng_size_t dsizes_plat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_plon[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon, ndims_plat, ndims_plon;
  NclBasicDataTypes type_lat, type_lon, type_plat, type_plon;
 
/*
 * output variable 
 */
  logical *tfval;
  ng_size_t size_tfval;
  NclBasicDataTypes type_tfval;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,tsize;
  int itmp;
  double tol = 1.e-10;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  plat = (void*)NclGetArgValue(
          0,
          4,
          &ndims_plat,
          dsizes_plat,
          NULL,
          NULL,
          &type_plat,
          DONT_CARE);

  plon = (void*)NclGetArgValue(
          1,
          4,
          &ndims_plon,
          dsizes_plon,
          NULL,
          NULL,
          &type_plon,
          DONT_CARE);
  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          3,
          4,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check dimension sizes. The lat and lon arguments must be
 * arrays with rightmost dimension 2.  The plat and plon
 * arguments must have one less dimension than the lat and
 * lon variables and the dimension sizes of plat and plon
 * must agree with those of lat and lon prior to the final
 * dimension.
 */

/*
 * Check rightmost dimension sizes for lat and lon.
 */
  if(dsizes_lat[ndims_lat-1] != 2 || dsizes_lon[ndims_lon-1] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_onarc: the last (rightmost) dimension of the rightmost two arrays must be 2");
    return(NhlFATAL);
  }

/*
 * Check that the lat and lon arrays have the same number of dimensions.
 */
  if (!(ndims_lat == ndims_lon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_onarc: the rightmost lat and lon input arrays must have the same number of dimensions.");
    return(NhlFATAL);
  }

/* 
 * Check that the dimension sizes for the lat and lon arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_onarc: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Check that plat and plon have the same number of dimesions and
 * one less dimension than lat and lon, except in the case that 
 * they all have one dimension.
 */
  if (!(ndims_plat == ndims_plon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_onarc: the first two input arrays must have the same number of dimensions.");
   return(NhlFATAL);
  }
  if (ndims_lat == 1) {
    if ( ndims_plat != 1 || ndims_plon != 1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_onarc: if the final two arrays are singly dimensioned, then the first two must be as well.");
      return(NhlFATAL);
    }
  }
  else {
    if (ndims_plat != ndims_lat-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_onarc: the first two input arrays must have exactly one less dimension than the last two.");
      return(NhlFATAL);
    }
  }

/*
 *  Check on dimension sizes of plat/plon versus lat/lon.
 */
  if (ndims_lat > 0) {
    for(i = 0; i < ndims_lat-1; i++) {
      if ((dsizes_plat[i] != dsizes_lon[i]) || 
              (dsizes_plon[i] != dsizes_lon[i]))  {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "gc_onarc: the dimensions sizes for the first two arrays must agree with the dimension sizes of the last two up through the penultimate dimension of the last two.");
        return(NhlFATAL);
      }
    }
  }
  
/*
 * Determine size for the return array.
 */
  size_tfval = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_tfval *= dsizes_lat[i];
  }

/*
 * Determine total size of input arrays.
 */
  tsize = 1;
  for (i = 0; i < ndims_lat; i++) {
    tsize *= dsizes_lat[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, 2*size_tfval, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, 2*size_tfval, 0, NULL, NULL);
  dplat  = coerce_input_double(plat, type_plat, size_tfval, 0, NULL, NULL);
  dplon  = coerce_input_double(plon, type_plon, size_tfval, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL || dplat == NULL || dplon ==NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_onarc: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_tfval = NCL_logical;
  tfval = (logical *)calloc(size_tfval, sizeof(logical));
  if(tfval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_onarc: Unable to allocate memory for return.");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_tfval; i++ ) {
    itmp = NGCALLF(gconarc,GCONARC)(dplat+i, dplon+i, 
             dlat+2*i, dlon+2*i, dlat+2*i+1, dlon+2*i+1, &tol);
    if (itmp == 1) {
      tfval[i] = True;
    }
    else {
      tfval[i] = False;
    }
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);
  if((void*)dplat != plat) NclFree(dplat);
  if((void*)dplon != plon) NclFree(dplon);

/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    return(NclReturnValue(tfval,1,dsizes_lat,NULL,type_tfval,0));
  }
  else {
    return(NclReturnValue(tfval,ndims_lat-1, dsizes_lat,NULL,type_tfval,0));
  }
}

NhlErrorTypes gc_pnt2gc_W( void )
{
/*
 * Input variables
 */
  void *plat, *plon, *lat, *lon;
  double *dplat, *dplon, *dlat, *dlon;

  ng_size_t dsizes_plat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_plon[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon, ndims_plat, ndims_plon;
  NclBasicDataTypes type_lat, type_lon, type_plat, type_plon;
 
/*
 * output variable 
 */
  void *dist; 
  double *tmp_dist;
  ng_size_t size_dist;
  NclBasicDataTypes type_dist;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  plat = (void*)NclGetArgValue(
          0,
          4,
          &ndims_plat,
          dsizes_plat,
          NULL,
          NULL,
          &type_plat,
          DONT_CARE);

  plon = (void*)NclGetArgValue(
          1,
          4,
          &ndims_plon,
          dsizes_plon,
          NULL,
          NULL,
          &type_plon,
          DONT_CARE);
  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          3,
          4,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check dimension sizes. The lat and lon arguments must be
 * arrays with rightmost dimension 2.  The plat and plon
 * arguments must have one less dimension than the lat and
 * lon variables and the dimension sizes of plat and plon
 * must agree with those of lat and lon prior to the final
 * dimension.
 */

/*
 * Check rightmost dimension sizes for lat and lon.
 */
  if(dsizes_lat[ndims_lat-1] != 2 || dsizes_lon[ndims_lon-1] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_pnt2gc: the last (rightmost) dimension of the rightmost two arrays must be 2");
    return(NhlFATAL);
  }

/*
 * Check that the lat and lon arrays have the same number of dimensions.
 */
  if (!(ndims_lat == ndims_lon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_pnt2gc: the rightmost lat and lon input arrays must have the same number of dimensions.");
    return(NhlFATAL);
  }

/* 
 * Check that the dimension sizes for the lat and lon arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_pnt2gc: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Check that plat and plon have the same number of dimesions and
 * one less dimension than lat and lon, except in the case that 
 * they all have one dimension.
 */
  if (!(ndims_plat == ndims_plon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_pnt2gc: the first two input arrays must have the same number of dimensions.");
   return(NhlFATAL);
  }
  if (ndims_lat == 1) {
    if ( ndims_plat != 1 || ndims_plon != 1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_pnt2gc: if the final two arrays are singly dimensioned, then the first two must be as well.");
      return(NhlFATAL);
    }
  }
  else {
    if (ndims_plat != ndims_lat-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_pnt2gc: the first two input arrays must have exactly one less dimension than the last two.");
      return(NhlFATAL);
    }
  }

/*
 *  Check on dimension sizes of plat/plon versus lat/lon.
 */
  if (ndims_lat > 0) {
    for(i = 0; i < ndims_lat-1; i++) {
      if ((dsizes_plat[i] != dsizes_lon[i]) || 
              (dsizes_plon[i] != dsizes_lon[i]))  {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "gc_pnt2gc: the dimensions sizes for the first two arrays must agree with the dimension sizes of the last two up through the penultimate dimension of the last two.");
        return(NhlFATAL);
      }
    }
  }
  
/*
 * Determine size for the return array.
 */
  size_dist = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_dist *= dsizes_lat[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, 2*size_dist, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, 2*size_dist, 0, NULL, NULL);
  dplat  = coerce_input_double(plat, type_plat, size_dist, 0, NULL, NULL);
  dplon  = coerce_input_double(plon, type_plon, size_dist, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL || dplat == NULL || dplon ==NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_pnt2gc: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_dist = NCL_float;
  if (type_lat == NCL_double || type_lon == NCL_double) {
    type_dist = NCL_double;
    dist = (void *)calloc(size_dist, sizeof(double));
    if(dist == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_pnt2gc: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    dist = (void *)calloc(size_dist, sizeof(float));
    if(dist == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_qarea: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  tmp_dist = (double *)calloc(1,sizeof(double));
  for( i = 0; i < size_dist; i++ ) {

/*
 *  If the type of the return variable is double, then call the
 *  Fortran function with tmp_dist set to the correct address in
 *  the output variable.
 */
    if (type_dist == NCL_double) tmp_dist = &(((double *)dist)[i]);
      *tmp_dist = NGCALLF(gcpnt2gc,GCPNT2GC)(dlat+2*i, dlon+2*i, 
                             dlat+2*i+1, dlon+2*i+1, dplat+i, dplon+i);

/*
 *  If the type of the return variable is not double, then return floats
 *  in the output array.
 */
    if(type_dist != NCL_double) {
      ((float *) dist)[i] = (float)(*tmp_dist);
    }
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);
  if((void*)dplat != plat) NclFree(dplat);
  if((void*)dplon != plon) NclFree(dplon);
  if(type_dist != NCL_double) NclFree(tmp_dist);

/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    return(NclReturnValue(dist,1,dsizes_lat,NULL,type_dist,0));
  }
  else {
    return(NclReturnValue(dist,ndims_lat-1, dsizes_lat,NULL,type_dist,0));
  }
}

NhlErrorTypes gc_dangle_W( void )
{
/*
 * Input variables
 */
  void *lat, *lon;
  double *dlat, *dlon;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *dangle; 
  double *tmp_dangle;
  ng_size_t size_dangle;
  NclBasicDataTypes type_dangle;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat = (void*)NclGetArgValue(
          0,
          2,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

/*
 * Check dimension sizes. The arguments must be arrays with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for lat.
 */
  if(dsizes_lat[ndims_lat-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_dangle: the last (rightmost) dimension of arrays must be 3");
    return(NhlFATAL);
  }

/*
 * Check that the arrays have the same number of dimensions.
 */
  if (!(ndims_lat == ndims_lon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
      "gc_dangle: the input arrays must have the same number of dimensions.");
  }

/* 
 * Check that the dimension sizes for the arrays are the same. 
 */  
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_dangle: the arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Determine size for the return array.
 */
  size_dangle = 1;
  for (i = 0; i < ndims_lat-1; i++) {
    size_dangle *= dsizes_lat[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dlat  = coerce_input_double(lat, type_lat, 3*size_dangle, 0, NULL, NULL);
  dlon  = coerce_input_double(lon, type_lon, 3*size_dangle, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_dangle: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_dangle = NCL_float;
  if (type_lat == NCL_double || type_lon == NCL_double) {
    type_dangle = NCL_double;
    dangle = (void *)calloc(size_dangle, sizeof(double));
    if(dangle == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_dangle: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    dangle = (void *)calloc(size_dangle, sizeof(float));
    if(dangle == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_dangle: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  tmp_dangle = (double *)calloc(1,sizeof(double));
  for( i = 0; i < size_dangle; i++ ) {

/*
 *  If the type of the return variable is double, then call the
 *  Fortran function with tmp_dangle set to the correct address in
 *  the output variable.
 */
    if (type_dangle == NCL_double) tmp_dangle = &(((double *)dangle)[i]);
      *tmp_dangle = NGCALLF(gcdangle,GCDANGLE)(dlat+3*i, dlon+3*i, 
                             dlat+3*i+1, dlon+3*i+1, dlat+3*i+2, dlon+3*i+2);

/*
 *  If the type of the return variable is not double, then return floats
 *  in the output array.
 */
    if(type_dangle != NCL_double) {
      ((float *) dangle)[i] = (float)(*tmp_dangle);
    }
  }

/*
 * free memory.
 */
  if((void*)dlat != lat) NclFree(dlat);
  if((void*)dlon != lon) NclFree(dlon);
  if(type_dangle != NCL_double) NclFree(tmp_dangle);

/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    return(NclReturnValue(dangle,1,dsizes_lat,NULL,type_dangle,0));
  }
  else {
    return(NclReturnValue(dangle,ndims_lat-1,dsizes_lat,NULL,type_dangle,0));
  }
}
