#include "wrapper.h"

extern double NGCALLF(gctarea,GCTAREA)(double*,double*,double*,
                                       double*,double*,double*);
extern double NGCALLF(gcqarea,GCQAREA)(double*,double*,double*,
                                       double*,double*,double*,
                                       double*,double*);
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
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *aangle; 
  double *tmp_aangle;
  int size_aangle;
  NclBasicDataTypes type_aangle;

/*
 * Declare various variables for random purposes.
 */
  int i;

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
          2);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *qarea; 
  double *tmp_qarea;
  int size_qarea;
  NclBasicDataTypes type_qarea;

/*
 * Declare various variables for random purposes.
 */
  int i;

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
          2);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
      *tmp_qarea = NGCALLF(gcqarea,GCQAREA)(dlat+4*i, dlon+4*i, 
		             dlat+4*i+1, dlon+4*i+1, dlat+4*i+2, dlon+4*i+2,
                             dlat+4*i+3, dlon+4*i+3);

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
    return(NclReturnValue(qarea,1,dsizes_lat,NULL,type_qarea,0));
  }
  else {
    return(NclReturnValue(qarea,ndims_lat-1, dsizes_lat,NULL,type_qarea,0));
  }
}

NhlErrorTypes gc_clkwise_W( void )
{
/*
 * Input variables
 */
  void *lat, *lon;
  double *dlat, *dlon, *tlat, *tlon;
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  logical *tfval; 
  int *tmp_tfval,itmp;
  int size_tfval,tsize,npts,nptsp1,jpol;
  NclBasicDataTypes type_tfval;

/*
 * Declare various variables for random purposes.
 */
  int i,j;

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
          2);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
      itmp = NGCALLF(gccwise,GCCWISE)(tlat,tlon,&nptsp1);
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
      itmp = NGCALLF(gccwise,GCCWISE)(dlat+jpol,dlon+jpol,&npts);
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
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *tarea; 
  double *tmp_tarea;
  int size_tarea;
  NclBasicDataTypes type_tarea;

/*
 * Declare various variables for random purposes.
 */
  int i;

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
          2);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
		             dlat+3*i+1, dlon+3*i+1, dlat+3*i+2, dlon+3*i+2);

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

/*
 * Return.
 */
  if (ndims_lat == 1) {
    dsizes_lat[0] = 1;
    return(NclReturnValue(tarea,1,dsizes_lat,NULL,type_tarea,0));
  }
  else {
    return(NclReturnValue(tarea,ndims_lat-1, dsizes_lat,NULL,type_tarea,0));
  }
}

NhlErrorTypes gc_inout_W( void )
{
/*
 * Input variables
 */
  void *plat, *plon, *lat, *lon;
  double *dplat, *dplon, *dlat, *dlon, *tlat, *tlon;

  int dsizes_plat[NCL_MAX_DIMENSIONS], dsizes_plon[NCL_MAX_DIMENSIONS];
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon, ndims_plat, ndims_plon;
  NclBasicDataTypes type_lat, type_lon, type_plat, type_plon;
 
/*
 * output variable 
 */
  logical *tfval;
  int size_tfval;
  NclBasicDataTypes type_tfval;

/*
 * Declare various variables for random purposes.
 */
  int i,itmp,npts,nptsp1,jpol,tsize;
  double *work;

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
          2);

  plon = (void*)NclGetArgValue(
          1,
          4,
          &ndims_plon,
          dsizes_plon,
          NULL,
          NULL,
          &type_plon,
          2);
  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          2);

  lon = (void*)NclGetArgValue(
          3,
          4,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

/*
 * Check number of dimensions for lat and lon.
 */
  if (ndims_lon != ndims_lat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: the lat/lon arguments must have the same number of dimensions");
    return(NhlFATAL);
  }

/*
 * Check that the dimension sizes for the lat/lon arrays are the same.
 */ 
  for(i = 0; i < ndims_lat; i++) {
    if (!(dsizes_lat[i] == dsizes_lon[i]))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_inout: the lat/lon arrays must have the same dimension sizes");
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
      "gc_inout: the first two input arrays must have the same number of dimensions.");
   return(NhlFATAL);
  }
  if (ndims_lat == 1) {
    if ( ndims_plat != 1 || ndims_plon != 1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_inout: if the final two arrays are singly dimensioned, the the first two must be as well.");
      return(NhlFATAL);
    }
  }
  else {
    if (ndims_plat != ndims_lat-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "gc_inout: the first two input arrays must have exactly one less dimension than the last two.");
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
          "gc_inout: the dimensions sizes for the first two arrays must agree with the dimension sizes of the last two up through the penultimate dimension of the last two.");
        return(NhlFATAL);
      }
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
  dplat  = coerce_input_double(plat, type_plat, size_tfval, 0, NULL, NULL);
  dplon  = coerce_input_double(plon, type_plon, size_tfval, 0, NULL, NULL);

  if(dlat == NULL || dlon == NULL || dplat == NULL || dplon ==NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_tfval = NCL_logical;
  tfval = (logical *)calloc(size_tfval, sizeof(logical));
  if(tfval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_inout: Unable to allocate memory for return.");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.  Close the polygons
 * if they are not closed.
 */
  work = (double *)calloc(4*(npts+1), sizeof(double));
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
      itmp = NGCALLF(gcinout,GCINOUT)(dplat+i,dplon+i,tlat,tlon,&nptsp1,work);
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
      itmp = NGCALLF(gcinout,GCINOUT)(dplat+i,dplon+i,dlat+jpol,dlon+jpol,
                                      &npts,work);
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
  if((void*)dplat != plat) NclFree(dplat);
  if((void*)dplon != plon) NclFree(dplon);
  NclFree(work);

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

NhlErrorTypes gc_onarc_W( void )
{
/*
 * Input variables
 */
  void *plat, *plon, *lat, *lon;
  double *dplat, *dplon, *dlat, *dlon;

  int dsizes_plat[NCL_MAX_DIMENSIONS], dsizes_plon[NCL_MAX_DIMENSIONS];
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon, ndims_plat, ndims_plon;
  NclBasicDataTypes type_lat, type_lon, type_plat, type_plon;
 
/*
 * output variable 
 */
  logical *tfval;
  int size_tfval;
  NclBasicDataTypes type_tfval;

/*
 * Declare various variables for random purposes.
 */
  int i,tsize,itmp;
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
          2);

  plon = (void*)NclGetArgValue(
          1,
          4,
          &ndims_plon,
          dsizes_plon,
          NULL,
          NULL,
          &type_plon,
          2);
  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          2);

  lon = (void*)NclGetArgValue(
          3,
          4,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
        "gc_onarc: if the final two arrays are singly dimensioned, the the first two must be as well.");
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

  int dsizes_plat[NCL_MAX_DIMENSIONS], dsizes_plon[NCL_MAX_DIMENSIONS];
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon, ndims_plat, ndims_plon;
  NclBasicDataTypes type_lat, type_lon, type_plat, type_plon;
 
/*
 * output variable 
 */
  void *dist; 
  double *tmp_dist;
  int size_dist;
  NclBasicDataTypes type_dist;

/*
 * Declare various variables for random purposes.
 */
  int i,*size_tmp;

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
          2);

  plon = (void*)NclGetArgValue(
          1,
          4,
          &ndims_plon,
          dsizes_plon,
          NULL,
          NULL,
          &type_plon,
          2);
  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          2);

  lon = (void*)NclGetArgValue(
          3,
          4,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
        "gc_pnt2gc: if the final two arrays are singly dimensioned, the the first two must be as well.");
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
  int dsizes_lat[NCL_MAX_DIMENSIONS], dsizes_lon[NCL_MAX_DIMENSIONS];
  int ndims_lat, ndims_lon;
  NclBasicDataTypes type_lat, type_lon;
 
/*
 * output variable 
 */
  void *dangle; 
  double *tmp_dangle;
  int size_dangle;
  NclBasicDataTypes type_dangle;

/*
 * Declare various variables for random purposes.
 */
  int i;

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
          2);

  lon = (void*)NclGetArgValue(
          1,
          2,
          &ndims_lon,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

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
