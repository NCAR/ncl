#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dmapgci,DMAPGCI)(double *,double *,double *,double *,
                                     int *,double *,double *);

extern double NGCALLF(dgcdist,DGCDIST)(double *,double *,double *,double *,
                                       int *);

void convert_lon(double *tmplon, int jcode, ng_size_t nlon)
{
  ng_size_t i;
/*
 * jcode >= 0 --> lon:    0 to 360
 * jcode <  0 --> lon: -180 to 180
 */
  for(i = 0; i < nlon; i++) {
    if((jcode >= 0 && tmplon[i] <    0.) ||
       (jcode <  0 && tmplon[i] < -180.)) {
      tmplon[i] += 360.;
    }
    else if(jcode < 0 && tmplon[i] > 180.) {
      tmplon[i] -= 360.;
    }
  }
}

/*
 * Outline of how the input arguments work for this function:
 *
 * dist = gc_latlon(lat1,lon1,lat2,lon2,npts,code)
 *
 * lat1, lon1, lat2, and lon2 can either be all the same dimension sizes,
 * or lat1, lon1 can be scalars, and lat2 and lon2 can be the same
 * dimension sizes as each other.
 *
 * What gets returned are the distances between the lat1,lon1 and
 * lat2,lon2 pairs, along with the interpolated lat, lon values and
 * the spacing used (these last three are returned via attributes).
 *
 * "npts" is the number of values that you want interpolated between
 * the pairs. If "npts" is <= 2, then no values are interpolated between
 * the pairs; just the pair values themselves are used.
 * 
 */

NhlErrorTypes gc_latlon_W( void )
{
/*
 * Input variables
 */
  void *lat1, *lon1, *lat2, *lon2, *tmp_nlatlon;
  ng_size_t dsizes_lat1[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat2[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon1[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon2[NCL_MAX_DIMENSIONS]; 
  int ndims_lat1, ndims_lon1, ndims_lat2, ndims_lon2;
  ng_size_t *nlatlon;
  int *code;
  NclBasicDataTypes type_lat1, type_lon1, type_lat2, type_lon2, type_nlatlon;
/*
 * Output variables.
 */
  void *dist, *spac, *lat, *lon;
  NclBasicDataTypes type_lon, type_lat, type_dist;
  int ndims_dist;
  ng_size_t *dsizes_dist;
  ng_size_t dsizes[1];
  NclQuark *units;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Other variables
 */
  double *tmp_lat1, *tmp_lon1, *tmp_lat2, *tmp_lon2;
  double *tmp_lat = NULL;
  double *tmp_lon = NULL;
  double *tmp_dist, *tmp_spac;
  ng_size_t i, nlatlon2, npts, nlatlon_output, nlatlon_new, index2;
  int inpts, is_scalar_latlon1, icode;
  extern void convert_lon(double*,int,ng_size_t);

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat1 = (void*)NclGetArgValue(
          0,
          6,
          &ndims_lat1,
          dsizes_lat1,
          NULL,
          NULL,
          &type_lat1,
          DONT_CARE);

  lon1 = (void*)NclGetArgValue(
          1,
          6,
          &ndims_lon1,
          dsizes_lon1,
          NULL,
          NULL,
          &type_lon1,
          DONT_CARE);

  lat2 = (void*)NclGetArgValue(
          2,
          6,
          &ndims_lat2,
          dsizes_lat2,
          NULL,
          NULL,
          &type_lat2,
          DONT_CARE);

  lon2 = (void*)NclGetArgValue(
          3,
          6,
          &ndims_lon2,
          dsizes_lon2,
          NULL,
          NULL,
          &type_lon2,
          DONT_CARE);

  tmp_nlatlon = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_nlatlon,
          DONT_CARE);

  nlatlon = get_dimensions(tmp_nlatlon,1,type_nlatlon,"gc_latlon");
  if(nlatlon == NULL) 
    return(NhlFATAL);

  code = (int*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  icode = abs(*code);

/*
 * Check dimension sizes. The lat/lon arrays must either all be the same
 * sizes, or lat1, lon1 can both be scalars.
 */
  if(ndims_lat1 != ndims_lon1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: lat1 and lon1 must have the same number of dimensions");
    return(NhlFATAL);
  }
  if(ndims_lat2 != ndims_lon2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: lat2 and lon2 must have the same number of dimensions");
    return(NhlFATAL);
  }
  is_scalar_latlon1 = is_scalar(ndims_lat1,dsizes_lat1);

  if(!is_scalar_latlon1 && ndims_lat1 != ndims_lat2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: lat1,lon1 must either both be scalars, or have the same dimensions as lat2,lon2");
    return(NhlFATAL);
  }

/*
 * Check that the dimension sizes for lat1 and lon1 are the same.
 */
  for(i = 0; i < ndims_lat1; i++) {
    if(dsizes_lat1[i] != dsizes_lon1[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: lat1 and lon1 must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Check that the dimension sizes for lat2 and lon2 are the same, and that
 * they are the same as lat1 and lon1, if lat1,lon1 are not scalars.
 * Also, calculate the total size of the lat2, lon2 input points.
 */
  nlatlon2 = 1;
  for(i = 0; i < ndims_lat2; i++) {
    if(dsizes_lat2[i] != dsizes_lon2[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: lat2 and lon2 must have the same dimension sizes");
      return(NhlFATAL);
    }
    if(!is_scalar_latlon1 && (dsizes_lat1[i] != dsizes_lat2[i])) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: lat1, lon1, lat2, lon2  must have the same dimension sizes if lat1, lon1 are not scalar");
      return(NhlFATAL);
    }
    nlatlon2 *= dsizes_lat2[i];
  }

/*
 * Figure out the "real" size of npts, because if nlatlon < 2, then
 * this means we are not doing any extra interpolation between the
 * lat1,lon1 and lat2,lon2 pairs, and hence npts is 0.  Otherwise,
 * npts is the number of lat,lon values *not* including the end points.
 */ 
  if(*nlatlon < 2) {
    npts        = 0;
    nlatlon_new = 2;
  }
  else {
    npts        = *nlatlon - 2;
    nlatlon_new = *nlatlon;
  }

/*
 * Test dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * nlatlon_output will be the total size of the interpolated lat/lon
 * points that are returned with this function.
 */
  nlatlon_output = nlatlon2 * nlatlon_new;

/*
 * Determine type and allocate space for return array (and attributes):
 * dist (spac, lat, lon).
 */
  type_dist = NCL_float;
  if(type_lat1 == NCL_double || type_lat2 == NCL_double) {
    type_lat  = NCL_double;
    type_dist = NCL_double;
    lat       = (void*)calloc(nlatlon_output,sizeof(double));
    if(lat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for return attributes");
      return(NhlFATAL);
    }
  }
  else {
    type_lat = NCL_float;
    lat = (void*)calloc(nlatlon_output,sizeof(float));
    if(lat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for return attributes");
      return(NhlFATAL);
    }
    if(npts > 0) {
      tmp_lat = (double*)calloc(npts,sizeof(double));
      if(tmp_lat == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for return attributes");
        return(NhlFATAL);
      }
    }
  }

  if(type_lon1 == NCL_double || type_lon2 == NCL_double) {
    type_lon  = NCL_double;
    type_dist = NCL_double;
    lon       = (void*)calloc(nlatlon_output,sizeof(double));
    if(lon == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for return attributes");
      return(NhlFATAL);
    }
  }
  else {
    type_lon = NCL_float;
    lon = (void*)calloc(nlatlon_output,sizeof(float));
    if(lon == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for output arrays and attributes");
      return(NhlFATAL);
    }
    if(npts > 0) {
      tmp_lon = (double*)calloc(npts,sizeof(double));
      if(tmp_lon == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for output arrays and attributes");
        return(NhlFATAL);
      }
    }
  }

  ndims_dist = ndims_lat2;
  dsizes_dist = (ng_size_t*)malloc(ndims_dist*sizeof(ng_size_t));
  for(i = 0; i < ndims_dist; i++) dsizes_dist[i] = dsizes_lat2[i];

  if(type_dist == NCL_double) {
    dist = (void*)calloc(nlatlon2,sizeof(double));
    spac = (void*)calloc(nlatlon2,sizeof(double));
  }
  else {
    dist = (void*)calloc(nlatlon2,sizeof(float));
    spac = (void*)calloc(nlatlon2,sizeof(float));
  }
  if(dist == NULL || spac == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to allocate memory for output arrays and attributes");
    return(NhlFATAL);
  }
/*
 * Create temporary arrays to hold double input.
 */
  tmp_dist = (double*)calloc(1,sizeof(double));
  tmp_spac = (double*)calloc(1,sizeof(double));
  tmp_lat1 = (double*)calloc(1,sizeof(double));
  tmp_lon1 = (double*)calloc(1,sizeof(double));
  tmp_lat2 = (double*)calloc(1,sizeof(double));
  tmp_lon2 = (double*)calloc(1,sizeof(double));

  if(tmp_lon1 == NULL || tmp_lat1 == NULL ||
     tmp_lon2 == NULL || tmp_lat2 == NULL ||
     tmp_dist == NULL || tmp_spac == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gc_latlon: Unable to create memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * If npts > 0 (and hence we have some lat,lon values to interpolate), 
 * loop through the number of input lat2/lon2 points we have, and pass
 * each scalar lat1,lon and lat2,lon2 pair to the Fortran function that
 * does the interpolation. Then, calculate the distance and spacing.
 */
  index2 = 0;
  for(i = 0; i < nlatlon2; i++ ) {
/*
 * If lat1 and lon1 are scalar, only coerce them once.
 */
    if(!is_scalar_latlon1 || (is_scalar_latlon1 && i == 0)) {
      coerce_subset_input_double(lat1,tmp_lat1,i,type_lat1,1,0,NULL,NULL);
      coerce_subset_input_double(lon1,tmp_lon1,i,type_lon1,1,0,NULL,NULL);
    }
    coerce_subset_input_double(lat2,tmp_lat2,i,type_lat2,1,0,NULL,NULL);
    coerce_subset_input_double(lon2,tmp_lon2,i,type_lon2,1,0,NULL,NULL);

    if(npts > 0) {
      if(type_lat == NCL_double) {
/*
 * Point tmp_lat to appropriate location in lat.  Don't forget that
 * the output lat is returning the end lat points as well, so we
 * have to index into lat accordingly. That is, lat[index2] contains
 * the first end point, lat[index2+1:index2+npts-1] contains the
 * interpolated values, and lat[index2+npts] contains the second end point.
 */
        tmp_lat = &((double*)lat)[index2+1];
      }
      if(type_lon == NCL_double) {
/*
 * Point tmp_lon to appropriate location in lon.  Don't forget that
 * the output lon is returning the end lon points as well, so we
 * have to index into lon accordingly. That is, lon[index2] contains
 * the first end point, lon[index2+1:index2+npts-1] contains the
 * interpolated values, and lon[index2+npts] contains the second end point.
 */
        tmp_lon = &((double*)lon)[index2+1];
      }
/*
 * Call Fortran function.
 */
      NGCALLF(dmapgci,DMAPGCI)(tmp_lat1,tmp_lon1,tmp_lat2,tmp_lon2,&inpts,
                               tmp_lat,tmp_lon);
/*
 * Copy latitudes and longitudes to output array.
 * We have to copy the two end points separately later.
 * For the longitudes, we have to convert them first based on
 * the value of "code".
 */
      convert_lon(tmp_lon,*code,npts);
      if(type_lat == NCL_float) {
        coerce_output_float_only(lat,tmp_lat,npts,index2+1);
      }
      if(type_lon == NCL_float) {
        coerce_output_float_only(lon,tmp_lon,npts,index2+1);
      }
    }  /*  if(npts > 0)  */
/*
 * Convert the begin and end lon points.
 */
    convert_lon(tmp_lon1,*code,1);
    convert_lon(tmp_lon2,*code,1);
/*
 * Copy over beginning lat and lon points.
 */
    coerce_output_float_or_double(lat,tmp_lat1,type_lat,1,index2);
    coerce_output_float_or_double(lon,tmp_lon1,type_lon,1,index2);
/*
 * Copy over end lat and lon points.
 */
    coerce_output_float_or_double(lat,tmp_lat2,type_lat,1,index2+npts+1);
    coerce_output_float_or_double(lon,tmp_lon2,type_lon,1,index2+npts+1);
/*
 * Calculate distance between two end points.
 */
    *tmp_dist = NGCALLF(dgcdist,DGCDIST)(tmp_lat1,tmp_lon1,
                                         tmp_lat2,tmp_lon2,&icode);
/*
 * Calculate the spacing between points.  If npts = 0, then the spacing
 * is just the distance between the two end points.
 */
    if(npts > 0) {
      *tmp_spac = NGCALLF(dgcdist,DGCDIST)(&tmp_lat[0],&tmp_lon[0],
                                           &tmp_lat[1],&tmp_lon[1],&icode);
    }
    else {
      *tmp_spac = *tmp_dist;
    }
    coerce_output_float_or_double(dist,tmp_dist,type_dist,1,i);
    coerce_output_float_or_double(spac,tmp_spac,type_dist,1,i);
/*
 * Advance the index counter.
 */
    index2 += nlatlon_new;
  }

/*
 * Free tmp arrays.
 */
  if(npts > 0) {
    if(type_lat != NCL_double) NclFree(tmp_lat);
    if(type_lon != NCL_double) NclFree(tmp_lon);
  }
  NclFree(tmp_lat1);
  NclFree(tmp_lon1);
  NclFree(tmp_lat2);
  NclFree(tmp_lon2);
  NclFree(tmp_dist);
  NclFree(tmp_spac);
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Set up variable to return.
 */
  dsizes[0]= nlatlon2;
  if(type_dist == NCL_float) {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              dist,
                              NULL,
                              ndims_dist,
                              dsizes_dist,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           spac,
                           NULL,
                           1,
                           dsizes,
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
                              dist,
                              NULL,
                              ndims_dist,
                              dsizes_dist,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           spac,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  _NclAddAtt(
             att_id,
             "spacing",
             att_md,
             NULL
             );

  dsizes[0] = nlatlon_output;
  if(type_lat == NCL_double) {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           lat,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  else {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           lat,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  _NclAddAtt(
             att_id,
             "gclat",
             att_md,
             NULL
             );

  if(type_lon == NCL_double) {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           lon,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  else {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           lon,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  _NclAddAtt(
             att_id,
             "gclon",
             att_md,
             NULL
             );

  if(1 <= icode && icode <=4) {
    dsizes[0] = 1;
    units  = (NclQuark*)NclMalloc(sizeof(NclQuark));
    switch(icode) {
    case  1:
      *units = NrmStringToQuark("radians");
      break;
    case  2:
      *units = NrmStringToQuark("degrees");
      break;
    case  3:
      *units = NrmStringToQuark("meters");
      break;
    case  4:
      *units = NrmStringToQuark("kilometers");
      break;
    }

    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           units,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypestringClass
                           );
    _NclAddAtt(
               att_id,
               "units",
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

  NclFree(dsizes_dist);
  NclFree(nlatlon);
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}

