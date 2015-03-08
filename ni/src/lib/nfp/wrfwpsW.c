#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(write_intermediate_wps,WRITE_INTERMEDIATE_WPS)
     (char *, char *, char *, char *, char *, char *, 
      float *, int *, char *, float *, float *, float *, 
      float *, float *, float *, float *, float *, float *, 
      float *, int *, int *, logical *, float *,
      int,int,int,int,int,int,int);

NhlErrorTypes wrf_wps_write_int_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  NrmQuark *wps_im_root_name;
  char *c_wps_im_root_name;
/*
 * Argument # 1
 */
  NrmQuark *field_lmask;
  int FIELD_LEN=9;
  char c_field_lmask[FIELD_LEN+1];
/*
 * Argument # 2
 */
  NrmQuark *units_lmask;
  int UNITS_LEN=25;
  char c_units_lmask[UNITS_LEN+1];
/*
 * Argument # 3
 */
  NrmQuark *descr_lmask;
  int DESCR_LEN=46;
  char c_descr_lmask[DESCR_LEN+1];
/*
 * Argument # 4
 */
  NrmQuark *hdate;
  int HDATE_LEN=24;
  char c_hdate[HDATE_LEN+1];
/*
 * Argument # 5
 */
  NrmQuark *map_source;
  int MAPSC_LEN=32;
  char c_map_source[MAPSC_LEN+1];
/*
 * Argument # 6
 */
  void *xlvl_lmask;
  float *tmp_xlvl_lmask;
  NclBasicDataTypes type_xlvl_lmask;

/*
 * Argument # 7
 */
  int *iproj;
/*
 * Argument # 8
 */
  NrmQuark *startloc;
  int STARTLOC_LEN=8;
  char c_startloc[STARTLOC_LEN+1];
/*
 * Argument # 9
 */
  void *startlat;
  float *tmp_startlat;
  NclBasicDataTypes type_startlat;

/*
 * Argument # 10
 */
  void *startlon;
  float *tmp_startlon;
  NclBasicDataTypes type_startlon;

/*
 * Argument # 11
 */
  void *delta_lat;
  float *tmp_delta_lat;
  NclBasicDataTypes type_delta_lat;

/*
 * Argument # 12
 */
  void *delta_lon;
  float *tmp_delta_lon;
  NclBasicDataTypes type_delta_lon;

/*
 * Argument # 13
 */
  void *xlonc;
  float *tmp_xlonc;
  NclBasicDataTypes type_xlonc;

/*
 * Argument # 14
 */
  void *truelat1;
  float *tmp_truelat1;
  NclBasicDataTypes type_truelat1;

/*
 * Argument # 15
 */
  void *truelat2;
  float *tmp_truelat2;
  NclBasicDataTypes type_truelat2;

/*
 * Argument # 16
 */
  void *nlats;
  float *tmp_nlats;
  NclBasicDataTypes type_nlats;

/*
 * Argument # 17
 */
  void *dx;
  float *tmp_dx;
  NclBasicDataTypes type_dx;

/*
 * Argument # 18
 */
  void *dy;
  float *tmp_dy;
  NclBasicDataTypes type_dy;

/*
 * Argument # 19
 */
  int *nx;
/*
 * Argument # 20
 */
  int *ny;
/*
 * Argument # 21
 */
  logical *is_wind_earth_rel;
/*
 * Argument # 22
 */
  void *lmask;
  float *tmp_lmask;
  ng_size_t dsizes_lmask[2];
  NclBasicDataTypes type_lmask;
/*
 * Various
 */
  int nlat, nlon, nlatnlon;

/*
 * Get argument # 0
 */
  wps_im_root_name = (NrmQuark*)NclGetArgValue(
           0,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string */
  c_wps_im_root_name = NrmQuarkToString(*wps_im_root_name);

/*
 * Get argument # 1
 */
  field_lmask = (NrmQuark*)NclGetArgValue(
           1,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string. */
  strncpy(c_field_lmask,NrmQuarkToString(*field_lmask),FIELD_LEN);
  c_field_lmask[FIELD_LEN] = '\0';

/*
 * Get argument # 2
 */
  units_lmask = (NrmQuark*)NclGetArgValue(
           2,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string */
  strncpy(c_units_lmask,NrmQuarkToString(*units_lmask),UNITS_LEN);
  c_units_lmask[UNITS_LEN] = '\0';

/*
 * Get argument # 3
 */
  descr_lmask = (NrmQuark*)NclGetArgValue(
           3,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  /* Convert to character string. */
  strncpy(c_descr_lmask,NrmQuarkToString(*descr_lmask),DESCR_LEN);
  c_descr_lmask[DESCR_LEN] = '\0';

/*
 * Get argument # 4
 */
  hdate = (NrmQuark*)NclGetArgValue(
           4,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  /* Convert to character string. */
  strncpy(c_hdate,NrmQuarkToString(*hdate),HDATE_LEN);
  c_hdate[HDATE_LEN] = '\0';
/*
 * Get argument # 5
 */
  map_source = (NrmQuark*)NclGetArgValue(
           5,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string */
  strncpy(c_map_source,NrmQuarkToString(*map_source),MAPSC_LEN);
  c_map_source[MAPSC_LEN] = '\0';

/*
 * Get argument # 6
 */
  xlvl_lmask = (void*)NclGetArgValue(
           6,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_xlvl_lmask,
           DONT_CARE);
/*
 * Get argument # 7
 */
  iproj = (int*)NclGetArgValue(
           7,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 8
 */
  startloc = (NrmQuark*)NclGetArgValue(
           8,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  /* Convert to character string. */
  strncpy(c_startloc,NrmQuarkToString(*startloc),STARTLOC_LEN);
  c_startloc[STARTLOC_LEN] = '\0';

/*
 * Get argument # 9
 */
  startlat = (void*)NclGetArgValue(
           9,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_startlat,
           DONT_CARE);
/*
 * Get argument # 10
 */
  startlon = (void*)NclGetArgValue(
           10,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_startlon,
           DONT_CARE);
/*
 * Get argument # 11
 */
  delta_lat = (void*)NclGetArgValue(
           11,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_delta_lat,
           DONT_CARE);
/*
 * Get argument # 12
 */
  delta_lon = (void*)NclGetArgValue(
           12,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_delta_lon,
           DONT_CARE);
/*
 * Get argument # 13
 */
  xlonc = (void*)NclGetArgValue(
           13,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_xlonc,
           DONT_CARE);
/*
 * Get argument # 14
 */
  truelat1 = (void*)NclGetArgValue(
           14,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_truelat1,
           DONT_CARE);
/*
 * Get argument # 15
 */
  truelat2 = (void*)NclGetArgValue(
           15,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_truelat2,
           DONT_CARE);
/*
 * Get argument # 16
 */
  nlats = (void*)NclGetArgValue(
           16,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_nlats,
           DONT_CARE);
/*
 * Get argument # 17
 */
  dx = (void*)NclGetArgValue(
           17,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_dx,
           DONT_CARE);
/*
 * Get argument # 18
 */
  dy = (void*)NclGetArgValue(
           18,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_dy,
           DONT_CARE);
/*
 * Get argument # 19
 */
  nx = (int*)NclGetArgValue(
           19,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 20
 */
  ny = (int*)NclGetArgValue(
           20,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 21
 */
  is_wind_earth_rel = (logical*)NclGetArgValue(
           21,
           23,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 22
 */
  lmask = (void*)NclGetArgValue(
           22,
           23,
           NULL,
           dsizes_lmask,
           NULL,
           NULL,
           &type_lmask,
           DONT_CARE);

  nlat     = dsizes_lmask[0];
  nlon     = dsizes_lmask[1];
  nlatnlon = nlat * nlon;

/* 
 * Coerce numeric values to float.
 */
  tmp_xlvl_lmask = coerce_input_float(xlvl_lmask,type_xlvl_lmask,1,0,NULL,NULL);
  if(tmp_xlvl_lmask == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'xlvl_lmask' to float");
    return(NhlFATAL);
  }
  tmp_startlat = coerce_input_float(startlat,type_startlat,1,0,NULL,NULL);
  if(tmp_startlat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'startlat' to float");
    return(NhlFATAL);
  }
  tmp_startlon = coerce_input_float(startlon,type_startlon,1,0,NULL,NULL);
  if(tmp_startlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'startlon' to float");
    return(NhlFATAL);
  }
  tmp_delta_lat = coerce_input_float(delta_lat,type_delta_lat,1,0,NULL,NULL);
  if(tmp_delta_lat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'delta_lat' to float");
    return(NhlFATAL);
  }
  tmp_delta_lon = coerce_input_float(delta_lon,type_delta_lon,1,0,NULL,NULL);
  if(tmp_delta_lon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'delta_lon' to float");
    return(NhlFATAL);
  }
  tmp_xlonc = coerce_input_float(xlonc,type_xlonc,1,0,NULL,NULL);
  if(tmp_xlonc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'xlonc' to float");
    return(NhlFATAL);
  }
  tmp_truelat1 = coerce_input_float(truelat1,type_truelat1,1,0,NULL,NULL);
  if(tmp_truelat1 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'truelat1' to float");
    return(NhlFATAL);
  }
  tmp_truelat2 = coerce_input_float(truelat2,type_truelat2,1,0,NULL,NULL);
  if(tmp_truelat2 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'truelat2' to float");
    return(NhlFATAL);
  }
  tmp_nlats = coerce_input_float(nlats,type_nlats,1,0,NULL,NULL);
  if(tmp_nlats == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'nlats' to float");
    return(NhlFATAL);
  }
  tmp_dx = coerce_input_float(dx,type_dx,1,0,NULL,NULL);
  if(tmp_dx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'dx' to float");
    return(NhlFATAL);
  }
  tmp_dy = coerce_input_float(dy,type_dy,1,0,NULL,NULL);
  if(tmp_dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'dy' to float");
    return(NhlFATAL);
  }
  tmp_lmask = coerce_input_float(lmask,type_lmask,nlatnlon,0,NULL,NULL);
  if(tmp_lmask == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'lmask' to float");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routine.
 */
  NGCALLF(write_intermediate_wps,WRITE_INTERMEDIATE_WPS)(c_wps_im_root_name, 
							 c_field_lmask, 
							 c_units_lmask, 
							 c_descr_lmask, 
							 c_hdate, 
							 c_map_source, 
							 tmp_xlvl_lmask, 
							 iproj, 
							 c_startloc, 
							 tmp_startlat, 
							 tmp_startlon, 
							 tmp_delta_lat, 
							 tmp_delta_lon, 
							 tmp_xlonc, 
							 tmp_truelat1, 
							 tmp_truelat2, 
							 tmp_nlats, 
							 tmp_dx, tmp_dy, 
							 nx, ny, 
							 is_wind_earth_rel, 
							 tmp_lmask,
							 strlen(c_wps_im_root_name),
							 FIELD_LEN,
							 UNITS_LEN,
							 DESCR_LEN,
							 HDATE_LEN,
							 MAPSC_LEN,
							 STARTLOC_LEN);

/*
 * Free unneeded memory.
 */
  if(type_xlvl_lmask!= NCL_float) NclFree(tmp_xlvl_lmask);
  if(type_startlat  != NCL_float) NclFree(tmp_startlat);
  if(type_startlon  != NCL_float) NclFree(tmp_startlon);
  if(type_delta_lat != NCL_float) NclFree(tmp_delta_lat);
  if(type_delta_lon != NCL_float) NclFree(tmp_delta_lon);
  if(type_xlonc     != NCL_float) NclFree(tmp_xlonc);
  if(type_truelat1  != NCL_float) NclFree(tmp_truelat1);
  if(type_truelat2  != NCL_float) NclFree(tmp_truelat2);
  if(type_nlats     != NCL_float) NclFree(tmp_nlats);
  if(type_dx        != NCL_float) NclFree(tmp_dx);
  if(type_dy        != NCL_float) NclFree(tmp_dy);
  if(type_lmask     != NCL_float) NclFree(tmp_lmask);

/*
 * This is a procedure, so no values are returned.
 */
  return(NhlNOERROR);
}
