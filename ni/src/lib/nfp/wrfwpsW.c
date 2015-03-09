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
  NrmQuark *field;
  int FIELD_LEN=9;
  char c_field[FIELD_LEN+1];
/*
 * Argument # 2
 */
  NrmQuark *units;
  int UNITS_LEN=25;
  char c_units[UNITS_LEN+1];
/*
 * Argument # 3
 */
  NrmQuark *descr;
  int DESCR_LEN=46;
  char c_descr[DESCR_LEN+1];
/*
 * Argument # 4
 */
  NrmQuark *hdate;
  int HDATE_LEN=24;
  char c_hdate[HDATE_LEN+1];
/*
 * Argument # 5
 */
  NrmQuark *mapsc;
  int MAPSC_LEN=32;
  char c_mapsc[MAPSC_LEN+1];
/*
 * Argument # 6
 */
  void *xlvl;
  float *tmp_xlvl;
  NclBasicDataTypes type_xlvl;

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
 * Argument # 20
 */
  logical *is_wind_earth_rel;
/*
 * Argument # 21
 */
  void *lmask;
  float *tmp_lmask;
  ng_size_t dsizes_lmask[2];
  NclBasicDataTypes type_lmask;
/*
 * Various
 */
  int i,slen,nlat,nlon,nlatnlon;

/*
 * Get argument # 0
 */
  wps_im_root_name = (NrmQuark*)NclGetArgValue(
           0,
           21,
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
  field = (NrmQuark*)NclGetArgValue(
           1,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string. */
  slen = strlen(NrmQuarkToString(*field));
  if(slen > FIELD_LEN) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: field string is greater than %d characters",FIELD_LEN);
    return(NhlFATAL);
  }
  strncpy(c_field,NrmQuarkToString(*field),FIELD_LEN);
  for(i = slen; i < FIELD_LEN; i++) strncpy(&c_field[i]," ",1);
  c_field[FIELD_LEN] = '\0';

/*
 * Get argument # 2
 */
  units = (NrmQuark*)NclGetArgValue(
           2,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string */
  slen = strlen(NrmQuarkToString(*units));
  if(slen > UNITS_LEN) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: units string is greater than %d characters",UNITS_LEN);
    return(NhlFATAL);
  }
  strncpy(c_units,NrmQuarkToString(*units),UNITS_LEN);
  for(i = slen; i < UNITS_LEN; i++) strncpy(&c_units[i]," ",1);
  c_units[UNITS_LEN] = '\0';

/*
 * Get argument # 3
 */
  descr = (NrmQuark*)NclGetArgValue(
           3,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  /* Convert to character string. */
  slen = strlen(NrmQuarkToString(*descr));
  if(slen > DESCR_LEN) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: description string is greater than %d characters",DESCR_LEN);
    return(NhlFATAL);
  }
  strncpy(c_descr,NrmQuarkToString(*descr),DESCR_LEN);
  for(i = slen; i < DESCR_LEN; i++) strncpy(&c_descr[i]," ",1);
  c_descr[DESCR_LEN] = '\0';

/*
 * Get argument # 4
 */
  hdate = (NrmQuark*)NclGetArgValue(
           4,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  /* Convert to character string. */
  slen = strlen(NrmQuarkToString(*hdate));
  if(slen > HDATE_LEN) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: hdate string is greater than %d characters",HDATE_LEN);
    return(NhlFATAL);
  }
  strncpy(c_hdate,NrmQuarkToString(*hdate),HDATE_LEN);
  for(i = slen; i < HDATE_LEN; i++) strncpy(&c_hdate[i]," ",1);
  c_hdate[HDATE_LEN] = '\0';

/*
 * Get argument # 5
 */
  mapsc = (NrmQuark*)NclGetArgValue(
           5,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  /* Convert to character string */
  slen = strlen(NrmQuarkToString(*mapsc));
  if(slen > MAPSC_LEN) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: map_source string is greater than %d characters",MAPSC_LEN);
    return(NhlFATAL);
  }
  strncpy(c_mapsc,NrmQuarkToString(*mapsc),MAPSC_LEN);
  for(i = slen; i < MAPSC_LEN; i++) strncpy(&c_mapsc[i]," ",1);
  c_mapsc[MAPSC_LEN] = '\0';

/*
 * Get argument # 6
 */
  xlvl = (void*)NclGetArgValue(
           6,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_xlvl,
           DONT_CARE);
/*
 * Get argument # 7
 */
  iproj = (int*)NclGetArgValue(
           7,
           21,
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
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  /* Convert to character string. */
  slen = strlen(NrmQuarkToString(*startloc));
  if(slen > STARTLOC_LEN) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: startloc string is greater than %d characters",STARTLOC_LEN);
    return(NhlFATAL);
  }
  strncpy(c_startloc,NrmQuarkToString(*startloc),STARTLOC_LEN);
  for(i = slen; i < STARTLOC_LEN; i++) strncpy(&startloc[i]," ",1);
  c_startloc[STARTLOC_LEN] = '\0';

/*
 * Get argument # 9
 */
  startlat = (void*)NclGetArgValue(
           9,
           21,
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
           21,
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
           21,
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
           21,
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
           21,
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
           21,
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
           21,
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
           21,
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
           21,
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
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_dy,
           DONT_CARE);
/*
 * Get argument # 19
 */
  is_wind_earth_rel = (logical*)NclGetArgValue(
           19,
           21,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 20
 */
  lmask = (void*)NclGetArgValue(
           20,
           21,
           NULL,
           dsizes_lmask,
           NULL,
           NULL,
           &type_lmask,
           DONT_CARE);

  nlat     = dsizes_lmask[0];    /* ny */
  nlon     = dsizes_lmask[1];    /* nx */
  nlatnlon = nlat * nlon;

/* 
 * Coerce numeric values to float.
 */
  tmp_xlvl = coerce_input_float(xlvl,type_xlvl,1,0,NULL,NULL);
  if(tmp_xlvl == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce 'xlvl' to float");
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
                                                         c_field, 
                                                         c_units, 
                                                         c_descr, 
                                                         c_hdate, 
                                                         c_mapsc, 
                                                         tmp_xlvl, 
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
                                                         &nlon, &nlat, 
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
  if(type_xlvl      != NCL_float) NclFree(tmp_xlvl);
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
