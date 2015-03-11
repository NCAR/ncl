#include <stdio.h>
#include <strings.h>
#include "wrapper.h"

extern void NGCALLF(write_intermediate_wps,WRITE_INTERMEDIATE_WPS)
     (char *, char *, char *, char *, char *, char *, 
      float *, int *, char *, float *, float *, float *, 
      float *, float *, float *, float *, float *, float *, 
      float *, int *, int *, logical *, int *, float *, float *, float *,
      int,int,int,int,int,int,int);

extern float *coerce_float(const char*name, logical is_value_set, logical is_required, 
                           void *value,NclBasicDataTypes type_value, float default_value);
extern NhlErrorTypes set_fixed_string(const char *name,logical is_set, logical is_required,
                                      NrmQuark *quark_value,char *char_value,char *default_value,
                                      int STR_LEN);
extern void pad_with_spaces(char *char_value,int slen,int STR_LEN);

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
  NrmQuark *description;
  int DESCRIPTION_LEN=46;
  char c_description[DESCRIPTION_LEN+1];

/*
 * Argument # 4
 */
  void *lmask;
  float *tmp_lmask;
  ng_size_t dsizes_lmask[2];
  NclBasicDataTypes type_lmask;

/*
 * Argument # 5
 */
  logical *opt;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;

/*
 * Possible attributes, listed by type.
 */

/* String attributes */
  NrmQuark *date=NULL, *map_source=NULL, *startloc=NULL;
  int DATE_LEN=24, MAP_SOURCE_LEN=32, STARTLOC_LEN=8;
  char c_date[DATE_LEN+1], c_map_source[MAP_SOURCE_LEN+1], c_startloc[STARTLOC_LEN+1];

/* Float attributes */
  void *forecast_hour=NULL, *level=NULL, *startlat=NULL, *startlon=NULL;
  void *deltalat=NULL, *deltalon=NULL, *center_lon=NULL;
  void *truelat1=NULL, *truelat2=NULL, *nlats=NULL, *dx=NULL, *dy=NULL;
  void *earth_radius=NULL; 

  float *tmp_forecast_hour, *tmp_level, *tmp_startlat, *tmp_startlon;
  float *tmp_deltalat, *tmp_deltalon, *tmp_center_lon;
  float *tmp_truelat1, *tmp_truelat2, *tmp_nlats, *tmp_dx, *tmp_dy;
  float *tmp_earth_radius;

  NclBasicDataTypes type_forecast_hour=NCL_float, type_level=NCL_float;
  NclBasicDataTypes type_startlat=NCL_float,type_startlon=NCL_float;
  NclBasicDataTypes type_deltalat=NCL_float, type_deltalon=NCL_float;
  NclBasicDataTypes type_center_lon=NCL_float, type_truelat1=NCL_float;
  NclBasicDataTypes type_truelat2=NCL_float, type_nlats=NCL_float;
  NclBasicDataTypes type_dx=NCL_float, type_dy=NCL_float;
  NclBasicDataTypes type_earth_radius=NCL_float; 

  /* Other attributes */
  int version, projection;
  char *c_projection;
  NclBasicDataTypes type_projection;
  logical is_wind_earth_relative;

  /* Logicals to keep track if an attribute has been set */
  int set_projection=False, set_is_wind_earth_relative=False;
  int set_date=False, set_version=False, set_map_source=False;
  int set_forecast_hour=False, set_level=False;
  int set_startlat=False, set_startlon=False, set_startloc=False; 
  int set_deltalat=False, set_deltalon=False, set_center_lon=False;
  int set_truelat1=False, set_truelat2=False, set_nlats=False;
  int set_dx=False, set_dy=False, set_earth_radius=False;
/*
 * Various
 */
  int nlat, nlon, nlatnlon; 
  float msg = -9999.9;

/*
 * Get argument # 0
 */
  wps_im_root_name = (NrmQuark*)NclGetArgValue(
           0,
           6,
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
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  if(set_fixed_string("field",True,True,field,&c_field[0],"",FIELD_LEN) == NhlFATAL) {
    return(NhlFATAL);
  }


/*
 * Get argument # 2
 */
  units = (NrmQuark*)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  if(set_fixed_string("units",True,True,units,&c_units[0],"",UNITS_LEN) == NhlFATAL) {
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  description = (NrmQuark*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  if(set_fixed_string("description",True,True,description,&c_description[0],"",DESCRIPTION_LEN) == NhlFATAL) {
    return(NhlFATAL);
  }

/*
 * Get argument # 4
 */
  lmask = (void*)NclGetArgValue(
           4,
           6,
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
 * Get argument # 5
 */
  opt = (logical*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Check for attributes attached to "opt"
 *
 *    "version"           - integer, default to 5
 *    "date"              - string,  required, must be of length 24
 *    "forecast_hour"     - float,   default to 0.0
 *    "map_source"        - string,  default to "Unknown data source"
 *    "level"             - float,   required
 *    "projection"        - integer or string, required, must be 0/1/3/4/5 (see extra note below)
 *    "startloc"          - string,  default to "SWCORNER"
 *    "startlat"          - float,   required 
 *    "startlon"          - float,   required
 *    "deltalat"          - float,   required if projection=0, else default to missing/dummy value
 *    "deltalon"          - float,   required if projection=0/4, else default to missing/dummy value
 *                          (yes, this is different than deltalat)
 *    "earth_radius"      - float,   default to set to 6367470. * 0.001
 *    "dx"                - float,   required if projection=1/3/5, else default to missing/dummy
 *    "dy"                - same as "dx"
 *    "truelat1"          - float,   required if projection=1/3/5, else default to missing/dummy
 *    "truelat2"          - float,   required if projection=3, else default to missing/dummy
 *    "center_lon"        - float,   required if projection=3/5, else default to missing/dummy
 *    "nlats"             - float,   required if projection=4, else default to missing/dummy
 *    "is_wind_earth_relative" - logical, required
 */
  if(*opt) {
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
 * att_id == -1 ==> no optional args given.
 */
        break;
      }
/* 
 * Get optional arguments.
 */
      if (attr_obj->att.n_atts > 0) {
/*
 * Get list of attributes.
 */
        attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them.
 */
        while (attr_list != NULL) {
          if(!strcasecmp(attr_list->attname, "version")) {
            version     = *(int *)attr_list->attvalue->multidval.val;
            set_version = True;
          }
          else if(!strcasecmp(attr_list->attname, "date")) {
            date     = (NrmQuark *) attr_list->attvalue->multidval.val;
            set_date = True;
          }
          else if(!strcasecmp(attr_list->attname, "forecast_hour")) {
            forecast_hour      = attr_list->attvalue->multidval.val;
            type_forecast_hour = attr_list->attvalue->multidval.data_type;
            set_forecast_hour  = True;
          }
          else if(!strcasecmp(attr_list->attname, "map_source")) {
            map_source     = (NrmQuark *) attr_list->attvalue->multidval.val;
            set_map_source = True;
          }
          else if(!strcasecmp(attr_list->attname, "level")) {
            level      = attr_list->attvalue->multidval.val;
            type_level = attr_list->attvalue->multidval.data_type;
            set_level  = True;
          }
          else if(!strcasecmp(attr_list->attname, "projection")) {
            type_projection = attr_list->attvalue->multidval.data_type;
            if(type_projection == NCL_string) {
              c_projection = NrmQuarkToString(*(NrmQuark *) attr_list->attvalue->multidval.val);
              if(!strcasecmp(c_projection,"Equidistant_Lat_Lon")) {
                projection = 0;
              }
              else if(!strcasecmp(c_projection,"Mercator")) {
                projection = 1;
              }
              else if(!strcasecmp(c_projection,"Lambert")) {
                projection = 3;
              }
              else if(!strcasecmp(c_projection,"Gaussian")) {
                projection = 4;
              }
              else if(!strcasecmp(c_projection,"Polar_Stereographic")) {
                projection = 5;
              }
            }
            else if(type_projection == NCL_int) {
              projection = *(int *)attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the projection attribute must be an integer or a string");
              return(NhlFATAL);
            }
            set_projection  = True;
          }
          else if(!strcasecmp(attr_list->attname, "startloc")) {
            startloc     = (NrmQuark *) attr_list->attvalue->multidval.val;
            set_startloc = True;
          }
          else if(!strcasecmp(attr_list->attname, "startlat")) {
            startlat      = attr_list->attvalue->multidval.val;
            type_startlat = attr_list->attvalue->multidval.data_type;
            set_startlat  = True;
          }
          else if(!strcasecmp(attr_list->attname, "startlon")) {
            startlon      = attr_list->attvalue->multidval.val;
            type_startlon = attr_list->attvalue->multidval.data_type;
            set_startlon  = True;
          }
          else if(!strcasecmp(attr_list->attname, "deltalat")) {
            deltalat      = attr_list->attvalue->multidval.val;
            type_deltalat = attr_list->attvalue->multidval.data_type;
            set_deltalat  = True;
          }
          else if(!strcasecmp(attr_list->attname, "deltalon")) {
            deltalon      = attr_list->attvalue->multidval.val;
            type_deltalon = attr_list->attvalue->multidval.data_type;
            set_deltalon  = True;
          }
          else if(!strcasecmp(attr_list->attname, "earth_radius")) {
            earth_radius      = attr_list->attvalue->multidval.val;
            type_earth_radius = attr_list->attvalue->multidval.data_type;
            set_earth_radius  = True;
          }
          else if(!strcasecmp(attr_list->attname, "dx")) {
            dx      = attr_list->attvalue->multidval.val;
            type_dx = attr_list->attvalue->multidval.data_type;
            set_dx  = True;
          }
          else if(!strcasecmp(attr_list->attname, "dy")) {
            dy      = attr_list->attvalue->multidval.val;
            type_dy = attr_list->attvalue->multidval.data_type;
            set_dy  = True;
          }
          else if(!strcasecmp(attr_list->attname, "truelat1")) {
            truelat1      = attr_list->attvalue->multidval.val;
            type_truelat1 = attr_list->attvalue->multidval.data_type;
            set_truelat1  = True;
          }
          else if(!strcasecmp(attr_list->attname, "truelat2")) {
            truelat2      = attr_list->attvalue->multidval.val;
            type_truelat2 = attr_list->attvalue->multidval.data_type;
            set_truelat2  = True;
          }
          else if(!strcasecmp(attr_list->attname, "center_lon")) {
            center_lon      = attr_list->attvalue->multidval.val;
            type_center_lon = attr_list->attvalue->multidval.data_type;
            set_center_lon  = True;
          }
          else if(!strcasecmp(attr_list->attname, "nlats")) {
            nlats      = attr_list->attvalue->multidval.val;
            type_nlats = attr_list->attvalue->multidval.data_type;
            set_nlats  = True;
          }
          else if(!strcasecmp(attr_list->attname, "is_wind_earth_relative")) {
           is_wind_earth_relative     = *(logical *) attr_list->attvalue->multidval.val;
           set_is_wind_earth_relative = True;
          }
          attr_list = attr_list->next;
        }
      default:
        break;
      }
    }
  }

/*
 * Some attributes are required, some are not.  Other attributes are 
 * required depending on values of other attributes.
 *
 * These attributes are not required:
 *
 *  version
 *  forecast_hour
 *  map_source
 *  startloc
 *  earth_radius
 *
 * These attributes are required, but some depend on the projection setting:
 *
 *  date
 *  level
 *  projection
 *  startlat
 *  startlon
 *  deltalat
 *  deltalon
 *  dx
 *  dy
 *  truelat1
 *  truelat2
 *  center_lon
 *  nlats
 *  is_wind_earth_relative
 *
 * String attributes with fixed lengths need to be padded with spaces.
 */

  if(!set_version) version = 5;
  if(set_fixed_string("map_source",set_map_source, False, map_source, &c_map_source[0],
                      "Unknown data source",MAP_SOURCE_LEN) == NhlFATAL) {
    return(NhlFATAL);
  }
  if(set_fixed_string("startloc",set_startloc, False, startloc, &c_startloc[0],
                      "SWCORNER",STARTLOC_LEN) == NhlFATAL) {
    return(NhlFATAL);
  }
  if(!set_projection || (set_projection && 
                         (projection != 0 && projection != 1 && projection != 3 && 
                          projection != 4 && projection != 5))) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the projection attribute must be set to 0, 1, 3, 4, or 5");
    return(NhlFATAL);
  }
  if(set_fixed_string("date", set_date, True, date, c_date, "", DATE_LEN) == NhlFATAL) {
    return(NhlFATAL);
  }
  if(!set_is_wind_earth_relative) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the is_wind_earth_relative attribute must be set to True or False");
    return(NhlFATAL);
  }

  /* Coerce numeric attributes to float */
  tmp_forecast_hour = coerce_float("forecast_hour", set_forecast_hour, False, forecast_hour, 
                                   type_forecast_hour, 0.);
  tmp_level         = coerce_float("level", set_level, True, level, type_level, 0.);
  tmp_startlat      = coerce_float("startlat", set_startlat, True, startlat, type_startlat, 0.);
  tmp_startlon      = coerce_float("startlon", set_startlon, True, startlon, type_startlon, 0.);

  /* These are attributes dependent on "projection" */
  if(!set_deltalat && projection == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the deltalat attribute must be set if projection is 0");
    return(NhlFATAL);
  }
  else {
    tmp_deltalat = coerce_float("deltalat", set_deltalat, False, deltalat ,type_deltalat, msg);
  }

  if(!set_deltalon && (projection == 0 || projection == 4)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the deltalon attribute must be set if projection is 0 or 4");
    return(NhlFATAL);
  }
  else {
    tmp_deltalon = coerce_float("deltalon", set_deltalon, False, deltalon, type_deltalon, msg);
  }

  tmp_earth_radius = coerce_float("earth_radius",set_earth_radius,False,earth_radius,
                                  type_earth_radius,6367.470);
  if((!set_dx || !set_dy) && (projection == 1 || projection == 3 || projection == 5)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the dx,dy attributes must be set if projection is 1, 3 or 5");
    return(NhlFATAL);
  }
  else {
    tmp_dx = coerce_float("dx", set_dx, False, dx, type_dx, msg);
    tmp_dy = coerce_float("dy", set_dy, False, dy, type_dy, msg);
  }

  if(!set_truelat1 && (projection == 1 || projection == 3 || projection == 5)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the truelat1 attribute must be set if projection is 1, 3 or 5");
    return(NhlFATAL);
  }
  else {
    tmp_truelat1 = coerce_float("truelat1", set_truelat1, False, truelat1, type_truelat1, msg);
  }

  if(!set_truelat2 && projection == 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the truelat2 attribute must be set if projection is 3");
    return(NhlFATAL);
  }
  else {
    tmp_truelat2 = coerce_float("truelat2", set_truelat2, False, truelat2, type_truelat2, msg);
  }

  if(!set_center_lon && (projection == 3 || projection == 5)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the center_lon attribute must be set if projection is 3 or 5");
    return(NhlFATAL);
  }
  else {
    tmp_center_lon = coerce_float("center_lon", set_center_lon, False, center_lon, type_center_lon, msg);
  }

  if(!set_nlats && projection == 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the nlats attribute must be set if projection is 4");
    return(NhlFATAL);
  }
  else {
    tmp_nlats = coerce_float("nlats", set_nlats, False, nlats, type_nlats, msg);
  }

  if(tmp_level    == NULL || tmp_startlat == NULL || tmp_startlon      == NULL || 
     tmp_deltalat == NULL || tmp_deltalon == NULL || tmp_center_lon    == NULL || 
     tmp_nlats    == NULL || tmp_dx       == NULL || tmp_dy            == NULL ||
     tmp_truelat1 == NULL || tmp_truelat2 == NULL || tmp_forecast_hour == NULL || 
     tmp_earth_radius == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: Unable to coerce one or more numeric attributes to float");
    return(NhlFATAL);
  }

/* Coerce main data array to float */
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
                                                         c_description, 
                                                         c_date, 
                                                         c_map_source, 
                                                         tmp_level, 
                                                         &projection, 
                                                         c_startloc, 
                                                         tmp_startlat, 
                                                         tmp_startlon, 
                                                         tmp_deltalat, 
                                                         tmp_deltalon, 
                                                         tmp_center_lon, 
                                                         tmp_truelat1, 
                                                         tmp_truelat2, 
                                                         tmp_nlats, 
                                                         tmp_dx, tmp_dy, 
                                                         &nlon, &nlat, 
                                                         &is_wind_earth_relative, 
                                                         &version,
                                                         tmp_forecast_hour,
                                                         tmp_earth_radius,
                                                         tmp_lmask,
                                                         strlen(c_wps_im_root_name),
                                                         FIELD_LEN,
                                                         UNITS_LEN,
                                                         DESCRIPTION_LEN,
                                                         DATE_LEN,
                                                         MAP_SOURCE_LEN,
                                                         STARTLOC_LEN);
/*
 * Free unneeded memory.
 */

  if(type_forecast_hour != NCL_float) NclFree(tmp_forecast_hour);
  if(type_level         != NCL_float) NclFree(tmp_level);
  if(type_startlat      != NCL_float) NclFree(tmp_startlat);
  if(type_startlon      != NCL_float) NclFree(tmp_startlon);
  if(type_deltalat      != NCL_float) NclFree(tmp_deltalat);
  if(type_deltalon      != NCL_float) NclFree(tmp_deltalon);
  if(type_center_lon    != NCL_float) NclFree(tmp_center_lon);
  if(type_truelat1      != NCL_float) NclFree(tmp_truelat1);
  if(type_truelat2      != NCL_float) NclFree(tmp_truelat2);
  if(type_nlats         != NCL_float) NclFree(tmp_nlats);
  if(type_dx            != NCL_float) NclFree(tmp_dx);
  if(type_dy            != NCL_float) NclFree(tmp_dy);
  if(type_lmask         != NCL_float) NclFree(tmp_lmask);
  if(type_earth_radius  != NCL_float) NclFree(tmp_earth_radius);

/*
 * This is a procedure, so no values are returned.
 */
  return(NhlNOERROR);
}

float *coerce_float(const char *name,logical is_set, logical is_required,
                    void *value,NclBasicDataTypes type_value, float default_value)
{
  float *ret;
  
  if(is_required && !is_set) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the %s float attribute must be set",name);
    return(NULL);
  }     
  if(is_set) {
    ret = coerce_input_float(value,type_value,1,0,NULL,NULL);
  }
  else {
    ret  = (float *)calloc(1,sizeof(float));
    *ret = default_value;
  }
  if(ret == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: unable to coerce %s attribute to float",name);
    return(NULL);
  }
  return(ret);
}

NhlErrorTypes set_fixed_string(const char *name,logical is_set, logical is_required, NrmQuark *quark_value,
                               char *char_value,char *default_value, int STR_LEN)
{
  int slen;

  if(is_required && !is_set) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the %s string attribute must be set",name);
    return(NhlFATAL);
  }     

  if(is_set) {
    slen = strlen(NrmQuarkToString(*quark_value));
    if(slen > STR_LEN) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the %s string is greater than %d characters",name,STR_LEN);
      return(NhlFATAL);
    }
    strncpy(&char_value[0],NrmQuarkToString(*quark_value),STR_LEN);
  }
  else {
    slen = strlen(default_value);
    strncpy(&char_value[0],default_value,STR_LEN);
  }
  pad_with_spaces(char_value,slen,STR_LEN);
  return(NhlNOERROR);
}

void pad_with_spaces(char *char_value, int clen, int STR_LEN)
{
  int i;
  for(i = clen; i < STR_LEN; i++) strncpy(&char_value[i]," ",1);
  char_value[STR_LEN] = '\0';
}
