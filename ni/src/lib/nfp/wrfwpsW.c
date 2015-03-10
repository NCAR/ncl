#include <stdio.h>
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
  NrmQuark *date, *map_source, *startloc;
  int DATE_LEN=24, MAP_SOURCE_LEN=32, STARTLOC_LEN=8;
  char c_date[DATE_LEN+1], c_map_source[MAP_SOURCE_LEN+1], c_startloc[STARTLOC_LEN+1];

/* Float attributes */
  void *xfcst, *level, *startlat, *startlon, *deltalat, *deltalon; 
  void *xlonc, *truelat1, *truelat2, *nlats, *dx, *dy, *earth_radius; 
  float *tmp_level, *tmp_startlat, *tmp_startlon, *tmp_deltalat; 
  float *tmp_deltalon, *tmp_xlonc, *tmp_truelat1, *tmp_truelat2;
  float *tmp_xfcst, *tmp_nlats, *tmp_dx, *tmp_dy, *tmp_earth_radius;
  NclBasicDataTypes type_xfcst, type_level, type_startlat, type_startlon; 
  NclBasicDataTypes type_deltalat, type_deltalon, type_xlonc; 
  NclBasicDataTypes type_truelat1, type_truelat2, type_earth_radius; 
  NclBasicDataTypes type_nlats, type_dx, type_dy;

  /* Other attributes */
  int version, proj;
  logical is_wind_earth_relative;

  /* Logicals to keep track if an attribute has been set */
  int set_version  = False, set_xfcst    = False, set_date    = False, set_map_source  = False; 
  int set_startloc = False, set_level    = False, set_startlat= False, set_startlon    = False;
  int set_deltalat = False, set_deltalon = False, set_xlonc   = False, set_earth_radius= False;
  int set_truelat1 = False, set_truelat2 = False, set_dx      = False, set_dy          = False;
  int set_proj     = False, set_nlats    = False, set_is_wind_earth_relative = False; 

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

  set_fixed_string("field",True,True,field,&c_field[0],"",FIELD_LEN);

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

  set_fixed_string("units",True,True,units,&c_units[0],"",UNITS_LEN);

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
  set_fixed_string("description",True,True,description,&c_description[0],"",DESCRIPTION_LEN);

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
 *    "version"        - integer, default to 5
 *    "date"           - string,  required, no default, must be of length 24
 *    "xfcst"          - float,   default to 0.0
 *    "map_source"     - string,  default to "Unknown data source"
 *    "level"          - float,   required
 *    "proj"           - integer, required, must be 0/1/3/4/5 (see extra note below)
 *    "startloc"       - string,  default to "SWCORNER"
 *    "startlat"       - float,   required, 
 *    "startlon"       - float,   required
 *    "deltalat"       - float,   required if proj=0, else default to missing/dummy value
 *    "deltalon"       - float,   required if proj=0/4, else default to missing/dummy value
 *                       (yes, this is different than deltalat)
 *    "earth_radius"   - float,   default to set to 6367470. * 0.001
 *    "dx"             - float,   required if proj=1/3/5, else default to missing/dummy
 *    "dy"             - same as "dx"
 *    "truelat1"       - float,   required if proj=1/3/5, else default to missing/dummy
 *    "truelat2"       - float,   required if proj=3, else default to missing/dummy
 *    "xlonc"          - float,   required if proj=3/5, else default to missing/dummy
 *    "nlats"          - float,   required if proj=4, else default to missing/dummy
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
	  else if(!strcasecmp(attr_list->attname, "xfcst")) {
	    xfcst      = attr_list->attvalue->multidval.val;
	    type_xfcst = attr_list->attvalue->multidval.data_type;
	    set_xfcst  = True;
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
	  else if(!strcasecmp(attr_list->attname, "proj")) {
	    proj     = *(int *) attr_list->attvalue->multidval.val;
	    set_proj = True;
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
	  else if(!strcasecmp(attr_list->attname, "xlonc")) {
	    xlonc      = attr_list->attvalue->multidval.val;
	    type_xlonc = attr_list->attvalue->multidval.data_type;
	    set_xlonc  = True;
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
 *  xfcst
 *  map_source
 *  startloc
 *  earth_radius
 *
 * These attributes are required:
 *
 *  date
 *  level
 *  proj
 *  startlat
 *  startlon
 *  deltalat
 *  deltalon
 *  dx
 *  dy
 *  truelat1
 *  truelat2
 *  xlonc
 *  nlats
 *  is_wind_earth_relative
 *
 * String attributes with fixed lengths need to be padded with spaces.
 */

  if(!set_version) version = 5;
  set_fixed_string("map_source",set_map_source, False, map_source, &c_map_source[0],
		   "Unknown data source",MAP_SOURCE_LEN);
  set_fixed_string("startloc",set_startloc, False, startloc, &c_startloc[0],
		   "SWCORNER",STARTLOC_LEN);

  if(!set_proj || (set_proj && (proj != 0 && proj != 1 && proj != 3 && proj != 4 && proj != 5))) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the proj attribute must be set to 0, 1, 3, 4, or 5");
    return(NhlFATAL);
  }
  set_fixed_string("date", set_date, True, date, c_date, "", DATE_LEN);
  if(c_date[0] == ' ') {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the date attribute must be set to a date string");
    return(NhlFATAL);
  }
  if(!set_is_wind_earth_relative) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the is_wind_earth_relative attribute must be set to True or False");
    return(NhlFATAL);
  }

  /* Coerce numeric attributes to float */
  tmp_xfcst     = coerce_float("xfcst",    set_xfcst,    False,xfcst,    type_xfcst,    0.);
  tmp_level     = coerce_float("level",    set_level,    True, level,    type_level,    0.);
  tmp_startlat  = coerce_float("startlat", set_startlat, True, startlat, type_startlat, 0.);
  tmp_startlon  = coerce_float("startlon", set_startlon, True, startlon, type_startlon, 0.);
  tmp_truelat2  = coerce_float("truelat2", set_truelat2, True, truelat2, type_truelat2, 0.);

  /* These are attributes dependent on "proj" */
  if(!set_deltalat && proj == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the deltalat attribute must be set if proj is 0");
    return(NhlFATAL);
  }
  else {
    tmp_deltalat = coerce_float("deltalat", set_deltalat, False, deltalat ,type_deltalat, msg);
  }

  if(!set_deltalon && (proj == 0 || proj == 4)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the deltalon attribute must be set if proj is 0 or 4");
    return(NhlFATAL);
  }
  else {
    tmp_deltalon = coerce_float("deltalon", set_deltalon, False, deltalon, type_deltalon, msg);
  }

  tmp_earth_radius = coerce_float("earth_radius",set_earth_radius,False,earth_radius,
				  type_earth_radius,6367.470);
  if((!set_dx || !set_dy) && (proj == 1 || proj == 3 || proj == 5)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the dx,dy attributes must be set if proj is 1, 3 or 5");
    return(NhlFATAL);
  }
  else {
    tmp_dx = coerce_float("dx", set_dx, False, dx, type_dx, msg);
    tmp_dy = coerce_float("dy", set_dy, False, dy, type_dy, msg);
  }

  if(!set_truelat1 && (proj == 1 || proj == 3 || proj == 5)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the truelat1 attribute must be set if proj is 1, 3 or 5");
    return(NhlFATAL);
  }
  else {
    tmp_truelat1 = coerce_float("truelat1", set_truelat1, False, truelat1, type_truelat1, msg);
  }

  if(!set_truelat2 && proj == 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the truelat2 attribute must be set if proj is 3");
    return(NhlFATAL);
  }
  else {
    tmp_truelat2 = coerce_float("truelat2", set_truelat2, False, truelat2, type_truelat2, msg);
  }

  if(!set_xlonc && (proj == 3 || proj == 5)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the xlonc attribute must be set if proj is 3 or 5");
    return(NhlFATAL);
  }
  else {
    tmp_xlonc = coerce_float("xlonc", set_xlonc, False, xlonc, type_xlonc, msg);
  }

  if(!set_nlats && proj == 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_wps_write_int: the nlats attribute must be set if proj is 4");
    return(NhlFATAL);
  }
  else {
    tmp_nlats = coerce_float("nlats", set_nlats, False, nlats, type_nlats, msg);
  }

  if(tmp_level    == NULL || tmp_startlat == NULL || tmp_startlon == NULL || tmp_xfcst == NULL ||
     tmp_deltalat == NULL || tmp_deltalon == NULL || tmp_dx       == NULL || tmp_dy    == NULL ||
     tmp_truelat1 == NULL || tmp_truelat2 == NULL ||tmp_xlonc     == NULL || tmp_nlats == NULL ||
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
                                                         &proj, 
                                                         c_startloc, 
                                                         tmp_startlat, 
                                                         tmp_startlon, 
                                                         tmp_deltalat, 
                                                         tmp_deltalon, 
                                                         tmp_xlonc, 
                                                         tmp_truelat1, 
                                                         tmp_truelat2, 
                                                         tmp_nlats, 
                                                         tmp_dx, tmp_dy, 
                                                         &nlon, &nlat, 
                                                         &is_wind_earth_relative, 
							 &version,
							 tmp_xfcst,
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

  if(type_xfcst     != NCL_float) NclFree(tmp_xfcst);
  if(type_level     != NCL_float) NclFree(tmp_level);
  if(type_startlat  != NCL_float) NclFree(tmp_startlat);
  if(type_startlon  != NCL_float) NclFree(tmp_startlon);
  if(type_deltalat  != NCL_float) NclFree(tmp_deltalat);
  if(type_deltalon  != NCL_float) NclFree(tmp_deltalon);
  if(type_xlonc     != NCL_float) NclFree(tmp_xlonc);
  if(type_truelat1  != NCL_float) NclFree(tmp_truelat1);
  if(type_truelat2  != NCL_float) NclFree(tmp_truelat2);
  if(type_nlats     != NCL_float) NclFree(tmp_nlats);
  if(type_dx        != NCL_float) NclFree(tmp_dx);
  if(type_dy        != NCL_float) NclFree(tmp_dy);
  if(type_nlats     != NCL_float) NclFree(tmp_nlats);
  if(type_lmask     != NCL_float) NclFree(tmp_lmask);
  if(type_earth_radius != NCL_float) NclFree(tmp_earth_radius);

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

  char_value[0] = " ";   /* For testing against later */

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
