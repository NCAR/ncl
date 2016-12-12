/* 
 * This code provides wrappers to "cdtime", a software package
 * developed by the Program for Climate Model Diagnosis and
 * Intercomparison (PCMDI) at Lawrence Livermore National Laboratory
 * (LLNL).
 */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "wrapper.h"
#include "nctime.h"
#include <math.h>

#define CU_FATAL 1           /* Exit immediately on fatal error */
extern int cuErrorOccurred;
extern int cuErrOpts;

extern int day_of_year(int, int, int, const char*);
extern int seconds_in_year(int, const char *);
extern double fraction_of_year (int year, int month, int day, 
                                int hour, int minute, double second,
				const char *calendar);

extern void set_all_missing(void *dt, ng_size_t total_size, 
                            NclScalar missing, NclBasicDataTypes typd_dt);

NhlErrorTypes cd_calendar_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  NrmQuark *sspec = NULL;
  char *cspec;
  int *option;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;

/*
 * Variables for retrieving attributes from the first argument.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  NrmQuark *scal;
  const char   *ccal = NULL;
  cdCalenType ctype;
/*
 * Output variables.
 */
  cdCompTime comptime;
  int year, month, day, hour, minute;
  double second;
  void *date = NULL;
  int ndims_date = 0;
  ng_size_t *dsizes_date;
  NclScalar missing_date;
  NclBasicDataTypes type_date = NCL_none;
  NclObjClass type_date_t = NCL_none;
/*
 * Variables for returning "calendar" attribute.
 */
  int att_id;
  NclQuark *calendar;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * various
 */
  int ret, return_all_missing;
  ng_size_t dsizes[1];
  ng_size_t i, total_size_x;
  ng_size_t total_size_date = 0;
  ng_size_t index_date;
  extern float truncf(float);

  /* initialize error flag */
  cuErrorOccurred = 0;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get option.
 */

  option = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);

/* 
 * The "units" attribute of "time" must be set, otherwise missing
 * values will be returned.
 *
 * The "calendar" option may optionally be set, but it must be equal to
 * one of the recognized calendars.
 */
  return_all_missing = 0;

  stack_entry = _NclGetArg(0, 2, DONT_CARE);
  switch (stack_entry.kind) {
  case NclStk_VAR:
    if (stack_entry.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
      if (attr_obj == NULL) {
        return_all_missing = 1;
        break;
      }
    }
    else {
/*
 * att_id == -1 ==> no attributes specified; return all missing.
 */
      return_all_missing = 1;
      break;
    }
/* 
 * Check for attributes. If none are specified, then return missing values.
 */
    if (attr_obj->att.n_atts == 0) {
      return_all_missing = 1;
      break;
    }
    else {
/*
 * Get list of attributes.
 */
      attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them.
 */
      while (attr_list != NULL) {
        if ((strcmp(attr_list->attname, "calendar")) == 0) {
          scal = (NrmQuark *) attr_list->attvalue->multidval.val;
          ccal = NrmQuarkToString(*scal);
          if(strcasecmp(ccal,"standard") && strcasecmp(ccal,"gregorian") &&
             strcasecmp(ccal,"proleptic_gregorian") &&
             strcasecmp(ccal,"noleap")  && strcasecmp(ccal,"no_leap") &&
             strcasecmp(ccal,"allleap") && strcasecmp(ccal,"all_leap") &&
             strcasecmp(ccal,"365_day") && strcasecmp(ccal,"365") &&
             strcasecmp(ccal,"366_day") && strcasecmp(ccal,"366") &&
             strcasecmp(ccal,"360_day") && strcasecmp(ccal,"360") &&
             strcasecmp(ccal,"julian")  && strcasecmp(ccal,"none")) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"cd_calendar: the 'calendar' attribute (%s) is not equal to a recognized calendar. Returning all missing values.",ccal);
            return_all_missing = 1;
          }
        }
        if ((strcmp(attr_list->attname, "units")) == 0) {
          sspec = (NrmQuark *) attr_list->attvalue->multidval.val;
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }

/*
 * If no calendar attribute set, or "none" was selected, then use 
 * the default "standard".
 */
  if(ccal == NULL || !strcasecmp(ccal,"none")) {
    ctype = calendar_type("standard");
  }
  else {
    ctype = calendar_type(ccal);
  }

/*
 * Convert sspec to character string.
 */
  if(sspec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_calendar: no 'units' attribute provided");
    return(NhlFATAL);
  }
  cspec = NrmQuarkToString(*sspec);

/*
 * Calculate size of input array.
 */
  total_size_x = 1;
  for( i = 0; i < ndims_x; i++ ) total_size_x *= dsizes_x[i];

/*
 * Calculate size and dimensions for output array, and allocate
 * memory for output array.  The output size will vary depending
 * on what option the user has specified.  Only options -5 to 4
 * are currently recognized. (option = -4 doesn't exist.)
 */

  if(*option < -5 || *option > 4 || *option == -4) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"cd_calendar: Unknown option, defaulting to 0.");
        *option = 0;
  }

  if(*option == 0) {
        type_date   = NCL_float;
        type_date_t = nclTypefloatClass;
        total_size_date = 6 * total_size_x;
        missing_date    = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
        ndims_date      = ndims_x + 1;
        date            = (float *)calloc(total_size_date,sizeof(float));
  }
  else if(*option == -5) {
/* identical to option=0, except returns ints */
        type_date       = NCL_int;
        type_date_t     = nclTypeintClass;
        total_size_date = 6 * total_size_x;
        missing_date    = ((NclTypeClass)nclTypeintClass)->type_class.default_mis;
        ndims_date      = ndims_x + 1;
        date            = (int *)calloc(total_size_date,sizeof(int));
  }
  else if(*option >= 1 && *option <= 4) {
        type_date       = NCL_double;
        type_date_t     = nclTypedoubleClass;
        total_size_date = total_size_x;
        missing_date    = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
        ndims_date      = ndims_x;
        date            = (double *)calloc(total_size_date,sizeof(double));
  }
  else if(*option >= -3 && *option <= -1) {
        type_date       = NCL_int;
        type_date_t     = nclTypeintClass;
        total_size_date = total_size_x;
        missing_date    = ((NclTypeClass)nclTypeintClass)->type_class.default_mis;
        ndims_date      = ndims_x;
        date            = (int *)calloc(total_size_date,sizeof(int));
  }
  dsizes_date = (ng_size_t *)calloc(ndims_date,sizeof(ng_size_t));

/*
 * Make sure we have enough memory for output.
 */
  if( date == NULL || dsizes_date == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_calendar: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

/*
 * Calculate output dimension sizes.
 */
  for( i = 0; i < ndims_x; i++ ) dsizes_date[i] = dsizes_x[i];
  if(*option == 0 || *option == -5) {
        dsizes_date[ndims_x] = 6;
  }

/*
 * Coerce missing values to double.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/* 
 * If we reach this point and return_all_missing is not 0, then either
 * "units" was invalid or wasn't set, or "calendar" was not a
 * recoginized calendar. We return all missing values in this case.
 */
  if(return_all_missing) {
    set_all_missing(date, total_size_date, missing_date, type_date);
    ret = NclReturnValue(date,ndims_date,dsizes_date,
                          &missing_date,type_date,0);
    NclFree(dsizes_date);
    return(ret);
  }
            
/*
 * Convert input to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                  &missing_dx);

/* 
 * Loop through each element and get the 6 values.
 */
  index_date = 0;
  for( i = 0; i < total_size_x; i++ ) {
    if(!has_missing_x ||
       (has_missing_x && tmp_x[i] != missing_dx.doubleval)) {
      (void)cdRel2Iso_minsec(ctype,cspec,tmp_x[i],&comptime,&minute,&second);
/*
 * Return all missing values if we encounter a fatal error. 
 * Only check this once.
 */
      if(i == 0 && (cuErrorOccurred && (cuErrOpts & CU_FATAL))) {
        set_all_missing(date, total_size_date, missing_date, type_date);
        ret = NclReturnValue(date,ndims_date,dsizes_date,
                             &missing_date,type_date,0);
        NclFree(dsizes_date);
        return(ret);
      }
      year  = (int)comptime.year;
      month = (int)comptime.month;
      day   = (int)comptime.day;
/*
 * comptime.hour is a double, and fractional. The "minute" and "second"
 * above are calculated from the fractional part of the hour.
 */
      hour  = (int)comptime.hour;

/*
 * Calculate the return values, based on the input option.
 */
      switch(*option) {

      case 0:
        ((float*)date)[index_date]   = (float)year;
        ((float*)date)[index_date+1] = (float)month;
        ((float*)date)[index_date+2] = (float)day;
        ((float*)date)[index_date+3] = (float)hour;
        ((float*)date)[index_date+4] = (float)minute;
        ((float*)date)[index_date+5] = second;
        break;

/* identical to option=0, except returns ints */
      case -5:
        ((int*)date)[index_date]   = year;
        ((int*)date)[index_date+1] = month;
        ((int*)date)[index_date+2] = day;
        ((int*)date)[index_date+3] = hour;
        ((int*)date)[index_date+4] = minute;
        ((int*)date)[index_date+5] = (int)truncf(second);
        break;

/*
 * YYYYMM
 */
      case -1:
        ((int*)date)[index_date] = (100*year) + month;
        break;

      case 1:
        ((double*)date)[index_date] = (double)(100*year) + (double)month;
        break;
/*
 * YYYYMMDD
 */
      case -2:
        ((int*)date)[index_date] = (10000*year) + (100*month) + day;
        break;

      case 2:
        ((double*)date)[index_date] = (double)(10000*year)
          + (double)(100*month) 
          + (double)day;
        break;

/*
 * YYYYMMDDHH
 */
      case -3:
        ((int*)date)[index_date] = (1000000*year) + (10000*month) 
          + (100*day) + hour;                
        break;
                
      case 3:
        ((double*)date)[index_date] = (double)(1000000*year) 
          + (double)(10000*month) 
          + (double)(100*day)
          + (double)hour;             
        break;
                
/*
 *  YYYY.fraction_of_year
 */
      case 4:
        ((double*)date)[index_date] = (double)year + fraction_of_year(year,month,day,hour,minute,second,ccal);
        break;
      }
    }
    else {
      switch(*option) {

      case 0:
        ((float*)date)[index_date]   = missing_date.floatval;
        ((float*)date)[index_date+1] = missing_date.floatval;
        ((float*)date)[index_date+2] = missing_date.floatval;
        ((float*)date)[index_date+3] = missing_date.floatval;
        ((float*)date)[index_date+4] = missing_date.floatval;
        ((float*)date)[index_date+5] = missing_date.floatval;
        break;

/* identical to option=0, except returns ints */
      case -5:
        ((int*)date)[index_date]   = missing_date.intval;
        ((int*)date)[index_date+1] = missing_date.intval;
        ((int*)date)[index_date+2] = missing_date.intval;
        ((int*)date)[index_date+3] = missing_date.intval;
        ((int*)date)[index_date+4] = missing_date.intval;
        ((int*)date)[index_date+5] = missing_date.intval;
        break;

      case 1:
      case 2:
      case 3:
      case 4:
        ((double*)date)[index_date] = missing_date.doubleval;
        break;

      case -1:
      case -2:
      case -3:
        ((int*)date)[index_date] = missing_date.intval;
        break;
      }
    }
    if(*option == 0 || *option == -5) {
      index_date += 6;
    }
    else {
      index_date++;
    }
  }

/*
 * Free the work arrays.
 */

  if(type_x != NCL_double) NclFree(tmp_x);

/*
 * Set up variable to return.
 */
  if(has_missing_x) {
        return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            date,
                            &missing_date,
                            ndims_date,
                            dsizes_date,
                            TEMPORARY,
                            NULL,
                            type_date_t
                            );
  }
  else {
        return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            date,
                            NULL,
                            ndims_date,
                            dsizes_date,
                            TEMPORARY,
                            NULL,
                            type_date_t
                            );
  }

/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  dsizes[0] = 1;

/*
 * Return "calendar" attribute.
 *
 * We can't just return "scal" here, because it's an NCL input
 * parameter and this seems to screw things up if we try to
 * return it as an attribute.
 */
  calendar = (NclQuark*)NclMalloc(sizeof(NclQuark));
  if(ccal != NULL) {
    *calendar = NrmStringToQuark(ccal);
  }
  else {
    *calendar = NrmStringToQuark("standard");
  }
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)calendar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "calendar",
             att_md,
             NULL
             );

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

    NclFree(dsizes_date);
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}

/*
 * This function was updated in NCL V6.4.0 to allow
 * the y/m/d/h/m input to be any type other than
 * ints, and the output to be int, long, or float,
 * or double (only double was allowed previously).
 */
NhlErrorTypes cd_inv_calendar_W( void )
{
/*
 * Variables for handling NCL variables.
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
/*
 * Input array variables
 */
  void *year, *month, *day, *hour, *minute, *second;
  long *tmp_year = NULL;
  short *tmp_month = NULL, *tmp_day = NULL;
  double *tmp_hour = NULL, *tmp_minute = NULL, *tmp_second = NULL;
  NrmQuark *sspec;
  char *cspec;
  ng_size_t *dsizes_year, *dsizes_month, *dsizes_day, *dsizes_hour;
  ng_size_t *dsizes_minute, *dsizes_second;
  ng_size_t size_year, size_month, size_day, size_hour, size_minute, size_second;
  int has_missing_year,has_missing_month,has_missing_day,has_missing_hour;
  int has_missing_minute,has_missing_second;
  int ndims_year,ndims_month,ndims_day,ndims_hour,ndims_minute,ndims_second;
  NclScalar missing_year,missing_month, missing_day, missing_hour;
  NclScalar missing_minute, missing_second;
  NclBasicDataTypes type_year,type_month,type_day,type_hour,type_minute,type_second;
  NclTypeClass type_class_year, type_class_month, type_class_day, type_class_hour;
  NclTypeClass type_class_minute, type_class_second;
  cdCompTime comptime;
/*
 * Variables for retrieving attributes from last argument.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;
  NrmQuark *scal, *sret_type;
  char   *ccal = NULL, *cret_type;
/*
 * Output variables.
 */
  void *x;
  double *tmp_x;
  int has_missing_x;
  NclScalar missing_x;
  NclTypeClass type_x_class;
  NclBasicDataTypes type_x;
/*
 * Variables for returning "units" and "calendar" attributes.
 */
  NclQuark *units, *calendar;
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * various
 */
  int ret;
  ng_size_t i, total_size_input;
  ng_size_t dsizes[1], return_all_missing;
  cdCalenType ctype;
  double fraction;
  logical is_missing;

  /* initialize error flag */
  cuErrorOccurred = 0;

/*
 * Retrieve parameters. We have to do this the "hard" way because of 
 * handling types that might get coerced to "lower" values (double to 
 * integer for example).
 *
 * To make things easier, assign various aspects of multidval to
 * individual variables.
 */

/* Year */  
  data = _NclGetArg(0,8,DONT_CARE);
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
  year             = tmp_md->multidval.val;
  ndims_year       = tmp_md->multidval.n_dims;
  dsizes_year      = tmp_md->multidval.dim_sizes;
  type_year        = tmp_md->multidval.data_type;
  type_class_year  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_year)));
  size_year        = tmp_md->multidval.type->type_class.size;
  missing_year     = tmp_md->multidval.missing_value.value;
  has_missing_year = tmp_md->multidval.missing_value.has_missing;
/* Month */  
  data = _NclGetArg(1,8,DONT_CARE);
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
  month             = tmp_md->multidval.val;
  ndims_month       = tmp_md->multidval.n_dims;
  dsizes_month      = tmp_md->multidval.dim_sizes;
  type_month        = tmp_md->multidval.data_type;
  type_class_month  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_month)));
  size_month        = tmp_md->multidval.type->type_class.size;
  missing_month     = tmp_md->multidval.missing_value.value;
  has_missing_month = tmp_md->multidval.missing_value.has_missing;
/* Day */  
  data = _NclGetArg(2,8,DONT_CARE);
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
  day             = tmp_md->multidval.val;
  ndims_day       = tmp_md->multidval.n_dims;
  dsizes_day      = tmp_md->multidval.dim_sizes;
  type_day        = tmp_md->multidval.data_type;
  type_class_day  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_day)));
  size_day        = tmp_md->multidval.type->type_class.size;
  missing_day     = tmp_md->multidval.missing_value.value;
  has_missing_day = tmp_md->multidval.missing_value.has_missing;
/* Hour */  
  data = _NclGetArg(3,8,DONT_CARE);
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
  hour             = tmp_md->multidval.val;
  ndims_hour       = tmp_md->multidval.n_dims;
  dsizes_hour      = tmp_md->multidval.dim_sizes;
  type_hour        = tmp_md->multidval.data_type;
  type_class_hour  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_hour)));
  size_hour        = tmp_md->multidval.type->type_class.size;
  missing_hour     = tmp_md->multidval.missing_value.value;
  has_missing_hour = tmp_md->multidval.missing_value.has_missing;
/* Minute */  
  data = _NclGetArg(4,8,DONT_CARE);
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
  minute             = tmp_md->multidval.val;
  ndims_minute       = tmp_md->multidval.n_dims;
  dsizes_minute      = tmp_md->multidval.dim_sizes;
  type_minute        = tmp_md->multidval.data_type;
  type_class_minute  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_minute)));
  size_minute        = tmp_md->multidval.type->type_class.size;
  missing_minute     = tmp_md->multidval.missing_value.value;
  has_missing_minute = tmp_md->multidval.missing_value.has_missing;
/* Second */  
  data = _NclGetArg(5,8,DONT_CARE);
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
  second             = tmp_md->multidval.val;
  ndims_second       = tmp_md->multidval.n_dims;
  dsizes_second      = tmp_md->multidval.dim_sizes;
  type_second        = tmp_md->multidval.data_type;
  type_class_second  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_second)));
  size_second        = tmp_md->multidval.type->type_class.size;
  missing_second     = tmp_md->multidval.missing_value.value;
  has_missing_second = tmp_md->multidval.missing_value.has_missing;

/* Error checking */
  if(ndims_year != ndims_month || ndims_year != ndims_day    || 
     ndims_year != ndims_hour  || ndims_year != ndims_minute ||
     ndims_year != ndims_second) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: The first six arguments must have the same dimensionality");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_year; i++ ) {
    if(dsizes_year[i] != dsizes_month[i]  ||
       dsizes_year[i] != dsizes_day[i]    || 
       dsizes_year[i] != dsizes_hour[i]   || 
       dsizes_year[i] != dsizes_minute[i] ||
       dsizes_year[i] != dsizes_second[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: The first six arguments must have the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Get spec string.
 */
  sspec = (NrmQuark *)NclGetArgValue(
           6,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/* 
 * Check the option argument to see if it contains any
 * attributes.  The following attributes are supported:
 *    "calendar"
 *    "return_type" - Added in V6.4.0
 */
  return_all_missing = 0;
  type_x = NCL_double;
  stack_entry = _NclGetArg(7, 8, DONT_CARE);
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
 * att_id == -1 ==> no attributes specified args given.
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
        if ((strcmp(attr_list->attname, "calendar")) == 0) {
          scal = (NrmQuark *) attr_list->attvalue->multidval.val;
          ccal = NrmQuarkToString(*scal);
          if(strcasecmp(ccal,"standard") && strcasecmp(ccal,"gregorian") &&
             strcasecmp(ccal,"proleptic_gregorian") &&
             strcasecmp(ccal,"noleap")  && strcasecmp(ccal,"no_leap") &&
             strcasecmp(ccal,"allleap") && strcasecmp(ccal,"all_leap") &&
             strcasecmp(ccal,"365_day") && strcasecmp(ccal,"365") &&
             strcasecmp(ccal,"366_day") && strcasecmp(ccal,"366") &&
             strcasecmp(ccal,"360_day") && strcasecmp(ccal,"360") &&
             strcasecmp(ccal,"julian")  && strcasecmp(ccal,"none")) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"cd_inv_calendar: the 'calendar' attribute is not equal to a recognized calendar. Returning all missing values.");
            return_all_missing = 1;
          }
        }
        if ((strcmp(attr_list->attname, "return_type")) == 0) {
          sret_type = (NrmQuark *) attr_list->attvalue->multidval.val;
          cret_type = NrmQuarkToString(*sret_type);
          if(strcasecmp(cret_type,"double") && strcasecmp(cret_type,"float") &&
             strcasecmp(cret_type,"long") &&
             strcasecmp(cret_type,"integer") && strcasecmp(cret_type,"int")) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"cd_inv_calendar: the return_type attribute can only be set to 'double', 'float', 'long', or 'integer'.\nWill default to 'double'.");
          }
          else {
            if(!strcasecmp(cret_type,"double")) {
              type_x = NCL_double;
            }
            else if(!strcasecmp(cret_type,"float")) {
              type_x = NCL_float;
            }
            else if(!strcasecmp(cret_type,"long")) {
              type_x = NCL_long;
            }
            else if(!strcasecmp(cret_type,"integer") || !strcasecmp(cret_type,"int")) {
              type_x = NCL_int;
            }
          }
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }

/*
 * If no calendar attribute set, or "none" was selected, then use 
 * the default "standard".
 */
  if(ccal == NULL || !strcasecmp(ccal,"none")) {
    ctype = calendar_type("standard");
  }
  else {
    ctype = calendar_type(ccal);
  }

/*
 * Convert sspec to character string.
 */
  cspec = NrmQuarkToString(*sspec);

/*
 * Calculate total size of input arrays, and size and dimensions for
 * output array, and alloc memory for output array. As of NCL V6.4.0,
 * the output type can be double, float, long, or integer.
 */
  total_size_input = 1;
  for( i = 0; i < ndims_year; i++ ) total_size_input *= dsizes_year[i];

  if(type_x == NCL_double) {
    x = (double *)calloc(total_size_input,sizeof(double));
  }
  else if(type_x == NCL_float) {
    x = (float *)calloc(total_size_input,sizeof(float));
  }
  else if(type_x == NCL_long) {
    x = (long *)calloc(total_size_input,sizeof(long));
  }
  else if(type_x == NCL_int) {
    x = (int *)calloc(total_size_input,sizeof(int));
  }
  if( x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Create tmp array for holding double output values.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(1,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }

/*
 * x will contain a _FillValue attribute if there's bad input, or any of the input
 * has a _FillValue attribute set. return_all_missing is a flag to indicate whether
 * all values should be set to missing.
 */
  if(return_all_missing || has_missing_year || has_missing_month || has_missing_day ||
     has_missing_hour || has_missing_minute || has_missing_second) {
    has_missing_x = 1;
/*
 * Get the default missing value for the return type.
 */
    if(type_x == NCL_double) {
      missing_x = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
    }
    else if(type_x == NCL_float) {
      missing_x = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    }
    else if(type_x == NCL_long) {
      missing_x = ((NclTypeClass)nclTypelongClass)->type_class.default_mis;
    }
    else if(type_x == NCL_int) {
      missing_x = ((NclTypeClass)nclTypeintClass)->type_class.default_mis;
    }
  }
  else {
    has_missing_x = 0;
  }

  if(return_all_missing) {
    set_all_missing(x, total_size_input, missing_x, type_x);
    ret = NclReturnValue(x,ndims_year,dsizes_year,&missing_x,
                         type_x,0);
    if(type_x != NCL_double) NclFree(tmp_x);
    return(ret);
  }
/*
 * Create tmp arrays for coercing input arrays to the appropriate types.
 * Before NCL V6.4.0, year, month, day, hour, minute all had to be integers.
 */
  if(type_year != NCL_long) {
    tmp_year = (long*)calloc(1,sizeof(long));
    if(tmp_year == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for coercing year array to long");
      return(NhlFATAL);
    }
  }
  if(type_month != NCL_short) {
    tmp_month = (short*)calloc(1,sizeof(short));
    if(tmp_month == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for coercing month array to short");
      return(NhlFATAL);
    }
  }
  if(type_day != NCL_short) {
    tmp_day = (short*)calloc(1,sizeof(short));
    if(tmp_day == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for coercing day array to short");
      return(NhlFATAL);
    }
  }
  if(type_hour != NCL_double) {
    tmp_hour = (double*)calloc(1,sizeof(double));
    if(tmp_hour == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for coercing hour array to double");
      return(NhlFATAL);
    }
  }
  if(type_minute != NCL_double) {
    tmp_minute = (double*)calloc(1,sizeof(double));
    if(tmp_minute == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for coercing minute array to double");
      return(NhlFATAL);
    }
  }
  if(type_second != NCL_double) {
    tmp_second = (double*)calloc(1,sizeof(double));
    if(tmp_second == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cd_inv_calendar: Unable to allocate memory for coercing second array to double");
      return(NhlFATAL);
    }
  }

/* 
 * Loop through each data value, and call cdComp2Rel routine.
 */ 
  for( i = 0; i < total_size_input; i++ ) {
    is_missing = False;
/*
 * Check for missing values. If any of the input is missing, then 
 * the output should be set to missing for the same loop index. 
 */
    if(has_missing_year) {
       _Ncleq(type_class_year,&is_missing,&(((char*)year)[i*size_year]),&missing_year,NULL,NULL,1,1);
    }
    if(!is_missing && has_missing_month) {
       _Ncleq(type_class_month,&is_missing,&(((char*)month)[i*size_month]),&missing_month,NULL,NULL,1,1);
    }
    if(!is_missing && has_missing_day) {
       _Ncleq(type_class_day,&is_missing,&(((char*)day)[i*size_day]),&missing_day,NULL,NULL,1,1);
    }
    if(!is_missing && has_missing_hour) {
       _Ncleq(type_class_hour,&is_missing,&(((char*)hour)[i*size_hour]),&missing_hour,NULL,NULL,1,1);
    }
    if(!is_missing && has_missing_minute) {
       _Ncleq(type_class_minute,&is_missing,&(((char*)minute)[i*size_minute]),&missing_minute,NULL,NULL,1,1);
    }
    if(!is_missing && has_missing_second) {
       _Ncleq(type_class_second,&is_missing,&(((char*)second)[i*size_second]),&missing_second,NULL,NULL,1,1);
    }
    if(is_missing) {
      has_missing_x = 1;
      if(type_x == NCL_double) {
         ((double*)x)[i] = missing_x.doubleval;
      }
      else if(type_x == NCL_float) {
        ((float*)x)[i] = missing_x.floatval;
      }
      else if(type_x == NCL_long) {
        ((long*)x)[i] = missing_x.longval;
      }
      else if(type_x == NCL_int) {
        ((int*)x)[i] = missing_x.intval;
      }
      continue;
    }
/*
 * No missing values detected, so coerce year, month, day, hour, 
 * minute to the appropriate type.
 */
    if(type_year != NCL_long) {
      force_subset_input_long(year,tmp_year,i,type_year,1);
    }
    else {
      tmp_year = &((long*)year)[i];
    }
    if(type_month != NCL_short) {
      force_subset_input_short(month,tmp_month,i,type_month,1);
    }
    else {
      tmp_month = &((short*)month)[i];
    }
    if(type_day != NCL_short) {
      force_subset_input_short(day,tmp_day,i,type_day,1);
    }
    else {
      tmp_day = &((short*)day)[i];
    }
    if(type_hour != NCL_double) {
      coerce_subset_input_double(hour,tmp_hour,i,type_hour,1,
                                 has_missing_hour,&missing_hour,NULL);
    }
    else {
      tmp_hour = &((double*)hour)[i];
    }
    if(type_minute != NCL_double) {
      coerce_subset_input_double(minute,tmp_minute,i,type_minute,1,
                                 has_missing_minute,&missing_minute,NULL);
    }
    else {
      tmp_minute = &((double*)minute)[i];
    }
    if(type_second != NCL_double) {
      coerce_subset_input_double(second,tmp_second,i,type_second,1,
                                 has_missing_second,&missing_second,NULL);
    }
    else {
      tmp_second = &((double*)second)[i];
    }

    if(type_x == NCL_double) tmp_x = &((double*)x)[i];

    fraction       = *tmp_minute/60. + *tmp_second/3600.;
    comptime.year  = *tmp_year;
    comptime.month = *tmp_month;
    comptime.day   = *tmp_day;
    comptime.hour  = *tmp_hour + fraction;
    (void)cdComp2Rel(ctype,comptime,cspec,tmp_x);
/*
 * Return all missing values if we encounter a fatal error.
 */
    if(i == 0 && (cuErrorOccurred && (cuErrOpts & CU_FATAL))) {
      set_all_missing(x, total_size_input, missing_x, type_x);
      ret = NclReturnValue(x,ndims_year,dsizes_year,&missing_x,
                           type_x,0);
      if(type_year   != NCL_long)   NclFree(tmp_year);
      if(type_month  != NCL_short)  NclFree(tmp_month);
      if(type_day    != NCL_short)  NclFree(tmp_day);
      if(type_hour   != NCL_double) NclFree(tmp_hour);
      if(type_minute != NCL_double) NclFree(tmp_minute);
      if(type_second != NCL_double) NclFree(tmp_second);
      if(type_x      != NCL_double) NclFree(tmp_x);
      return(ret);
    }
/*
 * Copy output values from temporary tmp_x to x. The
 * only types possible are double, float, long, or int.
 * If double, then no coercion necessary.
 */
    if(type_x == NCL_float) {
      coerce_output_float_only(x,tmp_x,1,i);
    }
    if(type_x == NCL_long) {
      coerce_output_long_only(x,tmp_x,1,i);
    }
    else if(type_x == NCL_int) {
      coerce_output_int_only(x,tmp_x,1,i);
    }
  }

  if(type_year   != NCL_long)   NclFree(tmp_year);
  if(type_month  != NCL_short)  NclFree(tmp_month);
  if(type_day    != NCL_short)  NclFree(tmp_day);
  if(type_hour   != NCL_double) NclFree(tmp_hour);
  if(type_minute != NCL_double) NclFree(tmp_minute);
  if(type_second != NCL_double) NclFree(tmp_second);
  if(type_x      != NCL_double) NclFree(tmp_x);

/*
 * Set up variable to return.
 */
  type_x_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
  if(has_missing_x) {
       return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            x,
                            &missing_x,
                            ndims_year,
                            dsizes_year,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_x_class
                            );
  }
  else {
        return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            x,
                            NULL,
                            ndims_year,
                            dsizes_year,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_x_class
                            );
  }

/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  dsizes[0] = 1;

/*
 * Return "units" attribute.
 *
 * We can't just return "sspec" here, because it's an NCL input
 * parameter and this seems to screw things up if we try to
 * return it as an attribute.
 */
  units  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *units = NrmStringToQuark(cspec);

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)units,
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

/*
 * Return "calendar" attribute.
 *
 * We can't just return "sspec" here, because it's an NCL input
 * parameter and this seems to screw things up if we try to
 * return it as an attribute.
 */
  calendar = (NclQuark*)NclMalloc(sizeof(NclQuark));
  if(ccal != NULL) {
    *calendar = NrmStringToQuark(ccal);
  }
  else {
    *calendar = NrmStringToQuark("standard");
  }
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)calendar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "calendar",
             att_md,
             NULL
             );

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
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);

}

void set_all_missing(void *dt, ng_size_t total_size, 
                     NclScalar missing, NclBasicDataTypes type_dt)
{
  ng_size_t i;

  if(type_dt == NCL_double) {
    for(i = 0; i < total_size; i++ ) {
      ((double*)dt)[i] = missing.doubleval;
    }
  }
  else if(type_dt == NCL_float) {
    for(i = 0; i < total_size; i++ ) {
      ((float*)dt)[i] = missing.floatval;
    }
  }
  else if(type_dt == NCL_long) {
    for(i = 0; i < total_size; i++ ) {
      ((long*)dt)[i] = missing.longval;
    }
  }
  else if(type_dt == NCL_int) {
    for(i = 0; i < total_size; i++ ) {
      ((int*)dt)[i] = missing.intval;
    }
  }
}
