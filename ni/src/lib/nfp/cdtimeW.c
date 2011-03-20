#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "wrapper.h"
#include "nctime.h"
#include <math.h>


extern int isleapyear(int);
extern int day_of_year (int, int, int);

NhlErrorTypes cdtime_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  string *sspec = NULL;
  char *cspec;
  int *option;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/* 
 * Variables for calculating fraction of year,  if the option is 4.
 */
  int nsid, doy, total_seconds_in_year, seconds_in_doy, seconds_in_hour;
  int seconds_in_minute; 
  double current_seconds_in_year, fraction_of_year;

/*
 * Variables for retrieving attributes from the first argument.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  string *scal;
  char   *ccal = NULL;
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
  int ret, return_missing;
  ng_size_t dsizes[1];
  ng_size_t i, total_size_x;
  ng_size_t total_size_date = 0;
  ng_size_t index_date;
  int months_to_days_fix=0, years_to_days_fix=0;
  extern float truncf(float);

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
  return_missing = 0;

  stack_entry = _NclGetArg(0, 2, DONT_CARE);
  switch (stack_entry.kind) {
  case NclStk_VAR:
    if (stack_entry.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
      if (attr_obj == NULL) {
        return_missing = 1;
        break;
      }
    }
    else {
/*
 * att_id == -1 ==> no attributes specified; return all missing.
 */
      return_missing = 1;
      break;
    }
/* 
 * Check for attributes. If none are specified, then return missing values.
 */
    if (attr_obj->att.n_atts == 0) {
      return_missing = 1;
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
          scal = (string *) attr_list->attvalue->multidval.val;
          ccal = NrmQuarkToString(*scal);
          if(strcasecmp(ccal,"standard") && strcasecmp(ccal,"gregorian") &&
             strcasecmp(ccal,"proleptic_gregorian") &&
             strcasecmp(ccal,"noleap") && strcasecmp(ccal,"no_leap") &&
             strcasecmp(ccal,"allleap") && strcasecmp(ccal,"all_leap") &&
             strcasecmp(ccal,"365_day") && strcasecmp(ccal,"365") &&
             strcasecmp(ccal,"366_day") && strcasecmp(ccal,"366") &&
             strcasecmp(ccal,"360_day") && strcasecmp(ccal,"360") &&
             strcasecmp(ccal,"julian")) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"cdtime: the 'calendar' attribute (%s) is not equal to a recognized calendar. Returning all missing values.",ccal);
            return_missing = 1;
          }
        }
        if ((strcmp(attr_list->attname, "units")) == 0) {
          sspec = (string *) attr_list->attvalue->multidval.val;
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }

/*
 * If no calendar attribute set, use the default "standard".
 */
  if(ccal == NULL) {
    ctype = calendar_type("standard");
  }
  else {
    ctype = calendar_type(ccal);
  }
/*
 * Convert sspec to character string.
 */
  if(sspec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cdtime: no 'units' attribute provided");
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
        NhlPError(NhlWARNING,NhlEUNKNOWN,"cdtime: Unknown option, defaulting to 0.");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cdtime: Unable to allocate memory for output arrays");
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
 * If we reach this point and return_missing is not 0, then either
 * "units" was invalid or wasn't set, or "calendar" was not a
 * recoginized calendar. We return all missing values in this case.
 */
  if(return_missing) {
        if(*option == 0) {
          for(i = 0; i < total_size_date; i++ ) {
                ((float*)date)[i] = missing_date.floatval;
          }
        }
        else if(*option == -5) {
/* identical to option=0, except returns ints */
          for(i = 0; i < total_size_date; i++ ) {
                ((int*)date)[i] = missing_date.intval;
          }
        }
        else if(*option >= 1 && *option <= 4) {
          for(i = 0; i < total_size_date; i++ ) {
                ((double*)date)[i] = missing_date.doubleval;
          }
        }
        else if(*option >= -3 && *option <= -1) {
          for(i = 0; i < total_size_date; i++ ) {
                ((int*)date)[i] = missing_date.intval;
          }
        }
/*
 * Return all missing values.
 */
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
        nsid             = 86400;      /* num seconds in a day */
        total_seconds_in_year  = isleapyear(year) ? 366*nsid : 365*nsid;
        doy                    = day_of_year(year,month,day);
        if(doy > 1) {
          seconds_in_doy = (doy-1) * nsid;
        }
        else {
          seconds_in_doy = 0;
        }
        if(hour > 1) {
          seconds_in_hour  = (hour-1) * 3600;
        }
        else {
          seconds_in_hour  = 0;
        }
        if(minute > 1) {
          seconds_in_minute  = (minute-1) * 60;
        }
        else {
          seconds_in_minute  = 0;
        }
        current_seconds_in_year = seconds_in_doy + 
          seconds_in_hour + 
          seconds_in_minute + 
          second;
        fraction_of_year = current_seconds_in_year/(double)total_seconds_in_year;
        ((double*)date)[index_date] = (double)year + fraction_of_year;
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

