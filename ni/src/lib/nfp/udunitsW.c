#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"
#include <udunits2.h>
#include <udunits.h>
#include "utCalendar2_cal.h"
#include <math.h>

/*
 * Function for initializing Udunits-2 package.
 */

ut_system *utopen_ncl()
{
  ut_system *us;
  const char *path = NULL;
  char udunits_file[_NhlMAXFNAMELEN];

/*
 *  If NCARG_UDUNITS is set, then this directory is used to
 *  look for the "udunits2.xml" file.
 *
 *  Otherwise, the path within NCL ($NCARG_ROOT/lib/ncarg/udunits/)
 *  is used.
 *
 *  UDUNITS2_XML_PATH is not recognized internally by NCL. This is
 *  because it could be very common for the user to have this set
 *  for some other package, like Udunits, and this other file might
 *  not be compatible with what NCL expects.
 *
 */
  path = getenv("NCARG_UDUNITS");
  if ( (void *)path == (void *)NULL) {
    path = _NGGetNCARGEnv("udunits");
  }
  strcpy(udunits_file,path);
  strcat(udunits_file,_NhlPATHDELIMITER);
  strcat(udunits_file,"udunits2.xml");
/*
 * Internally set UDUNITS2_XML_PATH, forcing it to overwrite
 * any setting the user might have.
 */
  setenv("UDUNITS2_XML_PATH",udunits_file,1);

  /* Turn annoying "override" errors off */
  ut_set_error_message_handler( ut_ignore );

  /* Init udunits-2 lib */
  us = ut_read_xml(NULL);

  /* Turn errors back on */
  ut_set_error_message_handler( ut_write_to_stderr );

  return(us);
}

/*
 * Function for closing up Udunits-2.
 */

void *utclose_ncl(ut_system *us)
{
    ut_free_system(us);
    us = NULL;
}


char *fix_units_for_360_bug(char *ccal, char *cunits, int *montoday,
                            int *yrtoday)
{
  int clen;
  char *cunits_change;
  char unit_base_month[13], unit_base_year[12];

  if(ccal != NULL && (!strcasecmp(ccal,"360") || 
                      !strcasecmp(ccal,"360_day"))) {
    clen  = strlen(cunits);
/*
 * Get the first part of the units string to see if it is 
 * "months since" or "years since".
 */
    if(clen >= 12) {
      strncpy(unit_base_month,cunits,12);
      unit_base_month[12] = '\0';
    }
    else {
      unit_base_month[0] = '\0';
    }
    if(clen >= 11) {
      strncpy(unit_base_year,cunits,11);
      unit_base_year[11] = '\0';
    }
    else {
      unit_base_year[0] = '\0';
    }
    if((strcasecmp(unit_base_month,"months since")==0)) {
      *montoday = 1;
      cunits_change = (char*)calloc(clen-1,sizeof(char));
      strncpy(cunits_change,"days since",10);
      strncpy(&cunits_change[10],&cunits[12],clen-12);
      cunits_change[clen-2] = '\0';
      return(cunits_change);
    }

    if((strcasecmp(unit_base_year,"years since")==0)) {
      *yrtoday = 1;
      cunits_change = (char*)calloc(clen,sizeof(char));
      strncpy(cunits_change,"days since",10);
      strncpy(&cunits_change[10],&cunits[11],clen-11);
      cunits_change[clen-1] = '\0';
      return(cunits_change);
    }
  }
  return(cunits);
}

/*
 * The NCAR/CSM convention states the following:
 *
 *    The calendar calculations done by the udunits package use a mixed
 *    Gregorian/Julian calendar, i.e., dates prior to 1582-10-15 are
 *    Assumed to use the Julian calendar. Time coordinates that use
 *    other calendars are thus not able to make use of the udunits
 *    library for this purpose. However, it is still required to use the
 *    time unit format described above as this contains all the
 *    information required to make calendar calculations once the
 *    calendar has been specified. We describe a calendar attribute for
 *    the time coordinate variable below that may be used for this
 *    purpose.
 *
 * The ut_calendar function below depends on the udunits package,
 * with a Udunits enhancement provided by the David Pierce, the
 * developer of "ncview".
 *
 * The NCL function below checks for a "calendar" attribute. If
 * it is not present or has the values "standard" or "gregorian",
 * then this function will return the dates as returned
 * from the udunits package.
 *
 * If the "calendar" attribute is present and it has values "noleap",
 * "360_day", "360", "365_day", or "365", then the ncview's version
 * of utCalendar will be used.
 * 
 * For an unrecognized calendar like "kyr B.P." or "common_year",
 * _FillValue's will be returned.
 *
 */

NhlErrorTypes ut_calendar_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  string *sspec;
  char *cspec, *cspec_orig;
  int *option;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
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
/*
 * Variables for Udunits package.
 */
  ut_system *utopen_ncl(), *unit_system;
  ut_unit *utunit;
/*
 * Output variables.
 */
  int year, month, day, hour, minute;
  double second;
  void *date;
  int ndims_date, *dsizes_date;
  NclScalar missing_date;
  NclBasicDataTypes type_date;
  NclObjClass type_date_t;
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
  int i, ret, return_missing, dsizes[1];
  int total_size_x, total_size_date, index_date;
  int months_to_days_fix=0, years_to_days_fix=0;
  extern float truncf(float);
  ut_system *unitSystem;

/*
 * Before we do anything, initialize the Udunits package.
 */
  unit_system = utopen_ncl();

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
             strcasecmp(ccal,"noleap") && strcasecmp(ccal,"365_day") &&
             strcasecmp(ccal,"365") && strcasecmp(ccal,"360_day") && 
             strcasecmp(ccal,"360") ) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"ut_calendar: the 'calendar' attribute is not equal to a recognized calendar. Returning all missing values.");
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
 * Convert sspec to character string.
 */
  cspec = NrmQuarkToString(*sspec);

/*
 * There's a bug in utInvCalendar2_cal that doesn't handle the
 * 360-day calendar correctly if units are "years since" or
 * "months since".
 *
 * To fix this bug, we convert these units to "days since", do the
 * calculation as "days since", and then convert back to the original
 * "years since" or "months since" requested units.
 */
  cspec_orig = (char*)calloc(strlen(cspec)+1,sizeof(char));
  strcpy(cspec_orig,cspec);

  cspec = fix_units_for_360_bug(ccal,cspec,&months_to_days_fix,
                                &years_to_days_fix);
/*
 * Make sure cspec is a valid udunits string.
 */
  utunit = ut_parse(unit_system, cspec, UT_ASCII);
  if(utunit == NULL) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ut_calendar: Invalid specification string. Missing values will be returned.");
    return_missing = 1;
  }

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
        NhlPError(NhlWARNING,NhlEUNKNOWN,"ut_calendar: Unknown option, defaulting to 0.");
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
  dsizes_date = (int *)calloc(ndims_date,sizeof(int));

/*
 * Make sure we have enough memory for output.
 */
  if( date == NULL || dsizes_date == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_calendar: Unable to allocate memory for output arrays");
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
 * This is the bug fix for 360 day calendars and a units
 * of "years since" or "months since". We have to convert
 * from "years since" or "months since" to "days since".
 *
 * See above for more information about the bug.
 */
  if(years_to_days_fix == 1) {
    for(i = 0; i < total_size_x; i++ ) tmp_x[i] *= 360.;
  }
  if(months_to_days_fix == 1) {
    for(i = 0; i < total_size_x; i++ ) tmp_x[i] *= 30.;
  }


/* 
 * Loop through each element and get the 6 values.
 */
  index_date = 0;
  for( i = 0; i < total_size_x; i++ ) {
    if(!has_missing_x ||
       (has_missing_x && tmp_x[i] != missing_dx.doubleval)) {
      (void) utCalendar2_cal(tmp_x[i],utunit,&year,&month,&day,
                             &hour,&minute,&second,ccal);
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
 * Close up Udunits.
 */
  utclose_ncl(unit_system);

/*
 * Free extra units
 */
  NclFree(cspec_orig);

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

NhlErrorTypes ut_inv_calendar_W( void )
{
/*
 * Input array variables
 */
  int *year, *month, *day, *hour, *minute;
  void *second;
  double *tmp_second;
  string *sspec;
  int *option;
  char *cspec, *cspec_orig;
  int ndims_year,   dsizes_year[NCL_MAX_DIMENSIONS],   has_missing_year;
  int ndims_month,  dsizes_month[NCL_MAX_DIMENSIONS],  has_missing_month;
  int ndims_day,    dsizes_day[NCL_MAX_DIMENSIONS],    has_missing_day;
  int ndims_hour,   dsizes_hour[NCL_MAX_DIMENSIONS],   has_missing_hour;
  int ndims_minute, dsizes_minute[NCL_MAX_DIMENSIONS], has_missing_minute;
  int ndims_second, dsizes_second[NCL_MAX_DIMENSIONS], has_missing_second;
  NclScalar missing_year;
  NclScalar missing_month;
  NclScalar missing_day;
  NclScalar missing_hour;
  NclScalar missing_minute;
  NclScalar missing_second;
  NclBasicDataTypes type_second;
/*
 * Variables for Udunits package.
 */
  ut_system *utopen_ncl(), *unit_system;
  ut_unit *utunit;
/*
 * Variables for retrieving attributes from last argument.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;
  string *scal;
  char   *ccal = NULL;
/*
 * Output variables.
 */
  double *x;
  int has_missing_x;
  NclScalar missing_x;
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
  int i, total_size_input, dsizes[1], return_missing;
  int months_to_days_fix=0, years_to_days_fix=0;

/*
 * Before we do anything, initialize the Udunits package.
 */
  unit_system = utopen_ncl();

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 * The first size input arrays must be the same dimension sizes.
 */
  year = (int*)NclGetArgValue(
           0,
           8,
           &ndims_year, 
           dsizes_year,
           &missing_year,
           &has_missing_year,
           NULL,
           DONT_CARE);
  month = (int*)NclGetArgValue(
           1,
           8,
           &ndims_month, 
           dsizes_month,
           &missing_month,
           &has_missing_month,
           NULL,
           DONT_CARE);
  day = (int*)NclGetArgValue(
           2,
           8,
           &ndims_day, 
           dsizes_day,
           &missing_day,
           &has_missing_day,
           NULL,
           DONT_CARE);
  hour = (int*)NclGetArgValue(
           3,
           8,
           &ndims_hour, 
           dsizes_hour,
           &missing_hour,
           &has_missing_hour,
           NULL,
           DONT_CARE);
  minute = (int*)NclGetArgValue(
           4,
           8,
           &ndims_minute, 
           dsizes_minute,
           &missing_minute,
           &has_missing_minute,
           NULL,
           DONT_CARE);
  second = (void*)NclGetArgValue(
           5,
           8,
           &ndims_second, 
           dsizes_second,
           &missing_second,
           &has_missing_second,
           &type_second,
           DONT_CARE);

  if(ndims_year != ndims_month || ndims_year != ndims_day    || 
     ndims_year != ndims_hour  || ndims_year != ndims_minute ||
     ndims_year != ndims_second) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: The first six arguments must have the same dimensionality");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_year; i++ ) {
    if(dsizes_year[i] != dsizes_month[i]  ||
       dsizes_year[i] != dsizes_day[i]    || 
       dsizes_year[i] != dsizes_hour[i]   || 
       dsizes_year[i] != dsizes_minute[i] ||
       dsizes_year[i] != dsizes_second[i]) {
      
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: The first six arguments must have the same dimensionality");
      return(NhlFATAL);
    }
  }
/* 
 * x will contain a _FillValue attribute if any of the input
 * has a _FillValue attribute set.
 */
  if(has_missing_year || has_missing_month || has_missing_day ||
     has_missing_hour || has_missing_minute || has_missing_second) {
    has_missing_x = 1;
/*
 * Get the default missing value for a double type.
 */
    missing_x = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  }
  else {
    has_missing_x = 0;
  }
/*
 * Get spec string.
 */
  sspec = (string*)NclGetArgValue(
           6,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Get option.
 */
  option = (int*)NclGetArgValue(
           7,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);

/* 
 * Check the "option" variable to see if it contains a "calendar"
 * attribute.
 */
  return_missing = 0;

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
          scal = (string *) attr_list->attvalue->multidval.val;
          ccal = NrmQuarkToString(*scal);
          if(strcasecmp(ccal,"standard") && strcasecmp(ccal,"gregorian") &&
             strcasecmp(ccal,"noleap") && strcasecmp(ccal,"365_day") &&
             strcasecmp(ccal,"365") && strcasecmp(ccal,"360_day") && 
             strcasecmp(ccal,"360") ) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"ut_inv_calendar: the 'calendar' attribute is not equal to a recognized calendar. Returning all missing values.");
            return_missing = has_missing_x = 1;
          }
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }

/*
 * Convert sspec to character string.
 */
  cspec = NrmQuarkToString(*sspec);

/*
 * There's a bug in utInvCalendar2_cal that doesn't handle the
 * 360-day calendar correctly if units are "years since" or
 * "months since".
 *
 * To fix this bug, we convert these units to "days since", do the
 * calculation as "days since", and then convert back to the original
 * "years since" or "months since" requested units.
 */
  cspec_orig = (char*)calloc(strlen(cspec)+1,sizeof(char));
  strcpy(cspec_orig,cspec);

  cspec = fix_units_for_360_bug(ccal,cspec,&months_to_days_fix,
                                &years_to_days_fix);

/*
 * Make sure cspec is a valid udunits string.
 */
  utunit = ut_parse(unit_system, cspec, UT_ASCII);
  if(utunit == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: Invalid specification string");
    return(NhlFATAL);
  }

/*
 * Calculate total size of input arrays, and size and dimensions for
 * output array, and alloc memory for output array.
 */
  total_size_input = 1;
  for( i = 0; i < ndims_year; i++ ) total_size_input *= dsizes_year[i];

  x = (double *)calloc(total_size_input,sizeof(double));

  if( x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Create tmp array for coercing second to double if necessary.
 */
  if(type_second != NCL_double) {
    tmp_second = (double*)calloc(1,sizeof(double));
    if(tmp_second == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: Unable to allocate memory for coercing second array to double precision");
      return(NhlFATAL);
    }
  }

/* 
 * Loop through each data value, and call Udunits routine.
 */ 
  for( i = 0; i < total_size_input; i++ ) {
/*
 * Coerce "second" to double, since this is what the original Udunits
 * routine is expecting. 
 */
    if(type_second != NCL_double) {
      coerce_subset_input_double(second,tmp_second,i,type_second,1,
                                 has_missing_second,&missing_second,NULL);
    }
    else {
      tmp_second = &((double*)second)[i];
    }

    if(!return_missing && (!has_missing_year   ||
        (has_missing_year && year[i]       != missing_year.intval))   &&
       (!has_missing_month ||
         (has_missing_month && month[i]    != missing_month.intval))  &&
       (!has_missing_day ||
         (has_missing_day && day[i]        != missing_day.intval))    &&
       (!has_missing_hour ||
         (has_missing_hour  && hour[i]     != missing_hour.intval))   &&
       (!has_missing_minute ||
         (has_missing_minute && minute[i]  != missing_minute.intval)) &&
       (!has_missing_second ||
        (has_missing_second && *tmp_second != missing_second.doubleval)) ) {

       (void)utInvCalendar2_cal(year[i],month[i],day[i],hour[i],
                                minute[i],*tmp_second,utunit,&x[i],ccal);

/*
 * This is the bug fix for 360 day calendars and a units
 * of "years since" or "months since". We have to convert
 * from "days since" to the original requested units.
 *
 * See above for more information about the bug.
 */
       if(years_to_days_fix  == 1) x[i] /= 360.;
       if(months_to_days_fix == 1) x[i] /= 30.;
    }
    else {
      x[i]  = missing_x.doubleval;
    }
  }

/*
 * Close up Udunits.
 */
  utclose_ncl(unit_system);

/*
 * Set original units back if necessary.
 */
  if(months_to_days_fix || years_to_days_fix) {
    cspec = cspec_orig;
  }
  else {
    NclFree(cspec_orig);
  }

/*
 * Set up variable to return.
 */
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
                            (NclObjClass)nclTypedoubleClass
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
                            (NclObjClass)nclTypedoubleClass
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
