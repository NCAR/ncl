#include <string.h>
#include <stdio.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/Workstation.h>
#include "wrapper.h"

NhlErrorTypes NhlGetNamedColorIndex_W( void )
{
  int i, j, ii, *ci, *wks, nid, total_cname_elements, total_wks_elements;
  NclHLUObj tmp_hlu_obj;
  string *cname;
  char *cval;
  int ndims_cname, dsizes_cname[NCL_MAX_DIMENSIONS];
  int ndims_wks, dsizes_wks[NCL_MAX_DIMENSIONS];
  int ndims_out, dsizes_out[NCL_MAX_DIMENSIONS];
  int ret_size = 1;	

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 *
 * Retrieve argument #1
 */
  wks   = (int*)NclGetArgValue(0,2,&ndims_wks,dsizes_wks,NULL,NULL,NULL,2);

/*
 * Retrieve argument #2
 */
  cname = (string *) NclGetArgValue(1,2,&ndims_cname,dsizes_cname,NULL,NULL,
									NULL,2);
/*
 * Compute total number of elements in wks array.
 */
  total_wks_elements = 1;
  for(i = 0; i < ndims_wks; i++) {
	total_wks_elements *= dsizes_wks[i];
  }
/*
 * Compute total number of elements in color name array.
 */
  total_cname_elements = 1;
  for(i = 0; i < ndims_cname; i++) {
	total_cname_elements *= dsizes_cname[i];
  }
  ci =  (int*)calloc(total_wks_elements*total_cname_elements*sizeof(int),1);
  
  ii = 0;
  for(i = 0; i < total_wks_elements; i++) {
	for(j = 0; j < total_cname_elements; j++) {
/*
 *  Determine the NCL identifier for the graphic object.
 */
	  tmp_hlu_obj = (NclHLUObj) _NclGetObj(wks[i]);
	  nid = tmp_hlu_obj->hlu.hlu_id;
	  ci[ii] = NhlGetNamedColorIndex(nid,NrmQuarkToString(cname[j]));
	  ii++;
	}
  }
/*   
 * If only one workstation has been given, then the number of dimensions
 * of the output is just equal to the dimensions of the colors inputted.
 */
  if(is_scalar(ndims_wks,dsizes_wks)) {
	ndims_out = ndims_cname;
	for( i = 0; i < ndims_cname; i++ ) {
	  dsizes_out[i] = dsizes_cname[i];
	}
  }
  else {
	ndims_out = ndims_cname + ndims_wks;
	for( i = 0; i < ndims_wks; i++ ) {
	  dsizes_out[i] = dsizes_wks[i];
	}
	for( i = 0; i < ndims_cname; i++ ) {
	  dsizes_out[i+ndims_wks] = dsizes_cname[i];
	}
  }
  return(NclReturnValue( (void *) ci, ndims_out, dsizes_out, NULL, NCL_int, 0));
}

