#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "wrapper.h"

/*
 * This is equivalent to the C version of tempnam.
 *
 * August 2003: replace call to C library function tempnam() with
 * call to C library function mkstemp().
 */
NhlErrorTypes tempnam_W(void)
{
    char    *dir,
            *prefix,
            return_name[255];
    int fid;

    NrmQuark *dname,
             *pname,
             *rname;
    ng_size_t ret_size = 1;

    /*
     * Retrieve directory name.
     */
    dname = (NrmQuark *) NclGetArgValue(
        0,
        2,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        DONT_CARE);

    /*
     * Retrieve prefix name.
     */
    pname = (NrmQuark *) NclGetArgValue(
        1,
        2,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        DONT_CARE);

    /*
     * Convert to character strings.
     */
    prefix = NrmQuarkToString(*pname);
    dir    = NrmQuarkToString(*dname);

    /*
     * Build a string like tempnam() would generate, guaranteeing a
     * writeable directory, and call the C library routine.
     */
    (void) sprintf(return_name, "/tmp/%sXXXXXX", prefix);
    fid = mkstemp(return_name);
    (void) close(fid);
    (void) unlink(return_name);

    rname  = (NrmQuark *) calloc(1, sizeof(NrmQuark));
    *rname = NrmStringToQuark(return_name);

    return NclReturnValue((void *) rname, 1, &ret_size,
                NULL, NCL_string, 0);
}

/*
 * This function provides a simplistic way of generating a unique
 * string name. Basically, it increments an integer counter so that
 * you are always assured of getting a unique number everytime you call
 * this function from the same NCL session. 
 */
NhlErrorTypes unique_string_W(void)
{
  char  *prefix, *return_string, tmp_string[20];
  NrmQuark *pname, *rname;
  int len, return_len, ret;
  ng_size_t ret_size = 1;
  static int counter = 0;

/*
 * Retrieve prefix name.
 */
   pname = (NrmQuark *) NclGetArgValue(
           0,
           1,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Convert prefix to character string.
 */
  prefix = NrmQuarkToString(*pname);
/*
 * Write integer counter out as a string.
 */
  len = sprintf(tmp_string,"%-d",counter);

  if(prefix != NULL && prefix[0] != '\0') {
    return_len    = strlen(prefix) + len + 1;
    return_string = (char *)calloc(return_len,sizeof(char));
    return_string = strncpy(return_string,prefix,strlen(prefix));
    return_string = strcat(return_string,tmp_string);
  }
  else {
    return_len    = len + 1;
    return_string = (char *)calloc(return_len,sizeof(char));
    strcpy(return_string,tmp_string);
  }

/*
 * Increment static counter.
 */
  counter++;

/*
 * Return.
 */
  rname  = (NrmQuark *) calloc(1,sizeof(NrmQuark));
  *rname = NrmStringToQuark(return_string);
  ret = NclReturnValue( (void *) rname, 1, &ret_size, NULL, NCL_string, 0);
  free(return_string);
  return(ret);
}
