# include   <stdio.h>

/*
 * Required NCAR Graphics header files.
 * See: ${NCARG_ROOT}/include
 */
# include   "wrapper.h"

/*
 * FORTRAN function prototype.  All character pointer arguments must have
 * a corresponding length argument passed to the FORTRAN function.  These
 * arguments follow the function's signature arguments. 
 */
extern void NGCALLF(print2d, PRINT2D)(char *, int *, int *, double *, char *,
                                      char *, int *, int *, int, int, int);


NhlErrorTypes   print2d_W(void)
{
    /*
     * Input variables and placeholders
     */

    /* filename, data output format, title */
    string  *fname,
            *fmtx,
            *t;

    /* number of columns/rows of data, row number option */
    int *columns,
        *rows,
        *rownumbers,
        *tspace;

    /* data components: dimensions, missing data/value, type */
    int dimsz_cols[2],
        dimsz_rows[2];

    void    *data;

    int ndims,
        has_missing,
        dimsz[NCL_MAX_DIMENSIONS];

    NclScalar   missing;
    NclBasicDataTypes   dtype;

    /* converted data parameters */
    double  *data_dbl;
    int total_data_sz = 0;
    NclScalar   missing_data_dbl;

    /* functional character parameters */
    char    *filename,
            *format,
            *title;

    /*
     * Retrieve parameters.  Note that any of the pointer parameters
     * can be set to NULL, indicating you don't care about its value.
     */

    /* Parameter #1: output filename */
    fname = (string *) NclGetArgValue(
        0,
        8,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        2);
    filename = NrmQuarkToString(*fname);

    /* Parameter #2: number of data columns */
    columns = (int *) NclGetArgValue(
        1,
        8,
        NULL,
        dimsz_cols,
        NULL,
        NULL,
        NULL,
        0);

    /* Parameter #3: number of data rows */
    rows = (int *) NclGetArgValue(
        2,
        8,
        NULL,
        dimsz_rows,
        NULL,
        NULL,
        NULL,
        0);

    /* Parameter #4: data (2D rectangular array) */
    data = (void *) NclGetArgValue(
        3,
        8,
        &ndims,
        dimsz,
        &missing,
        &has_missing,
        &dtype,
        0);

    if (ndims < 2) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "print2d: input data must be 2-dimensional.");
        return NhlFATAL;
    }

    /* presently, all data is converted to type "double" */
    total_data_sz = dimsz[0] * dimsz[1];
    data_dbl = (double *) coerce_input_double(data, dtype, total_data_sz,
                                              has_missing, &missing,
                                              &missing_data_dbl);

    if (data_dbl == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "print2d: cannot allocate memory for data.");
        return NhlFATAL;
    }

    /* Parameter #5: data print format */
    fmtx = (string *) NclGetArgValue(
        4,
        8,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);
    format = NrmQuarkToString(*fmtx);

    /* Parameter #6: title */
    t = (string *) NclGetArgValue(
        5,
        8,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);
    title = NrmQuarkToString(*t);

    /* Parameter #7: number of spaces to print before the title */
    tspace = (int *) NclGetArgValue(
        6,
        8,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);

    /* Parameter #8: option to print row numbers */
    rownumbers = (int *) NclGetArgValue(
        7,
        8,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);

    /* call FORTRAN function */
    NGCALLF(print2d, PRINT2D)(filename, columns, rows, data_dbl,
            format, title, tspace, rownumbers,
            strlen(filename), strlen(format), strlen(title));

    /* clean up */
    if (data_dbl != (double *) NULL)
        NclFree(data_dbl);

    return NhlNOERROR;
}
