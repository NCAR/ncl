/*
 * print2d
 *
 * Print 2D numeric array data
 */

# include   <stdio.h>

/*
 * Required NCAR Graphics header files.
 * See: ${NCARG_ROOT}/include
 */
# include   "wrapper.h"

/*
 * FORTRAN function prototypes.  All character pointer arguments must have
 * a corresponding length argument passed to the FORTRAN function.  These
 * arguments follow the function's signature arguments (as type "int").
 *
 * There are three different functions, based on data type:
 *      NCL_int (short/int/long): FORTRAN type INTEGER -- print2di()
 *      NCL_float (float): FORTRAN type REAL -- print2df()
 *      NCL_double (double): FORTRAN type DOUBLE PRECISION -- print2dd()
 */

extern void NGCALLF(print2di, PRINT2DI)(char *, int *, int *, int *, char *,
                                        char *, int *, int *, int, int, int);

extern void NGCALLF(print2df, PRINT2DF)(char *, int *, int *, float *, char *,
                                        char *, int *, int *, int, int, int);

extern void NGCALLF(print2dd, PRINT2DD)(char *, int *, int *, double *, char *,
                                        char *, int *, int *, int, int, int);



NhlErrorTypes   print2d_W(void)
{
    /*
     * Input variables and placeholders
     */

    /* from NCL: filename, data output format, title */
    string  *fname,
            *fmtx,
            *t;

    /* row number option, title spaces option */
    int *rownumbers,
        *tspace;

    /* 2D rectangular array components */
    void    *data;
    int has_missing,
        dimsz[NCL_MAX_DIMENSIONS];
    NclScalar   missing;
    NclBasicDataTypes   dtype;

    /* functional character parameters */
    char    *filename,
            *format,
            *title;

    /*
     * Retrieve parameters.  Note that any of the pointer parameters
     * can be set to NULL, indicating you don't care about its value.
     */

    /* Parameter #1: data (2D rectangular array) */
    data = (void *) NclGetArgValue(
        0,
        6,
        NULL,
        dimsz,
        &missing,
        &has_missing,
        &dtype,
        0);

    /* Parameter #2: data print format */
    fmtx = (string *) NclGetArgValue(
        1,
        6,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);
    format = NrmQuarkToString(*fmtx);

    /* Parameter #3: output filename */
    fname = (string *) NclGetArgValue(
        2,
        6,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        2);
    filename = NrmQuarkToString(*fname);

    /* Parameter #4: title */
    t = (string *) NclGetArgValue(
        3,
        6,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);
    title = NrmQuarkToString(*t);

    /* Parameter #5: number of spaces to print before the title */
    tspace = (int *) NclGetArgValue(
        4,
        6,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);

    /* Parameter #6: option to print row numbers */
    rownumbers = (int *) NclGetArgValue(
        5,
        6,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);

    /* call FORTRAN function */
    switch (dtype) {
        case NCL_short:
            /* fall through */
        case NCL_int:
            /* fall through */
        case NCL_long:
            NGCALLF(print2di, PRINT2DI)(filename, &dimsz[0], &dimsz[1], data,
                    format, title, tspace, rownumbers,
                    strlen(filename), strlen(format), strlen(title));
            break;

        case NCL_float:
            NGCALLF(print2df, PRINT2DF)(filename, &dimsz[0], &dimsz[1], data,
                    format, title, tspace, rownumbers,
                    strlen(filename), strlen(format), strlen(title));
            break;

        case NCL_double:
            NGCALLF(print2dd, PRINT2DD)(filename, &dimsz[0], &dimsz[1], data,
                    format, title, tspace, rownumbers,
                    strlen(filename), strlen(format), strlen(title));
            break;

        default:
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "print2d: input data must be of numeric type");
            return NhlFATAL;
            break;
    }

    return NhlNOERROR;
}
