/*
 * writematrix
 *
 * Write a 2D numeric matrix data
 */

# include   <stdio.h>
# include   "wrapper.h"

/*
 * FORTRAN function prototypes.  All character pointer arguments must have
 * a corresponding length argument passed to the FORTRAN function.  These
 * arguments follow the function's signature arguments (as type "int").
 *
 * There are three different functions, based on data type:
 *      NCL_int (short/int/long): FORTRAN type INTEGER -- writematrixi()
 *      NCL_float (float): FORTRAN type REAL -- writematrixf()
 *      NCL_double (double): FORTRAN type DOUBLE PRECISION -- writematrixd()
 */

extern void NGCALLF(writematrixi, WRITEMATRIXI)(char *, int *, int *, int *, char *,
                                        char *, int *, int *, int, int, int);

extern void NGCALLF(writematrixf, WRITEMATRIXF)(char *, int *, int *, float *, char *,
                                        char *, int *, int *, int, int, int);

extern void NGCALLF(writematrixd, WRITEMATRIXD)(char *, int *, int *, double *, char *,
                                        char *, int *, int *, int, int, int);



NhlErrorTypes   write_matrix_W(void)
{
    /*
     * Input variables and placeholders
     */

    /* from NCL: 2D rectangular matrix components */
    void    *data;
    int has_missing,
        dimsz[NCL_MAX_DIMENSIONS];
    NclScalar   missing;
    NclBasicDataTypes   data_type;

    /* from NCL: data output format */
    string  *fmtx;

    /* from NCL: option to specify output filename, title, title spaces, row numbers */
    logical *options;

    /* Optional arguments stack attributes (if specified) */
    NclAttList  *attr_list;
    NclAtt  attr_obj;
    NclStackEntry   stack_entry;

    /* Filename, title */
    string  *fname,
            *t;

    /* title spaces option */
    int tspace;

    /* rownumbers toggle */
    logical rownum;
    int rownumbers;

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
        3,
        NULL,
        dimsz,
        &missing,
        &has_missing,
        &data_type,
        0);

    /* Parameter #2: data print format */
    fmtx = (string *) NclGetArgValue(
        1,
        3,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);
    format = NrmQuarkToString(*fmtx);

    /* Parameter #3: optional argument specifier */
    options = (logical *) NclGetArgValue(
        2,
        3,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        0);

    /* Set default values; change them if user specifies */
    filename = "*";
    title = " ";
    tspace = 0;
    rownumbers = 0;

    /*
     * If optional parameters are provided, get them.  Grab (again)
     * the "options" parameter and look for attributes, if any.
     * If none are provided, use the defaults set above.
     */
    if (*options) {
        stack_entry = _NclGetArg(2, 3, DONT_CARE);
        switch (stack_entry.kind) {
            case NclStk_VAR:
                if (stack_entry.u.data_var->var.att_id != -1) {
                    attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
                    if (attr_obj == NULL) {
                        NhlPError(NhlWARNING, NhlENODATA,
                            "write_matrix: no attribute list, using defaults");
                        break;
                    }
                }
                else {
                    /* att_id == -1, no optional args given, defaults desired */
                    break;
                }

                /* get optional arguments;  if none specified, use defaults */
                if (attr_obj->att.n_atts == 0) {
                    NhlPError(NhlWARNING, NhlENODATA,
                        "write_matrix: no optional arguments, using defaults");
                    break;
                }
                else {
                    /* att_n_atts > 0, retrieve optional arguments */
                    attr_list = attr_obj->att.att_list;
                    while (attr_list != NULL) {
                        if ((strcmp(attr_list->attname, "fout")) == 0) {
                            fname = (string *) attr_list->attvalue->multidval.val;
                            filename = NrmQuarkToString(*fname);
                        }

                        if ((strcmp(attr_list->attname, "title")) == 0) {
                            t = (string *) attr_list->attvalue->multidval.val;
                            title = NrmQuarkToString(*t);
                        }

                        if ((strcmp(attr_list->attname, "tspace")) == 0) {
                            tspace = *(int *) attr_list->attvalue->multidval.val;
                        }

                        if ((strcmp(attr_list->attname, "row")) == 0) {
                            rownum = *(logical *) attr_list->attvalue->multidval.val;
                            if (rownum)
                                rownumbers = 1;
                        }

                        attr_list = attr_list->next;
                    }
                }

            default:
                break;
        }
    }


    /* call FORTRAN function */
    switch (data_type) {
        case NCL_short:
            /* fall through */
        case NCL_int:
            /* fall through */
        case NCL_long:
            NGCALLF(writematrixi, WRITEMATRIXI)(filename, &dimsz[1], &dimsz[0], data,
                    format, title, &tspace, &rownumbers,
                    strlen(filename), strlen(format), strlen(title));
            break;

        case NCL_float:
            NGCALLF(writematrixf, WRITEMATRIXF)(filename, &dimsz[1], &dimsz[0], data,
                    format, title, &tspace, &rownumbers,
                    strlen(filename), strlen(format), strlen(title));
            break;

        case NCL_double:
            NGCALLF(writematrixd, WRITEMATRIXD)(filename, &dimsz[1], &dimsz[0], data,
                    format, title, &tspace, &rownumbers,
                    strlen(filename), strlen(format), strlen(title));
            break;

        default:
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "write_matrix: input data must be of numeric type");
            return NhlFATAL;
            break;
    }

    return NhlNOERROR;
}
