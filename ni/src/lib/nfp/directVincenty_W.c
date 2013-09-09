#include <stdio.h>
#include "DirectVincenty.h"
#include "wrapper.h"

NhlErrorTypes directVincenty_W(void) {
    /* Defining the arguments and Retrieving their values*/
    /* Argument # 0 */
    double *lat_in;
    NclScalar missing_lat_in;
    int ndims_lat_in, has_missing_lat_in;
    ng_size_t dsizes_lat_in[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lat_in;
    lat_in = (double *) NclGetArgValue(
            0,
            8,
            &ndims_lat_in,
            dsizes_lat_in,
            &missing_lat_in,
            &has_missing_lat_in,
            &type_lat_in,
            DONT_CARE);

    /* Argument # 1 */
    double *lon_in;
    NclScalar missing_lon_in;
    int ndims_lon_in, has_missing_lon_in;
    ng_size_t dsizes_lon_in[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lon_in;
    lon_in = (double *) NclGetArgValue(
            1,
            8,
            &ndims_lon_in,
            dsizes_lon_in,
            &missing_lon_in,
            &has_missing_lon_in,
            &type_lon_in,
            DONT_CARE);

    /* Argument # 2 */
    double *alpha;
    NclScalar missing_alpha;
    int ndims_alpha, has_missing_alpha;
    ng_size_t dsizes_alpha[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_alpha;
    alpha = (double *) NclGetArgValue(
            2,
            8,
            &ndims_alpha,
            dsizes_alpha,
            &missing_alpha,
            &has_missing_alpha,
            &type_alpha,
            DONT_CARE);

    /* Argument # 3 */
    double *s;
    NclScalar missing_s;
    int ndims_s, has_missing_s;
    ng_size_t dsizes_s[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_s;
    s = (double *) NclGetArgValue(
            3,
            8,
            &ndims_s,
            dsizes_s,
            &missing_s,
            &has_missing_s,
            &type_s,
            DONT_CARE);

    /* Argument # 4 */
    double *lat_out;
    NclScalar missing_lat_out;
    int ndims_lat_out, has_missing_lat_out;
    ng_size_t dsizes_lat_out[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lat_out;
    lat_out = (double *) NclGetArgValue(
            4,
            8,
            &ndims_lat_out,
            dsizes_lat_out,
            &missing_lat_out,
            &has_missing_lat_out,
            &type_lat_out,
            DONT_CARE);

    /* Argument # 5 */
    double *lon_out;
    NclScalar missing_lon_out;
    int ndims_lon_out, has_missing_lon_out;
    ng_size_t dsizes_lon_out[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lon_out;
    lon_out = (double *) NclGetArgValue(
            5,
            8,
            &ndims_lon_out,
            dsizes_lon_out,
            &missing_lon_out,
            &has_missing_lon_out,
            &type_lon_out,
            DONT_CARE);

    /* Argument # 6 */
    double *a;
    NclScalar missing_a;
    int ndims_a, has_missing_a;
    ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_a;
    a = (double *) NclGetArgValue(
            6,
            8,
            &ndims_a,
            dsizes_a,
            &missing_a,
            &has_missing_a,
            &type_a,
            DONT_CARE);

    /* Argument # 7 */
    double *f;
    NclScalar missing_f;
    int ndims_f, has_missing_f;
    ng_size_t dsizes_f[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_f;
    f = (double *) NclGetArgValue(
            7,
            8,
            &ndims_f,
            dsizes_f,
            &missing_f,
            &has_missing_f,
            &type_f,
            DONT_CARE);

    /* Checking the sizes */
    if (!(ndims_lat_in == ndims_lon_in &&
            ndims_lat_in == ndims_alpha &&
            ndims_lat_in == ndims_s &&
            ndims_lat_in == ndims_lat_out &&
            ndims_lat_in == ndims_lon_out)) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "directVincenty: The first six input arguments must of the same size");
        return (NhlFATAL);
    }

    long int nElements = 1;
    int i;
    for (i = 0; i < ndims_lat_in; i++) {
        nElements *= dsizes_lat_in[i];
    }

    double tmpLon, tmpLat;
    double d2r = atan(1.0) / 45.0;
    double r2d = 45.0 / atan(1.0);
    for (i = 0; i < nElements; i++) {
        if (lat_in[i] != missing_lat_out.doubleval) {
            DirectVincenty(lat_in[i] * d2r, lon_in[i] * d2r, alpha[i] * d2r, s[i], &tmpLat, &tmpLon, a[0], f[0]);
            lat_out[i] = tmpLat*r2d;
            lon_out[i] = tmpLon*r2d;
        } else {
            lat_out[i] = missing_lat_out.doubleval;
            lon_out[i] = missing_lon_out.doubleval;
        }
    }

    return NhlNOERROR;
}

