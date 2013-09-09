#include <stdio.h>
#include <stdbool.h>
#include "KML_Funcs.h"
#include "wrapper.h"

NhlErrorTypes add_NCL_KML_Arrow_W(void) {
    /* Argument # 0 */
    NrmQuark * FileName;
    char * c_FileName;
    FileName = (NrmQuark *) NclGetArgValue(
            0,
            18,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_FileName = NrmQuarkToString(*FileName);
#ifdef DEBUG
    printf("KML File Name: %s\n", c_FileName);
#endif

    /* Argument # 1 */
    unsigned int *lat_stride;
    NclScalar missing_lat_stride;
    int ndims_lat_stride, has_missing_lat_stride;
    ng_size_t dsizes_lat_stride[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lat_stride;
    lat_stride = (unsigned int *) NclGetArgValue(
            1,
            18,
            &ndims_lat_stride,
            dsizes_lat_stride,
            &missing_lat_stride,
            &has_missing_lat_stride,
            &type_lat_stride,
            DONT_CARE);

    /* Argument # 2 */
    unsigned int *lon_stride;
    NclScalar missing_lon_stride;
    int ndims_lon_stride, has_missing_lon_stride;
    ng_size_t dsizes_lon_stride[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lon_stride;
    lon_stride = (unsigned int *) NclGetArgValue(
            2,
            18,
            &ndims_lon_stride,
            dsizes_lon_stride,
            &missing_lon_stride,
            &has_missing_lon_stride,
            &type_lon_stride,
            DONT_CARE);

    /* Argument # 3 */
    float *lat;
    NclScalar missing_lat;
    int ndims_lat, has_missing_lat;
    ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lat;
    lat = (float *) NclGetArgValue(
            3,
            18,
            &ndims_lat,
            dsizes_lat,
            &missing_lat,
            &has_missing_lat,
            &type_lat,
            DONT_CARE);

    /* Argument # 4 */
    float *lon;
    NclScalar missing_lon;
    int ndims_lon, has_missing_lon;
    ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lon;
    lon = (float *) NclGetArgValue(
            4,
            18,
            &ndims_lon,
            dsizes_lon,
            &missing_lon,
            &has_missing_lon,
            &type_lon,
            DONT_CARE);

    /* Argument # 5 */
    float *u;
    NclScalar missing_u;
    int ndims_u, has_missing_u;
    ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_u;
    u = (float *) NclGetArgValue(
            5,
            18,
            &ndims_u,
            dsizes_u,
            &missing_u,
            &has_missing_u,
            &type_u,
            DONT_CARE);

    /* Argument # 6 */
    float *v;
    NclScalar missing_v;
    int ndims_v, has_missing_v;
    ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_v;
    v = (float *) NclGetArgValue(
            6,
            18,
            &ndims_v,
            dsizes_v,
            &missing_v,
            &has_missing_v,
            &type_v,
            DONT_CARE);

    /* Argument # 7 */
    float *h;
    NclScalar missing_h;
    int ndims_h, has_missing_h;
    ng_size_t dsizes_h[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_h;
    h = (float *) NclGetArgValue(
            7,
            18,
            &ndims_h,
            dsizes_h,
            &missing_h,
            &has_missing_h,
            &type_h,
            DONT_CARE);

    /* Argument # 8 */
    unsigned int *cIndex;
    NclScalar missing_cIndex;
    int ndims_cIndex, has_missing_cIndex;
    ng_size_t dsizes_cIndex[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_cIndex;
    cIndex = (unsigned int *) NclGetArgValue(
            8,
            18,
            &ndims_cIndex,
            dsizes_cIndex,
            &missing_cIndex,
            &has_missing_cIndex,
            &type_cIndex,
            DONT_CARE);

    /* Argument # 9 */
    NrmQuark * BeginTimeStr;
    char * c_BeginTimeStr;
    BeginTimeStr = (NrmQuark *) NclGetArgValue(
            9,
            18,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_BeginTimeStr = NrmQuarkToString(*BeginTimeStr);

    /* Argument # 10 */
    NrmQuark * EndTimeStr;
    char * c_EndTimeStr;
    EndTimeStr = (NrmQuark *) NclGetArgValue(
            10,
            18,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_EndTimeStr = NrmQuarkToString(*EndTimeStr);

    /* Argument # 11 */
    float *ArrowScale;
    NclScalar missing_ArrowScale;
    int ndims_ArrowScale, has_missing_ArrowScale;
    ng_size_t dsizes_ArrowScale[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_ArrowScale;
    ArrowScale = (float *) NclGetArgValue(
            11,
            18,
            &ndims_ArrowScale,
            dsizes_ArrowScale,
            &missing_ArrowScale,
            &has_missing_ArrowScale,
            &type_ArrowScale,
            DONT_CARE);

    /* Argument # 12 */
    float *ArrowTipAngle;
    NclScalar missing_ArrowTipAngle;
    int ndims_ArrowTipAngle, has_missing_ArrowTipAngle;
    ng_size_t dsizes_ArrowTipAngle[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_ArrowTipAngle;
    ArrowTipAngle = (float *) NclGetArgValue(
            12,
            18,
            &ndims_ArrowTipAngle,
            dsizes_ArrowTipAngle,
            &missing_ArrowTipAngle,
            &has_missing_ArrowTipAngle,
            &type_ArrowTipAngle,
            DONT_CARE);

    /* Argument # 13 */
    float *ArrowTipScale;
    NclScalar missing_ArrowTipScale;
    int ndims_ArrowTipScale, has_missing_ArrowTipScale;
    ng_size_t dsizes_ArrowTipScale[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_ArrowTipScale;
    ArrowTipScale = (float *) NclGetArgValue(
            13,
            18,
            &ndims_ArrowTipScale,
            dsizes_ArrowTipScale,
            &missing_ArrowTipScale,
            &has_missing_ArrowTipScale,
            &type_ArrowTipScale,
            DONT_CARE);

    /* Argument # 14 */
    unsigned int *AltMode;
    NclScalar missing_AltMode;
    int ndims_AltMode, has_missing_AltMode;
    ng_size_t dsizes_AltMode[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_AltMode;
    AltMode = (unsigned int *) NclGetArgValue(
            14,
            18,
            &ndims_AltMode,
            dsizes_AltMode,
            &missing_AltMode,
            &has_missing_AltMode,
            &type_AltMode,
            DONT_CARE);

    /* Argument # 15 */
    logical * Extrude;
    bool c_Extrude;
    Extrude = (logical *) NclGetArgValue(15, 18, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Extrude = (*Extrude) ? true : false;


    /* Argument # 16 */
    logical * Tessellate;
    bool c_Tessellate;
    Tessellate = (logical *) NclGetArgValue(16, 18, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Tessellate = (*Tessellate) ? true : false;

    /* Argument # 17 */
    logical * Visible;
    bool c_Visible;
    Visible = (logical *) NclGetArgValue(17, 18, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Visible = (*Visible) ? true : false;


    /* Doing some tests */
    if (ndims_lat != 2) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_Arrow: lat, lon, u, v, h, and cIndex must be 2 dimensional array");
        return (NhlFATAL);
    }
    if (ndims_lat != ndims_lon ||
            ndims_lat != ndims_u ||
            ndims_lat != ndims_v ||
            ndims_lat != ndims_h ||
            ndims_lat != ndims_cIndex) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_Arrow: lat, lon, u, v, h, and cIndex must have same dimension");
        return (NhlFATAL);
    }

    if (dsizes_lat[0] != dsizes_lon[0] ||
            dsizes_lat[0] != dsizes_u[0] ||
            dsizes_lat[0] != dsizes_v[0] ||
            dsizes_lat[0] != dsizes_h[0] ||
            dsizes_lat[0] != dsizes_cIndex[0]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_Arrow: lat, lon, u, v, h, and cIndex must have same dimension");
        return (NhlFATAL);
    }

    if (dsizes_lat[1] != dsizes_lon[1] ||
            dsizes_lat[1] != dsizes_u[1] ||
            dsizes_lat[1] != dsizes_v[1] ||
            dsizes_lat[1] != dsizes_h[1] ||
            dsizes_lat[1] != dsizes_cIndex[1]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_Arrow: lat, lon, u, v, h, and cIndex must have same dimension");
        return (NhlFATAL);
    }

    /* getting some sizes */
    unsigned int nlat = dsizes_lat[0];
    unsigned int nlon = dsizes_lat[1];

    /* Opening the KML file to append to */
    if (NCL_OpenKMLFile(c_FileName) != 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_Arrow: Cannot access KML file.");
        return (NhlFATAL);
    }

    /* this is where everything happens */
    add_NCL_KML_Arrow_Kernel(nlat, nlon, *lat_stride, *lon_stride,
            lat, lon,
            u, v, h,
            cIndex,
            c_BeginTimeStr, c_EndTimeStr,
            *ArrowScale,
            *ArrowTipAngle, *ArrowTipScale,
            *AltMode,
            c_Extrude, c_Tessellate, c_Visible);

    /* closing the file */
    NCL_CloseKMLFile();

    return NhlNOERROR;
}
