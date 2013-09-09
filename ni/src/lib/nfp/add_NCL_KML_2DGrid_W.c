#include <stdio.h>
#include <stdbool.h>
#include "KML_Funcs.h"
#include "wrapper.h"

NhlErrorTypes add_NCL_KML_2DGrid_W(void) {
    /* Argument # 0 */
    NrmQuark * FileName;
    char * c_FileName;
    FileName = (NrmQuark *) NclGetArgValue(
            0,
            9,
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
    float *lat;
    NclScalar missing_lat;
    int ndims_lat, has_missing_lat;
    ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lat;
    lat = (float *) NclGetArgValue(
            1,
            9,
            &ndims_lat,
            dsizes_lat,
            &missing_lat,
            &has_missing_lat,
            &type_lat,
            DONT_CARE);

    /* Argument # 2 */
    float *lon;
    NclScalar missing_lon;
    int ndims_lon, has_missing_lon;
    ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lon;
    lon = (float *) NclGetArgValue(
            2,
            9,
            &ndims_lon,
            dsizes_lon,
            &missing_lon,
            &has_missing_lon,
            &type_lon,
            DONT_CARE);

    /* Argument # 3 */
    float *h;
    NclScalar missing_h;
    int ndims_h, has_missing_h;
    ng_size_t dsizes_h[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_h;
    h = (float *) NclGetArgValue(
            3,
            9,
            &ndims_h,
            dsizes_h,
            &missing_h,
            &has_missing_h,
            &type_h,
            DONT_CARE);

    /* Argument # 4 */
    NrmQuark * StyleIDStr;
    char * c_StyleIDStr;
    StyleIDStr = (NrmQuark *) NclGetArgValue(
            4,
            9,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_StyleIDStr = NrmQuarkToString(*StyleIDStr);

    /* Argument # 5 */
    unsigned int *AltMode;
    NclScalar missing_AltMode;
    int ndims_AltMode, has_missing_AltMode;
    ng_size_t dsizes_AltMode[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_AltMode;
    AltMode = (unsigned int *) NclGetArgValue(
            5,
            9,
            &ndims_AltMode,
            dsizes_AltMode,
            &missing_AltMode,
            &has_missing_AltMode,
            &type_AltMode,
            DONT_CARE);

    /* Argument # 6 */
    logical * Extrude;
    bool c_Extrude;
    Extrude = (logical *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Extrude = (*Extrude) ? true : false;


    /* Argument # 7 */
    logical * Tessellate;
    bool c_Tessellate;
    Tessellate = (logical *) NclGetArgValue(7, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Tessellate = (*Tessellate) ? true : false;

    /* Argument # 8 */
    logical * Visible;
    bool c_Visible;
    Visible = (logical *) NclGetArgValue(8, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Visible = (*Visible) ? true : false;

    /* Doing some tests */
    if (ndims_lat != ndims_lon ||
            ndims_lat != ndims_h) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_2DGrid: lat, lon, u, v, h, and cIndex must have same dimension");
        return (NhlFATAL);
    }

    if (dsizes_lat[0] != dsizes_lon[0] ||
            dsizes_lat[0] != dsizes_h[0]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_2DGrid: lat, lon, u, v, h, and cIndex must have same dimension");
        return (NhlFATAL);
    }

    if (dsizes_lat[1] != dsizes_lon[1] ||
            dsizes_lat[1] != dsizes_h[1]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_2DGrid: lat, lon, u, v, h, and cIndex must have same dimension");
        return (NhlFATAL);
    }

    unsigned int nlat = dsizes_lat[0];
    unsigned int nlon = dsizes_lat[1];

    if (NCL_OpenKMLFile(c_FileName) != 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_2DGrid_W: Cannot access KML file.");
        return (NhlFATAL);
    }

    if (add_NCL_KML_2DGrid(nlat, nlon, lat, lon, h,
            c_StyleIDStr, *AltMode, c_Extrude, c_Tessellate, c_Visible) != 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_2DGrid_W: Cannot export grid lines.");
        return (NhlFATAL);
    }

    NCL_CloseKMLFile();

    return NhlNOERROR;

}